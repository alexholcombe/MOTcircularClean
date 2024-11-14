#expects datAnalyze, iv, factorsForBreakdownForAnalysis
if (!exists('factorsForBreakdownForAnalysis'))
{ cat('Need factorsForBreakdownForAnalysis variable. Tells me how to break down data for fitting. I will add subject') }
factorsForBreakdown<- factorsForBreakdownForAnalysis

source('helpers/psychometricHelpRobust6.R') #load my custom version of binomfit_lims

varyLapseRate = FALSE
#Set global variables needed by psychometricGgplotHelpRobust.R
if (varyLapseRate) { lapseMinMax= c(0,0.05) }  else  #range of lapseRates to try for best fit
	{ lapseMinMax = c(0.01,0.01) }
chanceRate=.5

xLims=c(.04,1.5);  if (iv=="tf") {xLims=c(.5,7)}
yLims=c(.3,1.05)
numPointsForPsychometricCurve=150 #250
#end global variables expected
verbosity=0 #0-don't print much debugging stuff, 1 prints more, and 2 even more

colrFactr = paste('factor(',factorsForBreakdown[1],')',sep='')
if ( length(factorsForBreakdown)>1 ) 
  shapeFactr = paste('factor(',factorsForBreakdown[2],')',sep='') else #can't break line before else
  { shapeFactr = colrFactr }
facetCols='subject' #'.'
facetRows='.'
if (length(factorsForBreakdown)>1)
  facetRows = factorsForBreakdown[2]
faceting=paste('~',facetCols,sep='')
factorsPlusSubject<-factorsForBreakdown
if (!"subject" %in% factorsForBreakdown) {
  factorsPlusSubject[ length(factorsForBreakdown)+1 ]<- "subject"
}

#Fit psychometric functions to data ########################################
initialMethod<-"glmCustomlink" #"brglm.fit"  # "glmCustomlink" #  

fitData <- function(df,groupvars,         iv,lapseMinMax,initialMethod,verbosity=0) {
  #data comes in one row per trial, but binomFit wants total correct, numTrials
  #so now I have to count number of correct, incorrect trials for each speed
  #assuming there's no other factors to worry about
  if (verbosity > -1) {
    cat("Finding best fit (calling fitParms) for")
    print(groupvars)
  }
  
  if (iv=="speed")
    sumry = plyr::ddply(df,plyr::.(speed),summarizNumTrials) #also calculates chance
  else if (iv=="tf")
    sumry = plyr::ddply(df,plyr::.(tf),summarizNumTrials) #also calculates chance
  #curveFit(sumry$speed,sumry$correct,sumry$numTrials,subjectname,lapsePriors,meanPriors,widthPriors,'MAPEstimation')  
  returnAsDataframe=TRUE #this allows keeping the text of the warning messages. (Boot can't do this)
  #cat("Calling fitBrglmKludge with sumry which should include chance:\n"); print(sumry) #debugON
  fitParms = fitBrglmKludge(sumry,lapseMinMax, returnAsDataframe,initialMethod,verbosity)
  return( fitParms )
}

datAnalyze$subject <- factor(datAnalyze$subject)

fitParms<- datAnalyze |> #filter(subject==32) |>
    group_by(  !!! syms(factorsPlusSubject)  ) |> #Send each subset of the data to curvefit
    #Take each group's condition variables as a tibble and add the results of the curve fit
    group_modify( fitData, iv,lapseMinMax,initialMethod,verbosity=-1)  

#Does this well now, using penalized.deviance to compare across lapse rates
#tempDat<- subset(dat,numObjects==2 & numTargets==1 & subject=="AH" ) 


#To-do. Change psychometrics myCurve to accommodate rescaling based on method
#       Stop setting global variables
#     Figure out way to pass method through to binomfit_limsAlex

psychometrics<- fitParms |>
  group_by(  !!! syms(factorsPlusSubject)  ) |> #Send each subset of the data to curvefit
  group_modify( plotCurve, iv,xLims[1],xLims[2],numPointsForPsychometricCurve )  #Take each group's parameters as a tibble and add the results of the curve fit

#Usually ggplot with stat_summary will collapse the data into means, but for some plots and analyses can't do it that way.
#Therefore calculate the means for later plotting
calcMeans<-function(df,groupVars) { #Surely this can be done with mutate, but maybe without the validation
  if ( !("correct" %in% names(df)) )
    warning("your dataframe must have a column named 'correct'",immediate.=TRUE)
  numCorrect<-sum(df$correct==1)
  numTrials= sum(complete.cases(df$correct))
  pCorr <- numCorrect/numTrials
  result= data.frame(pCorr)
  return(result)
}  
factorsPlusSubjectAndIv <- factorsPlusSubject
factorsPlusSubjectAndIv[ length(factorsPlusSubjectAndIv)+1 ] <- iv

#Collapse the binary data by calculating the mean for each subject*condition*speed
datMeans<- datAnalyze |> 
  group_by ( !!!syms(factorsPlusSubjectAndIv) ) |>
  group_modify( calcMeans )

calcPctCorrThisIvVal <- function(df,iv,val) {
  #Take dataframe with fitted psychometric function, 
  #where only one row tests this iv val, or none and must interpolate
  thisValIdx<- which(df[,iv]==val)
  if (length(thisValIdx) > 1) {
    stop('calcPctCorrThisSpeed passed a dataframe with more than one instance of speed s')
  }
  if (length(thisValIdx) ==1) {
    answer<- df$pCorr[thisValIdx] #equivalent to df[thisSpeedIdx,'pCorr']
  } else {  #This speed wasn't tested, so have to interpolate to estimate pCorr
    smallers <- which(df[,iv]<val)
    if (length(smallers)==0)
      stop(paste('IV val queried,',val,' is smaller than smallest val tested,',min(df[,iv])))
    closestSmaller<- max( df[smallers,iv] )
    largers <- which(df[,iv]>val)
    if (length(largers)==0)
      stop(paste('IV val queried,',val,' is larger than largest val tested,',max(df[,iv])))
    closestLarger<- min( df[largers,iv] )
    #calculate what fraction of the way s is to the larger
    fractionWayToLarger<- (val-closestSmaller)/(closestLarger-closestSmaller)
    largerPctCorr<- df$pCorr[ which(df[,iv]==closestLarger) ]
    smallerPctCorr<- df$pCorr[ which(df[,iv]==closestSmaller) ]
    answer<- smallerPctCorr + fractionWayToLarger*(largerPctCorr-smallerPctCorr)
    #print(paste('closestSmalledfr=',closestSmaller,'closestLarger=',closestLarger))
    #print(paste('fractionWayToLarger=',fractionWayToLarger,'largerPctCorr=',largerPctCorr,'smallerPctCorr=',smallerPctCorr,'answer=',answer))
  }
  return (answer)
}

cat(paste('I give you fitParms, psychometrics, datMeans and function calcPctCorrThisIvVal.'))
stopifnot(exists("fitParms"))
stopifnot(exists("psychometrics"))
stopifnot(exists("datMeans"))
stopifnot(exists("calcPctCorrThisIvVal"))

