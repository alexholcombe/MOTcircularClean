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

#fit psychometric functions to data ########################################
initialMethod<-"brglm.fit"  # "glmCustomlink" #  

getFitParms <- makeParamFit(iv,lapseMinMax,initialMethod,verbosity) #use resulting function for one-shot curvefitting

#Changed this to work with group_modify
getFitParmsPrintProgress <- function(df,groupvars) {  #So I can see which fits yielded a warning, print out what was fitting first.
  #cat("getFitParmsPrintProgress, df=");   print(df)
  dgg<<-df
  cat("Finding best fit (calling fitParms) for")
  print(groupvars)
  #for (i in 1:length(factorsPlusSubject) ) #Using a loop print them all on one line
  #  cat( paste( factorsPlusSubject[i],"=",df[1,factorsPlusSubject[i]])," " )
  return( getFitParms(df) )
}
datAnalyze$subject <- factor(datAnalyze$subject)

#Does this well now, using penalized.deviance to compare across lapse rates
#tempDat<- subset(dat,numObjects==2 & numTargets==1 & subject=="AH" ) 


#group_modify is the closest thing to deprecated ddply
#group_modify() replaces each group with the results of .f
#The first argument is passed .SD,the data.table representing the current group; the second argument is passed .BY, a list giving the current values of the grouping variables. The function should return a list or data.table.
#For a one parameter function, you can make it work with an anonymous function backslash trick
# datAnalyze |>
#   group_by(  !!! syms(factorsPlusSubject)  ) |>
#   group_modify(\(df, groupvars)  
#                                 as_tibble(is.na(df))  ) #This returns a dataframe with the result of is.na applied to each cell
# 
# datAnalyze |>
#   group_by(  !!! syms(factorsPlusSubject)  ) |>
#   group_modify(\(df, groupvars)  getFitParmsPrintProgress(df) )
# 

fitParms<- datAnalyze |>
  group_by(  !!! syms(factorsPlusSubject)  ) |> #Send each subset of the data to curvefit
  group_modify( getFitParmsPrintProgress )  #Take each group's parameters as a tibble and add the results of the curve fit

#To-do. Change psychometrics myCurve to accommodate rescaling based on method
#       Stop setting global variables
#     Figure out way to pass method through to binomfit_limsAlex



#prediction tracking two if only can track one. myPlotCurve then calculates it.
#use the fitted parameters to get the actual curves
myPlotCurve <- makeMyPlotCurve(iv,xLims[1],xLims[2]+.5,numPointsForPsychometricCurve)

psychometrics<- fitParms |>
  group_by(  !!! syms(factorsPlusSubject)  ) |> #Send each subset of the data to curvefit
  group_modify( myPlotCurve )  #Take each group's parameters as a tibble and add the results of the curve fit
#psychometrics<- plyr::ddply(fitParms,factorsPlusSubject,myPlotCurve)  

#Usually ggplot with stat_summary will collapse the data into means, but for some plots and analyses can't do it that way.
#Therefore calculate the means
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
