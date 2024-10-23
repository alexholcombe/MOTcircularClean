
#Check whether all the fixations for each trial is within the desired fixation zone.
#It also outputs whether the subject blinked on each trial.

#library(stringr)
#library(plyr); library(dplyr) #must be done in this order
library(tidyverse)
library(eyelinkReader)

EDFreadAndCheckResolution<- function(EDF_name,widthPix,heightPix) {
  
  #If first parameter passed came in various ways from tidyverse functions such as reframe, will be list
  if (typeof(EDF_name)=='list') { 
    # Reduce list to an unnamed simple variable
    EDF_name <- EDF_name[[1]]
  }
  if (!file.exists(EDF_name)) {
    stop( paste0("ERROR the EDF_name file ",EDF_name," does NOT exist") )
  }
  
  EDFstuff <- eyelinkReader::read_edf(EDF_name,
                                      import_saccades=FALSE,import_blinks=FALSE,import_recordings=FALSE,
                                      import_fixations = TRUE, import_events = TRUE,
                                      import_samples = TRUE,
                                      sample_attributes = c('time', 'gx', 'gy'),
                                      verbose=FALSE)
  if (length(EDFstuff)==0) {
    cat('Failure to read EDF file with eyelinkReader!')
  }
  
  #Try to find eyetracker message that says what it thinks the resolution of the screen is
  #widthPix is width of screen 
  #DISPLAY_COORDS is usually the first message
  first_message<- EDFstuff$events$message[1]
  # Split the string into an array based on whitespace
  message_parts<- stringr::str_split(first_message, "\\s+")[[1]]
  if (message_parts[1] == "DISPLAY_COORDS") { #What I'm expecting
    widthPix_according_to_eyetracker = message_parts[4]
    heightPix_according_to_eyetracker = message_parts[5]
    if (widthPix != widthPix_according_to_eyetracker) {
      message('Eyetracker thinks widthPix = ',widthPix_according_to_eyetracker,' not ',widthPix,' like you said.')
    }
    if (heightPix != heightPix_according_to_eyetracker) {
      message('Eyetracker thinks heightPix = ',heightPix_according_to_eyetracker,' not ',heightPix,' like you said.')
    }
  } else {
    message('Did not find DISPLAY_COORDS')
  }
  
  ##########################################################################################
  #Check which eye was tracked
  # EDFstuff$samples contains the x,y locations but before parsing into saccades, blinks
  samples<- EDFstuff$samples
  
  #Check which eye was tracked
  numNotNArightEye <- sum( !is.na(samples$gxR) )
  numNotNAleftEye <- sum( !is.na(samples$gxL) )
  if (numNotNArightEye>0 & numNotNAleftEye==0) {
    #message("The right eye was tracked exclusively.")
    samples$x <- samples$gxR
    samples$y <- samples$gyR
  }
  if (numNotNAleftEye>0 & numNotNArightEye==0) {
    #message("The left eye was tracked exclusively.")
    samples$x <- samples$gxL
    samples$y <- samples$gyL
  }
  if (numNotNAleftEye>0 & numNotNArightEye>0) {
    message("Sometimes the left eye was tracked and on others the right, and I'm not set up to handle that. You should check each trial.")
  }
  
  #Check whether average sample position very far from what expect to be screen center.
  screenCtrTold <-  data.frame( x=widthPix/2, y= heightPix/2)
  ridiculouslyFarFromCenterCriterion = 100
  avgSampleRelCtr<- samples |>
    summarise(x = mean(x,na.rm=TRUE), y = mean(y,na.rm=TRUE)) - 
    screenCtrTold
  
  if ( !is.nan(avgSampleRelCtr$x) & !is.nan(avgSampleRelCtr$y) ) {
    deviationLarge<- sqrt(avgSampleRelCtr$x^2 + avgSampleRelCtr$y^2)  > 
                     ridiculouslyFarFromCenterCriterion
  }
  else {
    deviationLarge<- FALSE
  }
  
  # #Function to check for any TRUE values but return FALSE if all are NaN, as will occur if there are
  # #  no values because the other eye was tracked or there is no data.
  # any_true_except_all_nan <- function(x) {
  #   if (all(is.nan(x))) {
  #     return(FALSE)
  #   } else {
  #     return(any(x, na.rm = TRUE))
  #   }
  # }
  # #Find out whether average position ()
  # deviationLarge<- any_true_except_all_nan( 
  #                     abs(avgSampleRelCtr) > ridiculouslyFarFromCenterCriterion      )
  
  #If average position large deviations from expected screen center, tell user.
  if ( deviationLarge ) { 
    msg = paste0("Average eye location should be near screen center (",widthPix/2,",",heightPix/2,") but")
    msg=paste0(msg,"for this participant, it's more than")
    msg=paste(msg,ridiculouslyFarFromCenterCriterion,"pixels from the center, on average the eye position relative to center was (x,y)")
    msg=paste0(msg,'(',round(avgSampleRelCtr$x,1),',',round(avgSampleRelCtr$y,1),')')
    msg=paste(msg,"This happens because the eyetracker or its calibration was no good, or the participant didn't look near center much.")
    message(msg)
  } else {
    if ( is.nan(avgSampleRelCtr$x) | is.nan(avgSampleRelCtr$y) ) {
      msg<- "There seems to be no samples in this file."
    } else {
      msg="Good! Average sample dx,dy="
      msg=paste0(msg,'(',round(avgSampleRelCtr$x,1),',',round(avgSampleRelCtr$y,1),')')
      msg=paste0(msg,' relative to believed widthPix,heightPix reasonable ')
      msg=paste0(msg,' because not very far from center')
      message(msg)
    } 
  }
  
  return (EDFstuff)
}

#################################################################################################
driftCorrect<- function(fixatns, intervalAssumeGazeCentral, maxDistToDriftCorrect) {
#Internal function for EDFsummarise
  
#Assume that fixatns$dx, fixatns$dy are the coordinates relative to screen center
  
#Use intervalAssumeGazeCentral by calculating the average sample position during that interval for each trial.
#Then subtract that from all the other samples during the trial.
#Should also probably add a check that that value isn't too far from the center according to the eyetracker, as that
#  would be a sign that probably the person did look away from there on that particular trial.
#A more sophisticated algorithm would calculate a running average across several trials to be more robust to eye movements occuring during individual trials.

#Have to use fixation coordinates, not samples because it doesn't have sttime_rel, it only has time_rel which is
#  since the beginning of the recording session so I'd have to get the time markers.

#A problem is that some trials have fixations that didn't start within the interval
#but did extend into the interval, so shouldn't just test against sttime_rel. 
# So a criterion to catch fixations that either start or end in the interval can be:
#  start time being in the interval, or end time being in the interval,
# That still won't catch fixations that span the entire interval. To catch those, have to test for: 
#   start time being less than interval start AND end time being greater than interval end

#Find all the fixations that started during, ended during, or spanned the intervalAssumeGazeCentral
samplesAssumeGazeCentral<- intervalAssumeGazeCentral

intervalFixatns<- fixatns %>% 
  filter( (sttime_rel > samplesAssumeGazeCentral[1] & sttime_rel < samplesAssumeGazeCentral[2])    |     #started during interval
          (entime_rel < samplesAssumeGazeCentral[2] & entime_rel > samplesAssumeGazeCentral[1]) |  #ended during interval
          (sttime_rel < samplesAssumeGazeCentral[1] &
           entime_rel > samplesAssumeGazeCentral[2]) #spanned the interval
        )

# Label each fixation based on when it occurred
intervalFixatns <- intervalFixatns %>%
  mutate(
    category = case_when(
      (sttime_rel > samplesAssumeGazeCentral[1]) & (entime_rel < samplesAssumeGazeCentral[2]) ~ "entirelyInInterval",
      (sttime_rel > samplesAssumeGazeCentral[1] & sttime_rel < samplesAssumeGazeCentral[2]) ~ "startedInInterval",
      (entime_rel < samplesAssumeGazeCentral[2] & entime_rel > samplesAssumeGazeCentral[1])  ~ "endedInInterval",
      sttime_rel < samplesAssumeGazeCentral[1] & entime_rel > samplesAssumeGazeCentral[2] ~ "spannedInterval",
      TRUE ~ NA_character_  # Default case if none of the conditions are met
    )
  )

#Calculate the average position during the intervalAssumeGazeCentral, to use as drift correction.
#However, not all fixations during intervalAssumeGazeCentral are as good as the others.
#When I plotted the different categories for one(?) participant with analyseIndividualPerson.qmd,
# I could see that the category of ending in the interval is no good, probably because 
# participants look elsewhere sometimes at very beginning and shift after this category fixation to start looking in the right place, 
#lots of large distFromFixatn values, whereas other categories are OK, although entirelyInInterval not great.
#So don't use the endedInInterval fixations for the drift correction.
avgPosDuringInterval <- intervalFixatns %>% filter(category != "endedInInterval") %>%
  group_by(trial) %>%
  summarise(dx=mean(dx), dy=mean(dy))

#During intervalAssumeGazeCentral, on some trials distance from fixation might be greater than maxDistToDriftCorrect
#  and the idea is to not use those for drift correction because the person probably(?) looking away from fixation then.
#Exclude those where dist > maxDistToDriftCorrect
avgPosDuringInterval$distFromFixatn <- sqrt( avgPosDuringInterval$dx^2 + avgPosDuringInterval$dy^2  )
#ggplot(avgPosDuringInterval,aes(x=dx,y=dy)) + geom_point()
avgDuringInterval <- avgPosDuringInterval %>% filter(distFromFixatn < maxDistToDriftCorrect)

#Some trials might not have a row at all
#Do the join first and then deal with that, by 
# expanding avgPosDuringInterval to provide one row per trial.
avgDuringInterval<- avgDuringInterval |> select(-distFromFixatn) #get rid of this column because there may also be one in fixatns
avgDuringInterval<-avgDuringInterval |> 
  rename( dxForCorrectn=dx, dyForCorrectn=dy )
#ggplot(avgDuringInterval,aes(x=dxForCorrectn)) + geom_histogram()
#ggplot(avgDuringInterval,aes(x=dxForCorrectn,y=dyForCorrectn)) + geom_point()

#Join
fixatnsAndCorrectn <- left_join(fixatns, avgDuringInterval, by=join_by(trial)) 

#Trials where there is no drift correction had no entry in avgDuringInterval,
#so need to set to 0,0 so no drift correction will happen
#So find all NAs and replace dxForCorrectn and dyForCorrectn with 0,0
fixatnsAndCorrectn<- fixatnsAndCorrectn %>% 
  mutate(dxForCorrectn = case_when( is.na(dxForCorrectn) ~ 0,
                                   TRUE ~ dxForCorrectn ),
         dyForCorrectn = case_when( is.na(dyForCorrectn) ~ 0,
                                   TRUE ~ dyForCorrectn ) )

#Check that avgPosDuringInterval has one row per trial.

#For each trial's fixations, subtract the drift correction result from the coordinates.
fixatnsAndCorrectn$dxCorrectd<- fixatnsAndCorrectn$dx - fixatnsAndCorrectn$dxForCorrectn
fixatnsAndCorrectn$dyCorrectd<- fixatnsAndCorrectn$dy - fixatnsAndCorrectn$dyForCorrectn
#ggplot(fixatnsAndCorrectn,aes(x=dxCorrectd,y=dyCorrectd)) + geom_point(color="green",alpha=0.5) +
#              geom_point( aes(x=dx,y=dy), color="black", alpha=0.4 ) #+xlim(-50,50)+ylim(-50,50)

# tt<- fixatnsAndCorrectn |> filter(sttime_rel > initialDurToExclude) |> 
#   group_by(trial) |> 
#   transmute( outOfCentralArea =  (abs(dx) >= centralZoneWidthPix/2)   |
#                (abs(dy) >= centralZoneWidthPix/2)   )
# cc<-fixatnsAndCorrectn |> filter(sttime_rel > initialDurToExclude) |> 
#   group_by(trial) |> 
#   transmute( outOfCentralArea =  (abs(dxCorrectd) >= centralZoneWidthPix/2)   |
#                (abs(dyCorrectd) >= centralZoneWidthPix/2)   )
# tt$outOfCentralArea == cc$outOfCentralArea

#Sanity-check by calculating the average position after drift correction, which should be very close to zero.
avgNewX <- round(mean(fixatnsAndCorrectn$dxCorrectd),1)
avgNewY <- round(mean(fixatnsAndCorrectn$dyCorrectd),1)
avgOldX <- round(mean(fixatnsAndCorrectn$dx),1)
avgOldY <- round(mean(fixatnsAndCorrectn$dy),1)
#Calculate how much drift correction helped, in terms of (assumed) distance from fixation
distReduction<-mean( sqrt(fixatnsAndCorrectn$dx^2 + fixatnsAndCorrectn$dy^2) -
  sqrt(fixatnsAndCorrectn$dxCorrectd^2 + fixatnsAndCorrectn$dyCorrectd^2) )
message("Drift correction reduced average fixation offset by ",round(distReduction,2),
        " pixels. Average x,y = ",avgNewX,",",avgNewY, "; before, =",avgOldX,",",avgOldY)
if ( abs(avgNewX) > 20   |  abs(avgNewY) > 20 ) {
  message("Because it's bigger than 15, it suggests the drift correction not effective or person didn't fixate")
}

return (fixatnsAndCorrectn)
}



#For testing
#EDFsummarise(inputEDF,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix,
#            initialDurToExclude,driftCorrect,intervalAssumeGazeCentral)
#EDF_name<-inputEDF; maxDistToDriftCorrect<- 50
#############################################################################################################
################################################################################################################
EDFsummarise<- function(EDF_name,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix,
                        initialDurToExclude,driftCorrect,intervalAssumeGazeCentral,
                        maxDistToDriftCorrect,plotQuartiles) {
  #############################################################################################################
  #In results, this will return the following variables which reflect all trials combined:
  #   pSamplesFailed , proportion of samples across all trials NA because eye couldn't be tracked, which includes blinks
  #   lostData, boolean, which indicates that at least at one point eyetracker completely failed
  
  #For each trial:
  #   number of blinks
  #   total duration of blinks
  # whether any blinks were longer than 400ms
  # total time outside the designated area
  # number of fixations outside the designated area
  results <- list()
  message("EDF_name=", EDF_name)
  
  EDFreadAndCheckResolution(EDF_name,widthPix,heightPix)

  EDFstuff <- eyelinkReader::read_edf(EDF_name,
                                      import_blinks=TRUE,
                                      import_saccades=FALSE,import_recordings=FALSE,
                                      import_fixations = TRUE, import_events = TRUE,
                                      import_samples = TRUE,
                                      sample_attributes = c('time', 'gx', 'gy'),
                                      verbose=FALSE)
  
  results$EDF_name<- EDF_name
  
  #If wanted to get samplingRate from the file, would need to search messages for the message that is like this: 
  #   "RECCFG CR 1000 1 0 R" , the 38th message in the file I looked at. EDFstuff$events$message[40]
  
  #Eyelink reports eye position in pixels
  leftLimitPixel = widthPix/2 - centralZoneWidthPix/2
  rightLimitPixel = widthPix/2 + centralZoneWidthPix/2
  bottomLimitPixel = heightPix/2 + centralZoneHeightPix/2
  topLimitPixel = heightPix/2 - centralZoneHeightPix/2

  # EDFstuff$samples contains the x,y locations but before parsing into saccades, blinks
  samples<- EDFstuff$samples
  
  #Check which eye was tracked
  numNotNArightEye <- sum( !is.na(samples$gxR) )
  numNotNAleftEye <- sum( !is.na(samples$gxL) )
  if (numNotNArightEye>0 & numNotNAleftEye==0) {
    #message("The right eye was tracked exclusively.")
    samples$x <- samples$gxR
    samples$y <- samples$gyR
  }
  if (numNotNAleftEye>0 & numNotNArightEye==0) {
    #message("The left eye was tracked exclusively.")
    samples$x <- samples$gxL
    samples$y <- samples$gyL
  }
  if (numNotNAleftEye>0 & numNotNArightEye>0) {
    message("Sometimes the left eye was tracked and on others the right, and I'm not set up to handle that. You should check each trial.")
  }
  
  ################################################################
  #Check what proportion of the time the eye was lost completely
  #When the eye cannot be tracked (for example during blinks) null values (".") are returned for the gaze X,Y data, and the Pupil Size data is zero
  #Can check this by looking at rate of samples being NA, or by ?
  proportnSamplesFailed <- samples |> summarise( proportnNA = mean( is.na(x) ) )
  #message('proportnSamplesFailed$proportnNA= ',round(mean(proportnSamplesFailed$proportnNA),3))
  results$pSamplesFailed <- proportnSamplesFailed$proportnNA
  
  #Check for events$type=='LOST_DATA_EVENT'
  events<- EDFstuff$events %>% mutate(dataLost = ifelse(type == "LOST_DATA_EVENT", TRUE, FALSE))
  results$anyDataLost <- FALSE
  if (any(events$dataLost)) { #Check whether lost data ever occurred
    warning( "data was lost! A LOST_DATA_EVENT occurred somewhere in this EDF file")
    results$anyDataLost <- TRUE
  }
    
  #Set duration after which care whether fixation was elsewhere
  
  ###################################################
  #BLINKS
  blinks <- EDFstuff$blinks
  #Visualize outliers with histogram
  #ggplot(blinks,aes(y=duration)) + geom_histogram() + ylim(-100,1000) 
  
  #Mark too-long blinks. Somebody (the Eyelink support person said >400ms is highly unlikely)
  blinksAfterDurToExclude<- blinks |> dplyr::filter(sttime_rel > initialDurToExclude)
  blinksAfterDurToExclude<-  blinksAfterDurToExclude |>  mutate(tooLong = (duration>400))
  #Blinks can also be too short to be realistic, but I guess they could occur via person lowering their eyelid a bit and then re-opening,
  # especially because anytime it loses track of the eyes, it calls it a blink

  blinksPerTrial <- blinksAfterDurToExclude |> group_by(trial) |>
                      summarise(totalBlinkDur = sum(duration), blinksTooLong = sum(tooLong))

  ##################################################
  #FIXATION ANALYSIS
  #Start trying to calculate whether eye was out of central zone too much
  #Go through every fixation for all trials and indicate whether each event falls within the designated limits
  fixatns <- EDFstuff$fixations
  fixatns$dx <- fixatns$gavx - widthPix/2
  fixatns$dy <- fixatns$gavy - heightPix/2
  fixatns$distFromFixatn = sqrt( fixatns$dx^2 + 
                                 fixatns$dy^2  )
  
  #Calculate max distFromFixatn each trial
  fixatnsTrialReport<- fixatns |> filter(sttime_rel > initialDurToExclude) |> 
                            group_by(trial) |> 
                            summarise(longestDist = max(distFromFixatn))
  #ggplot(fixatnsTrialReport, aes(x=longestDist)) + geom_histogram()
  if (plotQuartiles) {
    # Calculate quartiles
    quartiles <- quantile( unique(samples$trial) ) #divide up based on trials, not fixation number (which differs across trials)
    # Create the new column 'quartile'
    fixatns <- fixatns %>%
      mutate(quartile = cut(trial, breaks = quartiles, include.lowest = TRUE, labels = FALSE))
    
    avgFixatnPosEachQuartile<- fixatns |> group_by(quartile) |> 
      summarise(meanX = mean(dx, na.rm=T), meanY = mean(dy, na.rm=T))
    
    avgPlot<- ggplot(avgFixatnPosEachQuartile, aes(x= meanX, y= meanY, label=quartile))+  
      geom_point() +geom_text(hjust=0, vjust=0)
    #Add cross at center
    avgPlot<-avgPlot +
      geom_point(data=tibble(x=0,y=0),
                 aes(x=x,y=x,label=NULL),color="darkred",shape=3) 
    #Add title and set graph limits
    avgPlot<-avgPlot +  
      ggtitle('Average fixation position of each quartile', subtitle=', with red cross showing screen center') +
      xlim(-widthPix/2,widthPix/2) + ylim(-heightPix/2,heightPix/2) +
      theme_bw() + theme( panel.grid.minor=element_blank(),panel.grid.major=element_blank() )
    
    show(avgPlot)
  }
  #Calculate num fixations per trial outside of centralZoneWidthPix, centralZoneHeightPix
  #For some studies like Momo's, vertical eye position doesn't matter as much as horizontal
  outOfCentral<- fixatns |> filter(sttime_rel > initialDurToExclude) |> 
        group_by(trial) |> 
        transmute( outOfCentralArea =  (abs(dx) >= centralZoneWidthPix/2)   |
                                       (abs(dy) >= centralZoneHeightPix/2)          )
  #table(outOfCentral$outOfCentralArea)
  #Calculate num fixations outOfCentralArea for each trial
  outOfCentral<- outOfCentral |> group_by(trial) |>
        summarize( numOutOfCentralArea = sum(outOfCentralArea, na.rm=T) )
  outOfCentral$numOutOfCentralArea
  #ggplot(outOfCentral, aes(x=numOutOfCentralArea)) + geom_histogram()
  
  perTrialStuff <- fixatnsTrialReport |> full_join(outOfCentral, by="trial")

  #add blinks information, which likely has NAs, so we won't bother checking for NAs after
  perTrialStuff<- perTrialStuff |> full_join(blinksPerTrial, by="trial")

  #Do my own drift correction
  if (doDriftCorrect) {
    fixatnsDriftCorrected<- driftCorrect(fixatns, intervalAssumeGazeCentral,maxDistToDriftCorrect)
    #This should still have multiple fixations per trial.
    #Calculate which trials are outside central area on basis of drift-corrected coordinates rather than raw
    outOfCentralCorrected<- fixatnsDriftCorrected |> 
              filter(sttime_rel > initialDurToExclude) |> 
              group_by(trial) |> 
              transmute( outOfCentralAreaCorrected =  (abs(dxCorrectd) >= centralZoneWidthPix/2)   |
                                                      (abs(dyCorrectd) >= centralZoneHeightPix/2)          )
    #Reduce outOfCentralCorrected to one row per trial
    outOfCentralCorrected<- outOfCentralCorrected |> 
                            group_by(trial) |> 
                            summarise( numOutOfCentralAreaCorrected = sum(outOfCentralAreaCorrected) )
    #outOfCentralCorrected$numOutOfCentralAreaCorrected
    
    #Join corrected with uncorrected and then compare
    perTrialStuff<- perTrialStuff |> full_join(outOfCentralCorrected, by="trial")
    
    toThrowOutOriginal <- sum(perTrialStuff$numOutOfCentralArea > 0, na.rm=T)
    toThrowOutDriftCorrected<- sum(perTrialStuff$numOutOfCentralAreaCorrected > 0, na.rm=T)
    reductionTrialsToThrowOut<- toThrowOutOriginal - toThrowOutDriftCorrected
                                 
    message("Implementing drift correction avoided having to exclude ",reductionTrialsToThrowOut," trials, of ",
            toThrowOutOriginal, " otherwise to be thrown out, of ", nrow(perTrialStuff), " trials total." )
  } 

  results$perTrialStuff <- perTrialStuff
  
  return( results )
}

TESTME = TRUE #Unfortunately no equivalent in R of python __main__. Would have to use testhat I guess
if (TESTME) {

  EDF_name<- file.path("dataForTestingOfCode", "C55b.EDF") #C55b.EDF A421.EDF A20b.EDF Y481.EDF 
  
  widthPix = 800 #1024
  heightPix = 600 #768
  EDFstuff<- EDFreadAndCheckResolution(EDF_name,widthPix,heightPix)
  
  centralZoneHeightPix = 100 #10
  centralZoneWidthPix = 100 #10
  initialDurToExclude<- 2000 #analyseIndiv uses trialMinTimeBeforeCuesOff=2
  doDriftCorrect = FALSE; intervalAssumeGazeCentral <- c(300,800); maxDistToDriftCorrect<-30

  plotQuartiles<-TRUE #sanity check show meanX, mean Y
  doDriftCorrect = TRUE; intervalAssumeGazeCentral <- c(300,800); #analyseIndiv uses 300,800
  maxDistToDriftCorrect<-30 #analyseIndiv uses 30
  plotQuartiles<-FALSE #sanity check show meanX, mean Y
  
  #Do the calculations, returns answers both without and with drift correction if doDriftCorrect TRUE
  EDFsummary<- EDFsummarise(EDF_name,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix,
                            initialDurToExclude,doDriftCorrect,intervalAssumeGazeCentral,
                            maxDistToDriftCorrect,plotQuartiles)
  
  #Calculate proportion of trials with a fixation outOfCentralArea
  pOut <- EDFsummary$perTrialStuff |> 
        summarise( pOut = mean(numOutOfCentralArea>0, na.rm=TRUE), 
                   pOutDriftC = mean(numOutOfCentralAreaCorrected>0, na.rm=TRUE) ) 
  message("Proportion of trials with a fixation outside the central (",centralZoneWidthPix,",",
          centralZoneHeightPix,") pixel area without vs with drift correction:",
          round(pOut$pOut,3), ',', round(pOut$pOutDriftC,3))
  
  #Plot histogram of number out of central area
  #EDFsummary$perTrialStuff |> ggplot( aes(x=numOutOfCentralAreaCorrected)) + geom_histogram()
  
  #Plot number out of central area against trialnum
  EDFsummary$perTrialStuff |> ggplot( aes(x=trial,y=numOutOfCentralAreaCorrected) ) +
              geom_point(color="green",alpha=.5) + 
              geom_point( aes(x=trial,y=numOutOfCentralArea),color="black",alpha=.5 )
}

VISUALIZE=FALSE
if (TESTME && VISUALIZE) {
  EDF_exampleYoungOld <- file.path("dataForTestingOfCode", "K041.EDF") # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"
  EDFstuff <- read_edf(EDF_exampleYoungOld) #,import_events=TRUE,import_recordings=FALSE
  trialnum = 5
  # extracting fixations and saccades for the first trial
  fixations <- EDFstuff$fixations[EDFstuff$fixations$trial == trialnum, ]
  saccades <- EDFstuff$saccades[EDFstuff$saccades$trial == trialnum, ]

  commonScreenResolutions <- data.frame( widthPix = c(800,1024), 
                                         heightPix=c(600,768),
                                         resolution=c("800x600","1024x768"))
    
  #eyelinkReader:::plot.eyelinkRecording(gaze,trial=1)
  library(ggplot2)
  gg<- ggplot() + coord_fixed(ratio=1) +
    #coord_equal( xlim=c(0,widthPix), ylim=c(0,heightPix) ) + #this interferes with scale_y_reverse somehow
    # define screen limits and INVERSE y-axis to match Eyelink
    scale_x_continuous(name = "x", limits = gaze$display_coords[c(1, 3)]) +
    scale_y_reverse(name = "y", limits = gaze$display_coords[c(4, 2)]) +
    # draw fixations as circles
    geom_point(data = fixations, aes(x = gavx, y = gavy, size = duration), alpha=0.3) +
    # draw saccades as line segments
    geom_segment(data = saccades, aes(x = gstx, y = gsty, xend = genx, yend = geny, color = sttime_rel)) +
    # better legend titles
    labs(size = "Fixation duration [ms]", color = "Saccade onset [ms]") +
    geom_point(data=commonScreenResolutions,
               aes(x=widthPix/2,y=heightPix/2),color="darkred",shape=3) + 
    ggtitle('All fixations in the trial, with screen center in dark red')
  show(gg)
  
  #Calculate average across all trials
  avgFixatn<- EDFstuff$fixations %>% 
    summarise(meanX = mean(gavx), meanY = mean(gavy))
}

sanityCheckEyeTracking=FALSE
if (TESTME && sanityCheckEyeTracking) { #Calculate average fixation event location, should be widthPix/2, heightPix/2
  avgFix<- gaze$fixations %>% summarise(meanX = mean(gavx), meanY = mean(gavy))
  deviationFromScreenCenter <- avgFix  -    data.frame( meanX=widthPix/2, meanY= heightPix/2)
  if ( any(abs(deviationFromScreenCenter) >40) ) { #check if deviation from screen center of average fixation location is greater than 40 pixels
    msg = paste0("Average fixation location should be near screen center (",widthPix/2,",",heightPix/2,") but")
    msg=paste0(msg," instead it's (",round(avgFix$meanX,1),",",round(avgFix$meanY,1),") so")
    msg=paste(msg,"either your screen widthPix, heightPix are wrong, the eyetracker sucked, or participant didn't look near center much")
    print(msg)
  }
}
