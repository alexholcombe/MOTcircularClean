
#Check whether all the fixations for each trial is within the desired fixation zone.
#It also outputs whether the subject blinked on each trial.

#library(stringr)
#library(plyr); library(dplyr) #must be done in this order
library(tidyverse)
library(eyelinkReader)

EDFsummarise<- function(inputEDF,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix,
                        initialDurToExclude,driftCorrect,intervalToAssumeGazeCentral) {
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
  
  #If first parameter passed came in various ways from tidyverse functions such as reframe, will be list
  if (typeof(inputEDF)=='list') { 
    # Reduce list to an unnamed simple variable
    inputEDF <- inputEDF[[1]]
  }
  if (!file.exists(inputEDF)) {
    stop( paste0("ERROR the inputEDF file ",inputEDF," does NOT exist") )
  }

  EDFstuff <- eyelinkReader::read_edf(inputEDF,
                                  import_samples = TRUE,
                                  sample_attributes = c('time', 'gx', 'gy'))

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
  } 
  
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
  proportn_na <- samples |> summarise( proportnNA = mean( is.na(x) ) )
  results$pSamplesFailed <- proportn_na
  
  ##########################################################################################
  #Check how often eye ridiculously far from screen center, suggesting something went wrong
  screenCtr <-  data.frame( x=widthPix/2, y= heightPix/2)
  ridiculouslyFarFromCenterCriterion = 100
  avgSampleRelCtr<- samples |>
    summarise(x = mean(x,na.rm=TRUE), y = mean(y,na.rm=TRUE)) - screenCtr
  
  #Function to check for any TRUE values but return FALSE if all are NaN, as will occur if there are
  #  no values because the other eye was tracked or there is no data.
  any_true_except_all_nan <- function(x) {
    if (all(is.nan(x))) {
      return(FALSE)
    } else {
      return(any(x, na.rm = TRUE))
    }
  }
  deviationLarge<- any_true_except_all_nan( abs(avgSampleRelCtr) > ridiculouslyFarFromCenterCriterion )
  
  if ( deviationLarge ) { #check if deviation from screen center of average fixation location is greater than ridiculouslyFarFromCenterCriterion
    msg = paste0("Average eye location should be near screen center (",widthPix/2,",",heightPix/2,") but")
    msg=paste0(msg,"for this participant, it's more than")
    msg=paste(msg,ridiculouslyFarFromCenterCriterion,"pixels from the center, on average the eye position relative to center was (x,y)")
    msg=paste0(msg,'(',round(avgSampleRelCtr$x,1),',',round(avgSampleRelCtr$y,1),')')
    msg=paste(msg,"This happens because the eyetracker or its calibration was no good, or the participant didn't look near center much.")
    message(msg)
  } else {
    msg="Average eye position was pretty close to screen center, deviated on average: (x,y) "
    msg=paste0(msg,'(',round(avgSampleRelCtr$x,1),',',round(avgSampleRelCtr$y,1),')')
    #message(msg)
  }
  
  #Check for events$type=='LOST_DATA_EVENT'
  events<- EDFstuff$events %>% mutate(lostData = ifelse(type == "LOST_DATA_EVENT", TRUE, FALSE))
  results$lostData <- FALSE
  if (any(events$lostData)) { #Check whether lost data ever occurred
    warning( "data was lost! A LOST_DATA_EVENT occurred somewhere in this EDF file")
    results$lostData <- TRUE
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
  fixatnsTrialReport<- fixatns |> filter(sttime_rel > initialDurToExclude) |> group_by(trial) |> 
                            summarise(longestDist = max(distFromFixatn))
  
  #ggplot(fixatnsTrialReport, aes(x=longestDist)) + geom_histogram()
  
  #Calculate num fixations per trial outside of centralZoneWidthPix, centralZoneHeightPix
  #For some studies like Momo's, vertical eye position doesn't matter as much as horizontal
  outOfCentral<- fixatns |> filter(sttime_rel > initialDurToExclude) |> 
        group_by(trial) |> 
        transmute( outOfCentralArea =  (abs(dx) >= centralZoneWidthPix/2)   |
                                    (abs(dy) >= centralZoneWidthPix/2)          )
  #table(outOfCentral$outOfCentralArea)
  #Calculate num fixations outOfCentralArea for each trial
  outOfCentral<- outOfCentral |> group_by(trial) |>
        summarize( outOfCentralArea = sum(outOfCentralArea, na.rm=T) )
  #ggplot(outOfCentral, aes(x=outOfCentralArea)) + geom_histogram()
  
  perTrialStuff <- fixatnsTrialReport |> full_join(outOfCentral, by="trial")
  #Make sure there are no NAs
  # Check if there are any NAs in any column of perTrialStuff
  any_NA_in_any_column <- any(   apply(perTrialStuff, 2,   function(col) any(is.na(col)))    )
  if (any_NA_in_any_column) {
    message("There are NA values in one or more columns of perTrialStuff, suggesting the columns didn't line up right.")
  }

  #add blinks information, which likely has NAs, so we won't bother checking for NAs after
  perTrialStuff<- perTrialStuff |> full_join(blinksPerTrial, by="trial")
  
  #Do my own drift correction
  #intervalToAssumeGazeCentral
  #Not imnplemented yet, copy it from analyseIndividualPerson.qmd
  
  results$perTrialStuff <- perTrialStuff
  
  return( results )
}

TESTME = FALSE #Unfortunately no equivalent in R of python __main__. Would have to use testhat I guess
if (TESTME) {

  #data(gaze) #to use built-in dataset
  inputEDF<- file.path("dataForTestingOfCode", "A20b.EDF") # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"
  
  widthPix = 800 #1024
  heightPix = 600 #768
  centralZoneHeightPix = 300 #10
  centralZoneWidthPix = 300 #10
  initialDurToExclude<- 1
  driftCorrect = F; intervalToAssumeGazeCentral <- c(.1,.9)
  
  EDFsummary<- EDFsummarise(inputEDF,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix,
                          initialDurToExclude,driftCorrect,intervalToAssumeGazeCentral)
  #Check proportion of trials with a fixation outOfCentralArea
  pOut <- EDFsummary$perTrialStuff |> summarise(pOut = mean(outOfCentral>0))
  message("Proportion of trials with a fixation outside the central area =",round(pOut,1))
  
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
