
#When I used to use fixation report (summariseEyelinkReport.R) instead of EDF, it would examine each fixation event designated by Eyelink
# and check whether all the fixations for each trial is within the desired fixation zone.
#It also outputs whether the subject blinked on each trial.
library(stringr)
library(plyr); library(dplyr) #must be done in this order
library(eyelinkReader)
library(ggplot2)

EDFsummarise<- function(inputEDF,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix) {
  #
  #gaze$fixations$trial
  #gaze$fixations$
  #When the eye cannot be tracked (for example during blinks) null values (".") are returned for the gaze X,Y data, and the Pupil Size data is zero

  if (!file.exists(inputEDF)) {
    stop( paste0("ERROR no file ",inputEDF," exists") )
  }

  EDFstuff <- eyelinkReader::read_edf(inputEDF,
                                  import_samples = TRUE,
                                  sample_attributes = c('time', 'gx', 'gy'))

  #widthPix is width of screen. Used to calculate center of screen 
  #Eyelink reports eye position in pixels
  leftLimitPixel = widthPix/2 - centralZoneWidthPix/2
  rightLimitPixel = widthPix/2 + centralZoneWidthPix/2
  bottomLimitPixel = heightPix/2 + centralZoneHeightPix/2
  topLimitPixel = heightPix/2 - centralZoneHeightPix/2
  
  EDFstuff <- eyelinkReader::read_edf(inputEDF,
                                  import_samples = TRUE,
                                  sample_attributes = c('time', 'gx', 'gy'))
  
  if (length(EDFstuff)==0) {
    cat('Failure to read EDF file with eyelinkReader!')
  }
  # gaze$samples contains the x,y locations but before parsing into saccades, blinks
  
  #Check what proportion of the time the eye was lost completely
  #Can check this by looking at rate of samples being NA, or by ?
  table(is.na(EDFstuff$samples$gxR))
  
  #Go through every sample for all trials and indicate whether each event falls within the designated limits
  gazeLocatn <- EDFstuff$samples %>% 
    mutate(outOfCentralArea = (gxR < leftLimitPixel) | (gxR > rightLimitPixel)  ) %>%
    mutate(outOfCentralArea = as.numeric(outOfCentralArea)) #Change boolean to 0/1
  
  #Check for events$type=='LOST_DATA_EVENT'
  #Add a column for lostData event
  events<- EDFstuff$events %>% mutate(lostData = ifelse(type == "LOST_DATA_EVENT", TRUE, FALSE))
  if (any(events$lostData)) { #Check whether lost data ever occurred
    warning( "data was lost! A LOST_DATA_EVENT occurred somewhere in this EDF file")
  }
  
  #For each trial, need to go through all the events and calculate and return
  # number of blinks
  # total duration of blinks
  # whether any blinks were longer than 400ms
  # total time outside the designated area
  # number of fixations outside the designated area

  #BLINKS  
  blinks <- EDFstuff$blinks
  #Visualize outliers with histogram
  ggplot(blinks,aes(y=duration)) + geom_histogram() + ylim(-100,1000) 
  #Mark too-long blinks
  blinks<- blinks %>% mutate(tooLong = (duration>400))
  #Blinks can also be too short to be realistic, but I guess they could occur via person lowering their eyelid a bit and then re-opening,
  # especially because anytime it loses track of the eyes, it calls it a blink
  
  blinksPerTrial <- blinks %>% group_by(trial) %>% summarise(totalBlinkDur = sum(duration), tooLongs = sum(tooLong))
  
  #FIXATIONS
  fixatns <- EDFstuff$fixations
  #take the global average which if everything worked right will be near widthPix/2, heightPix/2
  avgFix<- fixatns %>% summarise(meanX = mean(gavx), meanY = mean(gavy))  - 
                    data.frame( meanX=widthPix/2,     meanY= heightPix/2)
  #Discrepancy with screen center
  avgFix - data.frame(meanX= avgFix$meanX - widthPix/2,   meanY = avgFix$meanY - heightPix/2)
  eachTrial <- gazeLocatn %>% group_by(trial) %>% summarise(outOfCentralArea = mean(outOfCentralArea, na.rm=T))
  
  #Should I do my own drift correction?
  #Can I merge the events with the samples to know# 
  eachTrial<-eachTrial

  proportnOutside<- gazeLocatn %>% summarise(outOfCentralArea = mean(outOfCentralArea, na.rm=T)) #Have to ignore NAs, which might be blinks

  proportnOutside<- proportnOutside$outOfCentralArea
  
  message("Proportion of samples for this participant that are outside the central zone =", proportnOutside)
  #Should probably use gavx and gavy
  
  eachTrial <- gazeLocatn %>% group_by(trial) %>% summarise(outOfCentralArea = mean(outOfCentralArea, na.rm=T))
  proportnTrialsOutside = as.numeric( (eachTrial$outOfCentralArea > 0) )
  msg = paste("Proportion of trials for this participant with any sample outside the central zone =", mean(proportnTrialsOutside))
  message(msg)
  
  #Save as a CSV file the variable of whether in each trial the person's eyes were ever outside the central zone
  library(readr)
  outputFilename = paste0( inputEDF, ".csv" )
  readr::write_excel_csv( eachTrial, outputFilename )
  
  return( eachTrial )
}

TESTME = TRUE #Unfortunately no equivalent in R of python __main__. Would have to use testhat I guess
if (TESTME) {

  #data(gaze) #to use built-in dataset
  EDF_exampleYoungOld <- file.path("dataForTestingOfCode", "A20b.EDF") # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"
  #file.exists
  
  widthPix = 1024
  heightPix = 768
  centralZoneHeightPix = 300 #10
  centralZoneWidthPix = 300 #10

  eachT <- EDFsummarise(EDF_exampleYoungOld, widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix)

}

VISUALIZE=TRUE
if (TESTME && VISUALIZE) {
  EDF_exampleYoungOld <- file.path("dataForTestingOfCode", "K041.EDF") # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"
  EDFstuff <- read_edf(EDF_exampleYoungOld) #,import_events=TRUE,import_recordings=FALSE
  trialnum = 2
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

sanityCheckEyeTracking=TRUE
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
