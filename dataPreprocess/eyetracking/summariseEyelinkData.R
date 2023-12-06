#To get the eyetracking file that this file processes,
#Chris opens the EDF file created by Eyelink in Eyelink DataViewer
#He instructs the software to print out the following fields: 
#RECORDING_SESSION_LABEL 	TRIAL_LABEL	CURRENT_FIX_BLINK_AROUND	CURRENT_FIX_X
#E.g.,
#RECORDING_SESSION_LABEL	TRIAL_LABEL	CURRENT_FIX_BLINK_AROUND	CURRENT_FIX_X
#LN_20Apr2015_14-22	Trial: 1	NONE	403.80
#LN_20Apr2015_14-22	Trial: 2	AFTER	400.00

#The job of this function, eyelinkReportSummarise, is to examine each fixation event designated by Eyelink and check whether
#all the fixations for each trial is within the desired fixation zone.
#It also outputs whether the subject blinked on each trial.
library(stringr)
library(plyr); library(dplyr) #must be done in this order

eyelinkReportSummarise<- function(inputFilename,df,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix) {
  #df is dataframe gotten by using read.table to read in the Eyelink DataViewer report
  #trial num should be contained in either TRIAL_LABEL or TRIAL_INDEX
  #widthPix is width of screen. Used to calculate center of screen 
  #Eyelink reports eye position in pixels
  leftLimitPixel = widthPix/2 - centralZoneWidthPix/2
  rightLimitPixel = widthPix/2 + centralZoneWidthPix/2
  bottomLimitPixel = heightPix/2 + centralZoneHeightPix/2
  topLimitPixel = heightPix/2 - centralZoneHeightPix/2
  
  if ("TRIAL_LABEL" %in% colnames(df)) {
    #TRIAL_LABEL column contents are printed as as "Trial:1", "Trial:2". Let's delete "Trial:"
    df$TRIAL_LABEL<-gsub("Trial:", "", df$TRIAL_LABEL)
    #Converts Trial number in to numeric form  
    df<-transform(df, TRIAL_LABEL=as.numeric(TRIAL_LABEL))
    names(df)[names(df) == 'TRIAL_LABEL'] <- 'trial' #rename column
  } else if ("TRIAL_INDEX" %in% colnames(df)){
    names(df)[names(df) == 'TRIAL_INDEX'] <- 'trial' #rename column
  } else { 
    print("Neither TRIAL_LABEL nor TRIAL_INDEX present, need one!") 
  }
  
  colsExpected = c("RECORDING_SESSION_LABEL","trial","CURRENT_FIX_BLINK_AROUND",
                   "CURRENT_FIX_X","CURRENT_FIX_Y")
  colsExpectedNotPresent <- setdiff( colsExpected,colnames(df) )
  if (length(colsExpectedNotPresent) >0) {
    if (length(colsExpectedNotPresent)==1 && colsExpectedNotPresent[1]=="CURRENT_FIX_Y") {
      print("Chris early output did not include CURRENT_FIX_Y, that's ok for now")
    } else {
      stop( paste0("The file ",inputFilename," does not have the expected columns  :",colsExpectedNotPresent) )
    }
  }
  colsKindaExpected <- c(colsExpected,"TRIAL_INDEX")
  colsPresentNotExpected<- setdiff( colnames(df), colsKindaExpected )
  if (length(colsPresentNotExpected) >0) {
    cat("These columns are in the file",inputFilename," but were not expected:")
    cat( paste(colsPresentNotExpected,collapse=',') )
  }
  
  #File is formatted with potentially many rows for each trial. Multiple events in each trial according to Eyelink
  #Code whether a blink occurred around the time of every event
  dg<-mutate(df,blink=(CURRENT_FIX_BLINK_AROUND!="NONE")) #"NONE", "AFTER", or "BEFORE"
  
  #Go through every event for all trials and indicate whether each event falls within the designated limits
  cat(paste0("leftLimitPixel=",leftLimitPixel,"\n"))
  dg<-mutate(dg, outOfCentralArea= 
               (CURRENT_FIX_X<leftLimitPixel) | (CURRENT_FIX_X>rightLimitPixel) )
  
  if ("CURRENT_FIX_Y" %in% colnames(dg)) {
    dg<-mutate(dg, outOfCentralArea= 
                 (CURRENT_FIX_Y>=bottomLimitPixel) | (CURRENT_FIX_X<topLimitPixel) )  
  }
  
  #Change names of columns to something more readable
  #colnames(mydata)<- c("Subject", "Trial", "Blink", "Position")
  
  #Break apart by trial number, then find max deviation and whether any fell out of desired area, etc
  by_trial <- group_by(dg, trial)
  oneRowPerTrial <- dplyr::summarise(
                              by_trial,
                              outOfCentralArea = max(outOfCentralArea),
                              maxXdev = max( abs(CURRENT_FIX_X-widthPix/2) ),
                              blinks = sum( blink ),
                              meanX = mean(CURRENT_FIX_X) )
  
  if ("CURRENT_FIX_Y" %in% colnames(dg)) {
    oneRowPerTrialExtra <- dplyr::summarise(  #avoid confusion with plyr::summarise
                                     by_trial, 
                                     maxYdev = max( abs(CURRENT_FIX_Y-heightPix/2) ),
                                     meanY = mean(CURRENT_FIX_Y) )
    oneRowPerTrial<- merge(oneRowPerTrial, oneRowPerTrialExtra, by=c("trial"))
  }
  
  return (oneRowPerTrial)
}

TESTME = FALSE #Unfortunately no equivalent in R of python __main__. Would have to use testhat I guess
if (TESTME) {
  setwd(
    "/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/newTraj/newTraj_repo/dataPreprocess/eyetracking"
  )
  inputFilename<-"ANON_26May2015_13-44Eyetracking.txt"
  inputFilename<- "CF_10Jun2015_12-14EyetrackingReport.txt"
  inputDir<-"./dataForTestingOfCode/"
  files <- dir(path=inputDir)  #find all data files in this directory
  if (!(inputFilename %in% files)) {
    stop("test file not found")
  }
  inputFilenameWithPath<-paste0(inputDir,inputFilename)
  outputFilename<-paste0(inputDir,inputFilename,"_eachTrialSummary")
  df <- tryCatch( 
    read.table(inputFilenameWithPath, header = TRUE, sep ="\t"), 
    error=function(e) { 
      stop( paste0('EyeLink Report file exists: ',inputFilenameWithPath,"but ERROR reading the file :",e) )
    } )

  #Calculating the exclusion zone numbers
  exclusionDeg = 1.0 #if participant's eye is ever more than exclusionDeg away from fixation, Exclusion=1
  widthPix = 800
  heightPix = 600
  monitorWidth = 39.5 #cm
  viewdist = 57 #cm
  widthScreenDeg =  2*(atan((monitorWidth/2)/viewdist) /pi*180)
  pixelsPerDegree = widthPix / widthScreenDeg
  exclusionPixels = exclusionDeg * pixelsPerDegree
  centralZoneWidthPix = exclusionPixels*2
  centralZoneHeightPix = exclusionPixels*2
    
  whatIwantToKnowEachTrial<- 
     eyelinkReportSummarise(inputFilename,df,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix)
  head(whatIwantToKnowEachTrial)
  proportnTrialsOutOfCentralArea = sum(whatIwantToKnowEachTrial$outOfCentralArea != 0) / nrow(whatIwantToKnowEachTrial)
  msg=paste0(" fixation broken on ",as.character(round(proportnTrialsOutOfCentralArea*100,1)), "% of trials")
  print(msg)
  #write.table(whatIwantToKnowEachTrialAboutEye, paste0(inputDir, outputFilename,".txt"), sep="\t", row.names=FALSE)
}
