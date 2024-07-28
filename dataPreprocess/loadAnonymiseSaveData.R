#expecting current working directory to be top level of this git-indexed project, and this file to be in top level - dataPreprocess/
#Gets behavioral data and combines with eyetracking result and anonymises 
rm(list = ls()) #Clear workspace
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#The raw data, both Psychopy files and EDF files, are downloaded from Sharepoint place linked from the MOTyoungOld GoogleDrive folder

#To match up Psychopy files and EDF files?
#There will always be a Psychopy file, but not always an EDF file, so start with Psychopy files
#to parse out subject name and session number.
#Then look for matching EDF file. 
#Lots of one-off intervention for early false starts with naming, plus a separate column for pilot/real participant

expFoldersPrefix= file.path("..","dataRaw")
expFolder <- "youngOld"
anonymizedDir<-"dataAnonymized" #where the anonymized data will be exported to
destinationStudyFolderName = "youngOld"

thisExpFolder = file.path(expFoldersPrefix,expFolder)
thisExpFolderPsychopy = file.path(thisExpFolder,"Psychopy") #As opposed to EDF folder
  #paste0(expFoldersPrefix,expFolder,"Psychopy",expFoldersPostfix)
print(paste0("Finding files in '",thisExpFolderPsychopy,"'"))

#Filename should start with participant's first initial, two-digit subject number, and 
#   session letter (a,b, or c) or digit (1,2, or 3) ,
#   e.g. "K31a"

#Create list of subjects from file names
datafiles <- dir(path=thisExpFolderPsychopy,pattern='.tsv')  #find all data files in this directory

#remove the "trialHandler.tsv" files from the list. That gets created when you call the function that gets the data in Psychopy in tibble form
datafiles <- datafiles[  !grepl("trialHandler.tsv$", datafiles)   ]
#fileSizes <- file.size( file.path(thisExpFolderPsychopy,datafiles) )

datafiles <- data.frame(fname=datafiles)
#datafiles$size <- fileSizes
datafiles$comment <- "None" #Create a comment field to preserve notes about weirdness of how the run went

#Remove files that have PRACTICE in their names, PRAC in case someone didn't write the whole word correctly
#Data about practice sessions is manual in the Google Sheet
datafiles <- datafiles %>% filter( !str_detect(fname,"PRAC") )

#Determine number of trials in each file, then can consider files that are very short
nRowsOfTsv <- function(fname) {
  file_path <- file.path(thisExpFolderPsychopy,fname)  
  mydf<- readr::read_tsv(file_path, show_col_types=FALSE)
  nrows<- nrow(mydf)
  return (nrows)
}
datafiles <- datafiles %>% rowwise() %>% mutate( nrows = nRowsOfTsv(fname) )

#REMOVE/HANDLE DYSFUNCTIONAL RUNS
#S26 has two files with zero rows, Loretta confirmed they should be thrown out rather than being lost data. 
datafiles<- rows_delete(datafiles, tibble(fname=c("S26_1_01May2024_11-17.tsv","S26_1_01May2024_11-19.tsv")), by="fname")
#Also for S26 more trials based on the first session were done at the end because only 1 trialsPerCondition were mistakenly run
#so, S26_1_01May2024_11-21.tsv  and the other S26s are all good

#S45 has 3 and 0 trials for two files. Other files are good and all 3 sessions are there
#S45 "fatigued during and after second MOT trial, especially during the third MOT trial" - Sarah
datafiles<- rows_delete(datafiles, tibble(fname=c("S45_1_27May2024_11-45.tsv","S45_1_27May2024_11-43.tsv")), by="fname")

#K341 has two files with 0 rows
#datafiles %>% filter(str_starts(fname,"K34"))
#Said her eyes felt dry towards the end of the trials, and that it was hard to focus.
#"First session 60Hz, new monitor (second and third trials run at the correct Hz)
#Both were the same monitor, but we had just changed it to the new one. The first session I realised the settings hadn’t saved because the middle dot flashed occasionally, I checked and it was only 60rps, I redid the settings for the next trials"
#Delete 0-row files of K347
datafiles<- rows_delete( datafiles, tibble(fname=c("K341_1_15May2024_09-29.tsv","K341_1_15May2024_09-30.tsv")), by="fname") 

#Delete 0-row file of M323 that was run again to replace it (false start)
datafiles<- rows_delete( datafiles, tibble(fname=c("M323_3_10May2024_14-22.tsv")), by="fname") 
#Delete 0-row file of S392 that was run again to replace it (false start)
datafiles<- rows_delete( datafiles, tibble(fname=c("S392_2_17May2024_11-42.tsv")), by="fname") 

#Add comment to anomalous run
datafiles<- datafiles %>% 
  mutate(comment = ifelse(str_starts(fname,"M32"),
       "skipped practice trial, because practice.py was not able to open at this time. Did visual acuity and intelligence firsts before doing any trials. 
", comment) )

#Add comment to anomalous run
datafiles<- datafiles %>% 
  mutate(comment = ifelse(fname=="K341_1_15May2024_09-31.tsv", 
                              "Mistakenly run at 60Hz", 
                              comment) )

#Delete 0-row files.  I believe I checked all of them with Loretta
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_c_12Jun2024_14-12 - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_c_12Jun2024_14-12.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("D69_b_03Jul2024_12-36.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_a_12Jun2024_13-02 - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_a_12Jun2024_13-02trialHandler - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_b_12Jun2024_13-39 - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_b_12Jun2024_13-39trialHandler - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_c_12Jun2024_14-14 - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_c_12Jun2024_14-14trialHandler - Copy.tsv")), by="fname")

#“J55_a_18Jun2024_11-32.tsv” this is a one-off J55 (there is no b and c), but more importantly,
#there is a C55 with 3 sessions so let me know why you think there are two ID=55 participants
#Josh: those are two separate participants
#It screws up my code so until I hear from LORETTA about sessions b and c, delete
datafiles<- rows_delete( datafiles, tibble(fname=c("J55_a_18Jun2024_11-32.tsv")), by="fname")

#Loretta says lxx was just a test of the program, not a participant
datafiles<- rows_delete( datafiles, tibble(fname=c("lxx_a_27Jun2024_09-43.tsv")), by="fname")
#Loretta says lor was just a test of the program, not a participant
datafiles<- rows_delete( datafiles, tibble(fname=c("lor_a_12Jun2024_11-32.tsv")), by="fname")

#See if any remaining files have few rows
almostNoTrials <- datafiles %>% filter( nrows < 10 )
if ( nrow(almostNoTrials) ) {
  message("Hey, these datafiles you haven't taken note of have fewer than 10 trials:")
  print( almostNoTrials$fname )
}

#Calculate proportion of trials with lots of timingBlips
#First files had just a single timingBlips column
#Then I realized that the timingBlips were almost all happening at the very beginning of the trial while the cue was still
#on which keeps attention on the moving objects or even before any stimuli appeared.
#So then I programmed new columns numLongFramesAfterFixation	numLongFramesAfterCue

# Define a custom function that returns a tibble of columns to be added onto my datafiles tibble
calcTimingBlips <- function(fname) {
  file_path <- file.path(thisExpFolderPsychopy,fname)  
  mydf<- readr::read_tsv(file_path, show_col_types=FALSE)
  #Calculate proportion of trials with lots of timing blips
  if ( !("timingBlips" %in% names(mydf)) ) {
    message('Hey, was expecting a timingBlips column but it is not in :',file_path)
  }
  pTrialsLotsTimingBlips <- sum(mydf$timingBlips>5) / nrow(mydf)
  #If has additional columns for blips not in the very beginning of trial, calculate proportion of each of those
  pTrialsBlipsAfterFixatn <- NaN
  #message('Summarising timingBlips for',file_path)
  if ("numLongFramesAfterFixation" %in% names(mydf)) {
    pTrialsBlipsAfterFixatn <- sum(mydf$numLongFramesAfterFixation>0) / nrow(mydf)
  }
  pTrialsLongFramesAfterCue <- NaN
  if ("numLongFramesAfterCue" %in% names(mydf)) {
    pTrialsLongFramesAfterCue <- sum( mydf$numLongFramesAfterCue>0 ) / nrow(mydf)
  }

  #create columns in tibble form so can be added onto the df
  timingStuff <- tibble( pTrialsLotsTimingBlips, pTrialsBlipsAfterFixatn, pTrialsLongFramesAfterCue )
  return(timingStuff)
}

#Send all datafiles to the timingBlips analysis function to create summary columns
datafiles<- datafiles %>% rowwise() %>% 
                mutate( timingStuff = list(calcTimingBlips(fname)) ) %>%
                unnest_wider( timingStuff ) #unpack list of different timingBlip metrics

#Plenty of trials with a bunch of timingBlips
#ggplot(datafiles,aes(x=pTrialsLotsTimingBlips)) + geom_histogram(binwidth=.004) +xlim(-.1,1)
#But most of them are at very beginning of trial so should have separate column to report only later timingBlips

#Looking at this on 9 Jul, zero timingBlips after fixation! which suggests that's also true for
#earlier data runs before I programmed this facility.
#ggplot(datafiles,aes(x=pTrialsBlipsAfterFixatn)) + geom_histogram(binwidth=.004) +xlim(-.1,1)
#ggplot(datafiles,aes(x=pTrialsLongFramesAfterCue)) + geom_histogram(binwidth=.004) +xlim(-.1,1)
#table(datafiles$pTrialsBlipsAfterFixatn)

criterionProportnNumTimingBlipsForThrowingOutTrial = .02 

if (any(datafiles$pTrialsLongFramesAfterCue > criterionProportnNumTimingBlipsForThrowingOutTrial, na.rm=T)) {
  message('Some files actually had trials with more than ',criterionProportnNumTimingBlipsForThrowingOutTrial,
          'timing blips after the cue! So you need to write code to delete those.')
}

#There might be one participant without enough columns in the header, maybe the one Yuenchen ran that we can't find the data for

#Consider removing files with very few rows.  All good as of 10 Jul 2024
#datafiles<- datafiles %>% arrange(nrows)
#head(datafiles)

#Get ready to match up Psychopy datafiles with EDF files

#Parse out the subject ID and session number
#Validate that they all start with a letter followed by two numbers (the subject ID)
library(stringr)
grepForUppercaseLetterFollowedByTwoDigits <- "[A-Z]\\d{2}"
validEachFname<- str_detect(datafiles$fname, grepForUppercaseLetterFollowedByTwoDigits)
datafiles$validEachFname <- validEachFname
# invalid: "j33_3_13May2024_13-18.tsv"
#j33_3 is a typo and should be uppercase J, as certified by Loretta.
# Change it manually to valid
datafiles<- datafiles %>% mutate(validEachFname = ifelse(fname == "j33_3_13May2024_13-18.tsv", TRUE, validEachFname))
if ( length( which(!(datafiles$validEachFname)) ) ) {
  message("The following files are not valid in that they don't start with letter and two digits:")
  datafiles$fname[ !(datafiles$validEachFname) ]
}
datafiles$validEachFname <- NULL #Delete because don't need this column anymore

#Take first 4 characters again to get IDsession
datafiles$IDsession<- substr(datafiles$fname,1,4)

#reorder columns for convenience of visual inspection
datafiles<- datafiles %>% relocate(IDsession, .after=fname)

#For those that have an underscore after the first 3 characters, delete the underscore
underscoresInsteadOfSession <- datafiles %>% filter( str_ends(IDsession,"_") )
#View(underscoresInsteadOfSession) #Visually inspect to make sure not too weird
#All ok based on inspection 10 Jul, so delete the underscore to make them like the others
#How to delete only the first underscore?
datafiles<- datafiles %>% mutate(IDsession = 
                       ifelse(str_ends(IDsession, "_"), #if ends with underscore
                              gsub("_", "", fname), #replace with filename with underscore deleted
                              IDsession) )
#Because ones that had underscores will now have full filename, take first 4 characters again of all to get IDsession 
datafiles$IDsession<- substr(datafiles$IDsession,1,4)

#Parse out the ID, as opposed to the session number
datafiles$ID <- substr(datafiles$IDsession,1,3)
#Parse out the session number
datafiles$session <- substr(datafiles$IDsession,4,4)

#To match to the EDF files, they will
#The EDF files have names like M471.EDF, M472.EDF, M473.EDF, P23a.EDF, P23b.EDF
#What I'll do is for each Psychopy datafile, I'll locate the corresponding EDF file and store its name,
# or else note that one doesn't exist

datafiles$IDvalid <- str_detect(datafiles$ID,grepForUppercaseLetterFollowedByTwoDigits)
#j33_3 is a typo and should be uppercase J, as certified by Loretta.
# Change it manually to valid
datafiles<- datafiles %>% mutate(IDvalid = ifelse(IDsession == "j333", TRUE, IDvalid))

if (any(datafiles$IDvalid==FALSE)) {
  cat("Problem! These files have the wrong format as the subject ID is not an upper-case letter followed by two digits:")
  cat( datafiles %>% dplyr::filter(IDvalid==FALSE) )
}

datafiles$IDnum <- substr(datafiles$ID,2,3)

#Match EDF files to behavioral files
####################################
#Get a list of the EDF files
thisExpFolderEDF = file.path(thisExpFolder,"EDF") #As opposed to Psychopy data folder
EDFfiles <- dir(path=thisExpFolderEDF,pattern='.EDF')  #find all EDF files in this directory
EDFfiles<- data.frame(fname=EDFfiles)

#Do some validation of the EDF filenames
#Filename should start with participant's first initial, two-digit subject number, and 
#     session (4 characters in total), followed by ".EDF"

#Ignore all those that have only 3 characters

#Parse out the subject ID and session number
#Validate that they all start with a letter followed by two numbers (the subject ID)
grepForUppercaseLetterFollowedByTwoDigits <- "[A-Z]\\d{2}"
first4char<- substr(EDFfiles$fname,1,4)
validEachFname<- str_detect(first4char, grepForUppercaseLetterFollowedByTwoDigits)
if ( length( which(!(validEachFname)) ) ) {
  message("The following files are not valid in that they don't start with a letter and two digits:")
  EDFfiles$fname[ !(validEachFname) ]
}

EDFfiles<- EDFfiles %>% separate(fname, c("name", "suffix"))

#Check for files with no session letter/digit. This happened back when we didnt include a session
# number, and the subsequent EDF files for a session would overwrite previous sessions if no one
# got them off the computer first.
grepForValidNameButNoSessionID <- "[A-Za-z][0-9][0-9]$"
noSessionID <- str_detect(EDFfiles$name, grepForValidNameButNoSessionID)
if ( length( which(noSessionID) ) ) {
  message("The following files are not valid in that there is no session letter or digit (4th character is not a session letter/digit), which often happened back when we didnt include a session number, and the subsequent EDF files for a session would overwrite previous sessions if no one got them off the computer first.:")
  EDFfiles$name[ noSessionID ]
}
EDFfiles$noSessionID <- noSessionID

EDFfiles$session <- substr(EDFfiles$name,4,4)

#Validate that it is a session number/letter, e.g. "a" or "1", of those that have one
#26July no files have this problem
grepForDigitOrLowerCaseLetter <- "[0-9a-z]"
validSession<- str_detect(EDFfiles$session, grepForDigitOrLowerCaseLetter)
hasSessionIDbutInvalid <- which(!(noSessionID) & !validSession)
if ( length( hasSessionIDbutInvalid  ) ) {
  message("The following files are not valid in that the session is not a letter or digit:")
  EDFfiles$name[ hasSessionIDbutInvalid ]
}


#Match eyetracking (EDF) files to psychopy datafiles

#Assign closest match to corresponding behavioral file
#Visually inspect, and then deal case-by-case with anomalies

EDFfiles$IDnum <- substr(EDFfiles$name,2,3)
EDFfiles <- EDFfiles %>% arrange(IDnum)

#Join with datafiles dataframe by combination of ID and session columns
#But first rename EDFfiles columns so clear it refers to the EDF files
EDFfiles<- EDFfiles %>% rename(EDF_session = session, 
                               EDF_IDnum = IDnum, 
                               EDF_suffix = suffix,
                               EDF_name = name)


#there are two files for C53b, “C53_b_05Jun2024_14-05.tsv” and “C53_b_05Jun2024_14-35.tsv”, 
#Josh says the later one is the third session, so change its session to c
datafiles<- datafiles %>% 
  mutate(session = ifelse(str_starts(fname,"C53_b_05Jun2024_14-35.tsv"),
                          "c", session) )

#See how many behavioral files don't seem to have a match in the second tibble
#anti_join returns all rows from the first tibble where there are matching values in the second tibble
noMatchingEDFfile<- 
  anti_join(datafiles, EDFfiles, by = c("IDnum" = "EDF_IDnum", "session" = "EDF_session"))
noMatchingEDFfile<- noMatchingEDFfile %>% arrange(IDnum)

numSs<- length( unique(datafiles$IDnum) )
numSsWithoutMatchingEDFfile<- length( unique(noMatchingEDFfile$IDnum) )
message( paste(numSs,"Ss total, of which",
               numSsWithoutMatchingEDFfile,"do not have a matching EDF file with a session number."))

#Add a column to datafiles indicating whether there is a match
#Can do that with the noMatchingEDFfile by reducing it to the ID and session column and then joining
noMatchingEDF <- noMatchingEDFfile %>% select(IDnum,session) %>% arrange(IDnum)
noMatchingEDF$EDFmatchExists <- FALSE
joined <- left_join(datafiles, noMatchingEDF, 
                            by = c("IDnum", "session"))

#Now match the matches (as opposed to the non-matches, added above), so that have record of the EDF filename
joinedWithEDF<- left_join(joined, EDFfiles, 
                          by = c("IDnum" = "EDF_IDnum", "session" = "EDF_session"))

#A weird consequence of how I did this is that all the rows that *do* have a match are NA for EDFmatchExists,
# need to change that to TRUE
joined <- joinedWithEDF %>% mutate(EDFmatchExists = replace_na(EDFmatchExists,TRUE))
#double-check the result is consistent with the count I did above
numSsNoMatchingEDFfile<- nrow( unique( filter(joined,EDFmatchExists==FALSE) %>% select(IDnum) ) )
message( paste( length( unique(joined$IDnum) ),"Ss total, of which",
                numSsNoMatchingEDFfile,"do not have a matching EDF file with a session number."))

message( paste(nrow(joined),"files total, of which",
               summarize(joined, trues=sum(EDFmatchExists))$trues,
               "have a matching EDF file with a session number."))


#For the EDF files that don't have a session number, could try to figure out which session they
#correspond to by looking at their date/time

#Do I need to do any matching with sessions? 
#Maybe the only thing to do is create a sessionNum column that assigns 1,2,3 to the a,b,cs

#Create a sessionNum column that assigns 1,2,3 to the a,b,cs
joined<- joined %>% rowwise() %>%
  mutate(sessionNum = case_when(
    tolower(session) == "a" ~ 1,
    tolower(session) == "b" ~ 2,
    tolower(session) == "c" ~ 3,
    tolower(session) == "1" ~ 1,
    tolower(session) == "2" ~ 2,
    tolower(session) == "3" ~ 3,
    TRUE ~ -999  
  ))


#there are two files for C53_b, “C53_b_05Jun2024_14-05.tsv” and “C53_b_05Jun2024_14-35.tsv”, 
#Josh says the later one is the third session
#I guess I need to take care of that below when I create the anonymized files

#Next, save data to anonymised data folder and only then do eyetracking filtering?
#Ideally would strip all date and time info from the anonymised data
#That would mean re-saving each individual datafile as  IDnum + sessionNum.tsv and the EDF file

#EDF file privacy issue #####################################
#The preamble variable of the EDF file has exact date and time, so to anonymize should strip that out
#Could read each EDF file with eyelinkReader and then save all the variables inside it into an R data file

#In the meantime, could save the behavioral datafiles and then do the EDF file analyses in this private folder 
#For each row of joined, save that fname as IDnum_sessionNum.

#Assuming this script is being run from "dataPreprocess" subdirectory, so need to go one level up,
#then down into anonymized directory
destinationDir <- file.path("..",anonymizedDir,destinationStudyFolderName)
if ( !file.exists(destinationDir) ) {
  message( paste(destinationDir," destination directory does not exist!") )
}

#Copy all the files over
for (i in 1:nrow(joined)) {
  thisRow <- joined[i,]
  destinationName = paste0(thisRow$IDnum, '_', thisRow$sessionNum, '.tsv')
  destination<- file.path(destinationDir,destinationName)
  
  datafileToAnonymize<- file.path(thisExpFolderPsychopy,thisRow$fname)

  if ( !file.exists(datafileToAnonymize)  ) {
    message(paste("File",thisRow$fname,"from datafiles listing not found"))
  }
  
  succeeded<- file.copy(from = datafileToAnonymize,
                        to   = destination,   copy.date = FALSE)
  if (!succeeded) {
    message(paste("Copying to ",destination,"failed"))
  }
}

#file.exists(destination)

#Saved anonymised data for loading by doAllAnalyses.R
message( paste("Anonymised (first initial, date and time removed) data saved to",destinationDir) )

#Also save all the information in joined by saving everything except the filename, because it has the date/time
anonymisedMatchingOfDataAndEDF<- joined
joined$fname <- NULL
joined$IDsession <- NULL
joined$ID <- NULL

#To get rid of first initial from EDF files, would have to save them with a new name
#, simply with the first initial stripped




#table(d$speedRank,d$numObjects,d$numTargets,d$subject)







  #foldersThisExp <- list.dirs(path=thisExpFolder,recursive=FALSE) #each folder should be a subject
  #print("Loading data from folders:"); print(foldersThisExp)
  for (i in 1:length(foldersThisExp)) {
    thisSubjectDir <- foldersThisExp[i]
    files <- dir(path=thisSubjectDir,pattern='.txt')  #find all data files in this directory
    eyetrackIdxs = grep("Eyetracking",files)
    if (length(eyetrackIdxs) ==0) {
    		eyetrackFiles = FALSE
    } else { 
    		eyetrackFiles = TRUE 
    		eyetrackFiles = files[eyetrackIdxs]
	  }
    nonEyetrackIdxs = grep("Eyetracking",files,invert=TRUE)
    files<- files[nonEyetrackIdxs] #don't include eyetracking ones
    #allFilesStr <- paste(files,collapse=",") #print(allFilesStr)
    for (j in 1:length(files)) { #read in sessions of this subject
      file = files[j]
      fname = paste0(thisSubjectDir,"/",file)
	    rawDataLoad=tryCatch( 
      	    		read.table(fname,sep='\t',header=TRUE), 
      	    		error=function(e) { 
      	    	   			stop( paste0("ERROR reading the file ",fname," :",e) )
          		 } )
      rawDataLoad$exp <- expFolders[expi]
      rawDataLoad$file <- file
      #Search for eyetracking file
      fileNameLen = nchar(file)
      withoutSuffix<-substr(file,1,fileNameLen-4) 
      eyetrackFileNameShouldBe<- paste0(withoutSuffix,"EyetrackingReport.txt")
      whichFileIsEyetrack <- grep(toupper(eyetrackFileNameShouldBe), toupper(eyetrackFiles)) #allow for capitalisation diffs
      eyetrackFileFound = ( length(whichFileIsEyetrack) >0 )
      #print(paste0("Looked for eyetrack file ",eyetrackFileNameShouldBe," and found=", eyetrackFileFound))
      numTrials<- length(rawDataLoad$trialnum)
      msg=''
      rawDataThis<- rawDataLoad
      if (eyetrackFileFound) #load it in and merge with rawDataLoad
      {
      	trackFname = paste0(thisSubjectDir,"/", eyetrackFileNameShouldBe)
      	eyeTrackInfo = tryCatch( 
      	    read.table(trackFname,header=TRUE,sep='\t'), 
      	    error=function(e) { 
      	    	   stop( paste0('eyeTrackingFile exists: ',trackFname," but ERROR reading the file :",e) )
           } )
      	msg=paste0(" and loaded Eyetracking file. ")
    	    #Eyetracker begins trials with 1, whereas python and psychopy convention is 0
      	#So to match the eyetracker file with the psychopy file, subtract one from trial num
      	eyeTrackOneRowPerTrial<- 
      		eyelinkReportSummarise(trackFname,eyeTrackInfo,widthPix,heightPix,centralZoneWidthPix,centralZoneHeightPix)
      	eyeTrackOneRowPerTrial$trialnum = eyeTrackOneRowPerTrial$trial-1 #psychopy starts with zero, Eyelink with 1
      	proportnTrialsOutOfCentralArea = sum(eyeTrackOneRowPerTrial$outOfCentralArea != 0) / nrow(eyeTrackOneRowPerTrial)
      	msg=paste0(" fixation broken on ",as.character(round(proportnTrialsOutOfCentralArea*100,1)), "% of trials")
      	if (nrow(rawDataLoad) != nrow(eyeTrackOneRowPerTrial)) {  
      		stop( paste0('eyeTrackingFile ',trackFname," does not have same number of trials as behavioral data file:",file) )	
      	}
      	rawDataWithEyetrack<- merge(rawDataLoad, eyeTrackOneRowPerTrial, by=c("trialnum"))
      	rawDataThis<- rawDataWithEyetrack
    	  }
    	  else { msg = ' NO eyetracking file found'}
      cat(paste0("Loaded file ",file,msg))
      #omit first trial is total trials are odd, last probably a repeat. And first trial people often discombobulated      
      msg=""
      removeFirstTrialIfOdd = TRUE
      if (numTrials %% 2 ==1) {
      	msg=paste0(" Odd number of trials (",numTrials,"); was session incomplete, or extra trial at end?")  
        if (removeFirstTrialIfOdd) {
      	  rawDataThis <- subset(rawDataThis, !trialnum %in% c(0))
      	  cat("\tRemoved first trial- assuming it's a repeat")
        }
      }
	  if (rawDataThis$file[1] == "WN_26May2015_13-44.txt") { #Will's first session and needed practice,
	 	rawDataThis <- subset(rawDataThis, trialnum > 7) #so omit first several trials
	  } 
      cat(paste0(", now contains ",length(rawDataThis$trialnum)," trials ",msg))
      if (expi==1 & i==1 & j==1) { #first file of the first subject
        rawData<- rawDataThis
      } else {  #not the first file of the first subject, so combine it with previously-loaded data
        prevColNames<- colnames(rawData)
        newCols <- setdiff( colnames(rawDataThis),prevColNames )
        oldColsNotInNew <- setdiff( prevColNames,colnames(rawDataThis) )
        if (length(newCols) >0) {
          cat( "newCols are:")
          print( paste(newCols,collapse=','))
          for (n in 1:length(newCols)) {#add newCol to old data.frame with dummy value
            newCol = newCols[n]
            rawData[,newCol] <- NA 
            #if (is.numeric(rawDataThis[,newCol]))   #This seems too risky, might forget have -999 values
            #  rawData[,newCol] <- -999 #dummy value
          }
        }
        if (length(oldColsNotInNew) >0)
          for (n in 1:length(oldColsNotInNew)) { #add old col to new data.frame that doesn't have it
            if (n==1) {
              cat("Adding to new data the old columns:")
              print( paste(oldColsNotInNew,collapse=',') )
            }
            oldCol = oldColsNotInNew[n]
            rawDataThis[,oldCol]<- NA #dummy value
            #if (is.numeric(rawData[,oldCol]))  #seems too risky- might forget it is -999
            #  rawDataThis[,oldCol] <- -999 #dummy value
          }
        #Try to merge new data file with already-loaded
        colnamesNew <- colnames(rawDataThis)
        colnamesOld <- colnames(rawData)
		    #colnamesNewMsg <- paste(colnamesNew,collapse=",")
        #colnamesOldMsg <- paste(colnamesOld,collapse=",")
        #writeLines( paste('colnamesNew=',colnamesNewMsg,'\n colnamesOld=', colnamesOldMsg))
		    if ( length(setdiff(colnamesNew,colnamesOld)) >0 )
          writeLines( paste('New columns not in old are ', setdiff(colnamesNew,colnamesOld)) )
        tryCatch( rawData<-rbind(rawData,rawDataThis), #if fail to bind new with old,
                  error=function(e) { #Give feedback about how the error happened
                    cat(paste0("Tried to merge but error:",e))
                    colnamesNewFile <- colnames(rawDataThis)
                    colnamesOldFiles <- colnames(rawData)
                    #colnamesNewFileMsg <- paste(colnamesNewFile,collapse=",")
                    #colnamesOldFilesMsg <- paste(colnamesOldFiles,collapse=",")
                    #writeLines( paste('colnamesNew=',colnamesNewMsg,'\n colnamesOld=', colnamesOldMsg))
                    #c( 'New cols: ', setdiff(colnamesNewFile,colnamesOldFiles) )
                    newCols <- setdiff(colnamesNewFile,colnamesOld)
                    oldColsNotInNew<- setdiff(colnamesOldFiles,colnamesNew)
                    if (length(newCols)>0) {
                      writeLines( paste('New cols not in old: ', paste(newCols,collapse=",") ) ) 
                    }
                    writeLines( paste('Old cols not in new file: ', paste(oldColsNotInNew,collapse=",") ) )        
                    stop(paste0("ERROR merging, error reported as ",e))
                  } 
        )
      }      
    }		
  }
 rawData = subset(rawData, subject != "auto") #get rid of any autopilot data
 #check data counterbalancing of this exp
 source("analysis/helpers/checkCounterbalancing.R")
 checkCombosOccurEqually(rawData, c("numObjects","numTargets") )
 checkCombosOccurEqually(rawData, c("numObjects","numTargets","ringToQuery") )
 checkCombosOccurEqually(rawData, c("condition","leftOrRight") )
 checkCombosOccurEqually(rawData, c("condition","leftOrRight","offsetXYring0") ) #NO?
 checkCombosOccurEqually(rawData, c("numObjects","numTargets","speed") )  
}

dat <-rawData
#end data importation

#Also need to get age and sex from somewhere.. a manual copy of the Sharepoint participant sheet with only those columns?

#Eyemovement exclusion zone numbers
exclusionDeg = 1 #in any direction from fixation
widthPix = 800
heightPix = 600
monitorWidth = 39.5 #cm
viewdist = 57 #cm
widthScreenDeg =  2*(atan((monitorWidth/2)/viewdist) /pi*180)
pixelsPerDegree = widthPix / widthScreenDeg
exclusionPixels = exclusionDeg * pixelsPerDegree
centralZoneWidthPix = exclusionPixels*2
centralZoneHeightPix = exclusionPixels*2 #assumes the monitor is correct aspect ratio so that pixels are square


#If instead of using raw speed, I rank the speed within each numObjects*numTargets, then from that perspective everything should
#be perfectly counterbalanced, because each numObjects*numTargets combination has the same number of speeds tested
#But the rank for a speed depends on what numObjects-numTargets condition it's in. Should be easy with ddply
ordinalSpeedAssign <- function(df) {
#df$speedRank <- rank(df$speed)  #Rank won't work, always wants to break all ties. Whereas I want to preserve ties.
  df$speedRank <- match(df$speed,unique(df$speed))
  df
}
d<- plyr::ddply(dat,.(numObjects,numTargets),ordinalSpeedAssign)
#grouped<- group_by(dat,numObjects,numTargets) #can't get this to work with dpylr but involves something with .  http://stackoverflow.com/questions/22182442/dplyr-how-to-apply-do-on-result-of-group-by
#d<- dplyr::summarise(grouped, speedRank= match(speed,unique(.$speed)))
#dat %>% group_by(numObjects,numTargets) %>% do(match(speed,unique(.$speed)))
#check whether counterbalanced with for each speed list for a particular condition, did each equally often
#Might not be if ran multiple sessions with different speeds
checkCombosOccurEqually(d, c("numObjects","numTargets","speedRank") )

sanityCheckEyeTracking=TRUE
if (sanityCheckEyeTracking) {
  library(ggplot2)
  h<-ggplot(filter(dat,exp=="circleOrSquare_twoTargets"),
            aes(x=maxXdev,y=maxYdev,color=file)) + geom_point() +facet_grid(~subject)  #Have a look at fixation positions
  quartz("circleOrSquare_twoTargets"); show(h)
  h<-ggplot(filter(dat,exp=="offCenter"),
            aes(x=maxXdev)) + geom_histogram()+ facet_grid(~subject) #Have a look at fixation positions
  quartz("offCenter"); show(h)
}
dat$correct = dat$orderCorrect /3
dat$chanceRate= 1 / dat$numObjects

