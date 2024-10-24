#expecting current working directory to be top level of this git-indexed project, and this file to be in top level - dataPreprocess/
#Gets behavioral data and combines with eyetracking result and anonymises 
rm(list = ls()) #Clear workspace so that any code executed before won't contaminate this run
library(dplyr)
library(stringr)
library(tidyr); library(readr)
library(ggplot2)

expFoldersPrefix= file.path("..","dataRaw")
expFolder <- "youngOld"
anonymizedDir<-"dataAnonymized" #where the anonymized data will be exported to
destinationStudyFolderName = "youngOld"
destinationName="youngOld"
thisExpFolder = file.path(expFoldersPrefix,expFolder)
if (!dir.exists(thisExpFolder)) {
  message("Your data folder doesn't exist.")
}
#The raw data, both Psychopy files and EDF files, are downloaded from Sharepoint place linked from the MOTyoungOld GoogleDrive folder
#As is the participant datasheet

#Read in participant information
participantInfoFname<- "Participant Information.xlsx"
participantInfoFileWithPath<- file.path(thisExpFolder,participantInfoFname)
if (!file.exists(participantInfoFileWithPath)) {
  message(paste('Participant info file not found; expected to be',participantInfoFileWithPath))
}
library(readxl)
participantInfo<- readxl::read_excel(participantInfoFileWithPath)
#Temporarily, at least reduce to just ID, Age, Gender, and the acuity and crowding test info
#And add a small random number to the age for privacy protection
#Change all column names to lower case
participantInfo <- participantInfo |> rename_with(tolower)

pInfo_reducedForPrivacy<- participantInfo |> select("participant id",age,gender,crowding:"crowding exp notes")

#Create new ID column that doesn't include first letter
pInfo_reducedForPrivacy<- pInfo_reducedForPrivacy |> mutate( ID = substr(`participant id`,2,3) )
pInfo_reducedForPrivacy$`participant id` <- NULL   #Delete ID column that included letter

#Add random number to age to protect privacy
pInfo_reducedForPrivacy<- pInfo_reducedForPrivacy |> 
  mutate( agePerturbed = age + 
            round( runif(1,min=-3,max=3) ) )
pInfo_reducedForPrivacy$age<- NULL #Delete exact age column

#Change all values of gender to all lower case
pInfo_reducedForPrivacy$gender<- tolower(pInfo_reducedForPrivacy$gender)
#Now that the file has been anonymized, can save in the anonymized folder
#save in tsv format. Only thing that's sometimes screwed this up is if comment has a newline in it

destination_fname<- file.path("..",anonymizedDir,destinationStudyFolderName,destinationName="youngOld")
readr::write_tsv(pInfo_reducedForPrivacy, file = paste0(destination_fname,".tsv"))
message( paste("Anonymised (first initial, date and time removed) data aggregated into single file and saved to",destination_fname) )

#Match up Psychopy datafiles and EDF files
#There will always be a Psychopy datafile, but not always an EDF file, so start with Psychopy files
#to parse out subject name and session number.
#Then look for matching EDF file. 
#Lots of one-off intervention for early false starts with naming, plus a separate column for pilot/real participant

thisExpFolderPsychopy = file.path(thisExpFolder,"Psychopy") #As opposed to EDF folder
if (!dir.exists(thisExpFolderPsychopy)) {
  message("Your Psychopy data folder doesn't exist.")
}
print(paste0("Finding files in '",thisExpFolderPsychopy,"'"))

#Filename should start with participant's first initial, two-digit subject number, and 
#   session letter (a,b, or c) or digit (1,2, or 3) ,
#   e.g. "K31a"

#Create list of subjects from file names
datafiles <- dir(path=thisExpFolderPsychopy,pattern='.tsv')  #find all data files in this directory
message("Found ",length(datafiles)," Psychopy data files (4 or 5 files are created for each run).")
#remove the "trialHandler.tsv" files from the list. That gets created when you call the function that gets the data in Psychopy in tibble form
datafiles <- datafiles[  !grepl("trialHandler.tsv$", datafiles)   ]
#fileSizes <- file.size( file.path(thisExpFolderPsychopy,datafiles) )

datafiles <- data.frame(fname=datafiles)
#datafiles$size <- fileSizes
datafiles$comment <- "None" #Create a comment field to preserve notes about weirdness of how the run went

#Remove files that have PRACTICE in their names, PRAC in case someone didn't write the whole word correctly
#Data about practice sessions is manual in the Google Sheet
datafiles <- datafiles %>% filter( !str_detect(fname,"PRAC") )

#Got warnings below when reading J55b,c; J56a,b; J57a,b,c, but ok by participant 58
#There is an error in the code so it doesn't print fixatnPeriodFrames column label
#I can see someone must have accidentally deleted that part of the code after running
#first session of J55a, because it's there for J55a but not b
#The column names should be:
"trialnum	subject	session	basicShape	numObjects	speed	initialDirRing0 fixationPeriodFrames  orderCorrect	trialDurTotal	numTargets	whichIsTargetEachRing0	whichIsTargetEachRing1	whichIsTargetEachRing2	ringToQuery	direction0	direction1	direction2	respAdj0	respAdj1	respAdj2	rev0_0	rev0_1	rev0_2	rev0_3	rev0_4	rev0_5	rev0_6	rev0_7	rev0_8	rev1_0	rev1_1	rev1_2	rev1_3	rev1_4	rev1_5	rev1_6	rev1_7	rev1_8	rev2_0	rev2_1	rev2_2	rev2_3	rev2_4	rev2_5	rev2_6	rev2_7	rev2_8	timingBlips"
#For all files with J55_b, J55_c, or J56_ in their name with suffix .tsv,
#replace the first line of the file with "trialnum	subject	session	basicShape	numObjects	speed	initialDirRing0 fixationPeriodFrames  orderCorrect	trialDurTotal	numTargets	whichIsTargetEachRing0	whichIsTargetEachRing1	whichIsTargetEachRing2	ringToQuery	direction0	direction1	direction2	respAdj0	respAdj1	respAdj2	rev0_0	rev0_1	rev0_2	rev0_3	rev0_4	rev0_5	rev0_6	rev0_7	rev0_8	rev1_0	rev1_1	rev1_2	rev1_3	rev1_4	rev1_5	rev1_6	rev1_7	rev1_8	rev2_0	rev2_1	rev2_2	rev2_3	rev2_4	rev2_5	rev2_6	rev2_7	rev2_8	timingBlips"
#and then re-save the file.
# List all files matching the pattern J55_b, J55_c, or J56_ with the suffix .tsv
missingColumnName_datafiles <- datafiles %>% filter(str_detect(fname, "J55_b|J55_c|J56_"))
# Define the new header
new_header <- "trialnum\tsubject\tsession\tbasicShape\tnumObjects\tspeed\tinitialDirRing0\tfixationPeriodFrames\torderCorrect\ttrialDurTotal\tnumTargets\twhichIsTargetEachRing0\twhichIsTargetEachRing1\twhichIsTargetEachRing2\tringToQuery\tdirection0\tdirection1\tdirection2\trespAdj0\trespAdj1\trespAdj2\trev0_0\trev0_1\trev0_2\trev0_3\trev0_4\trev0_5\trev0_6\trev0_7\trev0_8\trev1_0\trev1_1\trev1_2\trev1_3\trev1_4\trev1_5\trev1_6\trev1_7\trev1_8\trev2_0\trev2_1\trev2_2\trev2_3\trev2_4\trev2_5\trev2_6\trev2_7\trev2_8\ttimingBlips"
# Loop through each file
for (file in missingColumnName_datafiles$fname) {
  # Read the file
  file_content <- read_lines( file.path(thisExpFolderPsychopy,fname)  )
  
  # Replace the first line with the new header
  file_content[1] <- new_header
  
  # Write the modified content back to the file
  write_lines(file_content, file)
}

#Define function for when need to catch warnings and errors, https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
myTryCatch <- function(expr) { 
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}

new_header <- "trialnum\tsubject\tsession\tbasicShape\tnumObjects\tspeed\tinitialDirRing0\tfixationPeriodFrames\torderCorrect\ttrialDurTotal\tnumTargets\twhichIsTargetEachRing0\twhichIsTargetEachRing1\twhichIsTargetEachRing2\tringToQuery\tdirection0\tdirection1\tdirection2\trespAdj0\trespAdj1\trespAdj2\trev0_0\trev0_1\trev0_2\trev0_3\trev0_4\trev0_5\trev0_6\trev0_7\trev0_8\trev1_0\trev1_1\trev1_2\trev1_3\trev1_4\trev1_5\trev1_6\trev1_7\trev1_8\trev2_0\trev2_1\trev2_2\trev2_3\trev2_4\trev2_5\trev2_6\trev2_7\trev2_8\ttimingBlips"
# Split the string based on the \t delimiter and unlist to get a character vector
correctColumnNames <- str_split(new_header, "\t") %>% unlist()

missingColumnName_datafiles <- datafiles %>% filter(str_detect(fname, "J55_b|J55_c|J56_"))
thisdatafile<- file.path(thisExpFolderPsychopy,missingColumnName_datafiles$fname[1])
testt <- readr::read_tsv(thisdatafile, col_names = correctColumnNames , skip=1)
                 
str_detect(fname, "J55_b|J55_c|J56_")
                        
                        
#Determine number of trials in each file, then can consider files that are very short
nRowsOfTsv <- function(fname) {
  file_path <- file.path(thisExpFolderPsychopy,fname)  
  #message("About to read ",fname)
  #read file
  #mydf<- readr::read_tsv(file_path, show_col_types=FALSE)
  dfWithWarnings<- myTryCatch( readr::read_tsv(file_path, show_col_types=FALSE) )
  if (!(is.null(dfWithWarnings$warning))) {
    message("Warning when tried to read ",fname)
    print(dfWithWarnings$warning)
  }
  mydf <- dfWithWarnings$value
  
  #determine how many rows in dataframe read from file
  nrowWithWarnings<- tryCatch ( nrow(mydf), 
                                warning=function(w) return(list(nrow(mydf),w)) )
  if (length(nrowWithWarnings)>1) {
      message(nrowWithWarnings[2])
  }
  return (nrowWithWarnings[1])
}
dfiles <- datafiles %>% rowwise() %>% mutate( nrows = nRowsOfTsv(fname) )

#REMOVE/HANDLE DYSFUNCTIONAL RUNS
#S26 has two files with zero rows, Loretta confirmed they should be thrown out rather than being lost data. 
datafiles<- rows_delete(datafiles, tibble(fname=c("S26_1_01May2024_11-17.tsv","S26_1_01May2024_11-19.tsv")), by="fname")
#Also for S26 more trials based on the first session were done at the end because only 1 trialsPerCondition were mistakenly run
#So Loretta advises throwing out the first session with 40 trials because it was re-done
datafiles<- rows_delete(datafiles, tibble(fname=c("S26_1_01May2024_11-21.tsv")), by="fname")
#so, S26_1_01May2024_11-21.tsv  and the other S26s are all good

#S45 has 3 and 0 trials for two files. Other files are good and all 3 sessions are there
#S45 "fatigued during and after second MOT trial, especially during the third MOT trial" - Sarah
datafiles<- rows_delete(datafiles, tibble(fname=c("S45_1_27May2024_11-45.tsv","S45_1_27May2024_11-43.tsv")), by="fname")

#N64 stella's comment when running: "found the outer ring very difficult, most of the time guessed (could not see it) + found the task very draining (fatigued)". partcipant also reported macula repair + cataracts
#Also overall percent correct not much above 50%, unlike everyone else who got closer to
#the staircase-converging values of 79% correct
datafiles<- rows_delete(datafiles, tibble(fname=c("N64_a_27Jun2024_10-21.tsv","N64_b_27Jun2024_10-59.tsv","N64_c_27Jun2024_11-38.tsv")), by="fname")

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
       "skipped practice trial, because practice.py was not able to open at this time. Did visual acuity and intelligence firsts before doing any trials.", comment) )

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

#MATCHING UP PSYCHOPY DATAFILES WITH EDF FILES
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
datafiles$IDnum <- substr(datafiles$ID,2,3)

#Parse out the session number
datafiles$session <- substr(datafiles$IDsession,4,4)

#The mouseClickArea problem was fixed on 10 May (SHA:802a331b80c544348da255ce61827583759bb879),
#prior to that it would sometimes attribute a response to the wrong ring if participant didn't click in the best place
#Affecting all participants <= 31, in other words: 22,23,24,26,27,28,29,30,31
datafiles<- datafiles %>% filter(IDnum>31)

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

#Match EDF files to behavioral files
####################################
#Get a list of the EDF files
thisExpFolderEDF = file.path(thisExpFolder,"EDF") #As opposed to Psychopy data folder
EDFfiles <- dir(path=thisExpFolderEDF,pattern='.EDF')  #find all EDF files in this directory
message("Found ",length(EDFfiles)," EDF files.")
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
notValidEDFfileNames<- EDFfiles$fname[ !(validEachFname) ]
knownDegenerateEDFfiles<- c("j5.EDF", "lora.EDF", "lxxa.EDF", "tea.EDF", "tema.EDF", "temp.EDF")
#remove the known degenerate ones that I already double-checked with Loretta and Josh
notValidEDFfileNames<- setdiff(notValidEDFfileNames,knownDegenerateEDFfiles)
if ( length( notValidEDFfileNames) )  {
  message("The following files are not valid in that they don't start with a letter and two digits:")
  notValidEDFfileNames
}

EDFfiles<- EDFfiles %>% separate(fname, c("name", "suffix")) #based on "."

#Check for files with no session letter/digit. This happened back when we didnt include a session
# number, and the subsequent EDF files for a session would overwrite previous sessions if no one
# got them off the computer first.
grepForValidNameButNoSessionID <- "[A-Za-z][0-9][0-9]$"
noSessionID <- str_detect(EDFfiles$name, grepForValidNameButNoSessionID)
if ( any(noSessionID) ) {
  message("The following EDF filenames are not valid in that there is no session letter or digit (4th character is not a session letter/digit), which often happened back when we didnt include a session number, and the subsequent EDF files for a session would overwrite previous sessions if no one got them off the computer first.:")
  cat(EDFfiles$name[ noSessionID ]); cat("\n")
}

grepForValidNameButNoSessionID <- "[A-Za-z][0-9][0-9]$"
noSessionID <- str_detect(EDFfiles$name, grepForValidNameButNoSessionID)
EDFfiles$noSessionID <- noSessionID
knownEDFfilesWithoutSession<- c() #Fill this in once decide which ones not to care about
noSessionID_EDFfiles<- EDFfiles$name[ noSessionID ]
noSessionID_EDFfiles<- setdiff(noSessionID_EDFfiles, knownEDFfilesWithoutSession)
if ( length( noSessionID_EDFfiles ) ) {
  message("The following EDF filenames are not valid in that there is no session letter or digit (4th character is not a session letter/digit), which often happened back when we didnt include a session number, and the subsequent EDF files for a session would overwrite previous sessions if no one got them off the computer first.:")
  cat(noSessionID_EDFfiles); cat("\n")
}

EDFfiles$session <- substr(EDFfiles$name,4,4)

#Validate that it is a session number/letter, e.g. "a" or "1", of those that have one
knownBadSessionNums<- c("j5","tea")

grepForDigitOrLowerCaseLetter <- "[0-9a-z]"
validSession<- str_detect(EDFfiles$session, grepForDigitOrLowerCaseLetter)
hasSessionIDbutInvalid <- which(!(noSessionID) & !validSession)
badSessionNums<- EDFfiles$name[ hasSessionIDbutInvalid ]

unknownBadSessionNums <- setdiff(badSessionNums,knownBadSessionNums)
if ( length( unknownBadSessionNums  ) ) {
  message("The following EDF filenames are not valid in that the session is not a letter or digit:")
  cat(unknownBadSessionNums)
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

#See how many behavioral files don't seem to have a match in the EDFfiles tibble
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
#https://stackoverflow.com/questions/78802310/why-does-this-create-an-nas-introduced-by-coercion-warning?noredirect=1#comment138937096_78802310
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

#Read in datafiles #########################################
#In previous projects, I usually read all the data into tibbles and then copied it over.
#This involves handling that later files have different numbers of columns because I added more timing variables

#Get the two kinds of column specification,for the early files with only timingBlips and the later with more columns
# to check against each file as it comes in
earlyFileWithoutSessionColumn<- "M22a_05Apr2024_11-02.tsv"
earlyFileWithSessionColumn<- "S381_1_17May2024_09-08.tsv"
lateFile<- "D61_a_25Jun2024_11-12.tsv"

columns_spec_early_file_without_session <- readr::spec_table(   file.path(thisExpFolderPsychopy,earlyFileWithoutSessionColumn)     )
columns_spec_early_file_with_session <- readr::spec_table(   file.path(thisExpFolderPsychopy,earlyFileWithSessionColumn)     )
columns_spec_late_file <- readr::spec_table(   file.path(thisExpFolderPsychopy,lateFile)     )

#Debug why one with comment ends up with just 53 rows
#thisRow<- joined %>% filter(fname=="M321_1_10May2024_13-43.tsv")
#Problem seems to be that the comment has a newline in it, which of course screws things up.

#Read all the behavioral files in and aggregate them into one massive tibble
for (i in 1:nrow(joined)) {
  thisRow <- joined[i,]
  thisFile<- file.path(thisExpFolderPsychopy,thisRow$fname)
  
  rawDataLoad=tryCatch( 
    readr::read_tsv(thisFile, show_col_types = FALSE),  #suppress the column specification output printout
      error=function(e) { 
        stop( paste0("ERROR reading the file ",fname," :",e) )
      } 
  )
  rawDataThis <- rawDataLoad
  
  #Validate columns in the file, that they're as expected
  #Should be one of 2 possible column specs
  cols_spec<- readr::spec_table( thisFile  )
  
  isEarlyFileWithoutSession<- identical(cols_spec, columns_spec_early_file_without_session)
  isEarlyFile<- identical(cols_spec, columns_spec_early_file_with_session)
  isLaterFile<- identical(cols_spec, columns_spec_late_file)
  
  if (!isEarlyFileWithoutSession && !isEarlyFile && !isLaterFile) {
    stop( paste0("File,",thisFile," is not in any of the three formats that I know about") )
  }

  if (!("session" %in% colnames(rawDataThis))) {
    rawDataThis$session<- NA
  } 
  if (!("numLongFramesAfterFixation" %in% colnames(rawDataThis))) {
    rawDataThis$numLongFramesAfterFixation<- NA
  } 
  if (!("numLongFramesAfterCue" %in% colnames(rawDataThis))) {
    rawDataThis$numLongFramesAfterCue<- NA
  } 
  if (!("fixatnPeriodFrames" %in% colnames(rawDataThis))) {
    rawDataThis$fixatnPeriodFrames<- NA
  } 

  #From the information I added during pre-parsing of the files list, add those columns
  #Such as the subject ID and session to each dataframe as go through it
  #IDnum and sessionNum
  rawDataThis$IDnum <- thisRow$IDnum
  rawDataThis$sessionNum <- thisRow$sessionNum
  rawDataThis$comment <- thisRow$comment
  rawDataThis$pTrialsLotsTimingBlips <- thisRow$pTrialsLotsTimingBlips
  rawDataThis$pTrialsBlipsAfterFixatn <- thisRow$pTrialsBlipsAfterFixatn
  rawDataThis$pTrialsLongFramesAfterCue <- thisRow$pTrialsLongFramesAfterCue
  rawDataThis$EDFmatchExists <- thisRow$EDFmatchExists
  
  #Delete subject because it contains their first initial
  rawDataThis$subject <- NULL

  removeFirstTrialIfOdd = FALSE #This was added because in previous programs, somehow there was an odd trial some of the time
  if (nrow(rawDataThis) %% 2 ==1) {
    msg=paste0(" Odd number of trials (",nrow(rawDataThis),"); was session incomplete, or extra trial at end?")  
    if (removeFirstTrialIfOdd) {
      rawDataThis <- subset(rawDataThis, !trialnum %in% c(0))
      cat("\tRemoved first trial- assuming it's a repeat")
    }
  }
  
  #If it's the first file of the first subject, start the big tibble
  if (i==1) { #first file of the first subject
    rawData<- rawDataThis
  } else {  #not the first file, so combine it with previously-loaded data
    prevColNames<- colnames(rawData)
    newCols <- setdiff( colnames(rawDataThis),prevColNames )
    #There should be no newCols thanks to my checking the format above
    oldColsNotInNew <- setdiff( prevColNames,colnames(rawDataThis) )
    if (length(newCols) >0) {
      message( paste("newCols in",thisRow$fname,"and they are:") )
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
          message( paste("Old columns not in this new file",thisRow$fname,"are:") )
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

numSs<- length( unique(rawData$IDnum) )
numSessions <- n_groups(  rawData %>% group_by(IDnum,session)  
                         )
perSubjSession<- rawData %>%
  group_by(IDnum,session) %>%
  filter(row_number()==1) %>%
  data.frame() %>%    #this is to prevent a adding grouping variables warning https://stackoverflow.com/a/51265245/302378
  select(EDFmatchExists) 
numSsWithoutMatchingEDFfile<- sum( perSubjSession$EDFmatchExists ==FALSE )
message( paste(numSs,"Ss total, and",numSessions,"sessions total, of which",
               numSsWithoutMatchingEDFfile,"do not have a matching EDF file with a session number."))

#If staircases work, average correct should be 0.794 in each condition.
avgCorrOverall<- rawData |> group_by(IDnum) |> summarise(correct=mean(orderCorrect==3),n=n())
pCorrPlot<- ggplot(avgCorrOverall,aes(x=IDnum,y=correct)) + geom_point()
#Originally subjects 23 to 31 have below 70% accuracy indicating that staircases didn't work.
#due to the mouseClickArea problem making the program malfunction, data now thrown out above.

#Saved anonymised data for loading by doAllAnalyses.R
destination_fname<- file.path(destinationDir,destinationStudyFolderName)
#save in tsv format. Only thing that's sometimes screwed this up is if comment has a newline in it
readr::write_tsv(rawData, file = paste0(destination_fname,".tsv"))
message( paste("Anonymised (first initial, date and time removed) data aggregated into single file and saved to",destination_fname) )

#Also save all the information about the files in joined,
# save everything except the filename, because it has the date/time
anonymisedMatchingOfDataAndEDF<- joined
anonymisedMatchingOfDataAndEDF$fname <- NULL
anonymisedMatchingOfDataAndEDF$IDsession <- NULL
anonymisedMatchingOfDataAndEDF$ID <- NULL
destination_fname = paste0(destination_fname,"_files_guide.tsv")
write_tsv(anonymisedMatchingOfDataAndEDF, file = destination_fname)

#Copy all the EDF files over?

#To get rid of first initial from EDF files, would have to save them with a new name
#, simply with the first initial stripped



