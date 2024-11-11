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

pInfo_reducedForPrivacy<- participantInfo |> select("participant id",age,gender,"edf transfered":"crowding exp notes")

#Create new ID column that doesn't include first letter
pInfo_reducedForPrivacy<- pInfo_reducedForPrivacy |> mutate( IDnum = substr(`participant id`,2,3) )
pInfo_reducedForPrivacy<- pInfo_reducedForPrivacy |> mutate( ID = substr(`participant id`,1,3) )
pInfo_reducedForPrivacy$`participant id` <- NULL   #Delete ID column that included letter

#Reorder columns to put ID first
pInfo_reducedForPrivacy<- pInfo_reducedForPrivacy %>% select(IDnum,ID,everything())

#Add random number to age to protect privacy
pInfo_reducedForPrivacy<- pInfo_reducedForPrivacy |> 
  mutate( agePerturbed = age + 
            round( runif(1,min=-3,max=3) ) )
pInfo_reducedForPrivacy$age<- NULL #Delete exact age column

#Change all values of gender to all lower case
pInfo_reducedForPrivacy$gender<- tolower(pInfo_reducedForPrivacy$gender)
#Now that the file has been anonymized, can save in the anonymized folder
#save in tsv format. Only thing that's sometimes screwed this up is if comment has a newline in it

destination_fname<- file.path("..",anonymizedDir,destinationStudyFolderName,
                              "participantInfo_reducedForPrivacy")
readr::write_tsv(pInfo_reducedForPrivacy, file = paste0(destination_fname,".tsv"))
message( paste("Anonymised (first initial, date and time removed, agePerturbed) data aggregated into single file and saved to",destination_fname) )

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
#"trialnum	subject	session	basicShape	numObjects	speed	initialDirRing0 fixatnPeriodFrames  orderCorrect	trialDurTotal	numTargets	whichIsTargetEachRing0	whichIsTargetEachRing1	whichIsTargetEachRing2	ringToQuery	direction0	direction1	direction2	respAdj0	respAdj1	respAdj2	rev0_0	rev0_1	rev0_2	rev0_3	rev0_4	rev0_5	rev0_6	rev0_7	rev0_8	rev1_0	rev1_1	rev1_2	rev1_3	rev1_4	rev1_5	rev1_6	rev1_7	rev1_8	rev2_0	rev2_1	rev2_2	rev2_3	rev2_4	rev2_5	rev2_6	rev2_7	rev2_8	timingBlips"
#For all files with J55_b, J55_c, or J56_ in their name with suffix .tsv,
#need special handling, like this:
new_header <- "trialnum\tsubject\tsession\tbasicShape\tnumObjects\tspeed\tinitialDirRing0\tfixatnPeriodFrames\torderCorrect\ttrialDurTotal\tnumTargets\twhichIsTargetEachRing0\twhichIsTargetEachRing1\twhichIsTargetEachRing2\tringToQuery\tdirection0\tdirection1\tdirection2\trespAdj0\trespAdj1\trespAdj2\trev0_0\trev0_1\trev0_2\trev0_3\trev0_4\trev0_5\trev0_6\trev0_7\trev0_8\trev1_0\trev1_1\trev1_2\trev1_3\trev1_4\trev1_5\trev1_6\trev1_7\trev1_8\trev2_0\trev2_1\trev2_2\trev2_3\trev2_4\trev2_5\trev2_6\trev2_7\trev2_8\ttimingBlips"
# Split the string based on the \t delimiter and unlist to get a character vector
correctColumnNames <- str_split(new_header, "\t") %>% unlist()
#missingColumnName_datafiles <- datafiles %>% filter(str_detect(fname, "J55_b|J55_c|J56_"))
#thisdatafile<- file.path(thisExpFolderPsychopy,missingColumnName_datafiles$fname[1])
#testt <- readr::read_tsv(thisdatafile, col_names = correctColumnNames , skip=1) #skip incorrect header


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

read_tsv_but_handle_erroneous_columns<- function(file_path) {
  #Check whether this is one of the files with the missing column label
  if (str_detect(file_path, "J55_b|J55_c|J56_|J57_")) { #One of the files with the missing column label
    #So supply correct column labels when read the file
    dfWithWarnings<- myTryCatch( 
      readr::read_tsv(file_path, show_col_types=FALSE, col_names=correctColumnNames, skip=1) #skip incorrect header
    )
  } else { #normal file
    #message(file_path)
    dfWithWarnings<- myTryCatch( 
      readr::read_tsv(file_path, show_col_types=FALSE) 
    )
  }
  return (dfWithWarnings)
}
  
#Determine number of trials in each file, then can consider files that are very short
# but need special handling of four participants with files with missing column name
nRowsOfTsv <- function(fname) {
  file_path <- file.path(thisExpFolderPsychopy,fname)
  
  dfWithWarnings<- read_tsv_but_handle_erroneous_columns(file_path)
  
  if (!(is.null(dfWithWarnings$warning))) {
    message("Warning when tried to read ",fname)
    print(dfWithWarnings$warning)
  }
  mydf <- dfWithWarnings$value
  
  #determine how many rows in dataframe read from file
  nrowWithWarnings<- tryCatch ( nrow(mydf), 
                                warning=function(w) return(list(nrow(mydf),w)) )
  if (length(nrowWithWarnings)>1) {
      message("warning from nrow:", nrowWithWarnings[2])
  }
  ans <- nrowWithWarnings[1]
  #message("ans=",ans)
  return (ans)
}

datfiles <- datafiles %>% rowwise() %>% mutate( nrows = nRowsOfTsv(fname) )

#REMOVE/HANDLE DYSFUNCTIONAL RUNS
#There are two C55a, one being an identical copy -  has the exact same time/datestamp and contents as the other and is called C55_a_12Jun2024_13-02 - Copy.tsv?
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_a_12Jun2024_13-02 - Copy.tsv")), by="fname")
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_a_12Jun2024_13-02trialHandler - Copy.tsv")), by="fname")
#Similarly for C55b and C55c
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_b_12Jun2024_13-39 - Copy.tsv")), by="fname")
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_b_12Jun2024_13-39trialHandler - Copy.tsv")), by="fname")
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_c_12Jun2024_14-14 - Copy.tsv")), by="fname")
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_c_12Jun2024_14-14trialHandler - Copy.tsv")), by="fname")

#S26 has two files with zero rows, Loretta confirmed they should be thrown out rather than being lost data. 
datfiles<- rows_delete(datfiles, tibble(fname=c("S26_1_01May2024_11-17.tsv","S26_1_01May2024_11-19.tsv")), by="fname")
#Also for S26 more trials based on the first session were done at the end because only 1 trialsPerCondition were mistakenly run
#So Loretta advises throwing out the first session with 40 trials because it was re-done
datfiles<- rows_delete(datfiles, tibble(fname=c("S26_1_01May2024_11-21.tsv")), by="fname")
#so, S26_1_01May2024_11-21.tsv  and the other S26s are all good

#S45 has 3 and 0 trials for two files. Other files are good and all 3 sessions are there
#S45 "fatigued during and after second MOT trial, especially during the third MOT trial" - Sarah
datfiles<- rows_delete(datfiles, tibble(fname=c("S45_1_27May2024_11-45.tsv","S45_1_27May2024_11-43.tsv")), by="fname")

#N64 stella's comment when running: "found the outer ring very difficult, most of the time guessed (could not see it) + found the task very draining (fatigued)". partcipant also reported macula repair + cataracts
#Also overall percent correct not much above 50%, unlike everyone else who got closer to
#the staircase-converging values of 79% correct
datfiles<- rows_delete(datfiles, tibble(fname=c("N64_a_27Jun2024_10-21.tsv","N64_b_27Jun2024_10-59.tsv","N64_c_27Jun2024_11-38.tsv")), by="fname")

#K341 has two files with 0 rows
#Delete 0-row files of K34
datfiles<- rows_delete( datfiles, tibble(fname=c("K341_1_15May2024_09-29.tsv","K341_1_15May2024_09-30.tsv")), by="fname") 

#Delete 0-row file of M323 that was run again to replace it (false start)
datfiles<- rows_delete( datfiles, tibble(fname=c("M323_3_10May2024_14-22.tsv")), by="fname") 
#Delete 0-row file of S392 that was run again to replace it (false start)
datfiles<- rows_delete( datfiles, tibble(fname=c("S392_2_17May2024_11-42.tsv")), by="fname") 

#Add comment to anomalous run
datfiles<- datfiles %>% 
  mutate(comment = ifelse(str_starts(fname,"M32"),
       "skipped practice trial, because practice.py was not able to open at this time. Did visual acuity and intelligence firsts before doing any trials.", comment) )

#Add comment to anomalous run
datfiles<- datfiles %>% 
  mutate(comment = ifelse(fname=="K341_1_15May2024_09-31.tsv", 
                              "Mistakenly run at 60Hz", 
                              comment) )

datfiles <- datfiles %>% filter(!str_starts(fname, "temp")) #Delete files that start with temp
datfiles <- datfiles %>% filter(!str_starts(fname, "te_a")) #Delete files that start with te_a
#Delete 0-row files.  I believe I checked all of them with Loretta
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_c_12Jun2024_14-12 - Copy.tsv")), by="fname")
datfiles<- rows_delete( datfiles, tibble(fname=c("C55_c_12Jun2024_14-12.tsv")), by="fname")
datfiles<- rows_delete( datfiles, tibble(fname=c("D69_b_03Jul2024_12-36.tsv")), by="fname")

datfiles<- rows_delete( datfiles, tibble(fname=c("J51a_a_03Jun2024_13-14.tsv")), by="fname")
datfiles<- rows_delete( datfiles, tibble(fname=c("J51a_a_03Jun2024_13-16.tsv")), by="fname")
datfiles<- datfiles %>% filter(!str_starts(fname, "J562_2")) #Delete three 0-row files
datfiles<- rows_delete( datfiles, tibble(fname=c("A92_b_14Oct2024_09-54.tsv")), by="fname") #false start
datfiles<- rows_delete( datfiles, tibble(fname=c("A92_c_14Oct2024_10-19.tsv")), by="fname") #false start
datfiles<- rows_delete( datfiles, tibble(fname=c("L84_b_28Aug2024_10-08.tsv")), by="fname") #false start
datfiles<- rows_delete( datfiles, tibble(fname=c("N79_a_08Aug2024_09-10.tsv")), by="fname") #false start
datfiles<- rows_delete( datfiles, tibble(fname=c("R91_b_23Sep2024_11-45.tsv")), by="fname") #false start
datfiles<- rows_delete( datfiles, tibble(fname=c("S75_c_24Jul2024_13-25.tsv")), by="fname") #false start
#Loretta says lxx was just a test of the program, not a participant
datfiles<- rows_delete( datfiles, tibble(fname=c("lxx_a_27Jun2024_09-43.tsv")), by="fname")
#Loretta says lor was just a test of the program, not a participant
datfiles<- rows_delete( datfiles, tibble(fname=c("lor_a_12Jun2024_11-32.tsv")), by="fname")
#K341 has all timing blips; "First session 60Hz, new monitor (second and third trials run at the correct Hz)" "Both were the same monitor, but we had just changed it to the new one. The first session I realised the settings hadn’t saved because the middle dot flashed occasionally, I checked and it was only 60rps, I redid the settings for the next trials"
#Because this sesssion was run at 60 Hz, should throw out.
datfiles<- rows_delete( datfiles, tibble(fname=c("K341_1_15May2024_09-31.tsv")), by="fname")
#there is both a C55 with 3 sessions and a J55 with 3 sessions, why are there two ID=55 participants?
#It's OK because Josh says: those are two separate participants

#There are two G77_b's because "b crashed after 30 trials, so I redid it" so I can use both b's
#But it's not worth writing code to merge these as it's the only such case, so delete the shorter G77b
datfiles<- rows_delete( datfiles, tibble(fname=c("G77_b_31Jul2024_11-38.tsv")), by="fname") #false start

#See if any remaining files have few rows
almostNoTrials <- datfiles %>% filter( nrows < 10 )
if ( nrow(almostNoTrials) ) {
  message("Hey, these data files you haven't taken note of have fewer than 10 trials:")
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
  
  dfWithWarnings<- read_tsv_but_handle_erroneous_columns(file_path)
  if (!(is.null(dfWithWarnings$warning))) {
    message("Warning when tried to read ",fname)
    print(dfWithWarnings$warning)
  }
  mydf <- dfWithWarnings$value
  
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
  pTrialsBlipsAfterCue <- NaN
  pTrialsLotsAfterCue<- NaN
  if ("numLongFramesAfterCue" %in% names(mydf)) {
    pTrialsBlipsAfterCue <- sum( mydf$numLongFramesAfterCue>0 ) / nrow(mydf)
    pTrialsLotsAfterCue <- sum( mydf$numLongFramesAfterCue>3 ) / nrow(mydf)
  }

  #create columns in tibble form so can be added onto the df
  timingStuff <- tibble( pTrialsLotsTimingBlips, pTrialsBlipsAfterFixatn, 
                         pTrialsBlipsAfterCue, pTrialsLotsAfterCue )
  return(timingStuff)
}

#Send all datfiles to the timingBlips analysis function to create summary columns
datfiles<- datfiles %>% rowwise() %>% 
                mutate( timingStuff = list(calcTimingBlips(fname)) ) %>%
                unnest_wider( timingStuff ) #unpack list of different timingBlip metrics

#Plenty of trials with a bunch of timingBlips
#ggplot(datfiles,aes(x=pTrialsLotsTimingBlips)) + geom_histogram(binwidth=.004) +xlim(-.1,1)
#But most of them are at very beginning of trial so should have separate column to report only later timingBlips

#Looking at this on 25-10-24, no more than 2% of trials with timingBlips after fixation! which suggests that's also true for
#earlier data runs before I programmed this facility.
#ggplot(datfiles,aes(x=pTrialsBlipsAfterFixatn)) + geom_histogram(binwidth=.005)
#similarly for after cue
#ggplot(datfiles,aes(x=pTrialsBlipsAfterCue)) + geom_histogram(binwidth=.005)# +xlim(-.1,1)

#While some files have up to 2% of trials with timingBlipsAfterCue, no files have more then 3 blips
filesMoreThanThreeAfterCue<- datfiles %>% filter(pTrialsLotsAfterCue>0)
#none with more than 3 long frames after cue, which suggests even files created before logged that
#are fine.
if (nrow(filesMoreThanThreeAfterCue)){
  message("Unexpectedly there are files with trials with more than 3 timing blips after cue.")
  message('So you need to write code to delete those.')
}
#ggplot(datfiles,aes(x=pTrialsLotsAfterCue)) + geom_histogram(binwidth=.005) + xlim(-.1,1)

#Consider files with very few rows. 


#Other than that, every remaining file has at least 90 trials, as of 25 Oct 2024


##############################################################################################
##############################################
#MATCHING UP PSYCHOPY DATAFILES WITH EDF FILES
#Get ready to match up Psychopy datafiles with EDF files

#Parse out the subject ID and session number
#Validate that they all start with a letter followed by two numbers (the subject ID)
library(stringr)
grepForUppercaseLetterFollowedByTwoDigits <- "[A-Z]\\d{2}"
validEachFname<- str_detect(datfiles$fname, grepForUppercaseLetterFollowedByTwoDigits)
datfiles$validEachFname <- validEachFname
# invalid: "j33_3_13May2024_13-18.tsv"
#j33_3 is a typo and should be uppercase J, as certified by Loretta.
# Change it manually to valid
datfiles<- datfiles %>% mutate(validEachFname = ifelse(fname == "j33_3_13May2024_13-18.tsv", TRUE, validEachFname))
if ( length( which(!(datfiles$validEachFname)) ) ) {
  message("The following files are not valid in that they don't start with letter and two digits:")
  datfiles$fname[ !(datfiles$validEachFname) ]
}
datfiles$validEachFname <- NULL #Delete because don't need this column anymore

#Take first 4 characters again to get IDsession
datfiles$IDsession<- substr(datfiles$fname,1,4)

#reorder columns for convenience of visual inspection
datfiles<- datfiles %>% relocate(IDsession, .after=fname)

#For those that have an underscore after the first 3 characters, delete the underscore
underscoresInsteadOfSession <- datfiles %>% filter( str_ends(IDsession,"_") )
#View(underscoresInsteadOfSession) #Visually inspect to make sure not too weird
#All ok based on inspection 26-10-24, so delete the underscore to make them like the others
#How to delete only the first underscore?
datfiles<- datfiles %>% mutate(IDsession = 
                       ifelse(str_ends(IDsession, "_"), #if ends with underscore
                              gsub("_", "", fname), #replace with filename with underscore deleted
                              IDsession) )
#Because ones that had underscores will now have full filename, take first 4 characters again of all to get IDsession 
datfiles$IDsession<- substr(datfiles$IDsession,1,4)

#Parse out the ID, as opposed to the session number
datfiles$ID <- substr(datfiles$IDsession,1,3)
datfiles$IDnum <- substr(datfiles$ID,2,3)

#Parse out the session number
datfiles$session <- substr(datfiles$IDsession,4,4)

#Exclusion: the mouseClickArea problem was fixed on 10 May https://github.com/alexholcombe/MOTcircularClean/commit/802a331b80c544348da255ce61827583759bb879),
#prior to that it would sometimes attribute a response to the wrong ring if participant didn't click in the best place
#Affecting all participants <= 31, in other words: 22,23,24,26,27,28,29,30,31
datfiles<- datfiles %>% filter(IDnum>31)

#To match to the EDF files, they will
#The EDF files have names like M471.EDF, M472.EDF, M473.EDF, P23a.EDF, P23b.EDF
#What I'll do is for each Psychopy datafile, I'll locate the corresponding EDF file and store its name,
# or else note that one doesn't exist

datfiles$IDvalid <- str_detect(datfiles$ID,grepForUppercaseLetterFollowedByTwoDigits)
#j33_3 is a typo and should be uppercase J, as certified by Loretta.
# Change it manually to valid
datfiles<- datfiles %>% mutate(IDvalid = ifelse(IDsession == "j333", TRUE, IDvalid))

if (any(datfiles$IDvalid==FALSE)) {
  cat("Problem! These files have the wrong format as the subject ID is not an upper-case letter followed by two digits:")
  cat( datfiles %>% dplyr::filter(IDvalid==FALSE) )
}

#Match EDF files to behavioral files
####################################
#Get a list of the EDF files
thisExpFolderEDF = file.path(thisExpFolder,"EDF") #As opposed to Psychopy data folder
EDFfiles <- dir(path=thisExpFolderEDF,pattern='.EDF')  #find all EDF files in this directory
message("Found ",length(EDFfiles)," EDF files and there are ",length(datfiles$session)," behavioral sessions (after excluding some participants).")
EDFfiles<- data.frame(fname=EDFfiles)
#Add the file size
# Get the full file paths
EDFfiles <- EDFfiles %>%
  mutate(file_path = file.path(thisExpFolderEDF, fname))
# Get the file size and add it as a new column
EDFfiles <- EDFfiles %>%
  mutate(file_size =  purrr::map_dbl(file_path, ~ file.info(.x)$size))
#Delete the full file path
EDFfiles$file_path <-NULL

#Do some validation of the EDF filenames
#Filename should start with participant's first initial, two-digit subject number, and 
#     session (4 characters in total), followed by ".EDF"

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
EDFfiles$EDF_firstLetter<- substr(EDFfiles$name,1,1)
  
#Get IDnum
EDFfiles$ID <- as.numeric( substr(EDFfiles$name,2,3) ) #returns warning because some are text because bad file name
#Exclusion: Ignore EDF files, just like behavioral files, prior to participant 31 before mouseClickArea was fixed
EDFfiles<- EDFfiles %>% filter( ID > 31 ) #This also deletes some degenerate files with text

EDFfiles$session <- substr(EDFfiles$name,4,4)
EDFfiles$IDnum <- substr(EDFfiles$name,2,3)
EDFfiles <- EDFfiles %>% arrange(IDnum)

#Will later join with datfiles dataframe by combination of ID and session columns
#But first rename EDFfiles columns so clear it refers to the EDF files
EDFfiles<- EDFfiles %>% rename(EDF_session = session, 
                               EDF_IDnum = IDnum, 
                               EDF_suffix = suffix,
                               EDF_name = name)

#Check for files with no session letter/digit. This happened back when we didn't include a session
# number, and the subsequent EDF files for a session would overwrite previous sessions if no one
# got them off the computer first.
grepForValidNameButNoSessionID <- "[A-Za-z][0-9][0-9]$"
noSessionID <- str_detect(EDFfiles$EDF_name, grepForValidNameButNoSessionID)
EDFfiles$noSessionID <- noSessionID
knownEDFfilesWithoutSession<- c("J33","J56","J57") #These are handled further down
noSessionID_EDFfiles<- EDFfiles$EDF_name[ noSessionID ]
noSessionID_EDFfiles<- setdiff(noSessionID_EDFfiles, knownEDFfilesWithoutSession)
if ( length( noSessionID_EDFfiles ) ) {
  message("The following EDF filenames are not valid in that there is no session letter or digit (4th character is not a session letter/digit), which often happened back when we didnt include a session number, and the subsequent EDF files for a session would overwrite previous sessions if no one got them off the computer first.:")
  cat(noSessionID_EDFfiles); cat("\n")
}

#For special cases, maybe I can just set the session manually and below code will then match it with corresponding
#Psychopy file
#Figure out which session J33.EDF applies to by checking date inside EDF file
#EDFfileWithPath<-file.path("..","dataRaw","youngOld","EDF")
#J33stuff <- eyelinkReader::read_edf( file.path(thisExpFolderEDF,"J33.EDF") )
#According to the EDF file preamble, J33.EDF was run at Mon May 13 14:47:36 , 
#and there is a J33_2_13May2024_12-46.tsv so I’m thinking the eyetracker computer is 2 hours off, 
#so I think this J33.EDF corresponds to the second session.
EDFfiles<- EDFfiles %>% 
  mutate(EDF_session = ifelse(str_starts(EDF_name,"J33"),
                              "2", EDF_session) )

#J56.EDF, is this the third session - because J56a.EDF and J56b.EDF also exist
#J56stuff <- eyelinkReader::read_edf( file.path(thisExpFolderEDF,"J56.EDF") )
#preamble says Jun 17 21:23:00, closest behavioural file is J56PRACTICE_18Jun2024_11-23.tsv
#which is weird because that's 10 hours off but it has 9 trials like the practice file
#So should just exclude J56.EDF
EDFfiles<- rows_delete(EDFfiles, tibble(EDF_name="J56"), by="EDF_name")
#J57preamble <- ( eyelinkReader::read_edf( file.path(thisExpFolderEDF,"J57.EDF") ) )$preamble
#Has only 10 trials, datetime 17 Jun 23:54, no behavioral file, must be abortive so delete              
EDFfiles<- rows_delete(EDFfiles, tibble(EDF_name="J57"), by="EDF_name")

#Why does C53b not match. Because the third session was also mistaknely labeled b.
#So I need to work out whether C53b.EDF is the second session or third session
#C53b_stuff <- eyelinkReader::read_edf( file.path(thisExpFolderEDF,"C53b.EDF") ) 
#preamble says 5 June 16:37, corresponding to Psychopy 3rd session file
EDFfiles<- EDFfiles %>% 
  mutate(EDF_session = ifelse(str_starts(EDF_name,"C53b"),
                              "c", EDF_session) )

##################################################################################
#Match eyetracking (EDF) files to psychopy datfiles
#Assign closest match to corresponding behavioral file
#Inspect and deal with anomalies

#Validate that it is a session number/letter, e.g. "a" or "1", of those that have one
knownBadSessionNums<- c()
grepForDigitOrLowerCaseLetter <- "[0-9a-z]"
validSession<- str_detect(EDFfiles$EDF_session, grepForDigitOrLowerCaseLetter)
if (any(!validSession)) {
  hasSessionIDbutInvalid <- which(!(noSessionID) & !validSession)
  badSessionNums<- EDFfiles$EDF_name[ hasSessionIDbutInvalid ]
  
  unknownBadSessionNums <- setdiff(badSessionNums,knownBadSessionNums)
  if ( length( unknownBadSessionNums  ) ) {
    message("The following EDF filenames are not valid in that the session is not a letter or digit:")
    cat(unknownBadSessionNums)
  }
}
#there are two psychopy files for C53b, “C53_b_05Jun2024_14-05.tsv” and “C53_b_05Jun2024_14-35.tsv”, 
#Josh says the later one is the third session, so change its session to c
datfiles<- datfiles %>% 
  mutate(session = ifelse(str_starts(fname,"C53_b_05Jun2024_14-35.tsv"),
                          "c", session) )
#There are two participants numbered 55, C55 and J55, so I need to match by first letter to disambiguate
datfiles$firstLetter<- substr(datfiles$IDsession,1,1)

#See how many behavioral files don't seem to have a match in the EDFfiles tibble
#anti_join returns all rows from the first tibble where there are matching values in the second tibble
noMatchingEDFfile<- 
  anti_join(datfiles, EDFfiles, 
            by = c("IDnum"="EDF_IDnum", "firstLetter"="EDF_firstLetter", "session"="EDF_session"))
noMatchingEDFfile<- noMatchingEDFfile %>% arrange(IDnum)

knownToNotHaveMatch<- c("J331","j333", #only one EDF file, for session 2, which was disambiguated above
                        "T52a","T52b","T52c",#Eyetracker wouldn't calibrate with glasses
                        "C53b") #One EDF file overwritten
#datfiles %>% filter(IDnum==53) %>% select(fname,IDnum,firstLetter,session)

#Have to include first letter because of the participant number (55, I think) that was used twice accidentally
temp<-left_join(datfiles, EDFfiles, by = c("IDnum" = "EDF_IDnum", "session" = "EDF_session","firstLetter"="EDF_firstLetter"))

# Identify and display the rows in EDFfiles that match multiple rows in datfiles
matches <- temp %>%
  group_by(IDnum, session, firstLetter) %>%
  filter(n() > 1) %>%
  ungroup()
if (nrow(matches)) {
  message('One or more multiple matches happened, you better look into that.')
  print(matches)
}

#For the EDF files that don't have a session number, could try to figure out which session they
#correspond to by looking at their date/time
#J33 I know about (Josh said it disappeared),
unexplained<-  noMatchingEDFfile$IDsession[ !(noMatchingEDFfile$IDsession %in% knownToNotHaveMatch) ]
if (length(unexplained)) {
  message("Unaccounted-for absences of EDF files:")
  print(unexplained)
}

#Add a column to datfiles indicating whether there is a match
#Can do that with the noMatchingEDFfile by reducing it to the ID and session column and then joining
noMatchingEDF <- noMatchingEDFfile %>% select(IDnum,session,firstLetter) %>% arrange(IDnum)
noMatchingEDF$EDFmatchExists <- FALSE
joined <- left_join(datfiles, noMatchingEDF, 
                            by = c("IDnum", "session","firstLetter"))

#Now match the matches (as opposed to the non-matches, added above), so that have record of the EDF filename
EDFfiles$ID<-NULL #Delete this because redundant with joined and causes two columns
joinedWithEDF<- left_join(joined, EDFfiles, 
                          by = c("IDnum"="EDF_IDnum", "session"="EDF_session","firstLetter"="EDF_firstLetter"))

numSs<- length( unique(datfiles$IDnum) )
numSsWithoutMatchingEDFfile<- length( unique(noMatchingEDFfile$IDnum) )
message( paste(numSs,"Ss total, of which",
               numSsWithoutMatchingEDFfile,"have at least one EDF file missing."))

#A weird consequence of how I did this is that all the rows that *do* have a match are NA for EDFmatchExists,
# need to change that to TRUE
joined <- joinedWithEDF %>% mutate(EDFmatchExists = replace_na(EDFmatchExists,TRUE))
#double-check the result is consistent with the count I did above
numSsNoMatchingEDFfile<- nrow( unique( filter(joined,EDFmatchExists==FALSE) %>% select(IDnum) ) )
message( paste( length( unique(joined$IDnum) ),"Ss total, of which",
                numSsNoMatchingEDFfile,"have at least one EDF file missing."))

message( paste(nrow(joined),"files total, of which",
               summarize(joined, trues=sum(EDFmatchExists))$trues,
               "have a matching EDF file with a session number."))

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

#Next, save data to anonymised data folder and only then do eyetracking filtering.
#Ideally would strip all date and time info from the anonymised data
#That would mean re-saving each individual datafile as  IDnum + sessionNum.tsv and the EDF file

#EDF file privacy issue #####################################
#The preamble variable of the EDF file has exact date and time, so to anonymize should strip that out
#Could read each EDF file with eyelinkReader and then save all the variables inside it into an R data file

#In the meantime, could save the behavioral datfiles and then do the EDF file analyses in this private folder 
#For each row of joined, save that fname as IDnum_sessionNum.

#Assuming this script is being run from "dataPreprocess" subdirectory, so need to go one level up,
#then down into anonymized directory
destinationDir <- file.path("..",anonymizedDir,destinationStudyFolderName)
if ( !file.exists(destinationDir) ) {
  message( paste(destinationDir," destination directory does not exist!") )
}

#Read in datfiles #########################################
#Read all the data into tibbles and then save it in the destination.
#This involves handling that later files have different numbers of columns because I added more timing variables

#Get the two kinds of column specification,for the early files with only timingBlips and the later with more columns
# to check against each file as it comes in
earlyFileWithoutSessionColumn<- "M22a_05Apr2024_11-02.tsv"
earlyFileWithSessionLetter<- "S381_1_17May2024_09-08.tsv"
earlyFileWithSessionNum<- "J51_a_03Jun2024_13-17.tsv"
lateFile<- "D61_a_25Jun2024_11-12.tsv"

columns_spec_early_file_without_session <- readr::spec_table(   file.path(thisExpFolderPsychopy,earlyFileWithoutSessionColumn)     )
columns_spec_early_file_with_sessionLetter <- readr::spec_table(   file.path(thisExpFolderPsychopy,earlyFileWithSessionNum)     )
columns_spec_early_file_with_sessionNum <- readr::spec_table(   file.path(thisExpFolderPsychopy,earlyFileWithSessionLetter)     )
columns_spec_late_file <- readr::spec_table(   file.path(thisExpFolderPsychopy,lateFile)     )

#Why one with comment ends up with just 53 rows
#thisRow<- joined %>% filter(fname=="M321_1_10May2024_13-43.tsv")
#Problem is that the comment has a newline in it, which of course screws things up. Changed by hand in partiicpant notes

#Read all the behavioral files in and aggregate them into one massive tibble
#J55_b_18Jun2024_12-20.tsv
for (i in 1:nrow(joined)) {
#for (i in 1:59) {
  thisRow <- joined[i,]
  thisFile<- file.path(thisExpFolderPsychopy,thisRow$fname)
  
  rawDataLoad=tryCatch( 
    read_tsv_but_handle_erroneous_columns(thisFile),  
    error=function(e) { 
        stop( paste0("ERROR reading the file ",fname," :",e) )
    } 
  )
  if (!(is.null(rawDataLoad$warning))) {
    message("Warning when tried to read ",thisFile)
    print(dfWithWarnings$warning)
  }
  rawDataThis <- rawDataLoad$value
  
  #Validate columns in the file, that they're as expected
  #Should be one of 2 possible column specs
  cols_spec<- readr::spec_table( thisFile  )
  
  isEarlyFileWithoutSession<- identical(cols_spec, columns_spec_early_file_without_session)
  isEarlyFileWithSessionNum<- identical(cols_spec, columns_spec_early_file_with_sessionNum)
  isEarlyFileWithSessionLetter<- identical(cols_spec, columns_spec_early_file_with_sessionLetter)
  isLaterFile<- identical(cols_spec, columns_spec_late_file)
  
  if (!isEarlyFileWithoutSession && !isEarlyFileWithSessionNum && !isEarlyFileWithSessionLetter && !isLaterFile) {
    #See what columns are different
    # Extract column names
    cols_spec_names <- names(cols_spec$cols)
    cols_laterFile <- names(columns_spec_late_file$cols)
    # Find columns in cols_spec that are not in columns_spec_late_file
    colsNotInLaterFile <- setdiff(cols_spec_names, cols_laterFile)
    laterFileColsNotInCurrent <- setdiff(cols_laterFile, cols_spec_names)
    
    #Or compare to columns_spec_early_file_with_sessionNum
    cols_early<- names(columns_spec_early_file_with_sessionNum$cols)
    colsNotInEarlyFile <- setdiff(cols_spec_names, cols_early)
    earlyFileColsNotInCurrent<- setdiff(cols_early,cols_spec_names)
    
    message( paste0("File,",thisFile," is not in any of the three formats that I know about") )   
    stop('Its unique columns are:',colsNotInLaterFile,laterFileColsNotInCurrent)
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
  rawDataThis$pTrialsLotsAfterCue <- thisRow$pTrialsLotsAfterCue
  #rawDataThis$pTrialsLongFramesAfterCue <- thisRow$pTrialsLongFramesAfterCue
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
      message( paste("newCols in",thisRow$fname,"will now add to old dataframe, and those new cols are:") )
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
} #Finished reading each behavioral file and aggregating into one huge tibble

#Summarise number of files
numSs<- length( unique(rawData$IDnum) )
numSessions <- n_groups(  rawData %>% group_by(IDnum,session)                          )
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
#Subjects 23 to 31 had below 70% accuracy, which is a red flag, which was explained 
#due to the mouseClickArea problem making the program malfunction, data now thrown out above.

#Check for participants with less than 71% overall correct.
terriblePerformers<- avgCorrOverall %>% filter(correct<0.71)
if (nrow(terriblePerformers)) {
  message("The following participants had less than 71% overall correct:")
  print(terriblePerformers)
}
#Participant 69 has about 67% accuracy. No immediate explanation from notes.
#Participant 73 has 70% accuracy. No immediate explanation from notes.

#Save anonymised Psychopy data, for later loading by doAllAnalyses.R
destination_fname<- file.path(destinationDir,
                              paste0(destinationStudyFolderName,"_psychopy_data.tsv"))
#save in tsv format. Only thing that's sometimes screwed this up is if comment has a newline in it
readr::write_tsv(rawData, file = destination_fname)
message( paste("Anonymised (first letter, date and time removed) data aggregated into single file and saved to",destination_fname) )


#Copy the matching (don't include the degenerate ones or those of the excluded particiants) EDF files over 
#To get rid of first letter from EDF files, would have to save them with a new name
#, simply with the first letter stripped. But then would have to specially handle 55 and any other 
#participant number that was used twice.

library(eyelinkReader)
#Create function that saves EDF file contents without date/time part of preface,
#saving entire thing as an R object, but without
#the preamble that contains the date/time information to prevent public knowledge
save_EDF_without_datetime <- function(thisExpFolderEDF,EDFpathAndFname,destinationPathAndFname) {
  
  #In case error reading EDF file, need to catch errors
  resultWithWarnings<- myTryCatch( 
    EDFstuff<- eyelinkReader::read_edf(EDFpathAndFname)
  )
  if (!is.null(resultWithWarnings$error)) {
    message("When trying to read ",EDFpathAndFname," got this error:",resultWithWarnings$error)
    #Make copy to send them all to SR research
    
    file.copy(from = EDFpathAndFname, to = file.path(thisExpFolderEDF, "unreadableButGoodSubjectCopy"))
    #NEED TO CHANGE FILE GUIDE TO say EDF file not available
  } else {
    #The absolute date/time is specified only in EDFstuff$preamble[1], so delete that
    EDFstuff$preamble[1] <- "Date/time redacted for privacy, to reduce chance of re-identification of participant identities"
    saveRDS(EDFstuff,file=destinationPathAndFname,compress=T)
  }
}

setupFilenamesAndResaveEDFcontentsWithoutDateTime<- function(EDFname) {
  EDFfname<- paste0(EDFname,".EDF")
  message("About to read ",EDFfname)
  EDFfnameWithPath<- file.path(thisExpFolderEDF,EDFfname)
  destination_fname<- paste0(EDFfname,".RDS")
  destination<- file.path(destinationDir,"EDFs",destination_fname)
  save_EDF_without_datetime(thisExpFolderEDF,EDFfnameWithPath,destination)
}

#Prevent public date/time info by saving entire thing as an R object, but without
#the preamble that contains the date/time information
#EDF1<- anonymisedMatchingOfDataAndEDF$EDF_name[1]
#setupFilenamesAndResaveEDFcontentsWithoutDateTime(EDF1)

EDFreadErrors<- c('E463','W87c', 'W78c', 'S88c', 'S75c', 'S393', 'R91c', 'M74c', 'M323', 'L84c', 'L81c', 'L72c', 'K86c', 'J57c', 'J51c', 'H83c', 'G58c')
#Add readError column and sort by that so can see sizes
EDFfiles <- EDFfiles %>%
  mutate(EDFreadError = if_else(EDF_name %in% EDFreadErrors, TRUE, FALSE))
EDFfiles <- EDFfiles %>% arrange(EDFreadError)
#Plot shows nothing strange about file sizes of those with EDF read error
#ggplot(EDFfiles, aes(x = file_size, fill = EDFreadError)) +
#  geom_histogram( position = "stack") + theme_minimal()

#E463 has read error, so does W87c, W78c, S88c, S75c, S393, R91c, M74c, M323, L84c, L81c, L72c,
# K86c, J57c, J51c, H83c, G58c 
#E463path<- file.path(thisExpFolderEDF,"E463.EDF" ) 
#EDFstuff<- eyelinkReader::read_edf(E463path)

#Also save all the information about the files in joined including EDF file match ,
# save everything except the filename, because it has the date/time
anonymisedMatchingOfDataAndEDF<- joined
anonymisedMatchingOfDataAndEDF$fname <- NULL
anonymisedMatchingOfDataAndEDF$IDsession <- NULL
anonymisedMatchingOfDataAndEDF$ID <- NULL
#add column for whether the EDF file can't be read
anonymisedMatchingOfDataAndEDF <- anonymisedMatchingOfDataAndEDF %>%
  mutate(EDFreadError = if_else(EDF_name %in% EDFreadErrors, TRUE, FALSE))
destination_fname<- file.path(destinationDir,
                              paste0(destinationStudyFolderName,"_files_guide.tsv"))
write_tsv(anonymisedMatchingOfDataAndEDF, file = destination_fname)

message("Now will read in all EDF files and save their contents without datetime. This will cause a lot of annoying messages because SRresearch currently won't let you mute them.")
EDF_names <- anonymisedMatchingOfDataAndEDF %>% #filter(EDF_name<"E463") %>%
                filter(EDFmatchExists==T) %>% select(EDF_name)
# Apply the function to each row of anonymisedMatchingOfDataAndEDF$EDF_name
purrr::walk(EDF_names$EDF_name, setupFilenamesAndResaveEDFcontentsWithoutDateTime)



