#expecting current working directory to be top level of this git-indexed project, and this file to be in top level - dataPreprocess/
#Gets behavioral data and combines with eyetracking result and anonymises 
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#The raw data, both Psychopy files and EDF files, are downloaded from Sharepoint place linked from the MOTyoungOld GoogleDrive folder

#How should we match up Psychopy files and EDF files?
#There will always be a Psychopy file, but not always an EDF file, so start with Psychopy files
#to parse out subject name and session number.
#Then look for matching EDF file. 
#Will need plenty of one-off intervention for early false starts with naming, plus a separate column for pilot/real participant
#Also need to get age and sex from somewhere.. a manual copy of the Sharepoint participant sheet with only those columns?

#Eyetracking files match

#Looks for eyetracking file, expects it in wide format (one row for each trial)     
#Expects eyetracking file name to be paste0(withoutSuffix,"EyetrackingReport.txt")
#Expects in the eyetracking file that there should be a column called Exclusion

#Load function that can figure out whether participant moved their eyes too much on each trial
#source('dataPreprocess/eyetracking/summariseEyelinkReport.R')

expFoldersPrefix= file.path("..","dataRaw/")
expFolder <- "youngOld"
destinationName = "youngOld"
destinatnDir<-"dataAnonymized/" #where the anonymized data will be exported to
anonymiseData <- TRUE

thisExpFolder = file.path(expFoldersPrefix,expFolder)
thisExpFolderPsychopy = file.path(thisExpFolder,"Psychopy") #As opposed to EDF folder
  #paste0(expFoldersPrefix,expFolder,"Psychopy",expFoldersPostfix)
print(paste0("Finding files in '",thisExpFolderPsychopy,"'"))

#Create list of subjects from file names
datafiles <- dir(path=thisExpFolderPsychopy,pattern='.tsv')  #find all data files in this directory

#remove the "trialHandler.tsv" files from the list. That is basically vestigial from when I was debugging
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

#Delete 0-row file , but don't know why it's zero yet!!!!!!!!!!!!!!!!!!!!!!!!!!
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_c_12Jun2024_14-12 - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_c_12Jun2024_14-12.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("D69_b_03Jul2024_12-36.tsv")), by="fname")
#Loretta says lxx was just a test of the program, not a participant
datafiles<- rows_delete( datafiles, tibble(fname=c("lxx_a_27Jun2024_09-43.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_a_12Jun2024_13-02trialHandler - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_b_12Jun2024_13-39trialHandler - Copy.tsv")), by="fname")
datafiles<- rows_delete( datafiles, tibble(fname=c("C55_c_12Jun2024_14-14trialHandler - Copy.tsv")), by="fname")

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
ggplot(datafiles,aes(x=pTrialsBlipsAfterFixatn)) + geom_histogram(binwidth=.004) +xlim(-.1,1)
ggplot(datafiles,aes(x=pTrialsLongFramesAfterCue)) + geom_histogram(binwidth=.004) +xlim(-.1,1)
table(datafiles$pTrialsBlipsAfterFixatn)

criterionProportnNumTimingBlipsForThrowingOutTrial = .02 

if (any(datafiles$pTrialsLongFramesAfterCue > criterionProportnNumTimingBlipsForThrowingOutTrial, na.rm=T)) {
  message('Some files actually had trials with more than ',criterionProportnNumTimingBlipsForThrowingOutTrial,
          'timing blips after the cue! So you need to write code to delete those.')
}

#There might be one participant without enough columns in the header, maybe the one Yuenchen ran that we can't find the data for

#Consider removing files with very few rows.  All good as of 10 Jul 2024
#datafiles<- datafiles %>% arrange(nrows)
#head(datafiles)

            
#Parse out the subject ID and session number
datafiles$IDsession<- substr(datafiles$datafiles,1,4)
#Validate that they all start with a letter followed by two numbers
grepForUppercaseLetterFollowedByTwoDigits <- "^[A-Z]\\d{2}$"
library(stringr)
ID <- substr(datafiles$datafiles,1,3)

#For those that have an underscore after the first 3 characters, delete the underscore
underscoresInsteadOfSession <- datafiles %>% filter( str_ends(IDsession,"_") )
#For all of them, just need to delete the underscore
datafiles<- datafiles %>% mutate(IDsession = 
                       ifelse(str_ends(IDsession, "_"), #if ends with underscore
                              gsub("_", "", datafiles), #replace with filename with underscore deleted
                              IDsession) )
#Then take first 4 characters again of all to get IDsession
datafiles$IDsession<- substr(datafiles$datafiles,1,4)

#Deal with those with underscore instead of session number
underscoreInsteadOfSession<- datafiles %>% filter(substr(datafiles,4,4)=="_")
#All of them have session number right after underscore, except S26 and S45
anomalies <- textConnection("
S45_1_27May2024_11-43.tsv
S45_1_27May2024_11-45.tsv
S451_1_27May2024_11-10.tsv

S26_1_01May2024_11-19.tsv #Can be deleted, <1Kb
S26_1_01May2024_11-21.tsv #Can be deleted, <1Kb
S26_1_01May2024_12-34.tsv #This is session 1 being redone at the end
S26_2_01May2024_11-43.tsv
S26_3_01May2024_12-13.tsv")
close(anomalies)

datafiles %>% mutate(IDsession = 
                      ifelse(str_starts(datafiles, "j33_3_13May2024"), "J333", 
                        IDsession) )

datafiles %>%
  mutate(IDsession = if(str_starts(datafiles, "j33_3_13May2024"))


datafiles %>%
  filter(str_starts(datafiles, "j33_3_13May2024"))


datafiles$IDvalid <- str_detect(ID,grepForUppercaseLetterFollowedByTwoDigits)
if (any(datafiles$IDvalid==FALSE)) {
  cat("Problem! These files have the wrong format as the subject ID is not an upper-case letter followed by two digits:")
  cat( datafiles %>% dplyr::filter(IDvalid==FALSE) )
  #j33_3 I can see it's a typo and should be uppercase J
}
shouldBeUppercaseLetter <- substr(ID,1,1)


result <- str_detect(my_list, "^[A-Z]\\d{2}$")
print(result)

result <- str_detect(my_list, "^[A-Za-z]+$")
print(result)

lapply(dataFiles,substr(1,4))

for (f in 1:length(datafiles)) {
  thisFname = datafiles[f]
  IDandSession<- substr(thisFname,1,4)
ID<- substr(IDandSession,1,3)
EDF$fixations$ID <- ID; EDF$blinks$ID<- ID
session <- substr(IDandSession,4,4)
if (grepl("^[a-z]$", session)) #session is lower-case letter
  session<- match( tolower(session), letters) #returns 1 for 'a', 2 for 'b', etc.

EDF$fixations$session<- session
EDF$blinks$session<- session
  
  
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

rotX <- function(ch,x) 
{ #rotate each letter of a string ch by x letters thru the alphabet, as long as x<=13
  old <- paste(letters, LETTERS, collapse="", sep="")
  new <- paste(substr(old, 2*x+1, 26*2), substr(old, 1, 26), sep="")
  chartr(old, new, ch)
}
if (anonymiseData) {
  keyFile = paste0('dataPreprocess/',"anonymisationKey.txt")
  if ( !file.exists(keyFile) ) {
  	stop(paste0('The file ',keyFile, ' does not exist!'))
  }
  linesFromFile= readLines(keyFile,warn=FALSE)
  key = as.numeric(linesFromFile[1]) #the key to encrypt the data with
  subjectNotanonymised<- dat$subject
  dat$subject <- rotX(subjectNotanonymised,key) #anonymise subject initials by rotating them by key characters
  print('Mapping from name to anonymised:')
  print(table(subjectNotanonymised,dat$subject))
}
	
#table(d$speedRank,d$numObjects,d$numTargets,d$subject)

#Save anonymised data for loading by doAllAnalyses.R
fname=paste(destinatnDir,destinationName,sep="")
save(dat, file = paste(fname,".RData",sep=""))
write.csv(dat, file = paste(fname,".csv",sep=""))
print(paste("saved data in ",fname,".RData and ",fname,".csv",sep=""))