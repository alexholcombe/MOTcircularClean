#expecting current working directory to be top level of this git-indexed project, and this file to be in top level - dataPreprocess/

#Gets behavioral data and combines with eyetracking result and anonymises 

#Eyetracking
#Looks for eyetracking file, expects it in wide format (one row for each trial)     
#Expects eyetracking file name to be paste0(withoutSuffix,"EyetrackingReport.txt")
#Expects in the eyetracking file that there should be a column called Exclusion

#Load function that can figure out whether participant moved their eyes too much on each trial
#source('dataPreprocess/eyetracking/summariseEyelinkReport.R')

expFoldersPrefix= "dataRaw/"
expFolders <- c("youngOld")
expFoldersPostfix = "" #"/rawdata"
destinationName = "youngOld"
destinatnDir<-"dataAnonymized/" #where the anonymized data will be exported to
anonymiseData <- TRUE

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

for (expi in 1:length(expFolders)) {
  thisExpFolder = paste0(expFoldersPrefix,expFolders[expi], expFoldersPostfix)
  print(paste0("From '",thisExpFolder,"'"))
  #Create list of subjects from file names, hopefully can get away with not having one folder per participant
  files <- dir(path=thisExpFolder,pattern='.tsv')  #find all data files in this directory
  
  
  foldersThisExp <- list.dirs(path=thisExpFolder,recursive=FALSE) #each folder should be a subject
  print("Loading data from folders:"); print(foldersThisExp)
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