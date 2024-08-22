library(eyelinkReader)
library(tidyverse)
rm(list = ls()) #Clear workspace so that any code executed before won't contaminate this run

#Explore how things went with eyetracking youngOld
widthPix = 800; heightPix = 600

#Calculate time by which it matters that they're fixating, which is when targets are no longer cued
fixationInterval = c(800,1300)
trackingExtraTime = 1.2 #seconds, which is how long the blobs are cued, but that's after the fixation interval
trialMinTimeCuesOff = min(fixationInterval) + trackingExtraTime

#Calculate time by which the objects must have stopped moving.
#Ideally would read in trialDurTotal, from behavioral file or else also send it as a message to Eyelink
trialDurMin = 2
trackVariableInterv = c(0,2.5)
maxTrialDur = trialDurMin + trackingExtraTime + max(trackVariableInterv)
trialDurTotalLowerLimit = maxTrialDur - max(trackVariableInterv)

#expecting current working directory to be top level of this git-indexed project, and this file to be in top level - dataPreprocess/
EDFfolder<- file.path("..","..","dataRaw","youngOld","EDF")
#EDFfolder<- file.path("EDFmomoTemp")

EDFfiles <- list.files(path=EDFfolder)  # c("A20b.EDF","S451.EDF","E401.EDF","M433.EDF","M471.EDF")
items_to_remove <- c("j5.EDF", #Loretta said it's nothing and it's only one trial
                     "tea.EDF", "tema.EDF", "temp.EDF")
  
EDFfiles <- setdiff(EDFfiles, items_to_remove)

#Around 29 May, we switched from using 1,2,3 for session number to a,b,c
fnames<-EDFfiles
fixatns<- data.frame(); blinks<- data.frame()

if (file.exists("fixatns.tsv") & file.exists("blinks.tsv")) { #Assume this means already read in all EDF files and saved them
  readInEDFfiles<- FALSE
  library(readr)
  fixatns<- readr::read_tsv("fixatns.tsv", show_col_types=FALSE)
  blinks<- readr::read_tsv("blinks.tsv", show_col_types=FALSE)
} else { #Try to read in all the EDF files
  failedFiles<-c()
  for (f in 1:length(fnames)) {
    EDFname <- file.path(EDFfolder, fnames[f])
    #some EDF files cannot be read, so need to catch that error and omit the file
    succeeded<- tryCatch(
      {
        EDF<- eyelinkReader::read_edf(EDFname, import_samples = TRUE,
                                      sample_attributes = c('time', 'gx', 'gy'))
        TRUE
      },
      error = function(cond) {
        message( paste(EDFname," yielded an error from read_edf:") )
        message( conditionMessage(cond) )
        # Choose a return value in case of error
        FALSE
      },
      warning = function(cond) {
        message(paste("Trying", EDFname," gave a warning from read_edf:"))
        message( conditionMessage(cond) )
        # Choose a return value in case of warning
        TRUE
      },
      finally = message("Processed ", EDFname)
    )
    if (!succeeded) {
      failedFiles <- c(failedFiles,EDFname)
    }
    if (succeeded) {
      EDF$fixations$fname <- fnames[f]
      EDF$blinks$fname <- fnames[f]
      if (substr(fnames[1],5,8)!=".EDF") {
        warning("File name should be eight characters and end with .EDF")
      }
      #Extract date from preamble so can optionally sort by date
      dateString<-EDF$preamble[1] #E.g. "DATE: Mon Jun 17 21:00:03 2024"
      dateString<- substr(dateString,11,99) #"Jun 17 21:00:03 2024"
      date<- parse_date_time(dateString, orders = "b d H:M:S Y", tz="Etc/GMT-10")
      EDF$fixations$date<- date
      #parse out session
      IDandSession<- substr(fnames[f],1,4)
      ID<- substr(IDandSession,1,3)
      EDF$fixations$ID <- ID; EDF$blinks$ID<- ID
      session <- substr(IDandSession,4,4)
      if (grepl("^[a-z]$", session)) #session is lower-case letter
        session<- match( tolower(session), letters) #returns 1 for 'a', 2 for 'b', etc.
      
      EDF$fixations$session<- session
      EDF$blinks$session<- session
      fixatns<- rbind(fixatns, EDF$fixations)
      blinks<- rbind(blinks, EDF$blinks)
    }
  }

  if (length(failedFiles)) {
    message("Failed to read files:",failedFiles)
  }
  
  library(readr)
  readr::write_tsv(fixatns,"fixatns.tsv") #In case want to use later without having to re-read all the files
  readr::write_tsv(blinks,"blinks.tsv") #In case want to use later without having to re-read all the files
} #file exists

fixatns$IDnum <- substr(fixatns$ID,2,3)
blinks$IDnum <- substr(blinks$ID,2,3)
#table(fixatns$ID,fixatns$session)

toThrowAway<- tibble(ID=c("lor","lxx")) #Non-legit participant files
#Delete non-legit files
fixatns<- rows_delete(fixatns, toThrowAway, by="ID")
blinks<- rows_delete(blinks, toThrowAway, by="ID")

#The mouseClickArea problem was fixed on 10 May (SHA:802a331b80c544348da255ce61827583759bb879),
#prior to that it would sometimes attribute a response to the wrong ring if participant didn't click in the best place
#Affecting all participants < 31, in other words: 22,23,24,26,27,28,29,30,31
fixatns<- fixatns |> filter(IDnum>31)
blinks<- blinks |> filter(IDnum>31)

#label pilot participants
pilotParticipants <- c("A12", "M13", "A14", "J16", "D15", "N17", "A18", "J19", 
          "A20", "K21", "M22", "P23", "M24", "R24", "P29", "S31", "S45", "E46")
fixatns$pilot <- fixatns$ID %in% pilotParticipants
blinks$pilot <- blinks$ID %in% pilotParticipants

avgFix<- fixatns |> group_by(ID) |> 
  summarise(meanX = mean(gavx), meanY = mean(gavy), date=first(date))

#Ideally, cange date variable to ordinal rank to avoid exact timestamps (privacy concern)
#But that would have to be done by having loadAnonymiseSaveData read in all the EDF files,
# strip out the timestamps, and then re-save. But because can't re-save as EDF file,
# would have to make a massive tibble for fixations, for blinks, and with raw if potentially want to analyze that

#Calculate whether in second 50% of subjects run, to break up graphs by first versus second bit
avgFix<- avgFix |> mutate(dateHalf =    date > quantile(avgFix$date,probs=c(.5)) )

#Plot
avgEachSubjectG<- ggplot(avgFix, aes(x= meanX, y= meanY, label=ID))+  
                          geom_point() +geom_text(hjust=0, vjust=0)
commonScreenResolutions <- data.frame( widthPix = c(800,1024,1512,1600,1600), 
                                       heightPix=c(600,768,982,900,1200),
                                       resolution=c("800x600","1024x768","1512x982","1600x900","1600x1200"))
av<-avgEachSubjectG + geom_point(data=commonScreenResolutions, 
                                 aes(x=widthPix/2,y=heightPix/2,label=NULL,color=resolution)) + 
  ggtitle('Average fixation location of each participant',subtitle=', with colored points showing centers of different screen resolutions') +
  theme_bw() + theme( panel.grid.minor=element_blank(),panel.grid.major=element_blank() )

av <- av + facet_grid(.~dateHalf)
show(av)

avgEachSubjectG<- ggplot(avgFix, aes(x= meanX, y= meanY, label=ID))+  geom_point() +
                          geom_text(hjust=0, vjust=0)
av<-avgEachSubjectG + geom_point(data=commonScreenResolutions[1:2,], 
                             aes(x=widthPix/2,y=heightPix/2,color=resolution,label=NULL)) +
                ggtitle('Average fixation location of each participant, with centers of different screen resolutions in color')
show(av)

overallAvgFix<- fixatns |> summarise(meanX = mean(gavx), meanY = mean(gavy))

avgFix<-avgFix |> mutate(deviatnX = meanX - widthPix/2, deviatnY = meanY - heightPix/2)
avgFix<-avgFix |> mutate( distFromCtr = sqrt(deviatnX^2 + deviatnY^2) )
fixatns<-fixatns |> mutate( distFromCtr = 
                             sqrt( (gavx-widthPix/2)^2 + (gavy-heightPix/2)^2 ) )

#Move date and datehalf to last columns
avgFix<-avgFix |> relocate(c(date,dateHalf), .after=last_col())

#check if deviation from screen center of average fixation location is greater than criterionDist pixels
criterionDist<- 40
distMoreThanCriterion<- avgFix |> filter(distFromCtr > criterionDist)
if (nrow(distMoreThanCriterion)==0) {
  msg = paste0("Great! All ", length(unique(avgFix$ID)), " participants' average fixation location",
      " did not exceed ", criterionDist, " pixels from the center. The worst case was: ")
  maxDist<- avgFix |> top_n(1,distFromCtr)
  print(msg)
  print(maxDist)
}

if (nrow(distMoreThanCriterion)>0) {
  msg = paste0("Average fixation location should be near screen center (",widthPix/2,",",heightPix/2,") but")
  msg=paste0(msg,"of the ",nrow(avgFix)," participants, it's more than")
  msg=paste(msg,criterionDist,"pixels from that for the following",nrow(distMoreThanCriterion),"participants:")
  print(msg)
  print(distMoreThanCriterion)
  msg="This happens because the eyetracker or its calibration was no good, or the participant didn't look near center much."
  print(msg)
}

#EXAMINE INDIVIDUAL PARTICIPANT
#What is with J33? Worst participant
library(stringr)
#Plot distance from fixation over time
pp<- fixatns %>% filter(sttime_rel>800) %>% filter(str_detect(ID,"J33")) %>%
  ggplot( aes(x=sttime_rel, y=distFromCtr, color=trial, shape=session) ) +
  geom_point() + geom_line(aes(group=trial)) +
  ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
  ggtitle('Fixation distance from center over each trial') +
  facet_grid(rows=vars(IDnum),cols=vars(session)) #
show(pp)
plotpath<- file.path('plots')
ggsave( file.path(plotpath, paste0('A18','.png'))  )

#Constrain axis and replace outliers with extreme value on axis
SsPerPlot = 2
minValToShow = 250; maxValToShow = 550
numPlots = round( max(fixatns$IDnum)/SsPerPlot )
#numPlots =1
for (i in 1:numPlots)  {
  #Plot distance from fixation over time
  pp<- fixatns %>% 
    mutate(outlier = ifelse(gavx<minValToShow | gavx>maxValToShow, TRUE, FALSE)) %>%  #determine outliers
    mutate(gavx =  ifelse(gavx > maxValToShow, maxValToShow, gavx)) %>%     #replace outliers
    mutate(gavx =  ifelse(gavx < minValToShow, minValToShow, gavx)) %>%
    filter(idNum <= i*SsPerPlot) %>% filter(idNum > (i-1)*SsPerPlot) %>%
    filter(sttime_rel>800) %>%
    ggplot( aes(x=sttime_rel, y=gavx, color=trial, shape=outlier) ) +
    ylim(minValToShow,maxValToShow) + #restrict axes
    scale_shape_manual(values = c(16, 21)) + #filled circle and unfilled, for outliers
    geom_point(fill='red') + #only the outlier symbol is fillable
    geom_line(aes(group=trial)) +
    ylab('gavx (pixels)') + xlab('sttime_rel (ms)') +
    ggtitle('gavx') +
    #coord_cartesian(ylim=c(0, 200)) +
    #facet_wrap(vars(ID)) 
    facet_grid(rows=vars(ID),cols=vars(session)) #
  show(pp)
  plotpath<- file.path('plots')
  ggsave( file.path(plotpath, paste0('gavxSamples',(i-1)*SsPerPlot+1,'-',i*SsPerPlot,'.png'))  )
}  

#negative gavx occurs when person seems to look offscreen but eyes aren't lost
#  E40 session 3 for example
E403<- fixatnsNotPilot %>% filter(fname=="E403.EDF")
E403 %>% filter(gavx < 0) %>% 
  ggplot( aes(x=gavx, y=gstx) ) +
  geom_point() + geom_line(aes(group=trial))

#DISTANCE FROM FIXATION
#Graph 5 Ss at a time. 
SsPerPlot = 5
numPlots = round( max(fixatnsNotPilot$idNum)/SsPerPlot )
for (i in 1:numPlots)  {
  #Plot distance from fixation over time
  pp<- fixatnsNotPilot %>% 
    filter(idNum <= i*SsPerPlot) %>% filter(idNum > (i-1)*SsPerPlot) %>%
    filter(sttime_rel>800) %>%
    ggplot( aes(x=sttime_rel, y=distFromCtr, color=trial, shape=session) ) +
    geom_point() + geom_line(aes(group=trial)) +
    ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
    ggtitle('Fixation distance from center, columns are sessions') +
    coord_cartesian(ylim=c(0, 200)) +
    #facet_wrap(vars(ID)) 
    facet_grid(rows=vars(ID),cols=vars(session)) #
  show(pp)
  plotpath<- file.path('plots')
  ggsave( file.path(plotpath, paste0('distOverTime',(i-1)*SsPerPlot+1,'-',i*SsPerPlot,'.png'))  )
}

#Just look at first 2900ms, because first 800->1300ms is fixation, then blobs cued for 1200ms
#So really I don't have to exclude until after 800+1200=2000
pp<- fixatns %>% filter(pilot==FALSE) %>%
  filter(sttime_rel<2900) %>%
  ggplot( aes(x=sttime_rel, y=distFromCtr, color=trial) ) +
  geom_point() + geom_line(aes(group=trial)) +
  ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
  ggtitle('Fixation distance from center over each trial') +
  facet_grid(rows=vars(fname))+ # facet_wrap(vars(fname)) +
  geom_vline(xintercept=800, color='grey') + 
  geom_vline(xintercept=1300, color='grey') +
  geom_vline(xintercept=2000, color='darkgreen') +
  geom_vline(xintercept=2500, color='darkgreen')
show(pp)
plotpath<- file.path('dataForTestingOfCode', 'examplePlots')
ggsave( file.path(plotpath, paste0('distOverTime','.png'))  )

#DRIFT
#Show drift over trials by plotting average position of first part of trial
SsPerPlot = 7
numPlots = round( max(fixatnsNotPilot$subjNum)/SsPerPlot )
for (i in 1:numPlots)  {
  #Plot distance from fixation over time first 4 Ss
  pp<- fixatnsNotPilot %>% 
    filter(subjNum <= i*SsPerPlot) %>% filter(subjNum > (i-1)*SsPerPlot) %>%
    filter(sttime_rel<2900) %>%
    group_by(trial,ID,session) %>%
    summarise(gavx = mean(gavx, na.rm = TRUE)) %>%
    ggplot( aes(x=trial,y=gavx) ) +
    geom_point() + geom_smooth() +
    coord_cartesian(ylim=c(0, widthPix)) + 
    facet_grid(rows=vars(ID),cols=vars(session)) +
    ggtitle('Drift of gavx')  +
    #facet_wrap(vars(ID)) 
    facet_grid(rows=vars(ID),cols=vars(session)) #
  show(pp)
  plotpath<- file.path('plots')
  ggsave( file.path(plotpath, paste0('drift',(i-1)*SsPerPlot+1,'-',i*SsPerPlot,'.png'))  )
}


fixatns %>% filter(sttime_rel<2900) %>%
  group_by(trial,ID,session) %>%
  summarise(gavx = mean(gavx, na.rm = TRUE)) %>%
  ggplot( aes(x=trial,y=gavx) ) +
  geom_point() + geom_smooth() +
  facet_grid(rows=vars(ID),cols=vars(session)) +
  ggtitle('Drift of gavx') 

fixatns %>% filter(sttime_rel<2900) %>%
  group_by(trial,fname) %>%
  summarise(gavy = mean(gavy, na.rm = TRUE)) %>%
  ggplot( aes(x=trial,y=gavy) ) +
  geom_point() + geom_smooth() +
  facet_grid(rows=vars(fname)) +
  ggtitle('Drift of gavy') 

#Assess how many trials excluded for different criteria
distCriterion <- 40
fixatnsNotPilot<- fixatnsNotPilot %>% mutate( tooFar = (distFromCtr > distCriterion) )
perTrial <- fixatnsNotPilot %>% filter(sttime_rel>800) %>%
  group_by(ID,session,trial) %>%
  summarise( tooFar = max(tooFar,na.rm=TRUE) )
perTrial$subjNum <- match(perTrial$ID, unique(perTrial$ID))  

exclusion<- perTrial %>% group_by(ID) %>% 
  summarise(excluded = sum(tooFar), numTrials=n()) %>%
  mutate(pExcluded = excluded/numTrials)

exclusion %>% ggplot(aes(x=pExcluded)) + geom_histogram()

pp<- perTrial %>% filter(subjNum<5) %>%
  ggplot( aes(x=trial, y=tooFar, color=session) ) +
  geom_point() + #geom_line(aes(group=trial)) +
  ylab('too far') + xlab('trial') +
  ggtitle('Fixation distance from center, columns are sessions') +
  facet_wrap(vars(ID)) 
show(pp)

##Examine BLINKS

#Plot blink durations per participant. Anytime the eyetracker loses the eyes the parser labels that as a blink!
#Check for participants with a lot of long blinks for possible investigation
blinks %>% filter(pilot==FALSE) %>% ggplot(aes(x=duration)) + geom_histogram(binwidth=30) +
  coord_cartesian(xlim=c(0, 500)) + xlab('blink duration (ms)') +
  facet_wrap(vars(ID))

#Sample fixation over time plots
#Graph several Ss at a time. 
SsPerPlot = 2
numPlots = round( max(fixatnsNotPilot$subjNum)/SsPerPlot )
for (i in 1:numPlots)  {
  #Plot distance from fixation over time
  pp<- fixatnsNotPilot %>% filter(subjNum<4) %>%
    filter(subjNum <= i*SsPerPlot) %>% filter(subjNum > (i-1)*SsPerPlot) %>%
    filter(sttime_rel>800) %>%
    ggplot( aes(x=sttime_rel, y=gavx, color=trial, shape=session) ) +
    geom_point() + geom_line(aes(group=trial)) +
    ylab('gavx (pixels)') + xlab('sttime_rel (ms)') +
    ggtitle('gavx') +
    #coord_cartesian(ylim=c(0, 200)) +
    #facet_wrap(vars(ID)) 
    facet_grid(rows=vars(ID),cols=vars(session)) #
  show(pp)
  plotpath<- file.path('plots')
  ggsave( file.path(plotpath, paste0('gavxSamples',(i-1)*SsPerPlot+1,'-',i*SsPerPlot,'.png'))  )
}


