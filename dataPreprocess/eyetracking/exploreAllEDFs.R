library(eyelinkReader)
library(dplyr)
library(ggplot2)

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
EDFfolder<- file.path("..","..","dataRaw","EDF")
EDFfiles <- list.files(path=EDFfolder)  # c("A20b.EDF","S451.EDF","E401.EDF","M433.EDF","M471.EDF")
#Around 29 May, we switched from using 1,2,3 for session number to a,b,c
fnames<-EDFfiles
fixatns<- data.frame(); blinks<- data.frame()

#Try to read in all the files
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
    finally = message("Processed", EDFname)
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

table(fixatns$ID,fixatns$session)
if (length(failedFiles)) {
  message("Failed to read files:",failedFiles)
}
library(readr)
readr::write_tsv(fixatns,"fixatns.tsv")

#label pilot participants
pilotParticipants <- c("A12", "M13", "A14", "J16", "D15", "N17", "A18", "J19", 
          "A20", "K21", "M22", "P23", "M24", "R24", "P29", "S31", "S45", "E46")
fixatns$pilot <- fixatns$ID %in% pilotParticipants
blinks$pilot <- blinks$ID %in% pilotParticipants

#BLINKS
blinks %>% filter(pilot==FALSE) %>% ggplot(aes(x=)) + geom_histogram()

#Caculate distance from fixation
fixatns$distFromFixatn = sqrt( (fixatns$gavx - widthPix/2)^2 + (fixatns$gavy - heightPix/2)^2 )

#What is with A18? Pilot participant
library(stringr)
#Plot distance from fixation over time
pp<- fixatns %>% filter(sttime_rel>800) %>% filter(str_detect(ID,"A18")) %>%
  ggplot( aes(x=sttime_rel, y=distFromFixatn, color=trial, shape=session) ) +
  geom_point() + geom_line(aes(group=trial)) +
  ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
  ggtitle('Fixation distance from center over each trial') +
  facet_grid(rows=vars(ID),cols=vars(session)) #
show(pp)
plotpath<- file.path('plots')
ggsave( file.path(plotpath, paste0('A18','.png'))  )

#Discard A18 completely for not fixating
fixatns <- fixatns %>% filter( !str_detect(ID,"A18") )

#Create subject num integer
fixatns$subjNum <- match(fixatns$ID, unique(fixatns$ID))  

fixatnsNotPilot<- fixatns %>% filter(pilot==FALSE)
#re-number so can use to divide up into separate figures
fixatnsNotPilot$subjNum <- match(fixatnsNotPilot$ID, unique(fixatnsNotPilot$ID))  

#Graph 5 Ss at a time. Distance from fixation
SsPerPlot = 5
numPlots = round( max(fixatnsNotPilot$subjNum)/SsPerPlot )
for (i in 1:numPlots)  {
  #Plot distance from fixation over time
  pp<- fixatnsNotPilot %>% 
    filter(subjNum <= i*5) %>% filter(subjNum > (i-1)*5) %>%
    filter(sttime_rel>800) %>%
    ggplot( aes(x=sttime_rel, y=distFromFixatn, color=trial, shape=session) ) +
    geom_point() + geom_line(aes(group=trial)) +
    ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
    ggtitle('Fixation distance from center, columns are sessions') +
    coord_cartesian(ylim=c(0, 200)) +
    #facet_wrap(vars(ID)) 
    facet_grid(rows=vars(ID),cols=vars(session)) #
  show(pp)
  plotpath<- file.path('plots')
  ggsave( file.path(plotpath, paste0('distOverTime',(i-1)*5+1,'-',i*5,'.png'))  )
}

#Just look at first 2900ms, because first 800->1300ms is fixation, then blobs cued for 1200ms
#So really I don't have to exclude until after 800+1200=2000
pp<- fixatns %>% filter(pilot==FALSE) %>%
  filter(sttime_rel<2900) %>%
  ggplot( aes(x=sttime_rel, y=distFromFixatn, color=trial) ) +
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
fixatnsNotPilot<- fixatnsNotPilot %>% mutate( tooFar = (distFromFixatn > distCriterion) )
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
