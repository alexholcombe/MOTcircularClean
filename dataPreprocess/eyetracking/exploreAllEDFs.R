library(eyelinkReader)
library(tidyverse)
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
EDFfolder<- file.path("..","..","dataRaw","youngOld","EDF")
#EDFfolder<- file.path("EDFmomoTemp")

EDFfiles <- list.files(path=EDFfolder)  # c("A20b.EDF","S451.EDF","E401.EDF","M433.EDF","M471.EDF")
items_to_remove <- c("j5.EDF", #Loretta said it's nothing and it's only one trial
                     "tea.EDF", "tema.EDF", "temp.EDF")
  
EDFfiles <- setdiff(EDFfiles, items_to_remove)

#Around 29 May, we switched from using 1,2,3 for session number to a,b,c
fnames<-EDFfiles
fixatns<- data.frame(); blinks<- data.frame()

if (file.exists("fixatns.tsv")) { #Assume this means already read in all EDF files and saved them
  readInEDFfiles<- FALSE
  library(readr)
  fixatns<- readr::read_tsv("fixatns.tsv")
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

table(fixatns$ID,fixatns$session)

#label pilot participants
pilotParticipants <- c("A12", "M13", "A14", "J16", "D15", "N17", "A18", "J19", 
          "A20", "K21", "M22", "P23", "M24", "R24", "P29", "S31", "S45", "E46")
fixatns$pilot <- fixatns$ID %in% pilotParticipants
blinks$pilot <- blinks$ID %in% pilotParticipants

avgFix<- fixatns |> group_by(ID) |> 
  summarise(meanX = mean(gavx), meanY = mean(gavy), date=first(date))

#Change date variable to ordinal rank to avoid exact timestamps (privacy concern)

#Calculate whether in second 20% of subjects run, to break up graphs by first versus second bit
avgFix<- avgFix |> mutate(dateHalf =    date > quantile(avgFix$date,probs=c(.8)) )

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

#plot momo's temporary
avgFix |> filter(ID=="K04") |>
  ggplot( aes(x= meanX, y= meanY, label=ID))+  
  geom_point() +geom_text(hjust=0, vjust=0)+
 geom_point(data=commonScreenResolutions, 
                                 aes(x=widthPix/2,y=heightPix/2,label=NULL,color=resolution)) + 
  ggtitle('Average fixation location of each participant',subtitle=', with colored points showing centers of different screen resolutions') +
  theme_bw() + theme( panel.grid.minor=element_blank(),panel.grid.major=element_blank() )


avgEachSubjectG<- ggplot(avgFix, aes(x= meanX, y= meanY, label=ID))+  geom_point() +
                          geom_text(hjust=0, vjust=0)
av<-avgEachSubjectG + geom_point(data=commonScreenResolutions, 
                             aes(x=widthPix/2,y=heightPix/2,color=resolution,label=NULL)) +
                ggtitle('Average fixation location of each participant, with centers of different screen resolutions in color')
show(av)

deviationFromScreenCenter <- avgFix  -    data.frame( meanX=widthPix/2, meanY= heightPix/2)
if ( any(abs(deviationFromScreenCenter) >40) ) { #check if deviation from screen center of average fixation location is greater than 40 pixels
  msg = paste0("Average fixation location should be near screen center (",widthPix/2,",",heightPix/2,") but")
  msg=paste0(msg," instead it's (",round(avgFix$meanX,1),",",round(avgFix$meanY,1),") so")
  msg=paste(msg,"either your screen widthPix, heightPix are wrong, the eyetracker sucked, or participant didn't look near center much")
  print(msg)
}

avgFix<- $fixations %>% summarise(meanX = mean(gavx), meanY = mean(gavy))  - 
  data.frame( meanX=widthPix/2,     meanY= heightPix/2)
if ( any( abs(avgFix) > 40 ) ) { #check if deviation from screen center of average fixation location is greater than 40 pixels
  msg = paste0("Average fixation location should be near screen center (",widthPix/2,",",heightPix/2,") but")
  msg=paste0(msg," instead it's (",round(avgFix$meanX,1),",",round(avgFix$meanY,1),") so")
  msg=paste(msg,"either your screen widthPix, heightPix are wrong, the eyetracker sucked, or participant didn't look near center much")
  print(msg)
}

#Plot blink durations per participant. Anytime the eyetracker loses the eyes the parser labels that as a blink!
blinks %>% filter(pilot==FALSE) %>% ggplot(aes(x=duration)) + geom_histogram(binwidth=30) +
  coord_cartesian(xlim=c(0, 500)) + xlab('blink duration (ms)') +
  facet_wrap(vars(ID))

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

#Constrain axis and replace outliers with extreme value on axis
SsPerPlot = 2
minValToShow = 250; maxValToShow = 550
numPlots = round( max(fixatnsNotPilot$subjNum)/SsPerPlot )
#numPlots =1
for (i in 1:numPlots)  {
  #Plot distance from fixation over time
  pp<- fixatnsNotPilot %>% 
    mutate(outlier = ifelse(gavx<minValToShow | gavx>maxValToShow, TRUE, FALSE)) %>%  #determine outliers
    mutate(gavx =  ifelse(gavx > maxValToShow, maxValToShow, gavx)) %>%     #replace outliers
    mutate(gavx =  ifelse(gavx < minValToShow, minValToShow, gavx)) %>%
    filter(subjNum <= i*SsPerPlot) %>% filter(subjNum > (i-1)*SsPerPlot) %>%
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


#Wnegative gavx occurs when person seems to look offscreen but eyes aren't lost
#  E40 session 3 for example
E403<- fixatnsNotPilot %>% filter(fname=="E403.EDF")
E403 %>% filter(gavx < 0) %>% 
  ggplot( aes(x=gavx, y=gstx) ) +
  geom_point() + geom_line(aes(group=trial))

#DISTANCE FROM FIXATION
#Graph 5 Ss at a time. 
SsPerPlot = 5
numPlots = round( max(fixatnsNotPilot$subjNum)/SsPerPlot )
for (i in 1:numPlots)  {
  #Plot distance from fixation over time
  pp<- fixatnsNotPilot %>% 
    filter(subjNum <= i*SsPerPlot) %>% filter(subjNum > (i-1)*SsPerPlot) %>%
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
  ggsave( file.path(plotpath, paste0('distOverTime',(i-1)*SsPerPlot+1,'-',i*SsPerPlot,'.png'))  )
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
