library(eyelinkReader)
library(dplyr)
library(ggplot2)

#Explore how things went with eyetracking youngOld
widthPix = 800; heightPix = 600

#expecting current working directory to be top level of this git-indexed project, and this file to be in top level - dataPreprocess/
EDFfolder<- file.path("..","..","dataRaw","EDF")
EDFfiles <- list.files(path=EDFfolder)  # c("A20b.EDF","S451.EDF","E401.EDF","M433.EDF","M471.EDF")
#Around 29 May, we switched from using 1,2,3 for session number to a,b,c
fnames<-EDFfiles
fixatns<- data.frame()

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
    if (substr(fnames[1],5,8)!=".EDF") {
      warning("File name should be eight characters and end with .EDF")
    }
    #parse out session
    IDandSession<- substr(fnames[f],1,4)
    ID<- substr(IDandSession,1,3)
    EDF$fixations$ID <- ID
    session <- substr(IDandSession,4,4)
    if (grepl("^[a-z]$", session)) #session is lower-case letter
      session<- match( tolower(session), letters) #returns 1 for 'a', 2 for 'b', etc.
    
    EDF$fixations$session<- session
    fixatns<- rbind(fixatns, EDF$fixations)
  }
}

table(fixatns$ID,fixatns$session)
if (length(failedFiles)) {
  print("Failed to read files:",failedFiles)
}

fixatns$distFromFixatn = sqrt( (fixatns$gavx - widthPix/2)^2 + (fixatns$gavy - heightPix/2)^2 )

#What is with A18?
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

#number subjects with integer
fixatns$subjNum <- match(fixatns$ID, unique(fixatns$ID))  

#Graph 5 Ss at a time
numPlots = round( max(fixatns$subjNum)/5 )
for (i in 1:numPlots)  {
  #Plot distance from fixation over time first 4 Ss
  pp<- fixatns %>% 
    filter(subjNum <= i*5) %>% filter(subjNum > (i-1)*5) %>%
    filter(sttime_rel>800) %>%
    ggplot( aes(x=sttime_rel, y=distFromFixatn, color=trial, shape=session) ) +
    geom_point() + geom_line(aes(group=trial)) +
    ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
    ggtitle('Fixation distance from center, columns are sessions') +
    #facet_wrap(vars(ID)) 
    facet_grid(rows=vars(ID),cols=vars(session)) #
  show(pp)
  plotpath<- file.path('plots')
  ggsave( file.path(plotpath, paste0('distOverTime',(i-1)*5+1,'-',i*5,'.png'))  )
}

#Plot distance from fixation over time first 4 Ss
pp<- fixatns %>% filter(sttime_rel>800) %>% filter(ID > "C36") %>%
  ggplot( aes(x=sttime_rel, y=distFromFixatn, color=trial, shape=session) ) +
  geom_point() + geom_line(aes(group=trial)) +
  ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
  ggtitle('Fixation distance from center, columns are sessions') +
  #facet_wrap(vars(ID)) 
  facet_grid(rows=vars(ID),cols=vars(session)) #
show(pp)
plotpath<- file.path('plots')
ggsave( file.path(plotpath, paste0('distOverTime2','.png'))  )

#Just look at first 2900ms, because first 800->1300ms is fixation, then blobs cued for 1200ms
#So really I don't have to exclude until after 800+1200=2000
pp<- fixatns %>% filter(sttime_rel<2900) %>%
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

#Show drift over trials by plotting average position of first part of trial
fixatns %>% filter(sttime_rel<2900) %>%
  group_by(trial,fname) %>%
  summarise(gavx = mean(gavx, na.rm = TRUE)) %>%
  ggplot( aes(x=trial,y=gavx) ) +
  geom_point() + geom_smooth() +
  facet_grid(rows=vars(fname)) +
  ggtitle('Drift of gavx') 
  
fixatns %>% filter(sttime_rel<2900) %>%
  group_by(trial,fname) %>%
  summarise(gavy = mean(gavy, na.rm = TRUE)) %>%
  ggplot( aes(x=trial,y=gavy) ) +
  geom_point() + geom_smooth() +
  facet_grid(rows=vars(fname)) +
  ggtitle('Drift of gavy') 
  
#Plot distance from fixation over trials
fixatns %>% filter(sttime_rel > 1000) %>%
  group_by(trial,fname) %>%
  summarise(dist = mean(distFromFixatn, na.rm = TRUE)) %>%
  ggplot( aes(x=trial,y=dist) ) +
  geom_point() + geom_smooth() +
  facet_grid(rows=vars(fname)) +
  ggtitle('Change in dist from fixation') 

