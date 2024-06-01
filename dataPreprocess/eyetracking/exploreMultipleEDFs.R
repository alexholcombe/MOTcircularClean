library(eyelinkReader)
library(dplyr)
library(ggplot2)

#Explore how things went with eyetracking youngOld
widthPix = 800; heightPix = 600
fnames<- c("A20b.EDF","S451.EDF","E401.EDF")
fixatns<- data.frame()
for (f in 1:length(fnames)) {
  EDFname <- file.path("dataForTestingOfCode", fnames[f]) # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"
  EDF<- eyelinkReader::read_edf(EDFname, import_samples = TRUE,
                                    sample_attributes = c('time', 'gx', 'gy'))
  EDF$fixations$fname <- fnames[f]
  #parse it into session later
  
  fixatns<- rbind(fixatns, EDF$fixations)
}


fixatns$distFromFixatn = sqrt( (fixatns$gavx - widthPix/2)^2 + (fixatns$gavy - heightPix/2)^2 )

#Plot distance from fixation over time
pp<- fixatns %>%
  ggplot( aes(x=sttime_rel, y=distFromFixatn, color=trial) ) +
  geom_point() + geom_line(aes(group=trial)) +
  ylab('dist average during fixation (pixels)') + xlab('sttime_rel (ms)') +
  ggtitle('Fixation distance from center over each trial') +
  facet_grid(rows=vars(fname)) # facet_wrap(vars(fname))
show(pp)
plotpath<- file.path('dataForTestingOfCode', 'examplePlots')
ggsave( file.path(plotpath, paste0('distOverTime','.png'))  )

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

