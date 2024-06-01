library(eyelinkReader)

#Explore how things went with eyetracking youngOld
EDFexample <- file.path("dataForTestingOfCode", "A20b.EDF") # "A421.EDF" #"/Users/alex/Documents/attention_tempresltn/multiple_object_tracking/newTraj/MOTcircular_repo/dataRaw/circleOrSquare_twoTargets/AM/AM_11Jun2015_11-51.EDF"

EDFstuff <- eyelinkReader::read_edf(EDFexample,
                                    import_samples = TRUE,
                                    sample_attributes = c('time', 'gx', 'gy'))

samples<- EDFstuff$samples
avgSampleRelCtr<- samples %>% summarise(meanX = mean(gxR,na.rm=TRUE), meanY = mean(gyR,na.rm=TRUE))  - 
                             data.frame( meanX=widthPix/2,             meanY= heightPix/2)
if any( abs(avgSampleRelCtr) > 40 ) { #check if deviation from screen center of average fixation location is greater than 40
  print("Either your screen coordinates are wrong, the eyetracker sucked, or participant didn't look near center much")
}

#Average FIXATION event location
fixatns <- EDFstuff$fixations
#take the global average which if everything worked right will be near widthPix/2, heightPix/2
avgFix<- fixatns %>% summarise(meanX = mean(gavx), meanY = mean(gavy))  - 
  data.frame( meanX=widthPix/2,     meanY= heightPix/2)
if any( abs(avgFix) > 40 ) { #check if deviation from screen center of average fixation location is greater than 40 pixels
  print("Either your screen coordinates are wrong, the eyetracker sucked, or participant didn't look near center much")
}

#Plot to see how it changes over time across trials
fixatns %>%
  group_by(trial) %>%
  summarise(gavx = mean(gavx, na.rm = TRUE)) %>%
  ggplot(aes(x = trial, y = gavx)) +
  geom_point() #You can see the drift

fixatns %>%
  group_by(trial) %>%
  summarise(gavy = mean(gavy, na.rm = TRUE)) %>%
  ggplot(aes(x = trial, y = gavy)) +
  geom_point()

#Show drift of average position across trials with 2D plot
fixatns %>%
  group_by(trial) %>%
  summarise(gavx = mean(gavx, na.rm = TRUE), gavy = mean(gavy, na.rm = TRUE)) %>%
  ggplot(aes(x = gavx, y = gavy)) +
  geom_point(aes(color=trial))

#ggplot(fixatns, aes(x=gavx, y=gavy, color=trial) ) +
#  stat_summary(fun="mean",geom="point")

#Implement drift correction,
# by taking average of every trial? Need to decide on limited interval at beginning of trial.


#Look at trace of several trials
fixatns %>% filter(trial<50) %>% 
  ggplot( aes(x=sttime_rel, y=gavx, color=trial) ) +
  geom_point() + geom_line(aes(group=trial))

fixatns %>% filter(trial>50) %>% 
  ggplot( aes(x=sttime_rel, y=gavx, color=trial) ) +
  geom_point() + geom_line(aes(group=trial))

ggplot(fixatns, aes(x=sttime_rel, y=gavx, color=trial) ) +
  geom_point() 

