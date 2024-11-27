#Intended to be called by analyze_youngOld.R, with 
#variables expected to have already been created:
#factorsPlusSubject
#fitParms
#psychometrics
#function calcPctCorrThisIvVal
#iv
#varyLapseRate, lapseMinMax
infoMsg=paste0(iv,"-fit")

lapseMsg=""
if (!varyLapseRate)
  lapseMsg=paste("lapseRate always",unique(lapseMinMax))
#go point by point to find thresholds for each criterion for each factorsPlusSubject
worstLapseRate <- max(fitParms$lapseRate)
if (worstLapseRate > .03) {
  paste("Can't calculate threshold above criterion level of",1-worstLapseRate,"because worst lapseRate is",worstLapseRate)
}
addMidPointThresh<- TRUE #Work out what the midpoint thresh criterion and thresh is for each number of objects
addThreeQuartersThresh<- TRUE #Work out what the three-quarters thresh criterion and thresh is for each number of objects
#c(0.0.562,0.625) are the halfway-threshold criteria for 4 objects and 8 objects
#maxCriterion <- 1-worstLapseRate
maxCriterion <- .95

threshCriteria<- c(.75) # seq(from=.67,to=maxCriterion,by=.06) #high thresholds
threshCriterion = round(threshCriteria,3) #because otherwise can include invisible 10^-21 diff which will trip you up later
threshes <- data.frame()

numObjects <- fitParms$objects
if (is.factor(numObjects)) {
  numObjects<- as.numeric(as.character(numObjects))
}
for (numObjectsThis in unique(numObjects)) {
  #Set up threshCriteria such as midpoint for this number of objects
  threshCriteriaThis = threshCriteria
  threshCriteriaNotes = rep("nothingSpecial",length(threshCriteriaThis))
  if (addMidPointThresh) {
    crit <- 1/numObjectsThis + 0.5*(1-1/numObjectsThis)  #midpoint threshold
    crit<- round(crit,3)
    threshCriteriaThis= c(threshCriteriaThis,crit)
    threshCriteriaNotes = c(threshCriteriaNotes,"midpoint")
  }
  if (addThreeQuartersThresh) {
    crit <- 1/numObjectsThis + 0.75*(1-1/numObjectsThis)
    crit<- round(crit,3)
    threshCriteriaThis= c(threshCriteriaThis,crit)
    threshCriteriaNotes = c(threshCriteriaNotes,"threeQuarters")    
  }
  for (i in 1:length(threshCriteriaThis)) {
    threshCriterion = threshCriteriaThis[i]
    #message('Extracting threshes for criterion:',threshCriterion)
    
    psychometricThis<- subset(psychometrics,objects==numObjectsThis)
    calcThreshForPredictn<- FALSE  #because tracking-two prediction for 6, 9 objects never gets that high. Anyway this is to address
    if (!calcThreshForPredictn)  
      if ("targets" %in% colnames(psychometricThis)) {
        psychometricThis <- subset(psychometricThis,targets!="2P")
      }
    #Don't do it where criterion corresponds to below chance
    #psychometricThis <- subset(psychometricThis, numObjects > 1/threshCriterion) #For these numObjects conditions, chance is above the current criterion
    
    threshesThisNumeric<- psychometricThis |>  group_by( !!!syms(factorsPlusSubject) ) |>
            group_modify(extractThreshFromCurveNumerically, iv,threshCriterion)
    
    threshesThisNumeric$criterion <- threshCriterion
    threshesThisNumeric$criterionNote <- threshCriteriaNotes[i]
    threshesThis<- merge(threshesThisNumeric,fitParms)
    threshes<- rbind(threshes, threshesThis)
  }
}
#non-catastrophic errors occurred with subject= 69  objects= 8  targets= 2, which looks appropriate, subject getting 25% wrong at
#slow speeds with objects = 8
#errorConditions<- threshes %>% filter(is.na(thresh))
#print(errorConditions)
threshes$age<- as.factor(threshes$age)
#ends up with Old before Young which is non-intuitive in the plot legends, so reverse
threshes$age <- factor(threshes$age, levels = levels(threshes$age)[c(2,1)])
#Similarly reverse male and female
threshes$gender<- as.factor(threshes$gender)
threshes$gender <- factor(threshes$gender, levels = levels(threshes$gender)[c(2,1)])

midpointThreshes<- subset(threshes,criterionNote=="midpoint")
##########worst performers ######################################
##Looks like the seven lowest-threshold participants in the 3 targets condition are all old
midpointThreshes<- midpointThreshes |> arrange(thresh)
worstPerformers<- c(69, 73, 54, 58, 85, 51, 60, 63, 53, 72, 85)

themeAxisTitleSpaceNoGridLinesLegendBox = theme_classic() + #Remove gridlines, show only axes, not plot enclosing lines
  theme(axis.line = element_line(linewidth=.3, color = "grey"), 
        axis.title.y=element_text(vjust=0.24), #Move y axis label slightly away from axis
        axis.title.x=element_text(vjust=.10), #Move x axis label slightly away from axis
        legend.key = element_blank(), #don't put boxes around legend bits
        legend.background= element_rect(color="grey90"), #put big light grey box around entire legend
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)   )
#############################################################
############# Individual subjects effect of num targets
##########Plot midpoint threshes, subject*numTargets*numObjects ################
tit=paste("individual_Ss_threshesSpeed_",infoMsg,"_midpointThresh",sep='')
quartz(title=tit,width=6,height=3) #create graph of thresholds
dodgeWidth<-.4
h<-ggplot(data=midpointThreshes,
          aes(x=targets,y=thresh,color=objects,shape=age, 
              group=interaction(subject,objects))) #this is critical for points and lines to dodge in same way
#h<-h+facet_grid(. ~ criterion)  #facet_grid(criterion ~ exp)
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
pd<- position_dodge(width=dodgeWidth)
h<-h+ geom_point(position=pd,size=1,alpha=0.5)
h<-h+ geom_line(position=pd,alpha=0.5) #plot individual lines for each subject
h<-h+ stat_summary(fun=mean,geom="point",size=5,fill=NA,shape=22,stroke=3,
                   aes(x=targets,y=thresh,color=objects,group=objects))
#If there any subjects whose threshold could not be estimated, add them at bottom of plot
couldNotBeEstimated<- midpointThreshes %>% filter(is.na(thresh))
if (nrow(couldNotBeEstimated)) {
  
}

h<-h+ylab(  paste('threshold ',iv,' (',ifelse(iv=="speed","rps","Hz"),')',sep='') )  
if (iv=="speed") { h<-h+ggtitle("Speed limits vary widely. 4,8 will converge when plot tf") 
} else h<-h+ggtitle('4,8 objects overlap validate tf limit.')
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
##################################################################
##### Look at worst performers - all old
View(midpointThreshes |> select(objects,targets,subject,thresh,gender,age,agePerturbed))

#############################################################
############# x=age collapse targets
########## ################
tit=paste("CollapseTargets",infoMsg,"_midpointThresh",sep='')
quartz(title=tit,width=2.8,height=3) #create graph of thresholds
threshTargetsCollapse<- midpointThreshes |> group_by(subject,age,objects) |>
  summarize(thresh = mean(thresh, na.rm = TRUE)) |> ungroup()
dodgeWidth<-.4
h<-ggplot(data=threshTargetsCollapse,
          aes(x=age,y=thresh,color=objects,
              group=interaction(subject,age))) #this is critical for points and lines to dodge in same way
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
pd<- position_dodge(width=dodgeWidth)
h<-h+ geom_point(position=pd,size=0.3,alpha=0.5)
#h + stat_summary(fun=mean,geom="line",position=pd,alpha=0.6)
h<-h+ stat_summary(fun=mean,geom="point",
                   aes(group=interaction(objects,age)),#override the subject-age grouping
                   size=3,position=pd)
h<-h+stat_summary(aes(group=interaction(objects,age)),#override the subject-age grouping
                  fun.data="mean_cl_boot",geom="errorbar",width=.2, position=pd,
                  fun.args=list(conf.int=.95))
#h<-h+ geom_line(position=pd,alpha=0.5) 
#If there any subjects whose threshold could not be estimated, add them at bottom of plot
couldNotBeEstimated<- midpointThreshes %>% filter(is.na(thresh))
if (nrow(couldNotBeEstimated)) {
  
}
h<-h+ylab(  paste('threshold ',iv,' (',ifelse(iv=="speed","rps","Hz"),')',sep='') )  
if (iv=="speed") { h<-h+ggtitle("4,8 will converge when plot tf") 
} else h<-h+ggtitle('4,8 objects overlap validate tf limit.')
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
#############################################################
#############################################################
############# t.f. x=age collapse targets
########## ################
tit=paste("tfCollapseTargets",infoMsg,"_midpointThresh",sep='')
quartz(title=tit,width=2.8,height=3) #create graph of thresholds
threshTargetsCollapse<- midpointThreshes |> group_by(subject,age,objects) |>
  summarize(thresh = mean(thresh, na.rm = TRUE)) |> ungroup()
if (iv=="speed") { #convert to t.f.
  threshesTargetsCollapseTF<- threshTargetsCollapse |> 
                  mutate(thresh = thresh*as.numeric(as.character(objects)))
}
dodgeWidth<-.4
h<-ggplot(data=threshesTargetsCollapseTF,
          aes(x=age,y=thresh,color=objects,
              group=interaction(subject,age))) #this is critical for points and lines to dodge in same way
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
pd<- position_dodge(width=dodgeWidth)
h<-h+ geom_point(position=pd,size=0.3,alpha=0.5)
#h + stat_summary(fun=mean,geom="line",position=pd,alpha=0.6)
h<-h+ stat_summary(fun=mean,geom="point",
                   aes(group=interaction(objects,age)),#override the subject-age grouping
                   size=3,position=pd)
h<-h+stat_summary(aes(group=interaction(objects,age)),#override the subject-age grouping
                  fun.data="mean_cl_boot",geom="errorbar",width=.2, position=pd,
                  fun.args=list(conf.int=.95))
#h<-h+ geom_line(position=pd,alpha=0.5) 
#If there any subjects whose threshold could not be estimated, add them at bottom of plot
couldNotBeEstimated<- midpointThreshes %>% filter(is.na(thresh))
if (nrow(couldNotBeEstimated)) {
  
}
h<-h+ylab('threshold (Hz)')  
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
#############################################################
############# x=age collapse all. Not appropriate for speed here because 
########## ################ 4 and 8 objects so different
tit=paste("collapseExceptAgeTF_",infoMsg,"_midpointThresh",sep='')
quartz(title=tit,width=1.8,height=3) #create graph of thresholds
midpointThreshes<- subset(threshes,criterionNote=="midpoint")
threshCollapseTFageOnly<- midpointThreshes
if (iv=="speed") { #convert to t.f.
  threshCollapseTFageOnly<- threshCollapseTFageOnly |> 
    mutate(thresh = thresh*as.numeric(as.character(objects)))
}
threshCollapseTFageOnly<- midpointThreshes |> group_by(subject,age) |>
  summarize(thresh = mean(thresh, na.rm = TRUE)) |> ungroup()

dodgeWidth<-.2
pd<- position_jitter(width=dodgeWidth)
h<-ggplot(data=threshCollapseTFageOnly,
          aes(x=age,y=thresh)) #this is critical for points and lines to dodge in same way
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
h<-h+ geom_point(position=pd,size=0.5,alpha=0.5)
h<-h+ stat_summary(fun=mean,geom="point", size=3)
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,
                  fun.args=list(conf.int=.95))
#h<-h+ geom_line(position=pd,alpha=0.5) 
#If there any subjects whose threshold could not be estimated, add them at bottom of plot
couldNotBeEstimated<- midpointThreshes %>% filter(is.na(thresh))
if (nrow(couldNotBeEstimated)) {
  
}
h<-h+ylab('threshold temporal frequency (Hz)')  
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
#############################################################
#############################################################
############# t.f. x=age collapse all
########## ################
tit=paste("tfCollapseExceptAge",infoMsg,"_midpointThresh",sep='')
quartz(title=tit,width=1.8,height=3) #create graph of thresholds
threshCollapseTFageOnly<- midpointThreshes
if (iv=="speed") { #convert to t.f.
  threshCollapseTFageOnly<- threshCollapseTFageOnly |> 
    mutate(thresh = thresh * as.numeric(as.character(objects)))
}
threshCollapseTFageOnly<- midpointThreshes |> group_by(subject,age) |>
  summarize(thresh = mean(thresh, na.rm = TRUE)) |> ungroup()

dodgeWidth<-.2
pd<- position_jitter(width=dodgeWidth)
h<-ggplot(data=threshCollapseTFageOnly,
          aes(x=age,y=thresh)) #this is critical for points and lines to dodge in same way
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
h<-h+ geom_point(position=pd,size=0.5,alpha=0.5)
h<-h+ stat_summary(fun=mean,geom="point", size=3)
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2,
                  fun.args=list(conf.int=.95))
#h<-h+ geom_line(position=pd,alpha=0.5) 
#If there any subjects whose threshold could not be estimated, add them at bottom of plot
couldNotBeEstimated<- midpointThreshes %>% filter(is.na(thresh))
if (nrow(couldNotBeEstimated)) {
  
}
h<-h+ylab('threshold (Hz)')  
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
#############################################################
############# TEMP x=objects
##########TEMP ################
quartz(title=tit,width=6,height=3) #create graph of thresholds
threshTargetsCollapse<- midpointThreshes |> group_by(subject,age,objects) |>
                      summarize(thresh = mean(thresh, na.rm = TRUE)) |> ungroup()
dodgeWidth<-.4
h<-ggplot(data=threshTargetsCollapse,
          aes(x=objects,y=thresh,color=age, 
              group=interaction(subject,age))) #this is critical for points and lines to dodge in same way
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
pd<- position_dodge(width=dodgeWidth)
h<-h+ geom_point(position=pd,size=1,alpha=0.5)
#h + stat_summary(fun=mean,geom="line",position=pd,alpha=0.6)
h<-h+ stat_summary(fun=mean,geom="point",
                   aes(group=interaction(objects,age)),#override the subject-age grouping
                   size=5,position=pd)
h<-h+ geom_line(position=pd,alpha=0.5) 
#If there any subjects whose threshold could not be estimated, add them at bottom of plot
couldNotBeEstimated<- midpointThreshes %>% filter(is.na(thresh))
if (nrow(couldNotBeEstimated)) {
  
}
################################################################################
############# Individual subjects effect of num target. Showcase Age with color
##########Plot midpoint threshes, subject*numTargets*numObjects ################
tit=paste("individual_Ss_Age_threshesSpeed_",infoMsg,"_midpointThresh",sep='')
quartz(title=tit,width=6,height=6) #create graph of thresholds
h<-ggplot(data=midpointThreshes,
          aes(x=targets,y=thresh,color=age,shape=objects, 
              group=interaction(subject,objects))) #this is critical for points and lines to dodge in same way
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox #theme_bw() 
pd<- position_dodge(width=dodgeWidth)
h<-h+ geom_point(position=pd,size=1,alpha=0.5)
h<-h+ geom_line(position=pd,alpha=0.5) #plot individual lines for each subject
#h<-h+ stat_summary(fun=mean,geom="point",size=5,fill=NA,shape=22,stroke=3,
#                   aes(x=targets,y=thresh,color=age,group=objects))
#If there any subjects whose threshold could not be estimated, add them at bottom of plot
couldNotBeEstimated<- midpointThreshes %>% filter(is.na(thresh))
if (nrow(couldNotBeEstimated)) {
  
}
h<-h+ylab(  paste('threshold ',iv,' (',ifelse(iv=="speed","rps","Hz"),')',sep='') )  
if (iv=="speed") { h<-h+ggtitle("4,8 will converge when plot tf") 
} else h<-h+ggtitle('4,8 objects overlap validate tf limit.')
show(h)
ggsave( paste('figs/',tit,'.png',sep='') )
#############################################################
#############################################################
#############
######Plot mean threshes against numTargets
thisCriterion<-"midpoint"
tit<-paste0("MeanThreshAgainstTargets)",infoMsg,"_",thisCriterion)
quartz(title=tit,width=4,height=3) 
threshesThis<- subset(threshes,criterionNote==thisCriterion)
couldNotBeEstimated<- threshesThis %>% filter(is.na(thresh))
threshesThisGood<- threshesThis %>% filter(!is.na(thresh))
h<-ggplot(data=threshesThisGood,   
          aes(x=targets,y=thresh,color=factor(objects)))
h<-h+ labs(color = "objects") # Change legend title from factor(objects) to objects
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
pd=position_dodge(width=dodgeWidth) 
h<-h+ stat_summary(fun=mean,geom="point",position=pd)
#h<-h+ stat_summary(fun=mean,geom="line")
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2, position=pd,
                  fun.args=list(conf.int=.95))
#Show individual points, for non-degenerate data
h<-h+ geom_point(data=threshesThisGood, 
                 position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5),
                 alpha=.7, size= 0.1)
#Represent degenerate subjects with faded question marks low on axis
if (nrow(couldNotBeEstimated)) {
  minYaxis<- layer_scales(h)$y$get_limits()[1]
  couldNotBeEstimated$thresh <- runif(nrow(couldNotBeEstimated), 
                                      min = minYaxis, max = minYaxis + 0.01)
  h<-h+ geom_point(data=couldNotBeEstimated, position=position_dodge(width=dodgeWidth),
                   size=3,alpha=0.5,shape = "\u003F") #, color="grey")
  
}
h<-h+ylab(  paste('threshold ',iv,' (',ifelse(iv=="speed","rps","Hz"),')',sep='')  ) 
if (iv=="speed") {  h<-h+ggtitle("4,8 difft validates t.f. limit. Speed limits vary widely")
} else h<-h+ggtitle('4,8 validate tf limit.')
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
h<-h+ggtitle(paste("4,8 difft validates t.f. limit. Speed limits vary widely",lapseMsg))
show(h)
ggsave( paste0('figs/',tit,'.png') )

message('I give you threshes')
####################
####### AGE ####################################################################
#################Plot threshes against distractors
######Plot threshes against numTargets
quartz(title=tit,width=4,height=6) 
thisCriterion<-"midpoint"
tit<-paste0("meanThreshAgainstTargets_age_",infoMsg,"_",thisCriterion)
threshesThis<- subset(threshes,criterionNote==thisCriterion)
couldNotBeEstimated<- threshesThis %>% filter(is.na(thresh))
threshesThisGood<- threshesThis %>% filter(!is.na(thresh))
h<-ggplot(data=threshesThisGood,   
          aes(x=targets,y=thresh,color=age,
              shape=factor(objects))) #group=interaction(subject,objects)))
h<-h+ labs(shape = "objects") # Change legend title from factor(objects) to objects
#h<-h+facet_grid(. ~ criterion)  #facet_grid(criterion ~ exp)
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
pd=position_dodge(width=dodgeWidth) 
h<-h+ stat_summary(fun=mean,geom="point", size=3, position=pd)
#h<-h+ stat_summary(fun=mean,geom="line")
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2, position=pd,
                  fun.args=list(conf.int=.95))
#Show individual points, for non-degenerate data
h<-h+ geom_point(data=threshesThisGood, 
                 position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5),
                 alpha=.7, size=0.6)
#Represent degenerate subjects with grey question marks low on axis
if (nrow(couldNotBeEstimated)) {
  minYaxis<- layer_scales(h)$y$get_limits()[1]
  couldNotBeEstimated$thresh <- runif(nrow(couldNotBeEstimated), 
                                      min = minYaxis, max = minYaxis + 0.01)
  h<-h+ geom_point(data=couldNotBeEstimated, position=position_dodge(width=dodgeWidth),
                   size=3,alpha=0.5,shape = "\u003F") #, color="grey")
  
}
#h<-h+ geom_line(position=pd,alpha=0.5) #plot individual lines for each subject

h<-h+ylab(  paste('threshold ',iv,' (',ifelse(iv=="speed","rps","Hz"),')',sep='')  ) 
if (iv=="speed") {  h<-h+ggtitle("4,8 difft validates t.f. limit. Speed limits vary widely")
} else h<-h+ggtitle('4,8 replicate partial tf limit for young, but worse with 8 objects for old.')
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
show(h)
ggsave( paste0('figs/',tit,'.png') )
####################
####### AGE ####################################################################
#################simplified plot for grant, collapse numTargets to showcase different effect of objects
######
quartz(title=tit,width=4,height=6) 
thisCriterion<-"midpoint"
tit<-paste0("meanThreshAgainstTargets_age_",infoMsg,"_",thisCriterion)
threshesThis<- subset(threshes,criterionNote==thisCriterion)
if (iv=="speed") { #convert to t.f.
  threshesThis$thresh<- threshesThis$thresh * as.numeric(as.character(threshesThis$objects))
}
couldNotBeEstimated<- threshesThis %>% filter(is.na(thresh))
threshesThisGood<- threshesThis %>% filter(!is.na(thresh))
h<-ggplot(data=threshesThisGood,   
          aes(x=objects,y=thresh,color=age,group=interaction(subject,objects)) )
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
pd=position_dodge(width=dodgeWidth) 
h<-h+ stat_summary(fun=mean,geom="point", size=3, position=pd)
h+geom_line()
h<-h+ stat_summary(fun=mean,geom="line", size=3, position=pd) #geom_line() 

#h<-h+ stat_summary(aes(group=interaction(age,numObjects)),fun=mean,geom="line", size=3, position=pd)
#h<-h+ geom_line(aes(group=interaction(age,numObjects)))

h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2, position=pd,
                  fun.args=list(conf.int=.95))
#Show individual points, for non-degenerate data
h<-h+ geom_point(data=threshesThisGood, 
                 position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5),
                 alpha=.7, size=0.6)
#Represent degenerate subjects with grey question marks low on axis
if (nrow(couldNotBeEstimated)) {
  minYaxis<- layer_scales(h)$y$get_limits()[1]
  couldNotBeEstimated$thresh <- runif(nrow(couldNotBeEstimated), 
                                      min = minYaxis, max = minYaxis + 0.01)
  h<-h+ geom_point(data=couldNotBeEstimated, position=position_dodge(width=dodgeWidth),
                   size=3,alpha=0.5,shape = "\u003F") #, color="grey")
  
}
#h<-h+ geom_line(position=pd,alpha=0.5) #plot individual lines for each subject

h<-h+ylab( 'temporal frequency threshold (Hz)' ) 
if (iv=="speed") {  h<-h+ggtitle("4,8 difft validates t.f. limit. Speed limits vary widely")
} else h<-h+ggtitle('4,8 replicate partial tf limit for young, but worse with 8 objects for old.')
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
show(h)
ggsave( paste0('figs/',tit,'.png') )
###################################
# ######GENDER###################################################################
# ##########
quartz(title=tit,width=5,height=7) 
thisCriterion<-"midpoint" #threeQuarters
tit<-paste0("meanThreshAgainstTargets_gender_",infoMsg,"_",thisCriterion)
threshesThis<- subset(threshes,criterionNote==thisCriterion)
couldNotBeEstimated<- threshesThis %>% filter(is.na(thresh))
threshesThisGood<- threshesThis %>% filter(!is.na(thresh))
couldNotBeEstimated<- threeQuartersThreshes %>% filter(is.na(thresh))
h<-ggplot(data=threshesThisGood,   
          aes(x=targets,y=thresh,color=age,
              shape=gender))
h<-h+scale_shape_manual(values = c("male" = "\u2642", "female" = "\u2640"))
#h<-h+ labs(color = "gender") # Change legend title from factor(objects) to objects
#h<-h+facet_grid(. ~ criterion)  #facet_grid(criterion ~ exp)
h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
#ylim(1.4,2.5) DO NOT use this command, it will drop some data
#h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
pd=position_dodge(width=dodgeWidth) 
h<-h+ stat_summary(fun=mean,geom="point", size=6, position=pd)
#h<-h+ stat_summary(fun=mean,geom="line")
h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.2, position=pd,
                  fun.args=list(conf.int=.95))
#Show individual points, for non-degenerate data
h<-h+ geom_point(data=threshesThisGood, #non-degenerate data
                 position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5),
                 alpha=.7, size= 2)
#Represent degenerate subjects with grey question marks low on axis
if (nrow(couldNotBeEstimated)) {
  minYaxis<- layer_scales(h)$y$get_limits()[1]
  couldNotBeEstimated$thresh <- runif(nrow(couldNotBeEstimated), 
                                      min = minYaxis, max = minYaxis + 0.01)
  h<-h+ geom_point(data=couldNotBeEstimated, position=position_dodge(width=dodgeWidth),
                   size=3,alpha=0.5,shape = "\u003F") #, color="grey")
  
}
h<-h+ylab(  paste('threshold ',iv,' (',ifelse(iv=="speed","rps","Hz"),')',sep='')  ) 
if (iv=="speed") {  h<-h+ggtitle("Gender pattern inconsistent")
} else h<-h+ggtitle('Gender pattern inconsistent')
#h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
show(h)
ggsave( paste0('figs/',tit,'.png') )

# ##########################################################################################
# ##########Plot mean speed threshes against distractors
# tit<-paste0('SpeedMeanThreshAgainstDistractors ',infoMsg,' threeQuarterThresh') 
# quartz(title=tit,width=4,height=3) #create graph of threshes
# threshes$numObjects <- as.numeric(threshes$numObjects) #Otherwise can't connect with lines
# threshes$targets <- threshes$targets #Otherwise can't connect with lines
# h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   #midpoint
#           aes(x=numObjects-1,y=thresh)) #,color=factor(targets) #I have no idea why but this doesn't work, hence put it in facet_grid
# h<-h+facet_grid(exp ~ targets)  #facet_grid(criterion ~ exp)
# h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# #h<-h+ geom_point() + geom_line(aes(group=interaction(subject,numObjects))) #plot individual lines for each subject
# dodgeWidth<-.3
# h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth))
# h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
# #h<-h+stat_summary(fun.data = mean_cl_normal, geom="errorbar", mult=1, width=.5, position=position_dodge(width=dodgeWidth))
# h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeWidth)) 
# h<-h+ylab(  paste('threshold ',dv,' (',ifelse(dv=="speed","rps","Hz"),')',sep='')  )
# h<-h+xlab('Distractors')
# xTicks= unique(threshes$numObjects-1) #put axis ticks at actual values used
# h<-h+scale_x_continuous(breaks=c( xTicks ))
# h<-h+ggtitle(paste("5,8 difft validates t.f. limit. Speed limits vary widely",lapseMsg))
# #h<-h+coord_cartesian(ylim=c(1.5,2.5)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
# show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
# ggsave( paste0('figs/E1_EpostVSStargets_',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
# ##########################################################################################
# ############Temporal frequency against targets, individual Ss
# threshes$tfThresh <- threshes$thresh*threshes$numObjects
# #p2 <- aes(x=numObjects-1,y=temporalFreq,color=targets); h %+% p2 #quick t.f. plot
# ### Pattern remarkably consistent across Ss, perhaps show in paper?
# tit=paste0("individualSsTemporalFreq ",infoMsg," threeQuarterThresh")
# quartz(title=tit,width=6,height=3)
# #Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
# h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   #midpoint
#           aes(x=numTargets,y=tfThresh,color=factor(numObjects)))
# h<-h+facet_grid(. ~ exp)  #facet_grid(criterion ~ exp)
# h<-h+ylab('threshold tf (Hz)')
# h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# dodgeAmt=.3
# h<-h+ geom_point(position=position_dodge(width=dodgeWidth)) 
# #I don't know why have to specify the group to get the lines to work
# h<-h+ geom_line(aes(group=interaction(subject,numObjects)),position=position_dodge(width=dodgeWidth)) 
# #h<-h+stat_summary(fun.data = mean_cl_normal, geom="errorbar", mult=1, width=.5, position=position_dodge(width=dodgeWidth))
# h<-h+ggtitle(paste(tit,lapseMsg))
# show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
# ggsave( paste0('figs/E1_EpostVSStargets_',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
# h %+% subset(threshes,criterionNote=="threeQuarters")
# ##########################################################################################
# ########tf mean threshes against targets
# tit=paste0("tfMeanThreshAgainstTargets ",infoMsg," threeQuarterThresh")
# quartz(title=tit,width=6,height=3)
# #Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
# h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   #midpoint
#           aes(x=targets,y=tfThresh,color=factor(numObjects)))
# h<-h+facet_grid(. ~ exp)  #facet_grid(criterion ~ exp)
# h<-h+ylab('threshold tf (Hz)')
# h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# dodgeAmt=.3
# h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeWidth))
# h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeWidth))
# h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeWidth)) 
# h<-h+ggtitle(paste(tit,lapseMsg))
# show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
# ggsave( paste0('figs/E1_EpostVSStargets_',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
# ##########################################################################################
# ########tf mean threshes against distractors
# tit=paste0("tfMeanThreshAgainstDistractors ",infoMsg," threeQuarterThresh")
# quartz(title=tit,width=4,height=3) 
# #Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
# h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   #midpoint
#           aes(x=numObjects-1,y=tfThresh))#,color=targets)) #color=targets gives error I don't know why
# h<-h+facet_grid(exp ~ targets)  #facet_grid(criterion ~ exp)
# h<-h+ylab('threshold tf (Hz)')
# h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
# h<-h+xlab('Distractors')
# xTicks= unique(threshes$numObjects-1) #put axis ticks at actual values used
# h<-h+scale_x_continuous(breaks=c( xTicks ))
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# dodgeAmt=.3
# h<-h+ stat_summary(fun.y=mean,geom="point",position=position_dodge(width=dodgeAmt)) 
# #h<-h+stat_summary(fun.data = mean_cl_normal, geom="errorbar", mult=1, width=.5, position=position_dodge(width=dodgeWidth))
# h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",width=.25,conf.int=.95,position=position_dodge(width=dodgeAmt)) 
# h<-h+ stat_summary(fun.y=mean,geom="line",position=position_dodge(width=dodgeAmt))
# h<-h+ggtitle(paste("Speed-limited for few distractrs, not much flattening by 3 targets",lapseMsg))
# show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
# ggsave( paste0('figs/E1_EpostVSStargets_',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
# ##########################################################################################
# ################tf individual Ss against distractors
# tit=paste0("tfSsAgainstDistractors ",infoMsg," threeQuarterThresh")
# quartz(title=tit,width=6,height=3) 
# #Not fair to include values above the worst-observer's lapse rate. Because then the speed limit cost of second target is infinite.
# h<-ggplot(data=subset(threshes,criterionNote=="threeQuarters"),   #midpoint
#           aes(x=numObjects-1,y=tfThresh,color=subject)) #color=targets,
# h<-h+facet_grid(exp~targets)  #facet_grid(criterion ~ exp)
# h<-h+ylab('threshold tf (Hz)')
# h<-h+themeAxisTitleSpaceNoGridLinesLegendBox
# h<-h+xlab('Distractors')
# xTicks= unique(threshes$numObjects-1) #put axis ticks at actual values used
# h<-h+scale_x_continuous(breaks=c( xTicks ))
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# dodgeAmt=.3
# h<-h+ geom_point()
# h<-h+ geom_line()
# h<-h+ggtitle(paste("individ Ss all show speed-limited for few distractrs, not much flattening by 3 targets",lapseMsg))
# show(h) #http://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2?rq=1
# ggsave( paste0('figs/E1_EpostVSStargets_',tit,'.png') ,bg="transparent" ) #bg option will be passed to png
# ##########################################################

###################################
#plot thresholds (at only one criterion level) for all 3 experiments at same time
# quartz()
# #tt<-subset(threshes,subject=="AH");  tt<-subset(tt,numTargets=="1")
# #tt$subject<-factor(tt$subject) #in case unused levels were the problem
# #h<-ggplot(data= fake, aes(x=separatnDeg,y=thresh))
# h<-ggplot(data= subset(threshes,numTargets!="2P"), aes(x=separatnDeg,y=thresh,color=numTargets,shape=exp))
# h<-h + facet_grid(exp~., scales="free") # ~criterion
# #h<-h+stat_summary(data=threshesThisNumeric,fun.data="mean_cl_boot",geom="errorbar",conf.int=.95,position=position_dodge(width=.2)) #error bar
# h<-h+stat_summary(fun.data="mean_cl_boot",geom="errorbar",conf.int=.95,position=position_dodge(width=.2)) #error bar
# h<-h+theme_bw() + xlab("Separation (deg)")
# #ylim(1.4,2.5) DO NOT use this command, it will drop some data
# #h<-h+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
# #h<-h+coord_cartesian(ylim=c(1.4,2.6)) #have to use coord_cartesian here instead of naked ylim() to don't lose part of threshline
# h<-h+ stat_summary(fun.y=mean,geom="point") + stat_summary(fun.y=mean,geom="line") 
# h+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines
# h<-h+ggtitle(paste(tit,lapseMsg))
# h
