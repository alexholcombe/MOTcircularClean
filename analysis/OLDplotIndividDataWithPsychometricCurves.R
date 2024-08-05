#Variables expected:
#dat
#iv - e.g. "tf" or "speed"
plotTf_as_tf_speed_as_speed <- TRUE #If false, plot speed on horizontal axis even if fit was to TF
source('helpers/rnc_ggplot2_border_themes_2013_01.r') # Simple extensions for removing graph sides, see http://egret.psychol.cam.ac.uk/statistics/R/extensions/rnc_ggplot2_border_themes_2013_01.r  
rowsLabeller <- function(variable,value) { #Label facet_grid with "3 deg" instead of "3"
  #cat(paste(variable)); cat(paste(value))
  if (variable=='separatnDeg') {
    return (paste(value,'deg')) 
  } else if (variable=='numObjects') {
    return (paste(value,' objects'))
  } else if (variable=='subject') { #subject
    return(levels(thisExpDat$subject)[value])
  }
}
themeAxisTitleSpaceNoGridLinesLegendBox = theme_classic() + #Remove gridlines, show only axes, not plot enclosing lines
  theme(axis.line = element_line(size=.3, color = "grey"), 
        axis.title.y=element_text(vjust=0.24), #Move y axis label slightly away from axis
        axis.title.x=element_text(vjust=.10), #Move x axis label slightly away from axis
        legend.key = element_blank(), #don't put boxes around legend bits
        legend.background= element_rect(color="grey90"), #put big light grey box around entire legend
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)   )


#function, not used, that plots the psychometric functions for a dataset / experiment /criterion,
plotIndividDataAndCurves <- function(df,psychometricCurves) {
  #draw individual psychometric functions
  g=ggplot(data= df,aes(x=speed,y=correct,color=factor(numTargets),shape=factor(separatnDeg)))
  g=g+stat_summary(fun.y=mean,geom="point", position=position_jitter(w=0.04,h=0),alpha=.95)
  g=g+facet_wrap(separatnDeg ~ subject,ncol=8)+theme_bw()

  #can't do this right now because depends on criterion
  # thisThreshes<- subset(threshesThisNumeric, exp==1)
  # threshLines <- ddply(thisThreshes,factorsPlusSubject,threshLine)
  # g<-g+ geom_line(data=threshLines,lty=3,size=0.9)  #,color="black") #emphasize lines so can see what's going on
  g<-g+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
  g
  g=g+geom_line(data=psychometricCurves)
  g=g+ geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
  g=g+xlab('Speed (rps)')+ylab('Correct')
  g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
  g <- g+ theme(axis.title.y=element_text(size=12,angle=90),axis.text.y=element_text(size=10),axis.title.x=element_text(size=12),axis.text.x=element_text(size=10))
  g<-g+ scale_x_continuous(breaks=c(0.5,1.0,1.5,2.0,2.5),labels=c("0.5","","1.5","","2.5"))
  g		
}

if (plotTf_as_tf_speed_as_speed) { abscissa=iv } else 
  { abcissa = "speed" }
if (abscissa=="speed" | abscissa=="logSpd") { unitsLabel = "(rps)" } else
  { unitsLabel = "(Hz)" }
for ( expThis in sort(unique(dat$exp)) ) {  #draw individual Ss' data, for each experiment
  title<-paste('E',expThis,'_indivSs_',iv,"_fit",sep='')
  thisExpDat <- subset(dat,exp==expThis)
  nrows = length(unique(thisExpDat$numObjects))
  winHeight = 1.0*nrows
  ncols = length(unique(thisExpDat$subject))
  winWidth = 1.5*ncols
  quartz(title,width=winWidth,height=winHeight)
  #g=ggplot(data= thisExpDat,aes(x=speed,y=correct,color=factor(numTargets)))
  g=ggplot(data= thisExpDat,aes_string(x=abscissa,y="correct",color="factor(numTargets)"))
  g=g+stat_summary(fun.y=mean,geom="point", position=position_jitter(w=0.04,h=0),alpha=.95)
  #The below rowsLabeller stopped working, seems like ggplot not sending both variables anymore
  if (packageVersion("ggplot2") != '1.0.1') {
    print("WARNING! Probably rowsLabeller wont work anymore, check help for details")
  }
  g=g+facet_grid(numObjects ~ subject, labeller = rowsLabeller) 
  #g<-g+ coord_cartesian( xlim=c(xLims[1],xLims[2]), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
  #draw psychometric functions  
  thisPsychometrics <- subset(psychometrics,exp==expThis)
  g=g+geom_line(data=thisPsychometrics)
  g=g+ geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
  g=g+ xlab( paste(abscissa,unitsLabel) )
  g=g+ ylab('Correct') +theme_bw()
  if (abscissa =="speed") {
    g<-g+ scale_x_continuous(breaks=c(0.5,1.0,1.5,2.0,2.5),labels=c("0.5","","1.5","","2.5"))
  }
  g<-g+ themeAxisTitleSpaceNoGridLinesLegendBox
  prettifyForPaper<-TRUE
  if (prettifyForPaper) {
    g<-g+guides(color=guide_legend(title="targets")) #change legend title
    g<-g+theme(strip.background = element_rect(fill = 'white',color='white'))
    if (nrows<4) {
      g<-g+theme(strip.text.y= element_text(vjust=1.0,size=9)) 
    } else {
      g<-g+theme(strip.text.y= element_text(vjust=1.0,size=10)) 
    }
    #g<-g+  theme(legend.position = c(0.05,0.84))
    g<-g+ scale_colour_discrete(name  ="Targets") + scale_shape_discrete(name="Objects",guide="none")
    g<-g+theme( panel.border = element_blank()  )
    g<-g+scale_y_continuous(breaks=c(0,.25,0.5,.75,1),labels=c("",".25",".50",".75","1.0"))
    #g<-g+ theme(panel.margin = unit(0.7, "lines")) #increase vertical spacing between graphs
    numColors<- length(unique(thisExpDat$numTargets))
    g<-g+ theme(legend.key.height=unit(0.8+(numColors-2)*.1,"line"))
    if (numColors == 2) { #kludge because other graph had 3 colors
      #and want to use same colors for the conditions in the 2-target conditions case
      g<-g+scale_colour_manual(values = c("#F8766D","#619CFF"))
    }
  }
  #g + scale_x_log10()
  show(g)
  ggsave( paste('figs/',title,'.png',sep='')  )
}
