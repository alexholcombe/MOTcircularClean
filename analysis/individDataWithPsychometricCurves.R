#This file's only purpose is to provide a plotIndividDataAndCurves function
#Variables expected:
#dat
#iv - "tf" or "speed"
if (!("speed" %in% colnames(psychometrics))) { #psychometrics must have been fit to tf
  stopifnot("tf" %in% colnames(psychometrics)) #confirm my interpretation that tf was fit
  psychometrics$speed = psychometrics$tf / psychometrics$numObjects #because always plot them in terms of speed
}

#function that plots the psychometric functions for a dataset / experiment /criterion,
plotIndividDataAndCurves <- function(expName,df,psychometricCurves,factors,xmin=NULL,xmax=NULL) {
  #draw individual psychometric functions for individual experiment
  #the F's, like colorF are factors to break down data by (expects psychometric functions to have these factors)
  #factors is named structure, like list. If no factor, should be ""
  colorF<-factors$colorF
  colF<-factors$colF
  rowF<-factors$rowF
  title<-paste0(expName,' individual Ss data')
  quartz(title,width=6,height=7)
  #print( table(df[colorF],df[colF],df[rowF) ) #debug
  g=ggplot(data=df,aes_string(x='speed',y='correct',color=colorF))
  g=g+stat_summary(fun.y=mean,geom="point", position=position_jitter(w=0.01,h=0.01),alpha=.95)
  if (colF != "" | rowF != "") {
    if (colF=="") { colF='.' }
    if (rowF=="") { rowF='.'}
    facetString = paste(rowF,"~",colF)
    g=g+facet_grid(facetString)
  }
  g=g+ theme_bw()
  
  show(g)
  #draw individual psychometric functions, for only one experiment  
  g=g+geom_line(data=psychometricCurves)
  g=g+geom_hline(mapping=aes(yintercept=chance),lty=2)  #draw horizontal line for chance performance
  g=g+xlab('Speed (rps)')+ylab('Proportion Correct')
  xlims= ggplot_build(g)$panel$ranges[[1]]$x.range #http://stackoverflow.com/questions/7705345/how-can-i-extract-plot-axes-ranges-for-a-ggplot2-object
  if (!is.null(xmin))
    xlims[1]<-xmin
  if (!is.null(xmax))
    xlims[2]<-xmax
  g<-g+ coord_cartesian( xlim=xlims )#have to use coord_cartesian here instead of naked ylim()  
  
  showNumPts=TRUE
  if (showNumPts) {#add count of data points per graph. http://stackoverflow.com/questions/13239843/annotate-ggplot2-facets-with-number-of-observations-per-facet?rq=1
    breakDownBy<- c(colF,rowF)
    
    numPts <- df |> group_by( !!!syms(breakDownBy)) |> 
                  summarize(n=paste("n =", length(correct))) 
  
    g=g+geom_text(data=numPts, aes(x=xlims[2]-.2, y=.95, label=n), 
                  colour="black", size=2, inherit.aes=FALSE, parse=FALSE)
  }  
  #g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
  #g<- g+ theme(axis.title.y=element_text(size=12,angle=90),axis.text.y=element_text(size=10),axis.title.x=element_text(size=12),axis.text.x=element_text(size=10))
  #g<-g+ scale_x_continuous(breaks=c(1.5,2.0,2.5),labels=c("1.5","","2.5"))
  #g<-g+ scale_x_continuous(breaks=speeds)
  show(g)
  ggsave( paste('figs/individPlotsE',expName,'.png',sep='')  )
  
  #can't show threshold lines right now because depends on criterion
  # thisThreshes<- subset(threshesThisNumeric, exp==1)
  # threshLines <- ddply(thisThreshes,factorsPlusSubject,threshLine)
  # g<-g+ geom_line(data=threshLines,lty=3,size=0.9)  #,color="black") #emphasize lines so can see what's going on
  
}

#condition: centered, slightly off-center, or fully off-center

# for ( expThis in sort(unique(dat$exp)) ) {  #draw individual Ss' data, for each experiment
#   title<-paste('E',expThis,' individual Ss data',sep='')
#   quartz(title,width=6,height=7)
#   thisExpDat <- subset(dat,exp==expThis)
#   g=ggplot(data= thisExpDat,aes(x=speed,y=correct,color=factor(condName)))
#   g=g+stat_summary(fun.y=mean,geom="point", position=position_jitter(w=0.01,h=0.01),alpha=.95)
#   g=g+facet_grid(leftOrRight ~ subject)+theme_bw()
#   g=g+facet_grid(ringToQuery ~ subject)+theme_bw()
#   
#   #g<-g+ coord_cartesian( xlim=c(1.0,2.45), ylim=yLims ) #have to use coord_cartesian here instead of naked ylim()
#   show(g)
#   #draw individual psychometric functions, for only one experiment  
#   thisPsychometrics <- subset(psychometrics,exp==expThis)
#   g=g+geom_line(data=thisPsychometrics)
#   g=g+geom_hline(mapping=aes(yintercept=chanceRate),lty=2)  #draw horizontal line for chance performance
#   g=g+xlab('Speed (rps)')+ylab('Proportion Correct')
#   showNumPts=TRUE
#   if (showNumPts) {#add count of data points per graph. http://stackoverflow.com/questions/13239843/annotate-ggplot2-facets-with-number-of-observations-per-facet?rq=1
#     numPts <- ddply(.data=thisExpDat, .(leftOrRight,subject), summarize, 
#                     n=paste("n =", length(correct)))
#     numPts <- ddply(.data=thisExpDat, .(ringToQuery,subject), summarize, 
#                     n=paste("n =", length(correct)))
#     g=g+geom_text(data=numPts, aes(x=2.2, y=.95, label=n), 
#                   colour="black", size=2, inherit.aes=FALSE, parse=FALSE)
#   }  
#   #g=g+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())# hide all gridlines.
#   #g<- g+ theme(axis.title.y=element_text(size=12,angle=90),axis.text.y=element_text(size=10),axis.title.x=element_text(size=12),axis.text.x=element_text(size=10))
#   #g<-g+ scale_x_continuous(breaks=c(1.5,2.0,2.5),labels=c("1.5","","2.5"))
#   #g<-g+ scale_x_continuous(breaks=speeds)
#   show(g)
#   ggsave( paste('figs/individPlotsE',expThis,'.png',sep='')  )
# }
