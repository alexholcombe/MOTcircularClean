#This is a stand-alone file for making plots of speed-tf tracking region
#working directory set by starting Rstudio via .Rproj file
setwd("/Users/alexh/Documents/attention_tempresltn/multiple\ object\ tracking/newTraj/newTraj_repo/experiment_specific/windowOfTrackingTheory")
source('psychometricHelpRobust6.R') #Notice there's another version in analysis/ #for makeMyPsychoCorr, 
themeAxisTitleSpaceNoGridLinesLegendBox = theme_classic() + #Remove gridlines, show only axes, not plot enclosing lines
  theme(axis.line = element_line(size=.3, color = "grey"), 
        axis.title.y=element_text(vjust=0.24), #Move y axis label slightly away from axis
        axis.title.x=element_text(vjust=.10), #Move x axis label slightly away from axis
        legend.key = element_blank(), #don't put boxes around legend bits
        legend.background= element_rect(color="grey90"), #put big light grey box around entire legend
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)   )

tfLimit1_2_3targets = c(7,4.2,2.5) #based on VSS2014 poster midpoint thresholds
speedLimit1_2_3targets = c(2.1,1.9,1.2) #from VSS2014 poster 1-distractor eyeballed thresholds
intersectionPoint = c(-99,-99,-99)
#the window of tracking
tfLimitCalc<-function(targetNum,distractorNum) {
  tfLimit1_2_3targets[targetNum] / (distractorNum+1)
}
#winT, the windowOfTracking
bouma = pi*4-1 #crowding limit is 4*pi-1 distractors
motionPerceptnHzLimit = 25 #according to Verstraten et al. 2000

winT = expand.grid( distractors = seq(1,ceiling(bouma),1), targets = seq(1,3,1), limit=c("speed","tf") )
winT$thresh=-1
calcWinLimitsAndIntersection<-function(winT,speedLimit1_2_3targets) {
  #Calculate intersection point where speed limit meets tf limit
  for (target in c(1,2,3)) {
    distractrs = winT[ winT$targets==target & winT$limit=="tf", ]$distractors  
    winT[ winT$targets==target & winT$limit=="tf", ]$thresh = tfLimitCalc(target,distractrs)
    winT[ winT$targets==target & winT$limit=="speed", ]$thresh = speedLimit1_2_3targets[target]
    #intersection is where tfLimit1_2_3targets[target] / (distractrs+1) = speedLimit
    intersectionPoint[target] = tfLimit1_2_3targets[target] / speedLimit1_2_3targets[target]  -1
  }
  return (list(intersectPts=intersectionPoint,winT=winT))
}
l<-calcWinLimitsAndIntersection(winT,speedLimit1_2_3targets)
intersectPoints<-l$intersectPts
winT<-l$winT 

#calculate speed threshold corresponding to lowest (constraining) limit for this num distractors
constrainingLimit<-function(intersectPoints,targetNum,distractorNum) {
  if (distractorNum <= intersectPoints[targetNum]) {
    return (speedLimit1_2_3targets[targetNum])
  }
  else { return (tfLimitCalc(targetNum,distractorNum)) }
}

#create shading inside intersection of limits. 
calcPolygonXYs<-function(targets,winT,intersectionPoints) {
  uniqDistractrs = unique(winT$distractors)
  tfLimitedDistractrs = uniqDistractrs[ uniqDistractrs>intersectionPoints[targets] ]
  #cut off right end at crowding limit
  numTfLimitedBeforeCrowdingCutoff = length(tfLimitedDistractrs)
  tfLimitedDistractrs = tfLimitedDistractrs[ tfLimitedDistractrs < bouma ]
  if (length(tfLimitedDistractrs) < numTfLimitedBeforeCrowdingCutoff) { #if was cut off by crowding limit
    tfLimitedDistractrs[length(tfLimitedDistractrs)+1] = bouma #will only work if only one was cut off
  }
  xs= c(min(uniqDistractrs),intersectionPoints[targets], tfLimitedDistractrs, #top edge of polygon
        bouma, min(uniqDistractrs)) #bottom edge of polygon  
  ys= unlist( lapply(xs[1:(length(xs)-2)],FUN=constrainingLimit,intersectPoints=intersectionPoints,targetNum=targets) ) #top edge
  ys = c(ys,0,0) #adding bottom edge
  return (list(x=xs,y=ys))
}
tst=calcPolygonXYs(1,winT,intersectPoints) #test the function
#vertices of the polygon of the tracking window
positions <- data.frame( #vertices of the polygon
  targets = rep(c(1), each = length(calcPolygonXYs(1,winT,intersectPoints)$x)), #targetsID
  limit = "tf",
  x= calcPolygonXYs(1,winT,intersectPoints)$x,
  y= calcPolygonXYs(1,winT,intersectPoints)$y
)

#Show motion perception limit and 1-target limit. 
tit="windowOfTracking_1target_motionPerceptionLimit"
quartz(tit,width=6.4,height=3.5)
g=ggplot(subset(winT,targets==1), 
         aes( x=distractors,y=thresh, color=factor(targets), linetype=factor(limit)) )
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(2,3)) #make them both dashed, then make solid the lowest limit
#g=g+scale_linetype_manual(values=c(2,2)) #make them both dashed, then make solid the lowest limit
includeAnnotatns<- FALSE #sometimes FALSE so that can add prettier in Keynote
g=g+scale_x_continuous(breaks=seq(min(winT$distractors), ceiling(bouma), 1))
g=g+geom_polygon(data = positions, aes(x, y, targets), fill="pink", color="transparent", alpha=.7)
g=g+annotate("text",x=6,y=1,label=paste(toString(tfLimit1_2_3targets[1]),"Hz"), angle=-6,size=4 )
g=g+annotate("text", x=4.6,y=speedLimit1_2_3targets[1], 
             label=paste(toString(speedLimit1_2_3targets[1]),"rps"), size=4)
g=g+annotate("text", x=2.5, y=.8, label="trackable", fontface=3, alpha=.8) #italics
g=g+coord_cartesian(xlim=c(min(winT$distractors),max(winT$distractors)),
                    ylim=c(0,max(motionPerceptnHzLimit/(winT$distractors+1))))
g=g+ylab('threshold (rps)')
g=g+geom_line(data=data.frame(x=winT$distractors,
                              y=motionPerceptnHzLimit/(winT$distractors+1), limit="tf"), 
              aes(x,y),color="grey",linetype=1, size=1.5)
g=g+geom_vline(xintercept=bouma,color="grey")#,size=1.5)
xmax<-ggplot_build(g)$panel$ranges[[1]]$x.range[2]
ymax<-ggplot_build(g)$panel$ranges[[1]]$y.range[2]
crowdingRegion <- data.frame( #vertices of the polygon
  targets = c(1,1,1,1), #targetsID
  limit = "tf",
  x= c(bouma,bouma,xmax,xmax),
  y=c(0,ymax,ymax,0)
) 
g=g+geom_polygon(data=crowdingRegion,aes(x,y,targets), fill="burlywood4",color="transparent",alpha=.8 )
g=g+annotate("text",x=bouma+.18,y=6,label="crowded", color="black", size=4, angle=90)
if (includeAnnotatns) {
  g=g+annotate("text",x=3,y=motionPerceptnHzLimit/4.3,label="motion perception", color="black", size=4, angle=-55)
}
g=g+annotate("text",x=6,y=3.6,label=paste(toString(motionPerceptnHzLimit),"Hz"), angle=-22,size=4 )
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")

#calcPolygonXYs not perfect, for 1 target, need to make it longer by 1 because different length
oneTargXs = c( calcPolygonXYs(1,winT,intersectPoints)$x[1], calcPolygonXYs(1,winT,intersectPoints)$x)
oneTargYs = c( calcPolygonXYs(1,winT,intersectPoints)$y[1], calcPolygonXYs(1,winT,intersectPoints)$y) 
positions <- data.frame( #vertices of the polygons for 1, 2, and 3 target limits
  targets = rep(c(1,2,3), each = length(calcPolygonXYs(3,winT,intersectPoints)$x)), #targetsID
  limit = "tf",
  x= c(oneTargXs, calcPolygonXYs(2,winT,intersectPoints)$x, calcPolygonXYs(3,winT,intersectPoints)$x),
  y= c(oneTargYs, calcPolygonXYs(2,winT,intersectPoints)$y, calcPolygonXYs(3,winT,intersectPoints)$y)
)

#Also create a polygon pretending that the 2-target and 3-target speed limits are not worse than 1 target
#This is for the manuscript figure that visualises the question being asked
speedLimit1targetThrice<- c(speedLimit1_2_3targets[1],speedLimit1_2_3targets[1],speedLimit1_2_3targets[1])
l<-calcWinLimitsAndIntersection(winT,speedLimit1targetThrice)
interPtsSpeedLimSimple<-l$intersectPts
one<-data.frame(targets=1, limit="tf", x=oneTargXs, y=oneTargYs)
two<-data.frame(targets=2, limit="tf", calcPolygonXYs(2,winT,interPtsSpeedLimSimple))
three<-data.frame(targets=3, limit="tf", calcPolygonXYs(3,winT,interPtsSpeedLimSimple))
positionsSimple<-rbind(one,two,three)

#Show 1,2,3-target trackable regions assuming same *speed* limit for all. For introductory figure. 
tit="windowOfTracking_123targets_1targSpeedLim"
includeAnnotatns<-TRUE
if (!includeAnnotatns) tit<-paste0(tit,"_noText")
winTlo<-subset(winT,distractors<9) #Show only up to 8 distractors to avoid crowding limit and
#because only test up to 8 distractors
quartz(tit,width=6.4,height=3.5)
g=ggplot(subset(winTlo,targets==1), 
         aes( x=distractors,y=thresh, color=factor(targets), fill=factor(targets), linetype=factor(limit)) )
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(1,1))#values=c(2,3)) #make them both dashed, then make solid the lowest limit
g=g+scale_fill_manual(values=c("pink","green","dodgerblue3")) #make them both dashed, then make solid the lowest limit
g=g+coord_cartesian(xlim=c(min(winTlo$distractors),max(winTlo$distractors)),ylim=c(0,max(winT$thresh)+.1))
g=g+scale_x_continuous(breaks=seq(min(winTlo$distractors), max(winTlo$distractors), 1))
g=g+geom_polygon(dat=positionsSimple, aes(x,y,targets), alpha=.4)
g=g+ylab('threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
if (includeAnnotatns) {
  g=g+annotate("text", x=6,y=1, label=paste(toString(tfLimit1_2_3targets[1]),"Hz"), size=4, angle=-15 )
  g=g+annotate("text", x=6,y=.60, label=paste(toString(tfLimit1_2_3targets[2]),"Hz"), size=4, angle=-8 )
  g=g+annotate("text", x=5.94,y=.34, label=paste(toString(tfLimit1_2_3targets[3]),"Hz"), size=4, angle=-5 )
  targetsOnlyInLegend <- TRUE
  if (!targetsOnlyInLegend) {
    g=g+annotate("text", x=3, y=1.4, label="1 target", fontface=3, angle=-30,size=4) #italics
    g=g+annotate("text", x=3, y=0.85, label="2 targets", fontface=3, angle=-15,size=4) #italics
    g=g+annotate("text", x=3, y=0.32, label="3 targets", fontface=3, angle=-8,size=4) #italics
  }
}
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")


#Show 1,2,3-target trackable regions. 
tit="windowOfTracking_123targets"
if (!includeAnnotatns) tit<-paste0(tit,"_noText")
winTlo<-subset(winT,distractors<9) #Show only up to 8 distractors to avoid crowding limit and
#because only test up to 8 distractors
quartz(tit,width=6.4,height=3.5)
g=ggplot(subset(winTlo,targets==1), 
         aes( x=distractors,y=thresh, color=factor(targets), fill=factor(targets), linetype=factor(limit)) )
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(1,1))#values=c(2,3)) #make them both dashed, then make solid the lowest limit
g=g+scale_fill_manual(values=c("pink","green","dodgerblue3")) #make them both dashed, then make solid the lowest limit
g=g+coord_cartesian(xlim=c(min(winTlo$distractors),max(winTlo$distractors)),ylim=c(0,max(winT$thresh)+.1))
g=g+scale_x_continuous(breaks=seq(min(winTlo$distractors), max(winTlo$distractors), 1))
g=g+geom_polygon(dat=positions, aes(x,y,targets), alpha=.4)
if (includeAnnotatns) {
  g=g+annotate("text", x=6,y=1, label=paste(toString(tfLimit1_2_3targets[1]),"Hz"), size=4, angle=-15 )
  g=g+annotate("text", x=6,y=.66, label=paste(toString(tfLimit1_2_3targets[2]),"Hz"), size=4, angle=-8 )
  g=g+annotate("text", x=6,y=.3, label=paste(toString(tfLimit1_2_3targets[3]),"Hz"), size=4, angle=-5 )
  g=g+annotate("text", x=4,y=speedLimit1_2_3targets[1], 
               label=paste(toString(speedLimit1_2_3targets[1]),"rps"), size=4 )
  g+text(x=1,y=speedLimit1_2_3targets[3], 
         label=paste(toString(speedLimit1_2_3targets[3]),"rps") )
  g=g+annotate("text", x=1,y=speedLimit1_2_3targets[2], 
               label=paste(toString(speedLimit1_2_3targets[2]),"rps"), size=4 )
  g=g+annotate("text", x=1,y=speedLimit1_2_3targets[3], 
               label=paste(toString(speedLimit1_2_3targets[3]),"rps"), size=4 )
  g=g+annotate("text", x=3, y=1.4, label="1 target", fontface=3, angle=-30,size=4) #italics
  g=g+annotate("text", x=3, y=0.9, label="2 targets", fontface=3, angle=-20,size=4) #italics
  g=g+annotate("text", x=3, y=0.4, label="3 targets", fontface=3, angle=-8,size=4) #italics
}
g=g+ylab('threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)
ggsave( paste('figs/',tit,'.png',sep=''), bg="transparent")

#Show 1,2,3-target trackable regions AND spatial crowding limit 
tit="windowOfTracking_123targets"
quartz(tit,width=6.4,height=3.5)
g=ggplot(subset(winT,targets==1), 
         aes( x=distractors,y=thresh, color=factor(targets), fill=factor(targets), linetype=factor(limit)) )
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(2,3)) #make them both dashed, then make solid the lowest limit
g=g+scale_fill_manual(values=c("pink","green","dodgerblue3")) #make them both dashed, then make solid the lowest limit
g=g+geom_polygon(data = positions, aes(x, y, targets), alpha=.4)
g=g+annotate("text", x=6,y=1, label=paste(toString(tfLimit1_2_3targets[1]),"Hz"), size=4, angle=-15 )
g=g+annotate("text", x=6,y=.6, label=paste(toString(tfLimit1_2_3targets[2]),"Hz"), size=4, angle=-8 )
g=g+annotate("text", x=6,y=.34, label=paste(toString(tfLimit1_2_3targets[3]),"Hz"), size=4, angle=-5 )
g=g+annotate("text", x=4,y=speedLimit1_2_3targets[1], 
             label=paste(toString(speedLimit1_2_3targets[1]),"rps"), size=4 )
#I don't know how to make text, e.g. 1.9 rps extend over axis boundary
# g=g+annotate("text", x=1,y=speedLimit1_2_3targets[2], 
#              label=paste(toString(speedLimit1_2_3targets[2]),"rps"), size=4 )
# g=g+annotate("text", x=1,y=speedLimit1_2_3targets[3], 
#              label=paste(toString(speedLimit1_2_3targets[3]),"rps"), size=4 )
g=g+annotate("text", x=3, y=1.4, label="1 target", fontface=3, angle=-30,size=4) #italics
g=g+annotate("text", x=3, y=0.9, label="2 targets", fontface=3, angle=-20,size=4) #italics
g=g+annotate("text", x=3, y=0.4, label="3 targets", fontface=3, angle=-8,size=4) #italics
g=g+ylab('speed threshold (rps)')
g=g+coord_cartesian(xlim=c(min(winT$distractors),ceiling(bouma)+.2),
                    ylim=c(0,max(winT$thresh)+.1))
g=g+geom_vline(xintercept=bouma,color="grey")
g=g+annotate("text",x=bouma+.3,y=1.35,label="crowded", size=4, angle=90)
#Maybe add a pale blue region for crowded, 
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)
ggsave( paste('figs/windowOfTracking/',tit,'.png',sep=''), bg="transparent")

########################
#Consider showing reduction in speed limits

#graph speed, tf limits, 
tit="windowOfTracking"
quartz(tit,width=6.4,height=3.5)
g=ggplot(winT, aes( x=distractors,y=thresh, color=factor(targets), linetype=factor(limit)) )
#g=g+geom_point()
g=g+geom_line(size=.75)
g=g+scale_linetype_manual(values=c(2,2)) #make them both dashed, then make solid the lowest limit
g=g+coord_cartesian(xlim=c(min(winT$distractors),max(winT$distractors)),ylim=c(0,max(winT$thresh)+.1))
g=g+geom_polygon(data = positions, aes(x, y, targets), fill="pink", color="transparent", alpha=.7)
g=g+annotate("text", x=6,y=1, label=paste(toString(6.5),"Hz"), angle=-15 )
g=g+ylab('speed threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)

#Rather than having one linetype for speed limit and one for tf limit, have the polygon enclosing
#the lower limit always solid, and the non-constraining limit dashed or dotted.
g=g+ geom_line(data=positions[1:(nrow(positions)-2),],aes(x,y-.03),linetype=1,size=1.5,alpha=.9)
show(g)



  
  y= constrainingLimit(targets,polygonXs,)
  y= c(speedLimit1_2_3targets[1],speedLimit1_2_3targets[1],0,0, )
  
  
  x = c(0,intersectionPoint[1],intersectionPoint[1],0),
  y = c(speedLimit1_2_3targets[1],speedLimit1_2_3targets[1],0,0) )

positions <- data.frame(
  targets = rep(c(1), each = 4), #targetsID
  limit = "tf",
  x = c(0,intersectionPoint[1],intersectionPoint[1],0),
  y = c(speedLimit1_2_3targets[1],speedLimit1_2_3targets[1],0,0) )



positions<- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

g=g+geom_polygon(data = shade, aes(x, y))

subset(winT,distractors > intersection)
shade <- rbind(c(0.12,0), subset(MyDF, x > 0.12), c(MyDF[nrow(MyDF), "X"], 0))


#Then use this new data.frame with geom_polygon
p + geom_segment(aes(x=0.12,y=0,xend=0.12,yend=ytop)) +
  geom_polygon(data = shade, aes(x, y))

tit="limits"
quartz(tit,width=6.4,height=3.5)
# lims = rbind(  data.frame(limit="tf",val=tfLimit1_2_3targets ), 
#                data.frame(limit="speed",val=speedLimit1_2_3targets )  )
lims = data.frame(tf=tfLimit1_2_3targets, speed=speedLimit1_2_3targets )
lims$targets = c(1,2,3)
g=ggplot(lims,aes(x=tf,y=speed,color=factor(targets))) + geom_point()
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
show(g)

  winT, aes( x=distractors,y=thresh, color=factor(targets), linetype=factor(limit)) ) 
g=g+geom_line(size=1.5)
g=g+annotate("text", x=6,y=1, label=paste(toString(tf),"Hz"), angle=-15 )
g=g+ylab('speed threshold (rps)')
g=g+themeAxisTitleSpaceNoGridLinesLegendBox
#g=g+facet_grid(targets~.) #facet_grid(targets~criterion)
show(g)

#################################################################################
#Multiply it with each other, then extract resulting threshes
pAfterBothLimits<- function(p1,p2,numObjects,lapse) {
  #Take guesses out (assumption: high-threshold model) of each p
  #to yield t, the "true function"- probability of successful tracking.
  #Otherwise, have to guess.
  #Then multiply probs together and re-insert guessing rate
  l = lapse
  c = 1/numObjects #chanceRate
  #Derived the below by taking standard psychometric function
  # p = l*c + (1-l)(t + (1-t)*c) #and solving for t
  t1 = (p1 - c) / (1 + l*c - l - c)
  t2 = (p2 - c) / (1 + l*c - l - c)
  #probability don't fall afoul of either limit
  b = t1*t2
  #re-insert lapse rate and guessing
  pAfter = l*c + (1-l)*(b + (1-b)*c)
  #cat('p1=',p1,' p2=',p2,' b=',b, ' pAfter=',pAfter)
  #cat('t1=',t1,' t2=',t2,' b=',b, ' pAfter=',pAfter)
  as.numeric(pAfter)
}



