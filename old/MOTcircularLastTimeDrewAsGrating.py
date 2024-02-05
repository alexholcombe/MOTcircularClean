from __future__ import print_function
__author__ = """Alex "O." Holcombe, Wei-Ying Chen""" ## double-quotes will be silently removed, single quotes will be left, eg, O'Connor
__version__ = "v'''" ## in-line comments are ignored, but comment characters within strings are retained
from psychopy import *
import psychopy.info
from psychopy import sound, monitors, logging
import numpy as np
import itertools #to calculate all subsets
from numpy import *  #some programs import random, which is also its own library as well as a numpy sub-module
from copy import deepcopy
from math import atan
import time, colorsys
import sys, platform, os, StringIO
#BEGIN helper functions from primes.py
def gcd(a,b): 
   """Return greatest common divisor using Euclid's Algorithm."""
   while b:      
	a, b = b, a % b
   return a
def lcm(a,b):
   """
   Return lowest common multiple."""
   return (a*b)/gcd(a,b)
def LCM(terms):
   "Return lcm of a list of numbers."   
   return reduce(lambda a,b: lcm(a,b), terms)
#END helper functions from primes.py
def calcCondsPerNumTargets(numRings,numTargets):
    #numRings is number of rings, each of which can have up to one target
    #numTargets is list or array of numTarget conditions, e.g. 1,2,3 means the experiment includes 1, 2, and 3 targets
    #Each target can be placed randomly in any of the rings.
    #Want all possibilities to be covered equally often. That means each target number condition has to include all the combinations
    #     of places that number of targets can go.
    #So that some targetNum conditinos don't have more trials than others, have to scale up each targetNum condition to the worst case.
    #Actually it's worse than that. To make them fit evenly, have to use least common multiple
    #3 rings choose 2 for targets, 3 rings choose 1 for target, have to have as many conditions as the maximum.
    #To find maximum, determine length of each.
    ringNums = np.arange(numRings)
    numPossibilitiesEach = list()
    for k in numTargets:
        numPossibilitiesCouldPutKtargets = len( list(itertools.combinations(ringNums,k)) )
        #print(numPossibilitiesCouldPutKtargets)
        numPossibilitiesEach.append(  numPossibilitiesCouldPutKtargets  )
    m = max( numPossibilitiesEach )  #because the worst case (number of targets) requires this many, have to have this many for all. Actually,
    leastCommonMultiple = LCM( numPossibilitiesEach )  #to have equal number of trials per numtargets, would have to use this figure for each
    #print('biggest=',m, ' Least common multiple=', leastCommonMultiple)
    return leastCommonMultiple
    
quitFinder = False
if quitFinder:
    applescript="\'tell application \"Finder\" to quit\'" #quit Finder.
    shellCmd = 'osascript -e '+applescript
    os.system(shellCmd)

subject='test'#'AH'
autoLogging = False
demo = False 
autopilot=False
if autopilot:  subject='auto'
feedback=True
exportImages= False #quits after one trial / output image
slitView = False  #visual barrier
screenshot= False; screenshotDone = False;showRefreshMisses=False;allowGUI = False;waitBlank = False
trackAllIdenticalColors = True#with tracking, can either use same colors as other task (e.g. 6 blobs but only 3 colors so have to track one of 2) or set all blobs identical color
Reversal =True
drawingAsGrating= False
antialiasGrating=False

timeAndDateStr = time.strftime("%d%b%Y_%H-%M", time.localtime()) 
respTypes=['order']; respType=respTypes[0]
bindRadiallyRingToIdentify=1 #0 is inner, 1 is outer
trackPostcueOrClick = 1 #postcue means say yes/no postcued was a target, click means click on which you think was/were the targets

#open directory
if os.path.isdir('.'+os.sep+'data'):
    dataDir='data'
else:
    print('"data" directory does not exist, so saving data in present working directory')
    dataDir='.'
fileName = dataDir+'/'+subject+'_TemporalFrequencyLimit_3Rings_'+timeAndDateStr
if not demo and not exportImages:
    dataFile = open(fileName+'.txt', 'w')  # sys.stdout  #StringIO.StringIO() 
    saveCodeCmd = 'cp \'' + sys.argv[0] + '\' '+ fileName + '.py'
    os.system(saveCodeCmd)  #save a copy of the code as it was when that subject was run
    logF = logging.LogFile(fileName+'.log', 
        filemode='w',#if you set this to 'a' it will append instead of overwriting
        level=logging.INFO)#errors, data and warnings will be sent to this logfile
if demo or exportImages: 
  dataFile = sys.stdout
  logging.console.setLevel(logging.ERROR)  #only show this level  messages and higher
logging.console.setLevel(logging.WARNING) #DEBUG means set the console to receive nearly all messges, INFO is for everything else, INFO, EXP, DATA, WARNING and ERROR 

#Trial parameter setting...............
numRings=3  #how many display rings
RANum=8 #reversal times record. Recording reversal times of each ring
radii=[4,8,12] # used in paper: [4,10,26]
respRadius=radii[0] #deg
hz=60 *1.0;  #set to the framerate of the monitor
trialDur =1 #3 4.8;
if demo:trialDur = 5;hz = 60.; 
tokenChosen=[-999,-999,-999,-999]  #mouse click use[WingAdd]
rampUpDur=.3; rampDownDur=.7; trackingExtraTime=.7; #giving the person time to attend to the cue (secs)
toTrackCueDur = rampUpDur+rampDownDur+trackingExtraTime
trialDurFrames=int(trialDur*hz)+int( trackingExtraTime*hz )
rampUpFrames = hz*rampUpDur;   rampDownFrames = hz*rampDownDur;  ShowTrackCueFrames = int( hz*toTrackCueDur )
rampDownStart = trialDurFrames-rampDownFrames
ballStdDev = 1.8
mouseChoiceArea = ballStdDev*0.6 # origin =1.3
mouseChoiceAreaCue = ballStdDev*0.8
units='deg' #'cm'
if showRefreshMisses:fixSize = 2.6  #make fixation bigger so flicker more conspicuous
else: fixSize = 0.3
timeTillReversalMin = 0.25; timeTillReversalMax = 2.95  #reversal time 
offsetXYeachRing=[[0,0],[0,0],[0,0],[0,0]]
jumpFrame=9;
patchAngleBase = 20#20
gratingTexPix=1024#1024 #numpy textures must be a power of 2. So, if numColorsRoundTheRing not divide without remainder into textPix, there will be some rounding so patches will not all be same size

#start definition of colors 
hues=arange(16)/16.
saturatns = array( [1]*16 )
vals= array( [1]*16 )
colors=list()
for i in range(16):
  if i==5 or i==6 or i==7 or i==8 or i==10 or i==11 :continue
  else: colors.append( colorsys.hsv_to_rgb(hues[i],saturatns[i],vals[i]) ) #actually colorsys is really lame!
colors = array(colors)  

maxlum = array([30.,80.,20.]); #I dunno if these are correct, they were for an old monitor of mine
gamma=array([2.2,2.2,2.2]);
totLums = array(   [0.0]*len(colors) )  #calculate luminance of each color
lums = zeros((len(colors),3))
for c in range( len(colors) ):
    for guni in range(3):
        lums[c][guni] = maxlum[guni] *  pow( colors[c][guni], gamma[guni] )  
        totLums[c] +=  lums[c][guni] 
minLum = min(totLums)  #apparently this is the worst-case lum for the hues we started with
colorsRescaled = colors
for i in range( len(colors) ):
  scaleFactor = minLum / totLums[i]
  desiredLums = lums[i,:] *scaleFactor #this might be fine for when one gun is 0, it preserves pairwise ratios
  for guni in range(3):
    colorsRescaled[i,guni] = pow(  desiredLums[guni]/maxlum[guni] , 1./gamma[guni] )
colorNames=['red','yellow','green','cyan','blue','fuchsia']
colors_all2 = colorsRescaled
colors_all = colors_all2  *2-1
total_colors = 6; #6 #universe of colors that nb_colors color set for this display is drawn from
nb_colors = 3; #3 #number unique colors in display. Works except answer and options doesn't take it into account. Assumes this value is 3

#definition of monitor
fullscr=0; scrn=0
widthPix =1024#1024  #monitor width in pixels
heightPix =768#768  #monitor height in pixels
monitorwidth = 38.5 #28.5 #monitor width in centimeters
viewdist = 57.; #cm
pixelperdegree = widthPix/ (atan(monitorwidth/viewdist) /pi*180)
bgColor = [-1,-1,-1] #black background
monitorname = 'mitsubishi' #in psychopy Monitors Center
if exportImages:
    fullscr=0; scrn=0
    widthPix = 600; heightPix = 450
    monitorwidth = 25.0
if demo:    
    scrn=0; fullscr=0
    widthPix = 800; heightPix = 600
    monitorname='testMonitor'
    allowGUI = True
    monitorwidth = 23#18.0

mon = monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)#fetch the most recent calib for this monitor
mon.setSizePix( (widthPix,heightPix) )
myWin = visual.Window(monitor=mon,size=(widthPix,heightPix),allowGUI=allowGUI,units=units,color=bgColor,colorSpace='rgb',fullscr=fullscr,screen=scrn,waitBlanking=waitBlank) #Holcombe lab monitor
myWin.setRecordFrameIntervals(False)
myMouse = event.Mouse(visible = 'true',win=myWin)
gaussian = visual.PatchStim(myWin, tex='none',mask='gauss',colorSpace='rgb',size=ballStdDev,autoLog=autoLogging)
gaussian2 = visual.PatchStim(myWin, tex='none',mask='gauss',colorSpace='rgb',size=ballStdDev,autoLog=autoLogging)
circle = visual.PatchStim(myWin, tex='none',mask='circle',size=mouseChoiceArea,colorSpace='rgb',color = (1,0,1),autoLog=autoLogging) #to outline chosen options
circleCue = visual.PatchStim(myWin, tex='none',mask='circle',size=mouseChoiceAreaCue,colorSpace='rgb',color = (1,1,1),autoLog=autoLogging)
blindspotFill = 0 #a way for people to know if they move their eyes
if blindspotFill:
    blindspotStim = visual.PatchStim(myWin, tex='none',mask='circle',size=4.8,colorSpace='rgb',color = (-1,1,-1),autoLog=autoLogging) #to outline chosen options
    blindspotStim.setPos([13.1,-2.7]) #AOH, size=4.8; pos=[13.1,-2.7] #DL: [13.3,-0.8]
fixation = visual.PatchStim(myWin,tex='none',colorSpace='rgb',color=(1,1,1),mask='circle',size=fixSize,autoLog=autoLogging)
fixationBlank= visual.PatchStim(myWin,tex='none',colorSpace='rgb',color=(-1,-1,-1),mask='circle',size=fixSize,autoLog=autoLogging)   
respText = visual.TextStim(myWin,pos=(0, -.8),colorSpace='rgb',color = (1,1,1),alignHoriz='center', alignVert='center', units='norm',autoLog=autoLogging)
NextText = visual.TextStim(myWin,pos=(0, 0),colorSpace='rgb',color = (1,1,1),alignHoriz='center', alignVert='center', units='norm',autoLog=autoLogging)
NextRemindText = visual.TextStim(myWin,pos=(.3, -.4),colorSpace='rgb',color = (1,1,1),alignHoriz='center', alignVert='center', units='norm',autoLog=autoLogging)
NextRemindCountText = visual.TextStim(myWin,pos=(-.1, -.4),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center', units='norm',autoLog=autoLogging)

#check screen refresh is what assuming it is, hz ########################################################################
Hzs=list()
myWin.setRecordFrameIntervals(True) #otherwise myWin.fps won't work
myWin.update(); myWin.update();myWin.update();myWin.update();
for i in range(50):
	myWin.update()
	Hzs.append( myWin.fps() )  #varies wildly on successive runs!
Hzs = array( Hzs );     Hz= median(Hzs)
msPerFrame= 1000./Hz
print('psychopy is reporting that frames per second are around = ', round(Hz,3),' and your drawing calculations are being done using the figure of ',hz, end=' ') 
NY='y'
if abs( (median(Hzs)-hz) / hz) > .05: 
	print(' which is off by more than 5%') 
	NY ='Y' # raw_input("Do you wish to continue nonetheless? ")
if NY.upper()<>'Y':
    core.quit()
    myWin.close()
# end testing of screen refresh########################################################################
if (not demo) and (myWin.size != [widthPix,heightPix]).any():
    myDlg = gui.Dlg(title="RSVP experiment", pos=(200,400))
    msgWrongResolution = 'Screen NOT the desired resolution of '+ str(widthPix)+'x'+str(heightPix)+ ' pixels!!'
    myDlg.addText(msgWrongResolution, color='Red')
    logging.error(msgWrongResolution)
    print(msgWrongResolution)
    myDlg.addText('Note: during the experiment, press ESC at response screen to quit', color='DimGrey')
    myDlg.show()
    core.quit()


# mask setting..................
maskOrbitSize = 2*(radii[1] +ballStdDev)#perhaps draw a big rectangle with a custom transparency layer (mask) in the shape of a thick ring
maskSz=512
myMask =ones([maskSz,maskSz]) #let everything through
blockingWidth = int( (ballStdDev+radii[0])/maskOrbitSize * maskSz )
#show the center so can see fixation point
centralShowDiam = round( fixSize/maskOrbitSize * maskSz )
for w in range(  int(maskSz/2-centralShowDiam), int(maskSz/2+centralShowDiam)  ):
    for h in range(  int(maskSz/2-centralShowDiam), int(maskSz/2+centralShowDiam)  ):
        myMask[h,w] = -1 #show (don't block)
#show a horizontal strip
blockingHeight = int(              (ballStdDev/2)/maskOrbitSize *maskSz       )
stripTop = int( maskSz /2. - blockingHeight/2 )
stripBtm = stripTop + blockingHeight
horizCoords = range( 0, blockingWidth )  #left side of rings
horizCoords = horizCoords + range(maskSz-1, maskSz-blockingWidth-1, -1) #right side of rings (to make it easier to fixate on center)
for w in horizCoords:
    for h in range( stripTop, stripBtm, 1):
        myMask[h,w] = -1 #show (don't block)
maskOrbit = visual.PatchStim(myWin,tex='none',colorSpace='rgb',color=(1,1,0),mask=myMask,size=maskOrbitSize,autoLog=autoLogging)  #depth=  , negative depth is closer
#end slit computation

stimList = []
# temporalfrequency limit test
numObjsInRing = [9]
speedsEachNumObjs = [ [0.01,.02],     #these correspond to the speeds to use for each entry of numObjsInRing
                                         [0.01,.02], 
                                         [0.01,.02]        ]
numTargets = np.array([1,2])  # np.array([1,2,3])
leastCommonMultipleSubsets = calcCondsPerNumTargets(numRings,numTargets)
leastCommonMultipleTargetNums = LCM( numTargets )  #have to use this to choose whichToQuery. For explanation see newTrajectoryEventuallyForIdentityTracking.oo3
print('leastCommonMultipleSubsets=',leastCommonMultipleSubsets)
                
for numObjs in numObjsInRing:
    idx = numObjsInRing.index(numObjs)
    speeds= speedsEachNumObjs[  idx   ]
    for speed in speeds:
        ringNums = np.arange(numRings)
        for nt in numTargets: #  3 choose 2, 3 choose 1, have to have as many conditions as the maximum
          subsetsThis = list(itertools.combinations(ringNums,nt)) #all subsets of length nt from the universe of ringNums
          numSubsetsThis = len( subsetsThis );   print('numSubsetsThis=',numSubsetsThis)
          repsNeeded = leastCommonMultipleSubsets / numSubsetsThis #that's the number of repetitions needed to make up for number of subsets of rings
          for r in xrange(repsNeeded):  #for nt with largest number of subsets, need no repetitions
                  for s in subsetsThis:
                      whichIsTarget = np.ones(numRings)*-999 #-999 is  value meaning no target in that ring. 1 will mean target in ring
                      for ring in s:
                         whichIsTarget[ring] = np.random.random_integers(0, numObjs-1, size=1) #1
                      print('numTargets=',nt,' whichIsTarget=',whichIsTarget,' and that is one of ',numSubsetsThis,' possibilities and we are doing ',repsNeeded,'repetitions')
                      for whichToQuery in xrange( leastCommonMultipleTargetNums ):  #for each subset, have to query one. This is dealed out to  the current subset by using modulus. It's assumed that this will result in equal total number of queried rings
                              whichSubsetEntry = whichToQuery % nt  #e.g. if nt=2 and whichToQuery can be 0,1,or2 then modulus result is 0,1,0. This implies that whichToQuery won't be totally counterbalanced with which subset, which is bad because
                                              #might give more resources to one that's queried more often. Therefore for whichToQuery need to use least common multiple.
                              ringToQuery = s[whichSubsetEntry];  print('ringToQuery=',ringToQuery,'subset=',s)
                              for relPhaseOuterRing in np.array([0]):
                                 for direction in [1.0]:  #WING why is this constant?
                                    relPhaseOuterRing = relPhaseOuterRing*(2*pi)/2
                                    relPhaseOuterRingPositiveInDirectionOfMotion = relPhaseOuterRing*direction
                                    stimList.append( {'numObjectsInRing':numObjs,'speed':speed, 'direction':direction,'slitView':slitView,'numTargets':nt,'whichIsTarget':whichIsTarget,\
                                                                 'ringToQuery':ringToQuery,'relPhaseOuterRing':relPhaseOuterRingPositiveInDirectionOfMotion} )

#set up record of proportion correct in various conditions
trialSpeeds = list() #purely to allow report at end of how many trials got right at each speed
for s in stimList: trialSpeeds.append( s['speed'] )
allSpeeds = set(trialSpeeds)  #reduce speedsUsed list to unique members
allSpeeds = np.array( list(allSpeeds) ) #to turn a set into an array, have to first cast it as a list
numRightWrongEachSpeedOrder = np.zeros([ len(allSpeeds), 2 ]); #summary results to print out at end
numRightWrongEachSpeedIdent = deepcopy(numRightWrongEachSpeedOrder)
#end setup of record of proportion correct in various conditions

blockReps=1       #14
trials = data.TrialHandler(stimList,blockReps) #constant stimuli method

try:
    runInfo = psychopy.info.RunTimeInfo(
            win=myWin,    ## a psychopy.visual.Window() instance; None = default temp window used; False = no win, no win.flips()
            refreshTest='grating', ## None, True, or 'grating' (eye-candy to avoid a blank screen)
            verbose=True, ## True means report on everything 
            userProcsDetailed=True,  ## if verbose and userProcsDetailed, return (command, process-ID) of the user's processes
            )
    logging.info(runInfo)
except Exception,e: print(str(e)) #print error

timeAndDateStr = time.strftime("%d%b%Y_%H-%M", time.localtime()) 
logging.info(  str('starting exp with name: "'+'TemporalFrequencyLimit'+'" at '+timeAndDateStr)   )
logF = StringIO.StringIO()  #kludge so I dont have to change all the print >>logF statements
logging.info(    'numtrials='+ str(trials.nTotal)+' and each trialDur='+str(trialDur)+' hz='+str(hz)      )

print(' numtrials=', trials.nTotal)
print('rampUpDur=',rampUpDur, ' rampDownDur=', rampDownDur, ' secs', file=logF);  logging.info( logF.getvalue() ); logF = StringIO.StringIO() 
logging.info('task='+'track'+'   respType='+respType)
logging.info( 'colors_all='+str(colors_all)+ '\ncolorNames='+str(colorNames)+ '  trackPostcueOrClick='+str(trackPostcueOrClick)+'  trackAllIdenticalColors='+str(trackAllIdenticalColors) )
logging.info(   'radii=' + str(radii)   )
logging.info(   'drawingAsGrating='+str(drawingAsGrating) +  ' gratingTexPix='+ str(gratingTexPix) + ' antialiasGrating=' + str(antialiasGrating)   )
logging.flush()

def  oneFrameOfStim(n,angleMovement,blobToCueEachRing,reversalValue,reversalNo,ShowTrackCueFrames): 
#defining a function to draw each frame of stim. So can call second time for tracking task response phase
          global cueRing,ringRadial,ringRadialR, currentlyCuedBlob #makes python treat it as a local variable
          global angleIni, correctAnswers

          if n<rampUpFrames:
                contrast = cos( -pi+ pi* n/rampUpFrames  ) /2. +.5 #starting from -pi trough of cos, and scale into 0->1 range
          elif n> rampDownStart:
                contrast = cos(pi* (n-rampDownStart)/rampDownFrames ) /2.+.5 #starting from peak of cos, and scale into 0->1 range
          else: contrast = 1
          contrast = 1
          fixation.draw()
          if n%2>=1: fixation.draw()#flicker fixation on and off at framerate to see when skip frame
          else:fixationBlank.draw()         
    
          if drawingAsGrating:
                ringRadialLocal = ringRadial
                centerInMiddleOfSegment =360./numObjects/2.0  #if don't add this factor, won't center segment on angle and so won't match up with blobs of response screen
                for noRing in range(numRings):
                        anglemove=moveDirection[noRing]*thisTrial['direction']*thisTrial['speed']*360*1.0*(n-(n-1))/hz
                        if Reversal and reversalNo[noRing] <= len(RAI[noRing]) and n>RAI[noRing][int(reversalNo[noRing])]*hz:
                                        reversalValue[noRing]=-1*reversalValue[noRing]
                                        reversalNo[noRing] +=1
                        angleMovement[noRing]=angleMovement[noRing]+anglemove*(reversalValue[noRing])
                        ringRadialLocal[noRing].setOri(angleIni[noRing]+angleMovement[noRing]+centerInMiddleOfSegment) 
                        ringRadialLocal[noRing].setContrast( contrast )
                        ringRadialLocal[noRing].draw()
                        if  (blobToCueEachRing[noRing] != -999) and n< ShowTrackCueFrames:  #-999 means there's a target in that ring
                            #if blobToCue!=currentlyCuedBlob: #if blobToCue now is different from what was cued the first time the rings were constructed, have to make new rings
                                #even though this will result in skipping frames
                                cueRing[noRing].setOri(angleIni[noRing]+angleMovement[noRing]+centerInMiddleOfSegment)
                                cueRing[noRing].setOpacity( 1- n*1.0/ShowTrackCueFrames ) #gradually make it transparent
                                cueRing[noRing].draw()
                        #draw tracking cue on top with separate object? Because might take longer than frame to draw the entire texture
                        #so create a second grating which is all transparent except for one white sector. Then, rotate sector to be on top of target
          else:
                for noRing in range(numRings):
                    for nobject in range(numObjects):
                        anglePair=(2*pi/numObjects)*nobject
                        nColor =nobject % nb_colors 
                        anglemove=moveDirection[noRing]*thisTrial['direction']*thisTrial['speed']*2*pi*(n-(n-1))/hz
                        if nobject==0:
                            if Reversal and reversalNo[noRing] <= len(RAI[noRing]) and n>RAI[noRing][int(reversalNo[noRing])]*hz:
                                    reversalValue[noRing]=-1*reversalValue[noRing]
                                    reversalNo[noRing] +=1
                            angleMovement[noRing]=angleMovement[noRing]+anglemove*(reversalValue[noRing])
                        x=offsetXYeachRing[noRing][0]+radii[noRing]*cos(angleIni[noRing]+anglePair+angleMovement[noRing]); 
                        y=offsetXYeachRing[noRing][1]+radii[noRing]*sin(angleIni[noRing]+anglePair+angleMovement[noRing])
                        if   n< ShowTrackCueFrames and nobject==blobToCueEachRing[noRing]: #cue in white  
                            weightToTrueColor = n*1.0/ShowTrackCueFrames #compute weighted average to ramp from white to correct color
                            blobColor = (1-weightToTrueColor)*array([1,1,1])  +  weightToTrueColor*colorsInInnerRingOrder[nColor] 
                            blobColor = blobColor*contrast #also might want to change contrast, if everybody's contrast changing in contrast ramp
                        else: blobColor = colorsInInnerRingOrder[nColor]*contrast    
                        gaussian.setColor( blobColor, log=autoLogging )
                        gaussian.setPos([x,y])
                        gaussian.draw()
          if blindspotFill:
              blindspotStim.draw()
          if thisTrial['slitView']: maskOrbit.draw()
          lastlocation=array([angleIni,angleMovement,reversalValue,reversalNo]) #fix counterclockwise displacement problems [WingAdd]
          return lastlocation   
# #######End of function definition that displays the stimuli!!!! #####################################

def constructRingAsGrating(numObjects,patchAngle,colors,stimColorIdxsOrder,gratingTexPix,blobToCue):
    myTex=list();cueTex=list();ringRadial=list();cueRing=list()
    stimColorIdxsOrder= stimColorIdxsOrder[::-1]  #reverse order of indices, because grating texture is rendered in reverse order than is blobs version
    ringRadialMask=[[0,0,0,1,1,] ,[0,0,0,0,0,0,1,1,],[0,0,0,0,0,0,0,0,0,0,1,1,]]
    numUniquePatches= max( len(stimColorIdxsOrder[0]),len(stimColorIdxsOrder[1]),len(stimColorIdxsOrder[2]))
    numCycles =double(numObjects) / numUniquePatches
    angleSegment = 360./(numUniquePatches*numCycles)
    if gratingTexPix % numUniquePatches >0: #gratingTexPix contains numUniquePatches. numCycles will control how many total objects there are around circle
        print('Warning: could not exactly render a ',numUniquePatches,'-segment pattern radially, will be off by ', (gratingTexPix%numUniquePatches)*1.0 /gratingTexPix, file=logF)
    if numObjects % numUniquePatches >0:
        msg= 'Warning: numUniquePatches ('+str(numUniquePatches)+') not go evenly into numObjects'; print(msg, file=logF); logging.warn(msg)
    #create texture for red-green-blue-red-green-blue etc. radial grating
    for i in range(numRings):
        #myTex.append(np.zeros([gratingTexPix,gratingTexPix,3])+[1,-1,1])
        myTex.append(np.zeros([gratingTexPix,gratingTexPix,3])+bgColor[0])#start with all channels in all locs = bgColor
        cueTex.append(np.ones([gratingTexPix,gratingTexPix,3])*bgColor[0])
    if patchAngle > angleSegment:
        msg='Error: patchAngle requested ('+str(patchAngle)+') bigger than maximum possible ('+str(angleSegment)+') numUniquePatches='+str(numUniquePatches)+' numCycles='+str(numCycles); 
        print(msg); print(msg, file=logF); logging.error(msg)
  
    oneCycleAngle = 360./numCycles
    segmentSizeTexture = angleSegment/oneCycleAngle *gratingTexPix #I call it segment because includes spaces in between, that I'll write over subsequently
    patchSizeTexture = patchAngle/oneCycleAngle *gratingTexPix
    patchSizeTexture = round(patchSizeTexture) #best is odd number, even space on either size
    patchFlankSize = (segmentSizeTexture-patchSizeTexture)/2.
    patchAngleActual = patchSizeTexture / gratingTexPix * oneCycleAngle
    if abs(patchAngleActual - patchAngle) > .04:
        msg = 'Desired patchAngle = '+str(patchAngle)+' but closest can get with '+str(gratingTexPix)+' gratingTexPix is '+str(patchAngleActual); 
        print(msg, file=logF);   logging.warn(msg)
    
    for colrI in range(numUniquePatches): #for that portion of texture, set color
        start = colrI*segmentSizeTexture
        end = start + segmentSizeTexture
        start = round(start) #don't round until after do addition, otherwise can fall short
        end = round(end)
        nColor = colrI #mimics code in drawoneframe
        ringColr=list();
        for i in range(numRings):ringColr.append(colors[ stimColorIdxsOrder[i][nColor] ])
        for colorChannel in range(3):
            for i in range(numRings):myTex[i][:, start:end, colorChannel] = ringColr[i][colorChannel]; 
            for cycle in range(int(round(numCycles))):
              base = cycle*gratingTexPix/numCycles
              for i in range(numRings):cueTex[i][:, base+start/numCycles:base+end/numCycles, colorChannel] = ringColr[1][colorChannel]
        #draw bgColor area (emptySizeEitherSideOfPatch) by overwriting first and last entries of segment 
        for i in range(numRings):
            myTex[i][:, start:start+patchFlankSize, :] = bgColor[0]; #one flank
            myTex[i][:, end-1-patchFlankSize:end, :] = bgColor[0]; #other flank
        
        for cycle in range(int(round(numCycles))): 
              base = cycle*gratingTexPix/numCycles
              for i in range(numRings):
                 cueTex[i][:,base+start/numCycles:base+(start+patchFlankSize)/numCycles,:] =bgColor[0]; 
                 cueTex[i][:,base+(end-1-patchFlankSize)/numCycles:base+end/numCycles,:] =bgColor[0]
        
    #color the segment to be cued white
    segmentLen = gratingTexPix/numCycles*1/numUniquePatches
    WhiteCueSizeAdj=0 # adust the white cue marker wingAdd 20110923
    if thisTrial['numObjectsInRing']==3:WhiteCueSizeAdj=110
    elif thisTrial['numObjectsInRing']==6:WhiteCueSizeAdj=25
    elif thisTrial['numObjectsInRing']==12:WhiteCueSizeAdj=-15
    elif thisTrial['numObjectsInRing']==2:WhiteCueSizeAdj=200
    
    for i in range(numRings):
            if blobToCue[i] >=0: #-999 means dont cue anything
                blobToCueCorrectForRingReversal = numObjects-1 - blobToCue[i] #grating seems to be laid out in opposite direction than blobs, this fixes postCueNumBlobsAway so positive is in direction of motion
                if blobToCueCorrectForRingReversal==0 and thisTrial['numObjectsInRing']==12:   WhiteCueSizeAdj=0
                cueStartEntry = blobToCueCorrectForRingReversal*segmentLen+WhiteCueSizeAdj
                cueEndEntry = cueStartEntry + segmentLen-2*WhiteCueSizeAdj
                cueTex[i][:, cueStartEntry:cueEndEntry, :] = -1*bgColor[0]    
                blackGrains = round( .25*(cueEndEntry-cueStartEntry) )#number of "pixels" of texture at either end of cue sector to make black. Need to update this to reflect patchAngle
                cueTex[i][:, cueStartEntry:cueStartEntry+blackGrains, :] = bgColor[0];  #this one doesn't seem to do anything?
                cueTex[i][:, cueEndEntry-1-blackGrains:cueEndEntry, :] = bgColor[0];
    angRes = 100 #100 is default. I have not seen any effect. This is currently not printed to log file!
    
    for i in range(numRings):
         ringRadial.append(visual.RadialStim(myWin, tex=myTex[i], color=[1,1,1],size=radii[i],#myTexInner is the actual colored pattern. radial grating used to make it an annulus 
         mask=ringRadialMask[i], # this is a 1-D mask dictating the behaviour from the centre of the stimulus to the surround.
         radialCycles=0, angularCycles=double(thisTrial['numObjectsInRing'])/numUniquePatches,
         angularRes=angRes, interpolate=antialiasGrating, autoLog=autoLogging))
         #the mask is radial and indicates that should show only .3-.4 as one moves radially, creating an annulus
    #end preparation of colored rings
    #draw cueing grating for tracking task. Have entire grating be empty except for one white sector
         cueRing.append(visual.RadialStim(myWin, tex=cueTex[i], color=[1,1,1],size=radii[i], #cueTexInner is white. Only one sector of it shown by mask
         mask=ringRadialMask[i], radialCycles=0, angularCycles=1, #only one cycle because no pattern actually repeats- trying to highlight only one sector
         angularRes=angRes, interpolate=antialiasGrating, autoLog=autoLogging) )#depth doesn't seem to work, just always makes it invisible?
    
    currentlyCuedBlob = blobToCue #this will mean that don't have to redraw 
    return ringRadial,cueRing,currentlyCuedBlob
    ######### End constructRingAsGrating ###########################################################

def  collectResponses(n,responses,responsesAutopilot,respRadius,expStop ): 
    respondedEachToken = zeros([numRings,numObjects])  #potentially two sets of responses, one for each of two concentric rings
    optionIdexs=list();baseSeq=list();numOptionsEachSet=list();numRespsNeeded=list()
    for i in range(numRings):
        optionIdexs.append([])
        noArray=list()
        for k in range(numObjects):noArray.append(colors_ind[0])
        baseSeq.append(array(noArray))
    for j in range(numRings):
        for i in range(numObjects):
            optionIdexs[j].append(baseSeq[j][i % len(baseSeq[j])] )
    
    numRespsNeeded = np.zeros(3) 
    
    for r in xrange(numRings):
        if r == thisTrial['ringToQuery']:
            numRespsNeeded[ r ] = 1
        else: numRespsNeeded[ r ] = 0
    for k in range(numRings):     numOptionsEachSet.append(len(optionIdexs[k]))
    optionSets=numRings;     respcount = 0;     tClicked = 0;       lastClickState=0;       mouse1=0
    for r in range(optionSets): 
            responses.append( list() )
            responsesAutopilot.append( [0]*numRespsNeeded[r] )  #autopilot response is 0
    passThisTrial = False; 
    baseAngle = lastStimAngle #needs to be in radians   #print 'baseAngle=lastStimAngle= ',baseAngle, ' in deg = ',baseAngle/(2*pi)*360.
    numRespSound=0
    while respcount < sum(numRespsNeeded): #collecting response
               for optionSet in range(optionSets):  #draw blobs available to click on
                  for ncheck in range( numOptionsEachSet[optionSet] ): 
                      if drawingAsGrating:
                        angle=baseAngle[0][optionSet]+baseAngle[1][optionSet]+centerInMiddleOfSegment 
                        opts=optionIdexs;
                        c = opts[optionSet][ncheck] #idx of color that this option num corresponds to. Need an extra [0] b/c list of arrays
                        if respondedEachToken[optionSet][ncheck]:  #draw circle around this one to indicate this option has been chosen
                               baseAngleAdj=baseAngle[0][optionSet]+baseAngle[1][optionSet]
                               baseAngleAdj *= -1;baseAngleAdj += 90
                               angle =  (baseAngleAdj)+ ncheck*1.0/numOptionsEachSet[optionSet] *360 #first ring [WingAdd]
                               stretchOutwardRingsFactor = 1
                               r=(radii[optionSet]/radii[0])*1.5+optionSet*(ballStdDev/3)
                               x = HdistAwayCent[optionSet]+r*cos(angle/180*pi);  y = VdistAwayCent[optionSet]+r*sin(angle/180*pi)
                               circle.setColor(array([1,-1,1]), log=autoLogging)
                               circle.setPos([x,y])
                               circle.draw()
                        ringRadial[optionSet].setOri(angle) 
                        ringRadial[optionSet].setContrast(1)
                        ringRadial[optionSet].draw()
                      else:
                        angle =  (baseAngle[0][optionSet]+baseAngle[1][optionSet]) + ncheck*1.0/numOptionsEachSet[optionSet] *2.*pi  #first ring [WingAdd]
                        stretchOutwardRingsFactor = 1
                        r = respRadius+ optionSet*(stretchOutwardRingsFactor*(radii[1]-radii[0]))
                        x = offsetXYeachRing[optionSet][0]+r*cos(angle);  y = offsetXYeachRing[optionSet][1]+r*sin(angle)
                        #draw colors, and circles around selected items. Colors are drawn in order they're in in optionsIdxs
                        opts=optionIdexs;
                        c = opts[optionSet][ncheck] #idx of color that this option num corresponds to. Need an extra [0] b/c list of arrays
                        if respondedEachToken[optionSet][ncheck]:  #draw circle around this one to indicate this option has been chosen
                               circle.setColor(array([1,-1,1]), log=autoLogging)
                               circle.setPos([x,y])
                               circle.draw()                
                        #gaussian.setRGB( colors_all[c] )  #draw blob
                        gaussian.setColor(  colors_all[0], log=autoLogging )  #draw blob
                        gaussian.setPos([x,y]);  
                        gaussian.draw()
                         
               mouse1, mouse2, mouse3 = myMouse.getPressed()
               if mouse1 and lastClickState==0:  #only count this event if is a new click. Problem is that mouse clicks continue to be pressed for along time
                    mouseX,mouseY = myMouse.getPos()
                    #print 'assumes window spans entire screen of ',monitorwidth,' cm; mouse position apparently in cm when units is set to deg = (',mouseX,',',mouseY,')'  
                    #because mouse apparently giving coordinates in cm, I need to convert it to degrees of visual angle because that's what drawing is done in terms of
                    cmperpixel = monitorwidth*1.0/widthPix
                    degpercm = 1.0/cmperpixel/pixelperdegree;  
                    dx = (mouseX) * degpercm #mouse x location relative to center, converted to degrees from pixels
                    monitorheight = cmperpixel*heightPix
                    dy = (mouseY) * degpercm #mouse x location relative to center, converted to degrees from pixels
                    for optionSet in range(optionSets):
                      for ncheck in range( numOptionsEachSet[optionSet] ): 
                            if drawingAsGrating:
                                baseAngleAdj=baseAngle[0][optionSet]+baseAngle[1][optionSet]
                                baseAngleAdj *= -1;baseAngleAdj += 90
                                angle =  (baseAngleAdj)+ ncheck*1.0/numOptionsEachSet[optionSet] *360 #first ring [WingAdd]
                                stretchOutwardRingsFactor = 1
                                r=(radii[optionSet]/radii[0])*1.5+optionSet*(ballStdDev/3)
                                x = HdistAwayCent[optionSet]+r*cos(angle/180*pi);  y = VdistAwayCent[optionSet]+r*sin(angle/180*pi)
                            else:   
                                angle =  (baseAngle[0][optionSet]+baseAngle[1][optionSet]) + ncheck*1.0/numOptionsEachSet[optionSet] *2.*pi #first quadrant [WingAdd]
                                r = respRadius+ optionSet*(radii[1]-radii[0]-0.2)-0.3
                                x = offsetXYeachRing[optionSet][0]+r*cos(angle);  y = offsetXYeachRing[optionSet][1]+r*sin(angle)                              
                            #check whether mouse click was close to any of the colors
                            #Colors were drawn in order they're in in optionsIdxs
                            distance = sqrt(pow((x-dx),2)+pow((y-dy),2))
                            #mouseToler = mouseChoiceArea/3.#deg visual angle?  origin=2
                            mouseToler = mouseChoiceArea/3+optionSet*mouseChoiceArea/6.#deg visual angle?  origin=2
                            if distance<mouseToler:
                                c = opts[optionSet][ncheck] #idx of color that this option num corresponds to
                                if respondedEachToken[optionSet][ncheck]:  #clicked one that already clicked on
                                    if lastClickState ==0: #only count this event if is a distinct click from the one that selected the blob!
                                        respondedEachToken[optionSet][ncheck] =0
                                        responses[optionSet].remove(c) #this redundant list also of course encodes the order
                                        
                                        respcount -= 1
                                        #print 'removed number ',ncheck,' color ',colorNames[c], ' from clicked list'
                                else:         #clicked on new one, need to add to response    
                                    numRespsAlready = len(  where(respondedEachToken[optionSet])[0]  )
                                    #print('numRespsAlready=',numRespsAlready,' numRespsNeeded= ',numRespsNeeded,'  responses=',responses)   #debugOFF
                                    if numRespsAlready >= numRespsNeeded[optionSet]:
                                        pass #not allowed to select this one until de-select other
                                    else:
                                        respondedEachToken[optionSet][ncheck] = 1 #register this one has been clicked
                                        responses[optionSet].append(c) #this redundant list also of course encodes the order
                                        respcount += 1  
                         
                                        #print 'added  ',ncheck,'th response, which is color ',colorNames[c], ' or color index ',c,' to clicked list'
                        #print 'response=', response, '  respcount=',respcount, ' lastClickState=',lastClickState, '  after affected by click'
                   #end if mouse clicked
                   
               for key in event.getKeys():       #check if pressed abort-type key
                      if key in ['escape','q']:
                          expStop = True
                          respcount = nb_colors
                      
               lastClickState = mouse1
               if autopilot: 
                    respcount = nb_colors
                    for i in xrange(numRings):
                        for j in xrange(numObjects):
                            respondedEachToken[i][j] = 1 #must set to True for tracking task with click responses, because it uses to determine which one was clicked on
               if blindspotFill:
                    blindspotStim.draw()
               #respText.draw()
               if numRespSound<2:
                    respSound.setVolume(1)
                    respSound.play()
                    numRespSound +=1
               myWin.flip(clearBuffer=True)  
               if screenshot and ~screenshotDone:
                   myWin.getMovieFrame()       
                   screenshotDone = True
                   myWin.saveMovieFrames('respScreen.jpg')
               #end response collection loop for non-'track' task
    #if [] in responses: responses.remove([]) #this is for case of tracking with click response, when only want one response but draw both rings. One of responses to optionset will then be blank. Need to get rid of it
    return responses,responsesAutopilot,respondedEachToken, expStop
    ####### #End of function definition that collects responses!!!! #################################################
    
#print header for data file
print('trialnum\tsubject\tnumObjects\tspeed\tdirection', end='\t', file=dataFile)
print('orderCorrect\ttrialDurTotal\tnumTargets', end= '\t', file=dataFile) 
for i in range(numRings):
    print('whichIsTarget',i,     sep='', end='\t', file=dataFile)
print('ringToQuery',end='\t',file=dataFile)
for i in range(numRings):dataFile.write('Direction'+str(i)+'\t')
for i in range(numRings):dataFile.write('respAdj'+str(i)+'\t')
for i in range(numRings):
    for j in range(RANum):dataFile.write('RAV'+str(i)+'_'+str(j)+'\t')  #reversal times for each ring
print('timingBlips', file=dataFile)
#end of header
trialClock = core.Clock()
nDone=0; numTrialsOrderCorrect=0; numAllCorrectlyIdentified=0; blueMistakes=0; expStop=False; framesSaved=0;
thisTrial = trials.next()
trialDurTotal=0;
ts = list();
while nDone <= trials.nTotal and expStop==False:
    angleIni=list();angleMovement=list();moveDirection=list();RAI=list();reversalValue=list();reversalNo=list();colors_ind=list();colorRings=list();preDrawStimToGreasePipeline = list()
    trackingVariableInterval=random.uniform(0,.8) #random interval extra for tracking so cant predict final position
    trialDurFrames= int( (trialDur+trackingExtraTime+trackingVariableInterval)*hz ) #variable the tracking time on each trial.
    trialDurTotal=trialDur+trackingExtraTime+trackingVariableInterval;
    xyTargets = np.zeros( [thisTrial['numTargets'], 2] ) #need this for eventual case where targets can change what ring they are in
    numDistracters = numRings*thisTrial['numObjectsInRing'] - thisTrial['numTargets']
    xyDistracters = np.zeros( [numDistracters, 2] )
    for Ini in range(numRings): # initial the parameter...
         #angleIni.append(random.uniform(0,2*pi));
         angleIni.append(random.uniform(0,360));
         angleMovement.append(0);
         moveDirection.append(-999);
         RAI.append(list());
         reversalValue.append(1);
         reversalNo.append(0);
         if Ini==0:
            colors_ind.append([0,0,0,0])
            #colors_ind.append(np.random.permutation(total_colors)[0:nb_colors]) #random subset of colors in random order
            colorRings.append(colors_all[colors_ind[Ini]])
         else: 
            '''colors_indnext= setdiff1d(xrange(total_colors), colors_ind[Ini-1])
            np.random.shuffle(colors_indnext)
            colors_ind.append(colors_indnext)'''
            colors_ind.append([0,0,0,0])
            colorRings.append(colors_all[colors_ind[Ini]])
    colorsInInnerRingOrder=colorsInOuterRingOrder=colors_all[[0, 0, 0]]
    '''for i in range(len(moveDirection)):# random direction
        moveDirection[i]=np.random.random_integers(1,2, size=1)
        if moveDirection[i]==2: moveDirection[i]=-1
        else:moveDirection[i]=1'''
    moveDirection=[1,-1,1] #WING what is this?
    stimColorIdxsOrder=[[0,0],[0,0],[0,0]]#this is used for drawing blobs during stimulus
    for u in range(numRings): # random to design reversal times
        thisReversalDur=trackingExtraTime
        countn=1
        while thisReversalDur<trialDurTotal:
            if countn==1:
                thisReversalDur=thisReversalDur+random.uniform(0.7,2)
            else:   thisReversalDur+=   random.uniform(timeTillReversalMin,timeTillReversalMax)
            RAI[u].append(thisReversalDur)
            countn=countn+1
 
    numObjects = thisTrial['numObjectsInRing']
    centerInMiddleOfSegment =360./numObjects/2.0
    blobsToPreCue=thisTrial['whichIsTarget']
    print('blobsToPreCue=',blobsToPreCue)  #debugON
    if drawingAsGrating:
        ringRadial,cueRing,currentlyCuedBlob = constructRingAsGrating(numObjects,patchAngleBase,colors_all,stimColorIdxsOrder,gratingTexPix,blobsToPreCue)
        preDrawStimToGreasePipeline.extend([ringRadial[0],ringRadial[1],ringRadial[2]])
    core.wait(.1)
    myMouse.setVisible(False)      
    fixatnPeriodFrames = int(   (np.random.rand(1)/2.+0.8)   *hz)  #random interval between 800ms and 1.3s (changed when Fahed ran outer ring ident)
    for i in range(fixatnPeriodFrames):
        if thisTrial['slitView']: maskOrbit.draw()  
        fixation.draw(); myWin.flip(clearBuffer=True)  
    trialClock.reset()
    for i in range(20):
        if thisTrial['slitView']: maskOrbit.draw()  
        fixation.draw(); myWin.flip(clearBuffer=True)  
    t0=trialClock.getTime(); t=trialClock.getTime()-t0     
    for L in range(len(ts)):ts.remove(ts[0]) # clear all ts array
    for n in range(trialDurFrames): #this is the loop for this trial's stimulus!
            lastStimAngle = oneFrameOfStim(n,angleMovement,blobsToPreCue,reversalValue,reversalNo,ShowTrackCueFrames) #da big function
            angleMovement=lastStimAngle[1]
            reversalValue=lastStimAngle[2]
            reversalNo=lastStimAngle[3]
            if exportImages:
                myWin.getMovieFrame(buffer='back') #for later saving
                framesSaved +=1              
            myWin.flip(clearBuffer=True)   
            t=trialClock.getTime()-t0; ts.append(t);
            if n==trialDurFrames-1: event.clearEvents(eventType='mouse');
    #end of big stimulus loop
    
    #check for timing problems
    interframeIntervs = diff(ts)*1000 #difference in time between successive frames, in ms
    #print >>logF, 'trialnum=',nDone, '   interframe intervs were ',around(interframeIntervs,1)
    longerThanRefreshTolerance = 0.1
    longFrameLimit = round(1000./hz*(1.0+longerThanRefreshTolerance),2) # round(1000/hz*1.5,2) #
    idxsInterframeLong = where( interframeIntervs > longFrameLimit ) [0] #frames that exceeded longerThanRefreshTolerance of expected duration
    numCasesInterframeLong = len( idxsInterframeLong )
    if numCasesInterframeLong >0:
       longFramesStr =  'ERROR,'+str(numCasesInterframeLong)+' frames were longer than '+str(longFrameLimit)+' ms'
       if demo: 
         longFramesStr += 'not printing them all because in demo mode'
       else:
           longFramesStr += ' apparently screen refreshes skipped, interframe durs were:'+\
                    str( around(  interframeIntervs[idxsInterframeLong] ,1  ) )+ ' and was these frames: '+ str(idxsInterframeLong)
       if longFramesStr != None:
                print('trialnum=',nDone,'  ',longFramesStr, file=logF)
                if not demo:
                    flankingAlso=list()
                    for idx in idxsInterframeLong: #also print timing of one before and one after long frame
                        if idx-1>=0:  flankingAlso.append(idx-1)
                        else: flankingAlso.append(NaN)
                        flankingAlso.append(idx)
                        if idx+1<len(interframeIntervs):  flankingAlso.append(idx+1)
                        else: flankingAlso.append(NaN)
                    #print >>logF, 'flankers also='+str( around( interframeIntervs[flankingAlso], 1) )
            #end timing check
    myMouse.setVisible(True)
    #ansIter=(answer).reshape(1,-1)[0]; ln=len(ansIter) #in case it's two dimensions like in bindRadially
    #print 'answer=',answer,' or ', [colorNames[ int(ansIter[i]) ] for i in range( ln )], ' it is type ',type(answer), ' and shape ', np.shape(answer)  
    #shuffledAns = deepcopy(answer);  #just to use for options, to ensure they are in a different order
    #if numObjects == 2:
    #     shuffledAns = shuffledAns[0:2]  #kludge. Really this should be controlled by nb_colors but that would require fancy array indexing where I currently have 0,2,1 etc above
   # np.random.shuffle(shuffledAns)  
    #if len(np.shape(answer)) >1: #more than one dimension, because bindRadiallyTask
    #     np.random.shuffle(shuffledAns[:,0]) #unfortunately for bindRadially task, previous shuffling shuffled pairs, not individuals
    #print 'answer after shuffling=',shuffledAns 
    passThisTrial=False
    # draw instruction words
    ringQuerySoundFileNames = [ 'innerring.wav', 'middlering.wav', 'outerring.wav' ]
    soundDir = 'sounds'
    soundPathAndFile= os.path.join(soundDir, ringQuerySoundFileNames[ thisTrial['ringToQuery'] ])
    respSound = sound.Sound(soundPathAndFile, secs=.2)
    postCueNumBlobsAway=-999 #doesn't apply to click tracking and non-tracking task
     # ####### response set up answer
    responses = list();  responsesAutopilot = list()
    print ("Entering collectResponses") #debugON
    responses,responsesAutopilot,respondedEachToken,expStop = collectResponses(n,responses,responsesAutopilot,respRadius,expStop)  #collect responses!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#############################
    #print("responses=",responses,";respondedEachToken=",respondedEachToken,"expStop=",expStop) #debugOFF
    core.wait(.1)
    if exportImages:  #maybe catch one frame of response
        myWin.saveMovieFrames('exported/frame.png')    
        expStop=True        
    #Handle response, calculate whether correct, ########################################
    if autopilot:responses = responsesAutopilot
    if True: #not expStop: #if short on responses, too hard to write code to handle it so don't even try
        orderCorrect=0; numColorsCorrectlyIdentified=0; blueMistake=0;respAdj=list();sCorrect=list();targetCorrect=0;
        for l in range(numRings):
                    if responses[l] !=[]: 
                       tokenChosen[l]=where(respondedEachToken[l])  [0][0] 
                       respAdjs= thisTrial['direction']*moveDirection[l]*reversalValue[l]*(tokenChosen[l]-thisTrial['whichIsTarget'][l])
                       if respAdjs> numObjects/2. : respAdjs-= numObjects  #code in terms of closest way around. So if 9 objects and 8 ahead, code as -1
                       if respAdjs < -numObjects/2. : respAdjs += numObjects
                       respAdj.append(respAdjs)
                       if tokenChosen[l]==thisTrial['whichIsTarget'][l]: 
                          sCorrects=1
                          sCorrect.append(sCorrects);
                          targetCorrect+=sCorrects
                    else:
                       respAdj.append(-999)
                       sCorrect.append(0)
        if targetCorrect==1: orderCorrect = 3
        else: orderCorrect = 0
                 
        if respType=='order':  #: this used to work without last conditional
            numColorsCorrectlyIdentified=-1
        else: 
            numColorsCorrectlyIdentified = len(   intersect1d(response,answer)   )
            if numColorsCorrectlyIdentified < 3:
                if 4 in answer and not (3 in answer): #dark blue present
                    if 3 in response: #light blue in answer
                        blueMistake =1
                elif 3 in answer and not (4 in answer): #light blue present
                    if 4 in response: #dark blue in answer
                        blueMistake =1                
        #end if statement for if not expStop
    if passThisTrial:orderCorrect = -1    #indicate for data analysis that observer opted out of this trial, because think they moved their eyes
    
    print(nDone,'\t',subject,'\t',thisTrial['numObjectsInRing'],'\t',thisTrial['speed'],'\t',thisTrial['direction'], end=' ', file=dataFile)
    print('\t', orderCorrect,'\t',trialDurTotal,'\t',thisTrial['numTargets'],'\t', end=' ', file=dataFile) #override newline end
    for i in range(numRings):  print( thisTrial['whichIsTarget'][i], end='\t', file=dataFile  )
    print( thisTrial['ringToQuery'],end='\t',file=dataFile )
    for i in range(numRings):dataFile.write(str(round(moveDirection[i],4))+'\t') 
    for i in range(numRings):dataFile.write(str(round(respAdj[i],4))+'\t') 
    for k in range(numRings):
        for i in range(len(RAI[k])):dataFile.write(str(round(RAI[k][i],4))+'\t') 
        if int(len(RAI[k]))<int(RANum+1):
            for j in range(RANum-len(RAI[k])):dataFile.write('-999\t') 
    print(numCasesInterframeLong, file=dataFile)
    numTrialsOrderCorrect += (orderCorrect >0)  #so count -1 as 0
    numAllCorrectlyIdentified += (numColorsCorrectlyIdentified==3)
    speedIdx = np.where(allSpeeds==thisTrial['speed'])[0][0]  #extract index, where returns a list with first element array of the indexes
    numRightWrongEachSpeedOrder[ speedIdx, (orderCorrect >0) ] +=1  #if right, add to 1th column, otherwise add to 0th column count
    numRightWrongEachSpeedIdent[ speedIdx, (numColorsCorrectlyIdentified==3) ] +=1
    blueMistakes+=blueMistake
    dataFile.flush(); logF.flush(); 
    
    if feedback and not expStop:
        if orderCorrect==3  :correct=1
        else:correct=0
        if correct:
            highA = sound.Sound('G',octave=5, sampleRate=6000, secs=.8, bits=8)
            highA.setVolume(0.8)
            highA.play()
        else: #incorrect
            lowD = sound.Sound('E',octave=3, sampleRate=6000, secs=.8, bits=8)
            lowD.setVolume(0.8)
            lowD.play()
    nDone+=1    
    
    if nDone< trials.nTotal:
        nextTrial=True
        NextText.setText('Press "SPACE" to continue!')
        NextText.draw()
        if nDone%(    max(trials.nTotal/4,1) ) ==0:  #have to enforce at least 1, otherwise will modulus by 0 when #trials is less than 4
            NextRemindCountText.setText(round((double(nDone)/double(trials.nTotal)*100),2))
            NextRemindText.setText(' % have done...')
            NextRemindCountText.draw()
            NextRemindText.draw()
        myWin.flip(clearBuffer=True) 
        while nextTrial:
           if autopilot: break
           elif expStop == True:break
           for key in event.getKeys():       #check if pressed abort-type key
                 if key in ['space']:nextTrial=False
        myWin.clearBuffer()
        thisTrial = trials.next()
    if thisTrial['slitView']:  #need to give preview so sudden appearance doesn't evoke saccade
        maskOrbit.draw()  
    core.wait(.1); time.sleep(.1)
    #end trials loop  ###########################################################
if expStop == True:
    print('user aborted experiment on keypress with trials nDone=', nDone, file=logF)
    print('user aborted experiment on keypress with trials nDone=', nDone)
print('finishing at ',timeAndDateStr, file=logF)
print('%corr order report= ', round( numTrialsOrderCorrect*1.0/nDone*100., 2)  , '% of ',nDone,' trials', end=' ')
print('%corr each speed: ', end=' ')
print(np.around( numRightWrongEachSpeedOrder[:,1] / ( numRightWrongEachSpeedOrder[:,0] + numRightWrongEachSpeedOrder[:,1]), 2))
print('\t\t\t\tnum trials each speed =', numRightWrongEachSpeedOrder[:,0] + numRightWrongEachSpeedOrder[:,1])
logging.flush(); dataFile.close(); logF.close()
core.quit()