#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Measure simulated orientation JND using a staircase method
"""
#https://www.psychopy.org/recipes/interleaveStaircases.html

#questplus helpers are incorporated into psychopy from https://github.com/hoechenberger/questplus
import questplus.psychometric_function #for weibull equation and inverse

from psychopy import core, visual, gui, data, event
from psychopy.tools.filetools import fromFile, toFile
import psychopy, time, os, pylab, scipy, copy
import numpy as np

try: #This only works if the code executing is in this folder
    import staircaseAndNoiseHelpers
except Exception as e:
    print("An exception occurred in staircase_tester_.py:",str(e))
    print('Could not import staircaseAndNoiseHelpers.py (you need that file to be in the staircase subdirectory, which needs an __init__.py file in it too)')

autopilot = True; showStimuli = False
descendingPsychometricCurve = False
saveDataInFile = False

expInfo = {'observer':'aohSim', 'refOrientation':0}
dateStr = time.strftime("%b_%d_%H%M", time.localtime())  # add the current time

results_folder = 'data_and_results_for_tests'

# make a text file to save data. Will also save as psydat pickle, which can be read in by JND_staircase_analysis.py
if saveDataInFile:
    fileName = expInfo['observer'] + dateStr + '.txt'
    output_file = os.path.join(results_folder, fileName)
    dataFile = open(output_file, 'w')
    dataFile.write('targetSide	oriIncrement	correct\n')

# create window and stimuli
globalClock = core.Clock()  # to keep track of time
trialClock = core.Clock()  # to keep track of time
if showStimuli:
    win = visual.Window([800, 600], allowGUI=False, monitor='testMonitor', units='deg',autoLog=False)
    foil = visual.GratingStim(win, sf=1, size=4, mask='gauss', ori=expInfo['refOrientation'])
    target = visual.GratingStim(win, sf=1,  size=4, mask='gauss', ori=expInfo['refOrientation'])
    fixation = visual.GratingStim(win, color='black', tex=None, mask='circle', size=0.2)
    message1 = visual.TextStim(win, pos=[0, + 3],
        text='Hit a key when ready.')
    message2 = visual.TextStim(win, pos=[0, -3],
        text="Then press left or right to identify the %.1fdegree probe." % expInfo['refOrientation'])


#---------------------
#create the staircases
#---------------------
#The experiment might have a lot of conditions
#We will distill these down to separate staircases only for the most important conditions
#stimList.append( {'basicShape':'circle', 'numObjectsInRing':8,'speed':1.5,'initialDirRing0':-1,
#                                'numTargets':2,'whichIsTargetEachRing':1,'ringToQuery':2} )
numObjsInRing = [4,8]
numTargets = [2,3]
speedsEachNumTargetsNumObjects =   [ [ [0.5,1.0,1.4,1.7], [0.5,1.0,1.4,1.7] ],     #For the first numTargets condition
                                     [ [0.2,0.5,0.7,1.0], [0.5,1.0,1.4,1.7] ]  ]  #For the second numTargets condition

conditionsListForStaircases =[]
for numObjs in numObjsInRing: #set up experiment design
    for nt in numTargets: #for each num targets condition,
        #determine the starting speed for the staircase
        numObjectsIdx = numObjsInRing.index(numObjs)
        numTargetsIdx = numTargets.index(nt)
        speeds = speedsEachNumTargetsNumObjects[  numTargetsIdx ][ numObjectsIdx ]
        startingSpeedThisStaircase = np.mean( np.array( speeds ) )
        conditionsListForStaircases.append({'numObjs':numObjs, 'numTargets':nt, 'startSpeed':startingSpeedThisStaircase})
        
                        #young people in Roudaia & Faubert: 2targets: 3.5 Hz, 3targets: 2.5 Hz
thresholdEachCondition = [ 3.5/numObjsInRing[0], 2.5/numObjsInRing[0],    #4 objects
                           3.5/numObjsInRing[1], 2.5/numObjsInRing[1] ]   #8 objects
maxTrialsEachStaircase = 50
quest = False
threshTryingToEstimate = 0.79

#create staircase for each main condition in a list, 'stairs'
staircases=[]
for conditionsThis in conditionsListForStaircases:
    #we need a COPY of the info for each staircase 
    #(or the changes here will be made to all the other staircases)
    #thisCond = copy.copy(thisCond)
    #now add any specific info for this staircase
    conditionsThis =  copy.copy(conditionsThis)
    
    # create the staircase handler
    if quest:  #actually not necessarily better than staircase, see https://direct.mit.edu/neco/article/34/2/338/108533/Model-Based-or-Model-Free-Comparing-Adaptive
        # trying to find out the point where subject's response is 50 / 50
        # if wanted to do a 2AFC then the defaults for pThreshold and gamma
        # are good. As start value, we'll use 50% contrast, with SD = 20%
        staircase = data.QuestHandler(0.5, 0.2,
            extraInfo = conditionsThis,
            pThreshold=threshTryingToEstimate, gamma=0.01,
            nTrials=maxTrialsEachStaircase, minVal=0, maxVal=1)
    else:
        staircase = data.StairHandler(startVal = conditionsThis['startSpeed'],
            extraInfo = conditionsThis,
            stepType='lin',
            stepSizes=[1, 1, 0.5, 0.2, 0.2, 0.1],  # change step size after each reversal
            minVal=0, maxVal=90,
            nUp=1, nDown=3,  # will home in on the 79.4% threshold; threshTryingToEstimate
            nTrials=maxTrialsEachStaircase)

    staircases.append(staircase)

np.random.seed(seed=233423) #so that simulated observer is reproducible. For this value, the curvefit works

if showStimuli:# display instructions and wait
    win=visual.Window([400,400])
    message1.draw()
    message2.draw()
    fixation.draw()
    win.flip()
    # check for a keypress
    event.waitKeys()

guessRate=0.5

def logistic(x, c, d):
    return 1 / (1. + np.exp(-c * (x - d)))
    
# create an idealized participant (weibull function)
def calc_pCorrect(intensity, guessRate, threshold):
    if descendingPsychometricCurve:
        intensity = 100-intensity
    pCorr = questplus.psychometric_function.weibull(intensity=intensity, threshold=threshold,
                        slope=2, lower_asymptote=guessRate, lapse_rate=0.00,
                        scale='linear').item()
    return pCorr

# Use idealized participant to get correct/incorrect on an individual trial
def get_response(intensity,guessRate,threshold):
    pCorr = calc_pCorrect(intensity,guessRate,threshold)
    dice_roll = np.random.random()
    return int(dice_roll <= pCorr)

#main program loop
trialnum = 0
maxTrials = maxTrialsEachStaircase * len(staircases)

while trialnum < maxTrials:
    #Psychopy staircase can only handle increasing psychometric function.
    #So we use outOfStaircase, in case descending psychometric function, flips it (100-x)
    whichStaircase = trialnum % len(staircases)
    staircaseThis = staircases[whichStaircase]
    
    intensityThisTrial = staircaseThis.next()
    intensityThisTrial = staircaseAndNoiseHelpers.outOfStaircase(intensityThisTrial, staircaseThis, descendingPsychometricCurve) 

    targetSide = round(np.random.random()) * 2 - 1  # +1 = right, -1 = left
    if showStimuli: # set location of stimuli
        foil.setPos([-5 * targetSide, 0])
        target.setPos([5 * targetSide, 0])  # in other location
    
        # set orientation of probe
        foil.setOri(expInfo['refOrientation'] + intensityThisTrial)

        # draw all stimuli
        foil.draw()
        target.draw()
        fixation.draw()
        win.flip()
    
        core.wait(0.5)  # wait 500ms (use a loop of x frames for more accurate timing)
    
        # blank screen
        fixation.draw()
        win.flip()

    # get response
    thresholdThis = thresholdEachCondition[whichStaircase]
    thisResp = None
    if autopilot:
        correct = get_response(intensityThisTrial,guessRate,thresholdThis)
        thisResp = correct
    else:
        while thisResp is None:
            allKeys = event.waitKeys()
            for thisKey in allKeys:
                if ((thisKey == 'left' and targetSide == -1) or
                    (thisKey == 'right' and targetSide == 1)):
                    thisResp = 1  # correct
                elif ((thisKey == 'right' and targetSide == -1) or
                    (thisKey == 'left' and targetSide == 1)):
                    thisResp = 0  # incorrect
                elif thisKey in ['q', 'escape']:
                    core.quit()  # abort experiment
            event.clearEvents('mouse')  # only really needed for pygame windows

    # add the data to the staircase so it can calculate the next level
    staircaseThis.addResponse(thisResp)
    if saveDataInFile:
        dataFile.write('%i	%.3f	%i\n' % (targetSide, intensityThisTrial, thisResp))
    trialnum = trialnum + 1
# End loop of trials

if saveDataInFile:
    dataFile.close()
    staircaseThis.saveAsPickle(output_file)  # special python data file to save all the info

#Output some results from the staircases
meanReversalsEachStaircase = np.zeros( len(staircases) )

for staircase in staircases:
    actualThreshold = thresholdEachCondition[ staircases.index(staircase) ]
    print('Staircase was trying to find the', str(100*threshTryingToEstimate), '% threshold, whose actual value for this condition is', actualThreshold)
    #Average all the reversals after the first few.
    numReversals = len(staircase.reversalIntensities)
    numRevsToUse = max( 1, numReversals-2 )
    meanOfFinalReversals = np.average(staircase.reversalIntensities[-numRevsToUse:])
    print('Mean of final', numRevsToUse,'reversals = %.2f' % meanOfFinalReversals)
    meanReversalsEachStaircase[ staircases.index(staircase) ] = meanOfFinalReversals

#from list of trials, tally up each intensity and calculate proportions correct. Even though this is a bad approach in a staircase condition
#  with different number of trials for each speed, that's the approach built into Psychopy
intensityForCurveFitting = staircaseAndNoiseHelpers.outOfStaircase(staircase.intensities,staircase,descendingPsychometricCurve)
combinedInten, combinedResp, combinedN = \
     psychopy.data.functionFromStaircase(intensityForCurveFitting, staircase.data, bins='unique')

#Alternative way of curve fitting because Psychopy's algorithm often fails
fit = None; fitFailed = False
try: #from https://stackoverflow.com/questions/56329180/fitting-a-logistic-curve-to-data?rq=3
    (locatn, slope), _ = scipy.optimize.curve_fit(logistic, combinedInten, combinedResp, method="trf")
    y_fit_Alex = logistic(combinedInten, locatn, slope)
except Exception as e:
    fitFailed = True
    print("An exception occurred when curvefitting with my method:",str(e))
    print("Fit failed.")

#plot staircases over time
pylab.subplot(111) #1 row, 1 column, which panel
pylab.title('circle = mean of final reversals; triangle = true threshold')
pylab.xlabel("staircase trial")
pylab.ylabel("speed (rps)")

for staircase in staircases:
    colors = 'grby'
    idx = staircases.index(staircase)
    pylab.plot(staircase.intensities, colors[idx]+'-')
    #plot mean of last reversals point
    lastTrial = len(staircase.intensities)
    pylab.plot( lastTrial, meanReversalsEachStaircase[ idx ], colors[idx]+'o' )
    #plot correct answer, to help visualize if staircase is converging on the right place
    pylab.plot( lastTrial+1, thresholdEachCondition[ idx ], colors[idx]+'<' )
    
#staircaseAndNoiseHelpers.plotDataAndPsychometricCurve(staircase,fit,descendingPsychometricCurve,threshVal=0.79)
#if not fitFailed:
#    pylab.plot(combinedInten, y_fit_Alex, 'g-') #Alex fitted curve

pylab.show() #must call this to actually show plot


if showStimuli:
    win.close()
core.quit()
