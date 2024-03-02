#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Measure simulated orientation JND using a staircase method
"""
#questplus helpers are incorporated into psychopy from https://github.com/hoechenberger/questplus
from questplus.psychometric_function import weibull 

from psychopy import core, visual, gui, data, event
from psychopy.tools.filetools import fromFile, toFile
import psychopy, time, os, pylab, scipy
import numpy as np

try: #This only works if the code executing is in this folder
    import staircaseAndNoiseHelpers
except Exception as e:
    print("An exception occurred in staircase_tester_.py:",str(e))
    print('Could not import staircaseAndNoiseHelpers.py (you need that file to be in the staircase subdirectory, which needs an __init__.py file in it too)')

autopilot = False; showStimuli = True
descendingPsychometricCurve = True
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

np.random.seed(seed=233423) #so that simulated observer is reproducible. For this value, the curvefit works
nTrials = 100
quest = False
threshTryingToEstimate = 0.79
# create the staircase handler
if quest:    #actually not necessarily better than staircase, see https://direct.mit.edu/neco/article/34/2/338/108533/Model-Based-or-Model-Free-Comparing-Adaptive
    # trying to find out the point where subject's response is 50 / 50
    # if wanted to do a 2AFC then the defaults for pThreshold and gamma
    # are good. As start value, we'll use 50% contrast, with SD = 20%
    staircase = data.QuestHandler(0.5, 0.2,
        pThreshold=threshTryingToEstimate, gamma=0.01,
        nTrials=nTrials, minVal=0, maxVal=1)
else:
    staircase = data.StairHandler(startVal=20.0,
        stepType='lin',
        stepSizes=[8, 4, 4, 2, 2, 1, 1],  # reduce step size every two reversals
        minVal=0, maxVal=90,
        nUp=1, nDown=3,  # will home in on the 79.4% threshold; threshTryingToEstimate
        nTrials=nTrials)

if showStimuli:# display instructions and wait
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
def calc_pCorrect(intensity, guessRate):
    if descendingPsychometricCurve:
        intensity = 100-intensity
    pCorr = weibull(intensity=intensity, threshold=30,
                        slope=.5, lower_asymptote=guessRate, lapse_rate=0.00,
                        scale='linear').item()
    return pCorr

# Use idealized participant to get correct/incorrect on an individual trial
def get_response(intensity,guessRate):
    pCorr = calc_pCorrect(intensity,guessRate)
    dice_roll = np.random.random()
    return int(dice_roll <= pCorr)

#main program loop
for thisIncrement in staircase:  # will step through the staircase
    
    #Psychopy staircase can only handle increasing psychometric function.
    #So we use outOfStaircase if descending psychometric function to flip it (100-x)
    thisIncrement = staircaseAndNoiseHelpers.outOfStaircase(thisIncrement, staircase, descendingPsychometricCurve)

    targetSide = round(np.random.random()) * 2 - 1  # +1 = right, -1 = left
    if showStimuli: # set location of stimuli
        foil.setPos([-5 * targetSide, 0])
        target.setPos([5 * targetSide, 0])  # in other location
    
        # set orientation of probe
        foil.setOri(expInfo['refOrientation'] + thisIncrement)

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
    thisResp = None
    if autopilot:
        correct = get_response(thisIncrement,guessRate)
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
    staircase.addResponse(thisResp)
    if saveDataInFile:
        dataFile.write('%i	%.3f	%i\n' % (targetSide, thisIncrement, thisResp))

# staircase has ended
if saveDataInFile:
    dataFile.close()
    staircase.saveAsPickle(output_file)  # special python data file to save all the info

# give some output to user
#print('printStaircase output:')
#staircaseAndNoiseHelpers.printStaircase(staircase, briefTrialUpdate=False, printInternalVal=True, alsoLog=False) #Is this what's showing the range error?

print('Staircase was trying to find the', str(100*threshTryingToEstimate), '% threshold')
print('Reversals occurred at:')
print(staircase.reversalIntensities)

#Print all the reversals after the first few.
numReversals = len(staircase.reversalIntensities)
numRevsToUse = max( 1, numReversals-2 )

meanOfFinalReversals = np.average(staircase.reversalIntensities[-numRevsToUse:])
print('Mean of final', numRevsToUse,'reversals = %.2f' % meanOfFinalReversals)

if autopilot:
    pCorrect = calc_pCorrect(meanOfFinalReversals,guessRate)
    print('Psychometric function pCorrect at meanOfFinalReversals= %.3f' % pCorrect)

#from list of trials, tally up each intensity and calculate proportions correct
intensityForCurveFitting = staircaseAndNoiseHelpers.outOfStaircase(staircase.intensities,staircase,descendingPsychometricCurve)
combinedInten, combinedResp, combinedN = \
     psychopy.data.functionFromStaircase(intensityForCurveFitting, staircase.data, bins='unique')

pCorr = np.array(combinedResp)
ns = np.array(combinedN)
print('pCorr=',pCorr)
#Calculate standard error of each percent correct observed, because the curvefitting asks for that (thus the curvefitting algorithm used is not really suitable for binomial data)
# which depends on the number of trials of course. For proportion data, it's sqrt(p*(1-p))/sqrt(n)
variances = pCorr*(1-pCorr)
#Problem with these variances is that if there's only one trial at an intensity, then the variance is calculated as zero.
#Which is an artifact of having only one trial, which is why calculating CIs of proportions is notorious.
#Deal with this by just imposing a floor and ceiling on the SEM, although there are sophisticated ways of doing it which I should do.
#Because really the max and min should depend on how many trials there are.
variances = variances.clip(.2*.8,.8*.2)  #truncate at reasonable values rather than letting extend to 0 and to 1
#print('variances=',variances)
sds = np.sqrt(variances)
stderrs = sds / np.sqrt(ns)
#print('stderrs=',stderrs)
try:
    fit = data.FitWeibull(combinedInten, combinedResp, expectedMin=guessRate, sems = stderrs)
    print('Fit a Weibull to the data=',fit)
    print('combinedInten=',combinedInten)
    print('combinedResp=',combinedResp)
    print('fit params=',fit.params)
    print('I have no idea why the fit often ends up with a negative slope even though the data are ascending')
    #Really should switch to a robust fit like logistic regression that is set up for binomial data.
    #print(dir(fit))
except Exception as e:
    print("An exception occurred when curvefitting with Psychopy method:",str(e))
    print("Fit failed.")
    fit = None

#Alternative way of curve fitting because Psychopy's algorithm often fails
try: #from https://stackoverflow.com/questions/56329180/fitting-a-logistic-curve-to-data?rq=3
    (locatn, slope), _ = scipy.optimize.curve_fit(logistic, combinedInten, combinedResp, method="trf")
    y_fit_Alex = logistic(combinedInten, locatn, slope)
except Exception as e:
    print("An exception occurred when curvefitting with my method:",str(e))
    print("Fit failed.")
    fit = None
    
staircaseAndNoiseHelpers.plotDataAndPsychometricCurve(staircase,fit,descendingPsychometricCurve,threshVal=0.79)
pylab.plot(combinedInten, y_fit_Alex, 'g-') #Alex fitted curve
pylab.show() #must call this to actually show plot

print('dir staircase=')
print(dir(staircase))

if showStimuli:
    win.close()
core.quit()
