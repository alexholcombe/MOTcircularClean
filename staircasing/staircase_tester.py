#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Measure your JND in orientation using a staircase method
"""
#questplus helpers are incorporated into psychopy from https://github.com/hoechenberger/questplus
from questplus.psychometric_function import weibull 

from psychopy import core, visual, gui, data, event
from psychopy.tools.filetools import fromFile, toFile
import time
import numpy as np

autopilot = True; showStimuli = False

expInfo = {'observer':'aohSim', 'refOrientation':0}
dateStr = time.strftime("%b_%d_%H%M", time.localtime())  # add the current time

# make a text file to save data. Will also save as psydat pickle, which can be read in by JND_staircase_analysis.py
fileName = expInfo['observer'] + dateStr
dataFile = open(fileName + '.txt', 'w')
dataFile.write('targetSide	oriIncrement	correct\n')

# create window and stimuli
globalClock = core.Clock()  # to keep track of time
trialClock = core.Clock()  # to keep track of time
win = visual.Window([800, 600], allowGUI=False, monitor='testMonitor', units='deg',autoLog=False)
foil = visual.GratingStim(win, sf=1, size=4, mask='gauss', ori=expInfo['refOrientation'])
target = visual.GratingStim(win, sf=1,  size=4, mask='gauss', ori=expInfo['refOrientation'])
fixation = visual.GratingStim(win, color='black', tex=None, mask='circle', size=0.2)
message1 = visual.TextStim(win, pos=[0, + 3],
    text='Hit a key when ready.')
message2 = visual.TextStim(win, pos=[0, -3],
    text="Then press left or right to identify the %.1fdegree probe." % expInfo['refOrientation'])

nTrials = 50
quest = False
# create the staircase handler
if quest:    #actually not necessarily better than staircase, see https://direct.mit.edu/neco/article/34/2/338/108533/Model-Based-or-Model-Free-Comparing-Adaptive
    # trying to find out the point where subject's response is 50 / 50
    # if wanted to do a 2AFC then the defaults for pThreshold and gamma
    # are good. As start value, we'll use 50% contrast, with SD = 20%
    staircase = data.QuestHandler(0.5, 0.2,
        pThreshold=0.82, gamma=0.01,
        nTrials=nTrials, minVal=0, maxVal=1)
else:
    staircase = data.StairHandler(startVal=20.0,
        stepType='lin',
        stepSizes=[8, 4, 4, 2, 2, 1, 1],  # reduce step size every two reversals
        minVal=0, maxVal=90,
        nUp=1, nDown=3,  # will home in on the 79.4% threshold
        nTrials=nTrials)

# display instructions and wait
message1.draw()
message2.draw()
fixation.draw()
win.flip()
# check for a keypress
event.waitKeys()

# create an idealized participant (weibull function)
def calc_pCorrect(intensity):
    pCorr = weibull(intensity=intensity, threshold=45,
                        slope=6.35, lower_asymptote=0.5, lapse_rate=0.00,
                        scale='linear').item()
    return pCorr

# Use idealized participant to get correct/incorrect on an individual trial
def get_response(intensity):
    pCorr = calc_pCorrect(intensity)
    dice_roll = np.random.random()
    return int(dice_roll <= pCorr)
    
    
for thisIncrement in staircase:  # will step through the staircase
    # set location of stimuli
    targetSide = round(np.random.random()) * 2 - 1  # +1 = right, -1 = left
    foil.setPos([-5 * targetSide, 0])
    target.setPos([5 * targetSide, 0])  # in other location

    # set orientation of probe
    foil.setOri(expInfo['refOrientation'] + thisIncrement)

    if showStimuli: # draw all stimuli
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
        correct = get_response(thisIncrement)
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
    dataFile.write('%i	%.3f	%i\n' % (targetSide, thisIncrement, thisResp))

# staircase has ended
dataFile.close()
staircase.saveAsPickle(fileName)  # special python data file to save all the info

# give some output to user
print('reversals:')
print(staircase.reversalIntensities)
                    #10 trials to let start converging
numRevsToUse = int( (nTrials - 10) / 10 )
meanOfFinalReversals = np.average(staircase.reversalIntensities[-numRevsToUse:])
print('mean of final', numRevsToUse,'reversals = %.2f' % meanOfFinalReversals)

if autopilot:
    pCorrect = calc_pCorrect(meanOfFinalReversals)
    print('Psychometric function pCorrect at meanOfFinalReversals= %.3f' % pCorrect)
    
win.close()
core.quit()

# The contents of this file are in the public domain.
