import numpy as np
import psychopy
from psychopy import visual, logging
import itertools
from math import log, sqrt
from copy import deepcopy
from pandas import DataFrame
import pylab, os
from matplotlib.ticker import ScalarFormatter

def toStaircase(x,descendingPsycho):
    #Don't need to take log, staircase internals will do that
    if isinstance(x,list):
        x = np.array(x) #because can't do math with lists

    if descendingPsycho:
        y = 100 - np.array(x) #100 because assuming maximum value is 100. E.g. percentNoise is 0 to 100
    else:
        y = np.array(x)
    return y

def outOfStaircase(y,staircase,descendingPsycho):
    #To get inside staircase, it was (100-x)
    #and inside log was taken. So y = log(100-x)
    #So to get x out, it's
    #10**y = 100 - x
    #-x = 10**y - 100
    # x = 100 - 10**y
    if isinstance(y,list):
        y = np.array(y) #because can't do math with lists
    
    if staircase.stepType == 'log': #HOW DO I KNOW IT IS BASE 10? and why doesnt psychopy protect me from log values. I guess actual intensities not meant for user
        x = 10**np.array(y)
    else:
        x = y
        
    if descendingPsycho:
        x = 100-x

    return x
    
def printStaircase(s, descendingPsycho=False, briefTrialUpdate=False, printInternalVal = False, alsoLog=False):
    #if briefTrialUpdate, don't print everything, just the kind of stuff you like to know after each trial
    #needs logging as a global variable, otherwise will fail when alsoLog=True
    #add is what to add to intensities,
    #mult is what to multiply intensities by,  e.g .if descending psychometric function had to fool QUEST by -1*i + 2
    msg = 'staircase.data (incorrect/correct)=' + str(s.data)
    print(msg)
    if alsoLog:     logging.info(msg)

    if printInternalVal:
        msg = '\tstaircase.intensities, *internal* values [' #(these are log intensities)=['
        for i in range( len(s.intensities) ):
            msg += '{:.2f}, '.format( s.intensities[i] ) #I cant figure out a simpler way to prevent scientific notation
        msg+= ']'
        print(msg)
        if alsoLog:     logging.info(msg)
    msg = '\tstaircase.intensities (back-transformed, if descendingPsycho), values [' 
    for j in range( len(s.intensities) ):
        msg += '{:.2f}, '.format( outOfStaircase(s.intensities[j], s, descendingPsycho) )
    msg+= ']'
    print(msg)
    if alsoLog:     logging.info(msg)

    if type(s) is psychopy.data.StairHandler:
        numReversals = len(s.reversalIntensities)
        msg= 'staircase number of reversals=' + str(numReversals) + '] '
        msg+= 'reversal intensities (back-transformed if descendingPsycho)=' + \
              str( np.array( outOfStaircase(s.reversalIntensities,s,descendingPsycho)) ) # str( 1- np.array( outOfStaircase(s.reversalIntensities,s,descendingPsycho)) )
        print(msg)
        if alsoLog:     logging.info(msg)
        if numReversals>0:
            numReversalsToAvg = numReversals-2
            msg= ('mean of final ' + str(numReversalsToAvg) + 
                      ' reversals (back-transformed if descendingPsycho) =' +
                      str( np.average(  outOfStaircase(s.reversalIntensities[-numReversalsToAvg:],s,descendingPsycho),   ) ) )
            print(msg)
            if alsoLog:     logging.info(msg)
    elif type(s) is psychopy.data.QuestHandler:
            #some of below are private initialization variables I'm not really supposed to access
            if not briefTrialUpdate:
                msg= ('\tpThreshold (proportion correct for which trying to zero in on the corresponding parameter value) =' +
                               str(s._quest.pThreshold) + '\n')
                msg+= ('\tstopInterval (min 5-95% confidence interval required for  thresh  before stopping. If both this and nTrials is specified, whichever happens first)='+
                               str(s.stopInterval) + '\n')
                msg+= '\tstepType=' + str(s.stepType) + '\n'
                msg+= '\tminVal=' + str(s.minVal) + '  maxVal=' + str(s.maxVal) + '\n'
                msg+= '\tnTrials=' + str(s.nTrials)
                print(msg)
                if alsoLog:     logging.info(msg)

    #below applies to both types of staircase
    if s.thisTrialN == -1:
        msg= 'thisTrialN = -1, suggesting you have not started it yet; need to call staircase.next()'
        print(msg)
        if alsoLog:     logging.info(msg)
    else:
        msg= 'staircase thisTrialN =' + str(s.thisTrialN)
        print(msg)
        if alsoLog:     logging.info(msg)
        # staircase.calculateNextIntensity() sounds like something useful to get a preview of the next trial. Instead, seems to be 
        #the internal function used to advance to the next trial.
    
def createNoise(proportnNoise,win,fieldWidthPix,noiseColor): 
    #noiseColor, assumes that colorSpace='rgb', triple between -1 and 1
    #Creates proportnNoise*area dots, in random positions, with color noiseColor (black)
    numDots = int(proportnNoise*fieldWidthPix*fieldWidthPix)
    if numDots ==0:
        return None
    #create a matrix of all possible pixel locations, shuffle it, pick off the first numDots ones
    #0,0 is center of field
    possibleXcoords = -fieldWidthPix/2 + np.arange(fieldWidthPix) 
    possibleXcoords += fieldWidthPix/30 #adding one-tenth because for some mysterious reason not centered, I guess letters aren't drawn centered
    possibleYcoords = deepcopy(possibleXcoords)
    def expandgrid(*itrs):
       product = list(itertools.product(*itrs))
       return product
    allFieldCoords = expandgrid(possibleXcoords,possibleYcoords)
    #shuffle it
    np.random.shuffle(allFieldCoords)
    dotCoords = allFieldCoords[0:numDots]

    #create opacity for each dot
    opacs = np.ones(numDots)#all opaque
    verticalAdjust = 3 #number of pixels to raise rectangle by. Using only uppercase letters and seem to be drawn above the line
    noise = visual.ElementArrayStim(win,units='pix', elementTex=None, elementMask=None,
        nElements=numDots, fieldSize=[fieldWidthPix,fieldWidthPix],
        fieldPos=(0.0, verticalAdjust),
        colorSpace='rgb',
        colors=noiseColor, #set to black
        xys= dotCoords, 
        opacities=opacs,
        sizes=1)
    return (noise,allFieldCoords,numDots) #Can just use noise, but if want to generate new noise of same coherence level quickly, can just shuffle coords

def fromStaircaseAggregateIntensityPcorrN(staircase,descendingPsycho): #this is redundant with psychopy.data.functionFromStaircase()
    
    intensLinear= outOfStaircase(staircase.intensities, staircase, descendingPsycho)
    #Use pandas to calculate proportion correct at each level
    df= DataFrame({'intensity': intensLinear, 'response': staircase.data})
    #print('df='); print(df) #debug
    grouped = df.groupby('intensity')
    groupMeans= grouped.mean() #a groupBy object, kind of like a DataFrame but without column names, only an index?
    intensitiesTested = list(groupMeans.index)
    pCorrect = list(groupMeans['response'])  #x.iloc[:]
    ns = grouped.count() #want n per trial to scale data point size
    ns = list(ns['response'])
    dfResults = DataFrame({'intensity': intensitiesTested, 'Pcorr': pCorrect, 'n': ns })
    return (dfResults)

#questplus helpers are incorporated into psychopy from https://github.com/hoechenberger/questplus
import questplus.psychometric_function #for weibull equation and inverse

#questplus helpers are incorporated into psychopy from https://github.com/hoechenberger/questplus
from questplus.psychometric_function import weibull 

# create an idealized participant (weibull function)
def calc_pCorrect(intensity,guessRate,thresh,descendingPsychometricCurve):
    #pCorrEachTrial = guessRate*.5 + (1-guessRate)* 1. / (1. + np.exp(-20*centeredOnZero)) #sigmoidal probability
    slope = 2
    if descendingPsychometricCurve:
        slope = -1*slope
        #intensity = 100-intensity
    
    pCorr = weibull(intensity=intensity, threshold=thresh,
                        slope=slope, lower_asymptote=guessRate, lapse_rate=0.00,
                        scale='linear').item()
                        
    return pCorr

# Use idealized participant to get correct/incorrect on an individual trial
def simulate_response(intensity,guessRate,thresh,descendingPsychometricCurve):
    pCorr = calc_pCorrect(intensity,guessRate,thresh,descendingPsychometricCurve)
    dice_roll = np.random.random()
    response = int(dice_roll <= pCorr)
    return response


#Alternative non-psychopy way of creating equation for weibull but not being used now
def make_my_weibull(chanceRate):
    def _weibull(x,a,b):
        y = chanceRate + (1.0-chanceRate)*(1 - np.exp( -(x/a)**(b) ) )
        return y
    return _weibull
        
#my_weibull = make_my_weibull(chanceRate)

def make_my_weibull_inverse(chanceRate):
    def _weibull_inverse(y,a,b):
        toTakeLogOf = (1.0 - y)/(1 - chanceRate)
        #print('toTakeLogOf=',toTakeLogOf)
        x = a * ( -np.log( toTakeLogOf )  ) ** (1.0/b)
        return x
    return _weibull_inverse

#my_weibull_inverse = make_my_weibull_inverse(chanceRate)

def plotDataAndPsychometricCurve(staircase,fit,descendingPsycho,threshVal):
    #Expects staircase, which has intensities and responses in it
    #May or may not be log steps staircase internals
    #Plotting with linear axes
    #Fit is a psychopy data fit object. Assuming that it couldn't handle descendingPsycho so have to invert the values from it
    intensLinear= outOfStaircase(staircase.intensities, staircase, descendingPsycho)
    if fit is not None:
        #generate psychometric curve
        intensitiesForCurve = pylab.arange(min(intensLinear), max(intensLinear), 0.01)
        thresh = fit.inverse(threshVal)
        if descendingPsycho:
            intensitiesForFit = 100-intensitiesForCurve
            #thresh = 100 - thresh
        else:
            intensitiesForFit = intensitiesForCurve
        ysForCurve = fit.eval(intensitiesForFit)
        print('intensitiesForCurve=',intensitiesForCurve)
        #print('ysForCurve=',ysForCurve) #debug

    #plot staircase over time in left hand panel
    pylab.subplot(121)
    pylab.plot(intensLinear,'b-')
    pylab.xlabel("staircase trial")
    pylab.ylabel("intensity")
    
    #plot pCorr and psychometric function on the right.
    ax1 = pylab.subplot(122)
    if fit is not None:
        pylab.plot(intensitiesForFit, ysForCurve, 'r-') #fitted curve
        pylab.plot([thresh, thresh],[0,threshVal],'r:') #vertical dashed line
        pylab.plot([0, thresh],[threshVal,threshVal],'r:') #horizontal dashed line
    
    #Find mean of last reversals and plot that, too
    meanOfFinalReversals = None
    if type(staircase) is psychopy.data.StairHandler: #as opposed to QUEST
        numReversals = len(staircase.reversalIntensities)
        if numReversals>2:
            numReversalsToAvg = numReversals-2
            meanOfFinalReversals = np.average(  
                                    outOfStaircase(staircase.reversalIntensities[-numReversalsToAvg:],staircase,descendingPsycho),   
                                    ) 
            #might need to subtract above from 100 if descending
    elif isinstance(staircase, psychopy.data.staircase.QuestHandler): #Quest staircase
            meanOfFinalReversals = staircase.quantile()
    
    #Plot mean of last reversals
    if meanOfFinalReversals is not None:
        pylab.plot([meanOfFinalReversals, meanOfFinalReversals],[0,threshVal],'b--') #vertical dashed line
        pylab.plot([0, meanOfFinalReversals],[threshVal,threshVal],'b--') #horizontal dashed line

    #print thresh proportion top of plot
    figure_title = 'mean of reversals= %0.2f' %(meanOfFinalReversals)
    if fit is not None:
        figure_title += ',\nfitted threshold (%.2f) = %0.2f' %(threshVal, thresh)

    pylab.title(figure_title,  loc='left')

    print('Aggregation of trials:'); 
    tallied = fromStaircaseAggregateIntensityPcorrN(staircase,descendingPsycho)
    print(tallied)
    
    ns = tallied.loc[:,"n"]
    Pcorr = tallied.loc[:,"Pcorr"]
    intensitiesTested = tallied.loc[:,"intensity"]
    
    #data point sizes. One entry in array for each datapoint
    pointSizes = 5+ 40 * np.array(ns) / max(ns) #the more trials, the bigger the datapoint size for maximum of 6
    #print('pointSizes = ',pointSizes)
    points = pylab.scatter(intensitiesTested, Pcorr, s=pointSizes, 
        edgecolors=(0,0,0), facecolors= 'none', linewidths=1,
        zorder=10, #make sure the points plot on top of the line
        )
    pylab.ylim([-0.01,1.01])
    pylab.xlim( [min(intensLinear), max(intensLinear)] )
    pylab.xlabel("intensity")
    pylab.ylabel("proportion correct")
    #save a vector-graphics format for future
    #outputFile = os.path.join(dataFolder, 'last.pdf')
    #pylab.savefig(outputFile)
    createSecondAxis = False
    if createSecondAxis: #presently not used, if fit to log would need this to also show linear scale
        #create second x-axis to show linear percentNoise instead of log
        ax2 = ax1.twiny()
        ax2.set(xlabel='%noise', xlim=[min(intensLinear), max(intensLinear)]) #not quite right but if go to 0, end up with -infinity? and have error
        #ax2.axis.set_major_formatter(ScalarFormatter()) #Show linear labels, not scientific notation
        #ax2 seems to be the wrong object. Why am I using pylab anyway? Matplotlib documentation seems more clear
        #for programming it is recommended that the namespaces be kept separate, http://matplotlib.org/api/pyplot_api.html
        #http://stackoverflow.com/questions/21920233/matplotlib-log-scale-tick-label-number-formatting
        ax2.set_xscale('log')
        ax2.tick_params(axis='x',which='minor',bottom='off')
        


    
if __name__ == "__main__":  #executable example of using these functions
    #Test staircase functions
    descendingPsychometricCurve = False
    guessRate = 0.5
    actualThresh = 40
    threshCriterion = 0.794 #same as what 1 up, 3 down staircase converges on
    staircaseTrials = 400
    useQuest = False

    np.random.seed(seed=233423) #so that simulated observer is reproducible. For this value, the curvefit works
    
    startVal=30
    minVal=0
    maxVal=100

    if useQuest:
        staircase = psychopy.data.QuestHandler(
            startVal = toStaircase(startVal, descendingPsychometricCurve),
            minVal= toStaircase(minVal, descendingPsychometricCurve),
            maxVal=  toStaircase(maxVal, descendingPsychometricCurve) ,
            startValSd = 30,
            stopInterval= 1, #sd of posterior has to be this small or smaller for staircase to stop, unless nTrials reached
            nTrials = staircaseTrials,
            #extraInfo = thisInfo,
            pThreshold = threshCriterion, #0.25,    
            gamma = 1./26,
            delta=0.02, #lapse rate, I suppose for Weibull function fit
            method = 'quantile', #uses the median of the posterior as the final answer
            stepType = 'log'  #will home in on the 80% threshold. But stepType = 'log' doesn't usually work
                        )
        print('Created QUEST staircase.')
    else:
        staircase = psychopy.data.StairHandler(startVal=20.0,
            minVal=0, maxVal=100,
            stepType='lin',
            stepSizes=[8, 4, 4, 2, 2, 1, 1],  # reduce step size every two reversals
            nUp=1, nDown=3,  # will home in on the 79.4% threshold; threshTryingToEstimate
            nTrials=staircaseTrials)
        print('Created conventional Levitt staircase.')
    
    trialnum = 0
    while trialnum < staircaseTrials:
        #Psychopy staircase can only handle increasing psychometric function.
        #So we use outOfStaircase, in case descending psychometric function, flips it (100-x)
        intensityThisTrial = staircase.next()
        intensityThisTrial = outOfStaircase(intensityThisTrial, staircase, descendingPsychometricCurve)
        
        #Simulate observer for this trial's intensity
        correct = simulate_response(intensityThisTrial,guessRate,actualThresh,descendingPsychometricCurve)
        staircase.addResponse( correct )
        trialnum = trialnum + 1
    
    printStaircase(staircase, briefTrialUpdate=False, printInternalVal=True, alsoLog=False)

    #Fit and plot data
    fit = None
    intensityForCurveFitting = outOfStaircase(staircase.intensities,staircase,descendingPsychometricCurve)
    print('intensityForCurveFitting=',intensityForCurveFitting) #debug
    if descendingPsychometricCurve: 
         intensityForCurveFitting = 100-np.array(staircase.intensities) #because fitWeibull assumes curve is ascending
         
    #from list of trials, tally up each intensity and calculate proportions correct
    combinedInten, combinedResp, combinedN = \
         psychopy.data.functionFromStaircase(intensityForCurveFitting, staircase.data, bins='unique')
    print('combinedInten=',combinedInten,'combinedResp=',combinedResp)
    
    #Calculate standard error of each percent correct observed, because that helps the curvefitting
    # which depends on the number of trials of course. For proportion data, it's sqrt(p*(1-p))/sqrt(n)
    tallied = fromStaircaseAggregateIntensityPcorrN(staircase,descendingPsychometricCurve)  #this is redundant with psychopy.data.functionFromStaircase
    ns = tallied.loc[:,"n"]
    Pcorr = tallied.loc[:,"Pcorr"]
#    intensitiesTested = tallied.loc[:,"intensity"]
    variances = Pcorr*(1-Pcorr)
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
        #Best to send it an estimate of the standard error of each observation, https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.curve_fit.html
        #sems: A scalar or 1-D sigma should contain values of standard deviations of errors in ydata. 
        fit = psychopy.data.FitWeibull(combinedInten, combinedResp, expectedMin=guessRate, sems=stderrs)
        #fit = psychopy.data.FitLogistic(combinedInten, combinedResp, expectedMin=guessRate, sems=stderrs)
        print('fit params=',fit.params)
        #print(dir(fit))
    except Exception as e:
        print("An exception occurred when curvefitting:",str(e))
        print("Fit failed.")
    plotDataAndPsychometricCurve(staircase,fit,descendingPsychometricCurve,threshVal=threshCriterion)
    pylab.show() #must call this to actually show plot

