#Plot percent correct by condition and speed, and fit logistic regression
import pandas as pd
import numpy as np
import itertools #to calculate all subsets
import matplotlib.pyplot as plt
import warnings

#df = trials.saveAsWideText("tempData",delim='\t')  #Only calling this to get the dataframe df
#groupBy dataframe by speedThisTrial, numTargets, numObjectsInRing, correctForFeedback 

df = pd.read_table('exampleTrialHandlerExportData4trialsPerCond.tsv')
#exampleTrialHandlerExportData1trialPerCond.tsv for data where fit fails

try: 
    #from analysisPython 
    import logisticRegression as logisticR
except Exception as e:
    print("An exception occurred:",str(e))
    print('Could not import logisticRegression.py (you need that file in the analysisPython directory, which needs an __init__.py file in its directory too)')

try: #This only works if the code executing is in this folder
    import logisticRegression as logisticR
except Exception as e:
    print("An exception occurred:",str(e))
    print('Could not import logisticRegression.py (you need that file which needs an __init__.py file in its directory too)')

# temporalfrequency limit test
numTargets =        [2,                 3] #[2]
numObjsInRing =     [4,                 8] #[4]      #Limitation: gratings don't align with blobs with odd number of objects

# Get all combinations of those two main factors
combinations = list(itertools.product(numTargets, numObjsInRing))
# Create the DataFrame with all combinations
mainCondsDf = pd.DataFrame(combinations, columns=['numTargets', 'numObjects'])
print('mainCondsDf=',mainCondsDf)
# set up plot
plt.subplot(111) #(122)
#plt.xlabel("speed (rps)")
plt.ylabel("Proportion correct")
plt.xlabel('speed (rps)')
threshVal = 0.794
maxX = df['speedThisTrial'].max()
plt.plot([0, maxX], [threshVal, threshVal], 'k--')  # horizontal dashed line
colors='rgby'
paramsEachCond = list()

#Plot data and  logistic regressions for each condition
for index, cond in mainCondsDf.iterrows():
    #Plot data first without logistic regression
    #actualThreshold = mainCondsDf[ ] #query for this condition. filtered_value = df.query('numTargets == 2 and numObjects == 4')['midpointThreshPrevLit'].item()
    # Create a mask to reference this specific condition in my df
    maskForThisCond = (df['numTargets'] == cond['numTargets']) & (df['numObjectsInRing'] == cond['numObjects'])
    dataThisCond =  df[ maskForThisCond  ]

    #Aggregate data into percent correct for plotting actual data
    grouped_df = dataThisCond.groupby(['speedThisTrial']).agg(
        pctCorrect=('correctForFeedback', 'mean'),
        n=('correctForFeedback', 'count')
    )
    aggregatedDf = grouped_df.reset_index()
    #print('grouped_df=',grouped_df)

    # plot points
    pointSizes = np.array(aggregatedDf['n']) * 5  # 5 pixels per trial at each point
    points = plt.scatter(aggregatedDf['speedThisTrial'], aggregatedDf['pctCorrect'], s=pointSizes,
        c= colors[index], 
        zorder=10,  # make sure the points plot on top of the line
        )

    print('condition about to do logistic regression on condition:', cond)
    x = dataThisCond['speedThisTrial' ] #data[['numObjectsInRing','numTargets','speedThisTrial' ]]
    y = dataThisCond['correctForFeedback']
    y = y.values #because otherwise y is a Series for some reason

    parametersGuess = [1,-2]

    #fit
    with warnings.catch_warnings(): #https://stackoverflow.com/a/36489085/302378
        warnings.filterwarnings('error')
        try:
            parameters = logisticR.fit(x, y, parametersGuess)
            fitSucceeded = True
        except Warning as e:
            print('error when doing logistic fit:', e)
            fitSucceeded = False
            
    #predict
    if fitSucceeded:
        paramsEachCond.append(parameters)
        mypredicted = logisticR.predict(x,parameters)
        #print('logistic regression-predicted values=', mypredicted)
        # Create a new column 'predicted' and assign the values from mypredicted
        # to the rows matching the condition
        df.loc[maskForThisCond, 'logisticPredicted'] = mypredicted

        xForCurve = np.arange(0,1.6,.02)
        xForCurve = pd.DataFrame(xForCurve)
        predicted = logisticR.predict(xForCurve, parameters) # np.array(paramsDoubleA) )
        predicted = predicted.flatten()
        plt.plot( xForCurve, predicted, colors[index]+'-' )

print('paramsEachCond=',paramsEachCond)
title = 'circle = mean of final reversals'
autopilot = True; simulateObserver = True
if autopilot and simulateObserver:
    title += 'triangle = true threshold'
plt.title(title)
plt.show()