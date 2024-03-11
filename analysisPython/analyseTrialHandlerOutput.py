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

#Fit logistic regressions for each condition
for cond in mainCondsDf:
    print('condition about to do logistic regression on condition:', cond)
    #actualThreshold = mainCondsDf[ ] #query for this condition. filtered_value = df.query('numTargets == 2 and numObjects == 4')['midpointThreshPrevLit'].item()
    # Create a mask to reference this specific condition in my df
    maskForThisCond = (df['numTargets'] == 2) & (df['numObjectsInRing'] == 4)
    dataThisCond =  df[ maskForThisCond  ]
    
    x = dataThisCond['speedThisTrial' ] #data[['numObjectsInRing','numTargets','speedThisTrial' ]]
    y = dataThisCond['correctForFeedback']
    y = y.values #because otherwise y is a Series for some reason
    #print('X=',x)
    #print('y=',y, 'type(y)=',type(y))
    
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
        print('parameters=',parameters)
        mypredicted = logisticR.predict(x,parameters)
        #print('logistic regression-predicted values=', mypredicted)
        # Create a new column 'predicted' and assign the values from mypredicted
        # to the rows matching the condition
        df.loc[maskForThisCond, 'logisticPredicted'] = mypredicted

#Aggregate data into percent correct for plotting actual data
grouped_df = df.groupby(['speedThisTrial']).agg(
    pctCorrect=('correctForFeedback', 'mean'),
    n=('correctForFeedback', 'count')
)
grouped_df = grouped_df.reset_index()
#print('grouped_df=',grouped_df)

# set up plot
plt.subplot(111) #(122)
#plt.xlabel("speed (rps)")
plt.ylabel("Proportion correct")
plt.xlabel('speed (rps)')
threshVal = 0.794
maxX = x.max()
plt.plot([0, maxX], [threshVal, threshVal], 'k--')  # horizontal dashed line

# plot points
pointSizes = np.array(grouped_df['n']) * 5  # 5 pixels per trial at each point
#points = plt.scatter(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], s=pointSizes,
#    edgecolors=(0, 0, 0), facecolor=(1, 1, 1), linewidths=1,
#    zorder=10,  # make sure the points plot on top of the line
#    )
#print(grouped_df)
speeds= grouped_df['speedThisTrial'].values
#print('speeds=',speeds)
""" speeds= [0.02   ,    0.03    ,   0.1  ,      0.23  ,     0.23   ,    0.28,
 0.29570313, 0.29570313, 0.33   ,    0.34570313, 0.39570313 ,0.43,
 0.49570313, 0.49570313, 0.59570312, 0.59570313, 0.69570313, 0.79570313,
 0.79570313, 0.84570313, 0.89570312 ,0.89570313, 0.94570313 ,0.99570313,
 1.04570313, 1.09570312 , 1.19570312] """
#speeds = speeds.values()
points = plt.plot(speeds, grouped_df['pctCorrect'].values.tolist(),
                  marker='o', markersize=2, linestyle='')

if fitSucceeded:
    xForCurve = np.arange(0,2,.02)
    xForCurve = pd.DataFrame(xForCurve)
    predicted = logisticR.predict(xForCurve, parameters) # np.array(paramsDoubleA) )
    predicted = predicted.flatten()
    plt.plot( xForCurve, predicted, 'k'+'-' )
    
title = 'circle = mean of final reversals'
autopilot = True; simulateObserver = True
if autopilot and simulateObserver:
    title += 'triangle = true threshold'
plt.title(title)
plt.show()