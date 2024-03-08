#Based on chatGPT-generated code

import numpy as np
import pandas as pd
import pylab, os
from scipy.optimize import fmin_tnc

def sigmoid(x): 
    return 1 / (1 + np.exp(-x))

def net_input(theta, x):
    return np.dot(x, theta)

def probability(theta, x):
    return sigmoid(net_input(theta, x))

def cost_function(theta, x, y):
    m = x.shape[0]
    total_cost = -(1 / m) * np.sum(
        y * np.log(probability(theta, x)) + (1 - y) * np.log(
            1 - probability(theta, x)))
    return total_cost

def gradient(theta, x, y):
    m = x.shape[0]
    return (1 / m) * np.dot(x.T, sigmoid(net_input(theta, x)) - y)

def fit(x, y, initialParametersGuess):

    # add an extra column of ones to act as the bias term in the model
    X = np.hstack((np.ones((x.shape[0], 1)), x))

    # initialize parameters to start search with
    initialParams = np.ones((X.shape[1], 1))
    initialParams = np.array([  [initialParametersGuess[0]], 
                                 [initialParametersGuess[1]]  ])

    opt_weights = fmin_tnc(func=cost_function, x0=initialParams,
                  fprime=gradient,args=(X, y.flatten()))
    return opt_weights[0]

def predict(x,params):

    if not isinstance(params, np.ndarray):
        params = np.array(params)

    # add an extra column of ones to act as the bias term in the model
    X = np.hstack((np.ones((x.shape[0], 1)), x))

    theta = params[:, np.newaxis]
    return probability(theta, X)

if __name__ == "__main__":  #executeable example of using these functions

    # load your data using pandas
    data = pd.read_csv('some_data.tsv',delimiter='\t')

    # assuming the last column is the target and the rest are features
    x = data[['speedThisTrial' ]] #data[['numObjectsInRing','numTargets','speedThisTrial' ]]
    y = data['correctForFeedback']
    y = y.values #because otherwise y is a Series for some reason

    #print('X=',X)
    #print('y=',y, 'type(y)=',type(y))
    parametersGuess = [1,-2]

    #fit
    parameters = fit(x, y, parametersGuess)
    print('parameters=',parameters, 'type(parameters)=', type(parameters))

    #predict
    predicted = predict(x,parameters)
    #print('predicted values=', predicted)
    #print('End predicted values')

    data['predicted']=predicted

    grouped_df = data.groupby(['speedThisTrial']).agg(
        pctCorrect=('correctForFeedback', 'mean'),
        n=('correctForFeedback', 'count'),
        predicted = ('predicted','mean')
    )
    grouped_df = grouped_df.reset_index()

    # plot curve
    pylab.subplot(111)
    pylab.xlabel("speed (rps)")
    pylab.ylabel("Percent correct")

    # plot points
    pointSizes = pylab.array(grouped_df['n']) * 5  # 5 pixels per trial at each point
    points = pylab.scatter(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], s=pointSizes,
        edgecolors=(0, 0, 0), facecolor=(1, 1, 1), linewidths=1,
        zorder=10,  # make sure the points plot on top of the line
        )
    pylab.plot( grouped_df['speedThisTrial'],grouped_df['predicted'], 'r'+'-' )

    paramsDoubleA = [ 2*parameters[0], parameters[1] ]
    print('paramsDoubleA=',paramsDoubleA, 'type(paramsDoubleA)=', type(paramsDoubleA))
    
    #Show the effect on the predictions of doubling the first parameter
    #fix so doesnt have to be numpy array
    predictedDoubleA = predict(x, paramsDoubleA) # np.array(paramsDoubleA) )
    predictedDoubleA = predictedDoubleA.flatten()
    print('predictedDoubleA=',predictedDoubleA, 'type=',type(predictedDoubleA))
    data['predictedDoubleA'] = predictedDoubleA

    grouped_df = data.groupby(['speedThisTrial']).agg(
        pctCorrect=('correctForFeedback', 'mean'),
        n=('correctForFeedback', 'count'),
        predicted = ('predicted','mean'),
        predictedDoubleA = ('predictedDoubleA','mean'),
    )
    grouped_df = grouped_df.reset_index()

    pylab.plot( grouped_df['speedThisTrial'],grouped_df['predictedDoubleA'], 'g'+'-' )

    pylab.ylim([0, 1])
    pylab.xlim([0, None])
    # save a vector-graphics format for future
    #dataFolder='.'
    #outputFile = os.path.join(dataFolder, 'last.pdf')
    #pylab.savefig(outputFile)
    #print('saved figure to: ' + outputFile)
    pylab.show()
