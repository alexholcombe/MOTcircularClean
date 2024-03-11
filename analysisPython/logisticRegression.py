#Based on chatGPT-generated code

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
from scipy.optimize import fmin_tnc

def my_logistic(x): 
    return 1 / (1 + np.exp(-x))

def usual_regression_part(theta, x):
    #theta is the bias terms and regression coefficients
    #Multiply them by the x's, like in any regression
    return np.dot(x, theta)

def probability(theta, x):
    ys = usual_regression_part(theta, x)
    return my_logistic( ys )

def cost_function(theta, x, y):
    m = x.shape[0]
    total_cost = -(1 / m) * np.sum(
        y * np.log(probability(theta, x)) + (1 - y) * np.log(
            1 - probability(theta, x)))
    return total_cost

def gradient(theta, x, y):
    m = x.shape[0]
    return (1 / m) * np.dot(x.T, my_logistic(usual_regression_part(theta, x)) - y)

def fit(x, y, initialParametersGuess):

    if not isinstance(x, pd.DataFrame):
        x = pd.DataFrame(x)  
    lenX = x.shape[0] #shape only works on DataFrames

    # add an extra column of ones to act as the bias term in the model
    X = np.hstack((np.ones((lenX, 1)), x))

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
    data = pd.read_table('some_data.tsv')

    # assuming the last column is the target and the rest are features
    x = data[['speedThisTrial']] #data[['numObjectsInRing','numTargets','speedThisTrial' ]]
    y = data['correctForFeedback']
    y = y.values #because otherwise y is a Series for some reason

    #print('x=',x,'type(x)=',type(x))
    #print('y=',y, 'type(y)=',type(y))
    parametersGuess = [1,-2]

    #fit
    parameters = fit(x, y, parametersGuess)
    print('parameters=',parameters, 'type(parameters)=', type(parameters))

    #predict
    predicted = predict(x,parameters)
    #print('predicted values=', predicted, 'type=',type(predicted))
    #print('End predicted values')

    data['predicted']=predicted

    grouped_df = data.groupby(['speedThisTrial']).agg(
        pctCorrect=('correctForFeedback', 'mean'),
        n=('correctForFeedback', 'count'),
        predicted = ('predicted','mean')
    )
    grouped_df = grouped_df.reset_index()

    #plt.plot(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], marker='o')
    #plt.plot( grouped_df['speedThisTrial'],grouped_df['predicted'], 'k'+'-' )

    # plot points
    pointSizes = np.array(grouped_df['n']) * 5  # 5 pixels per trial at each point
    points = plt.scatter(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], s=pointSizes,
        edgecolors=(0, 0, 0), facecolor=(1, 1, 1), linewidths=1,
        zorder=10,  # make sure the points plot on top of the line
        )
    plt.plot( grouped_df['speedThisTrial'],grouped_df['predicted'], 'k'+'-' )


    # set up plot
    #plt.subplot(111)
    plt.xlabel("speed (rps)")
    plt.ylabel("Percent correct")

    plt.show()
    QUIT
    
    threshVal = 0.794
    plt.plot([0, max(x)], [threshVal, threshVal], 'k--')  # horizontal dashed line

    # plot points
    pointSizes = np.array(grouped_df['n']) * 5  # 5 pixels per trial at each point
    points = plt.scatter(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], s=pointSizes,
        edgecolors=(0, 0, 0), facecolor=(1, 1, 1), linewidths=1,
        zorder=10,  # make sure the points plot on top of the line
        )
    plt.plot( grouped_df['speedThisTrial'],grouped_df['predicted'], 'k'+'-' )

    paramsDoubleA = [ 2*parameters[0], parameters[1] ]
    
    #Show the effect on the predictions of doubling the first parameter
    #fix so doesnt have to be numpy array
    xForCurve = np.arange(0,5,.02)
    xForCurve = pd.DataFrame(xForCurve)
    predictedDoubleA = predict(xForCurve, paramsDoubleA) # np.array(paramsDoubleA) )
    predictedDoubleA = predictedDoubleA.flatten()
    #print('predictedDoubleA=',predictedDoubleA, 'type=',type(predictedDoubleA))
    plt.plot( xForCurve, predictedDoubleA, 'g'+'-', label='double the bias' )

    #Show the effect on the predictions of quadrupling the second parameter
    #fix so doesnt have to be numpy array
    paramsQuadrupleB = [ parameters[0], 4*parameters[1] ]

    predictedQuadrupleB = predict(xForCurve, paramsQuadrupleB) # np.array(paramsDoubleA) )
    predictedQuadrupleB = predictedQuadrupleB.flatten()

    plt.plot( xForCurve, predictedQuadrupleB, 'r'+'-',label='double the slope')
    plt.title('fitted params (location,slope)=' +str(np.round(parameters,2))+"\nnote slope affects location")
    plt.ylim([0, None])
    plt.xlim([0, None])
    plt.legend()
    # save a vector-graphics format for future
    #dataFolder='.'
    #outputFile = os.path.join(dataFolder, 'last.pdf')
    #plt.savefig(outputFile)
    #print('saved figure to: ' + outputFile)
    plt.show()
