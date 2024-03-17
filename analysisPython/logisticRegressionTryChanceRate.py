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

def calcLogisticRegressionY(theta, x):
    #First, just do the usual regression part of mx+b for all x's
    ys = usual_regression_part(theta, x)
    #Then, apply the logistic function to each y, to transform it into a number (a probability) between 0 and 1
    return my_logistic( ys )

def cost_and_gradient(theta, x, y):
    #Can either return just the cost, or also the gradient
    #Return f and g, where f is the value of the function and g its gradient (a list of floats).
    m = x.shape[0]
    #To understand the cost function, see https://towardsdatascience.com/understanding-logistic-regression-9b02c2aec102
    #Recall that all data is in the form of 0s and 1s, so the cost function is just calculating the likelihood
    # of a either a 1 (the first part of the expression) or a 0 (the second part of the expression, the 1-y part, which yields 1)
    # And the probability of a 1 is just the predicted value, and the probability of a 0 is 1 - the predicted value
    # Then you take the log of those probabilities, for convenience so you don't have to multiply them which yields ridiculously small values, and average them
    total_cost = ( -(1 / m) * 
        np.sum(
                y * np.log(calcLogisticRegressionY(theta, x)) + (1 - y) * 
                np.log(   1 - calcLogisticRegressionY(theta, x)  )
            ) )
    
    #Understanding the gradient:
    #The gradient is the derivative of the cost function with respect to the parameters.
    #So, how does the cost function change as you change the parameters?
    #The prediction error is
    #ChatGPT explains this multiplication by the x as "the larger the error and the larger the feature value, the larger the adjustment to the corresponding weight"
    #I think that's because when x=0, the slope parameter(s) don't matter. But when x is large, the slope parameter(s) matter a lot.
    gradient = (1 / m) * np.dot(x.T, 
                                calcLogisticRegressionY(theta, x) - y
                                )

    return total_cost, gradient

# def gradient(theta, x, y):
#     m = x.shape[0]
#     return (1 / m) * np.dot(x.T, my_logistic(usual_regression_part(theta, x)) - y)

def fit(x, y, initialParametersGuess):
    x = pd.DataFrame(x) #If don't do this, dims difference can occur and screw up hstack

    # add an extra column of ones to act as the bias term in the model
    lenX = x.shape[0]
    onesForBiasTerm = np.ones( (lenX, 1) )
    #print('dims onesForBiasTerm=',onesForBiasTerm.ndim, 'dims x=',x.ndim)
    X = np.hstack(   ( onesForBiasTerm  , x)    )

    # initialize parameters to start search with
    initialParams = np.ones((X.shape[1], 1))
    initialParams = np.array([  [initialParametersGuess[0]], 
                                 [initialParametersGuess[1]]  ])

    result = fmin_tnc( func=cost_and_gradient, x0=initialParams,
                  args=(X, y.flatten()),
                  disp=0  )   # 1=output final results, 2=additional info
    optimized_weights = result[0]
    return optimized_weights

def predict(x,params):
    x = pd.DataFrame(x) #If don't do this, dims difference can occur and screw up hstack

    if not isinstance(params, np.ndarray):
        params = np.array(params)

    # add an extra column of ones to act as the bias term in the model
    lenX = x.shape[0]
    onesForBiasTerm = np.ones( (lenX, 1) )
    #print('dims onesForBiasTerm=',onesForBiasTerm.ndim, 'dims x=',x.ndim)
    X = np.hstack(   ( onesForBiasTerm  , x)    )

    theta = params[:, np.newaxis]
    return calcLogisticRegressionY(theta, X)

if __name__ == "__main__":  #executeable example of using these functions

    # load your data using pandas
    file = os.path.join('dataExamples', 
                        'auto_14Mar2024_15-38trialHandler10trialsPerCond.tsv') #'some_data.tsv')
    data = pd.read_table(file)

    # assuming the last column is the target and the rest are features
    x = data['speedThisTrial'] #data[['numObjectsInRing','numTargets','speedThisTrial' ]]
    y = data['correctForFeedback']
    y = y.values #because otherwise y is a Series for some reason

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

    plt.subplot(121)

    #plt.plot(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], marker='o')
    #plt.plot( grouped_df['speedThisTrial'],grouped_df['predicted'], 'k'+'-' )

    # plot points
    pointSizes = np.array(grouped_df['n']) * 5  # 5 pixels per trial at each point
    points = plt.scatter(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], s=pointSizes,
        linewidths=1, c='k',
        zorder=10,  # make sure the points plot on top of the line
        )
    plt.plot( grouped_df['speedThisTrial'],grouped_df['predicted'], 'k'+'-' )

    # set up plot
    plt.xlabel("speed (rps)")
    plt.ylabel("Proportion correct")

    threshVal = 0.794

    maxSpeed = data['speedThisTrial'].max()
    plt.plot([0, 1.5*maxSpeed], [threshVal, threshVal], 'k--')  # horizontal dashed line
    #plt.plot([0, max(x)], [threshVal, threshVal], 'k--')  # Error! because max(x) returns string:"speedThisTrial"

    plt.subplot(122)

    # plot points
    pointSizes = np.array(grouped_df['n']) * 5  # 5 pixels per trial at each point
    points = plt.scatter(grouped_df['speedThisTrial'], grouped_df['pctCorrect'], s=pointSizes,
        linewidths=1, c='k',
        zorder=1,  # make sure the lines plot on top of the points
        )
    plt.plot( grouped_df['speedThisTrial'],grouped_df['predicted'], 'k'+'-' )

    paramsDoubleA = [ 2*parameters[0], parameters[1] ]
    
    #Show the effect on the predictions of doubling the first parameter
    #fix so doesnt have to be numpy array
    xForCurve = np.arange(0,1.5*maxSpeed,.02)
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
    plt.xlabel("speed (rps)")
    plt.legend()
    # save a vector-graphics format for future
    dataFolder='.'
    outputFile = os.path.join(dataFolder, 'last.pdf')
    plt.savefig(outputFile)
    print('saved figure to: ' + outputFile)
    plt.show()
