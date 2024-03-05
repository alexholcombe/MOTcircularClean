#From chatGPT

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

def fit(x, y, theta):
    opt_weights = fmin_tnc(func=cost_function, x0=theta,
                  fprime=gradient,args=(x, y.flatten()))
    return opt_weights[0]

def predict(x,params):
    theta = params[:, np.newaxis]
    return probability(theta, x)

if __name__ == "__main__":  #executable example of using these functions

    # load your data using pandas
    data = pd.read_csv('some_data.tsv',delimiter='\t')

    # assuming the last column is the target and the rest are features
    X = data[['speedThisTrial' ]] #data[['numObjectsInRing','numTargets','speedThisTrial' ]]
    y = data['correctForFeedback']
    y = y.values #because otherwise y is a Series for some reason

    #print('X=',X)
    #print('y=',y, 'type(y)=',type(y))

    # add an extra column of ones to act as the bias term in the model
    X = np.hstack((np.ones((X.shape[0], 1)), X))

    # initialize theta to zeros
    theta = np.zeros((X.shape[1], 1))

    #fit
    parameters = fit(X, y, theta)
    print('parameters=',parameters)

    #predict
    predicted = predict(X,parameters)
    print('predicted values=', predicted)
    print('End predicted values')

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
    pylab.plot(grouped_df['speedThisTrial'],grouped_df['predicted'], 'r'+'-')

    pylab.ylim([0, 1])
    pylab.xlim([0, None])
    # save a vector-graphics format for future
    #dataFolder='.'
    #outputFile = os.path.join(dataFolder, 'last.pdf')
    #pylab.savefig(outputFile)
    #print('saved figure to: ' + outputFile)
    pylab.show()
