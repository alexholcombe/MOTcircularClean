#From chatGPT

import numpy as np
import pandas as pd
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

def predict(x):
    theta = params[:, np.newaxis]
    return probability(theta, x)

def accuracy(x, actual_classes, probab_threshold=0.5):
    predicted_classes = (predict(x) >= 
                         probab_threshold).astype(int)
    predicted_classes = predicted_classes.flatten()
    accuracy = np.mean(predicted_classes == actual_classes)
    return accuracy * 100

# load your data using pandas
data = pd.read_csv('some_data.tsv',delimiter='\t')

# assuming the last column is the target and the rest are features
X = data[['numObjectsInRing','numTargets','speedThisTrial' ]]
y = data['correctForFeedback']

print('X=',X)
print('y=',y)

QUIT

# add an extra column of ones to act as the bias term in the model
X = np.hstack((np.ones((X.shape[0], 1)), X))

# initialize theta to zeros
theta = np.zeros((X.shape[1], 1))

parameters = fit(X, y, theta)

accuracy = accuracy(X, y.flatten())

print(f"The accuracy of the model is {accuracy}%")