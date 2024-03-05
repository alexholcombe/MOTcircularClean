try: #This only works if the code executing is in this folder
    import logisticRegression as logisticR
except Exception as e:
    print("An exception occurred:",str(e))
    print('Could not import logisticRegression.py (you need that file which needs an __init__.py file in its directory too)')

import numpy as np
import pandas as pd
import pylab, os

# load your data using pandas
data = pd.read_csv('some_data.tsv',delimiter='\t')

# assuming the last column is the target and the rest are features
X = data[['speedThisTrial' ]] #data[['numObjectsInRing','numTargets','speedThisTrial' ]]
y = data['correctForFeedback']
y = y.values #because otherwise y is a Series for some reason

print('X=',X)
print('y=',y, 'type(y)=',type(y))

# add an extra column of ones to act as the bias term in the model
X = np.hstack((np.ones((X.shape[0], 1)), X))

# initialize theta to zeros
theta = np.zeros((X.shape[1], 1))

#fit
parameters = logisticR.fit(X, y, theta)
print('parameters=',parameters)

#predict
predicted = logisticR.predict(X,parameters)
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
