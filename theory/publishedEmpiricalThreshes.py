import pandas as pd
import numpy as np

threshHzPrevLit = { 'numTargets': [2,3],
                    'HzRoudaiaFaubertYoungMen': [4, 2.8],
                    'HzRoudaiaFaubertYoungWomen': [3, 1.8],
                    'HzHolcombeChenE1': [4.45, None],
                    'HzHolcombeChenE2': [4.05,2.7]       }
	#ADD  1 target including MARINOVIC finding of 7.2 Hz for one target, and Verstraten
	#If add old people findings, make sure adjust code in MOTcircular.py that uses for starting point of staircase

def getAvgMidpointThreshes():
	# Create DataFrame
	threshHz = pd.DataFrame(threshHzPrevLit)
	# Replace None with np.nan
	avgMidpointThreshes = threshHz.replace({None: np.nan})

	# Create a new column 'HzAverage' that is the mean of the other columns
	avgMidpointThreshes['HzAvgPreviousLit'] = avgMidpointThreshes.iloc[:, 1:].mean(axis=1)

	return(avgMidpointThreshes)