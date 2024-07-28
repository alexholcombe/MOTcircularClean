import sys
sys.path.append('/Users/alexh/Documents/attention_tempresltn/multiple object tracking/newTraj/newTraj_repo')
import plotHelpers
import pylab
#datFile = psychopy.tools.filetools.fromFile(filenamePickle+'.psydat')
#print datFile.data     #doing this to have a dataframe to test plotDataAndPsychometricCurve with in analyzeData.py
#Only way to convert it from idiosyncratic psychopy format to dataframe is with saveAsWideText
#df = datFile.saveAsWideText("temp.txt") #wide is useful for analysis with R or SPSS. Also returns dataframe df
#print('dataframe returned from saveAsWideText df.dtypes=\n',df.dtypes)
#fig = plotDataAndPsychometricCurve(df, dataFileName=None)
fig = plotHelpers.plotDataAndPsychometricCurve(df=None, dataFileName="exampleData/auto_10Jun2015_16-04.psydat")
pylab.show() #pauses until window manually closed. Have to save before calling this, because closing the window loses the figure

