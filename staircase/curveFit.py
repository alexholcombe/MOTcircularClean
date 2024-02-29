#https://stackoverflow.com/questions/56329180/fitting-a-logistic-curve-to-data?rq=3
import numpy as np
import scipy, pylab
import os
import psychopy, psychopy.tools.filetools

useBinaryData = True  #Scipy functions can't fit 0/1 data well, can only do well with percentages

if useBinaryData:
    #read in binary data from file
    fname = os.path.join('data_and_results_for_tests','aohSimMar_01_0918.txt.psydat')
    if not os.path.isfile(fname):
        print("File doesn't exist")
        quit()
    thisDat = psychopy.tools.filetools.fromFile(fname)
    assert isinstance(thisDat, psychopy.data.StairHandler)
    
    x = np.array(    thisDat.intensities   )
    y = np.array(    thisDat.data          )
else:
    x = np.array([0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0])
    y = np.array([0.073, 2.521, 15.879, 48.365, 72.68, 90.298, 92.111, 93.44, 93.439, 93.389, 93.381, 93.367, 93.94, 93.269, 96.376]) / 100.

#Basic curve fit from StackOverflow
#def f(x, a, b):
#    return 1 / (1. + np.exp(-a * (x - b)))
#
#(a, b), _ = scipy.optimize.curve_fit(f, x, y, method="trf")
#
#y_fit = f(x, a, b)
#fig, ax = pylab.subplots(1, 1, figsize=(6, 4))
#ax.plot(x, y, 'o')
#ax.plot(x, y_fit, '-')
#pylab.show()


#psychopy for Weibull uses 
#      y = chance + (1.0-chance)*(1-exp( -(xx/alpha)**(beta) ))
# inverse: x = alpha * (-log((1.0-y)/(1-chance)))**(1.0/beta)

#Psychopy function fiting doesn't work well, but the below does!
chanceRate=0.2
def make_my_weibull(chanceRate):
    def _weibull(x,a,b):
        y = chanceRate + (1.0-chanceRate)*(1 - np.exp( -(x/a)**(b) ) )
        return y
    return _weibull
        
my_weibull = make_my_weibull(chanceRate)

def make_my_weibull_inverse(chanceRate):
    def _weibull_inverse(y,a,b):
        toTakeLogOf = (1.0 - y)/(1 - chanceRate)
        #print('toTakeLogOf=',toTakeLogOf)
        x = a * ( -np.log( toTakeLogOf )  ) ** (1.0/b)
        return x
    return _weibull_inverse

my_weibull_inverse = make_my_weibull_inverse(chanceRate)

(a, b), _ = scipy.optimize.curve_fit(my_weibull, x, y, method="trf")

y_fit = my_weibull(x, a, b)
fig, ax = pylab.subplots(1, 1, figsize=(6, 4))
ax.plot(x, y, 'o')
ax.plot(x, y_fit, 'r-')
#pylab.show()

thresh_criterion = 0.79
print('a=',a,'b=',b,'thresh_criterion=',thresh_criterion)
threshold = my_weibull_inverse(thresh_criterion,a,b)

#plot chanceRate line
ax.plot([0, max(x)],[chanceRate,chanceRate],'k:') #horizontal dashed line

#plot threshold-showing line
ax.plot([threshold, threshold],[0,thresh_criterion],'r:') #vertical dashed line
ax.plot([0, threshold],[thresh_criterion,thresh_criterion],'r:') #horizontal dashed line

figure_title = 'fitted threshold (%.2f) = %0.2f' %(thresh_criterion, threshold)
pylab.title(figure_title)
pylab.show()
