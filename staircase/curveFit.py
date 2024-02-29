#https://stackoverflow.com/questions/56329180/fitting-a-logistic-curve-to-data?rq=3
import numpy as np
import scipy, pylab

x = np.array([0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0])
y = np.array([0.073, 2.521, 15.879, 48.365, 72.68, 90.298, 92.111, 93.44, 93.439, 93.389, 93.381, 93.367, 93.94, 93.269, 96.376]) / 100.

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

chanceRate=0.2
def make_my_weibull(chanceRate):
    def _weibull(x,a,b):
        y = chanceRate + (1.0-chanceRate)*(1 - np.exp( -(x/a)**(b) ) )
        return y
    return _weibull
        
my_weibull = make_my_weibull(chanceRate)


#chance=.2
#def weibull(x,chance,a,b):
#    y = chance + (1.0-chance)*(1 - np.exp( -(x/a)**(b) ) )
#    return y
#
#def weibullInverse(y,chance,a,b):
#    x = alpha * (-log((1.0-y)/(1-chance)))**(1.0/beta)
#    return x
    
#(chance, a, b), _ = scipy.optimize.curve_fit(weibull, x, y, method="trf")

(a, b), _ = scipy.optimize.curve_fit(my_weibull, x, y, method="trf")

stop

y_fit = weibull(x, chance, a, b)
fig, ax = pylab.subplots(1, 1, figsize=(6, 4))
ax.plot(x, y, 'o')
ax.plot(x, y_fit, '-')
pylab.show()