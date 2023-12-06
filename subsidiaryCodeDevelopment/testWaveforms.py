#test triangle wave
import pylab
import numpy as np
from numpy import pi, cos, sin
def triangleWave(period, phase):
       #triangle wave is in sine phase (starts at 0)
       y = -abs(phase % (2*period) - period) # http://stackoverflow.com/questions/1073606/is-there-a-one-line-function-that-generates-a-triangle-wave
       #y goes from -period to 0.  Need to rescale to -1 to 1 to match sine wave etc.
       y = y/period*2 + 1
       #Now goes from -1 to 1
       return y

fig=pylab.figure()
angle = np.arange(0,2*pi,.01)

pylab.plot(angle,cos(angle))
pylab.plot(angle,sin(angle))

ans = triangleWave(pi/2,angle)
pylab.plot(angle,ans)
pylab.xlabel("angle (radians)")
pylab.show()