#https://stackoverflow.com/questions/56329180/fitting-a-logistic-curve-to-data?rq=3
import numpy as np
import scipy.optimize as opt
import matplotlib.pyplot as plt

x = np.array([0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0])
y = np.array([0.073, 2.521, 15.879, 48.365, 72.68, 90.298, 92.111, 93.44, 93.439, 93.389, 93.381, 93.367, 93.94, 93.269, 96.376]) / 100.

def f(x, a, c, d):
    return a / (1. + np.exp(-c * (x - d)))

(a_, c_, d_), _ = opt.curve_fit(f, x, y, method="trf")

y_fit = f(x, a_, c_, d_)
fig, ax = plt.subplots(1, 1, figsize=(6, 4))
ax.plot(x, y, 'o')
ax.plot(x, y_fit, '-')
plt.show()