import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pylab as plt

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
ax = fig.add_subplot(111, aspect=0.01)
X, T = np.loadtxt('dist.dat', delimiter=',', unpack=True)
plt.scatter(X, T, marker='*', facecolor='w', edgecolor='k')
W = map(lambda w:float(w), sys.argv[1:])
Y = map(lambda x:sum(map(lambda (i,w):w * (x ** i), enumerate(W))), X)
plt.plot(X, Y, 'r-', lw=2)
plt.grid()
plt.xlim(-10.0, 10.0)
plt.ylim(-1000, 1000)
plt.xlabel("")
plt.ylabel("")
plt.savefig('plot.svg', bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot.eps', bbox_inches='tight', pad_inches=0.1)
os.remove('dist.dat')
webbrowser.open('file://%s' % os.path.realpath('plot.svg'))
