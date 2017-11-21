import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt
import math
import sys

plt.rcParams['font.family'] = 'monospace'

fig = plt.figure()

ax = fig.add_subplot(111, aspect='equal')
Z = np.loadtxt('dist.dat', delimiter=',')
X = np.linspace(-1, 2, 601)
Y = np.linspace(-1, 2, 601)
Xm, Ym = np.meshgrid(X, Y)
levels = 1
plt.imshow(Z.reshape((len(Y),len(X))), extent=(X[0], X[-1], Y[0], Y[-1]), vmin=0, vmax=1, cmap=cm.jet, origin='lower', interpolation='gaussian')
CR = plt.contour(Xm, Ym, Z, levels, colors='black', linewidths=[2])
CR.clabel(fontsize=12, fmt='%.2f')
if int(sys.argv[1]) == 0:
	ax.scatter([1,0,1], [0,1,1], marker='v', facecolor='red',   edgecolor='black', s=50)
	ax.scatter([0],     [0],     marker='^', facecolor='azure', edgecolor='black', s=50)
else:
	ax.scatter([1,0], [0,1], marker='v', facecolor='red',   edgecolor='black', s=50)
	ax.scatter([0,1], [0,1], marker='^', facecolor='azure', edgecolor='black', s=50)
plt.grid()
plt.xlim(-1.0, 2.0)
plt.ylim(-1.0, 2.0)
plt.xlabel("")
plt.ylabel("")
plt.savefig('plot%i.svg' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot%i.eps' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
