import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt
import math
import sys
from matplotlib import ticker

plt.rcParams['font.family'] = 'monospace'

fig = plt.figure()
rect = fig.add_subplot(111, aspect='equal')

data0 = np.loadtxt('data0.dat', delimiter=',')
data1 = np.loadtxt('data1.dat', delimiter=',')
dense = np.loadtxt('dense.dat', delimiter=',')

ID = int(sys.argv[1])

X = np.arange(-2.0, 2.05, 0.05)
Y = np.arange(-2.0, 2.05, 0.05)

Xm, Ym = np.meshgrid(X, Y)

vmin, vmax = dense.min(), dense.max()
if vmin * vmax < 0:
	vmin = -abs(max(-vmin, vmax))
	vmax = +abs(max(-vmin, vmax))

cr = rect.imshow(dense.reshape((len(Y), len(X))), extent=(X[0], X[-1], Y[0], Y[-1]), vmin=vmin, vmax=vmax, cmap=cm.coolwarm, origin='lower')
plt.contour(Xm, Ym, dense, levels=[-1, 1], cmap=cm.bwr, linestyles='dashed', linewidths=[2,2])
plt.contour(Xm, Ym, dense, levels=[0], colors='black', linestyles='dashed', linewidths=[2])
cb = plt.colorbar(cr, format='%+.1e')
cb.solids.set_edgecolor('face')
cb.set_ticks(ticker.LinearLocator(6))
cb.ax.tick_params(labelsize=12)

rect.scatter(data0[:,0], data0[:,1], marker='v', facecolor='red',  edgecolor='black', s=30, lw=1)
rect.scatter(data1[:,0], data1[:,1], marker='^', facecolor='blue', edgecolor='black', s=30, lw=1)

plt.xlim(X[0], X[-1])
plt.ylim(Y[0], Y[-1])

plt.xlabel("")
plt.ylabel("")

plt.grid()

plt.savefig('plot%i.svg' % ID, bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot%i.eps' % ID, bbox_inches='tight', pad_inches=0.1)
