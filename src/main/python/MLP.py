import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt
from matplotlib.colors import LinearSegmentedColormap
import sys

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
ax = fig.add_subplot(111, aspect='equal')
Z = np.loadtxt('dist.dat', delimiter=',')
if Z.max() > 1:
	X = np.linspace(-2, 2, Z.shape[1])
	Y = np.linspace(-2, 2, Z.shape[0])
else:
	X = np.linspace(-1, 2, Z.shape[1])
	Y = np.linspace(-1, 2, Z.shape[0])
Xm, Ym = np.meshgrid(X, Y)
if Z.max() > 1:
	colors = []
	colors.append((1.0, 1.0, 0.0, 1))
	colors.append((0.0, 0.0, 0.5, 1))
	colors.append((1.0, 0.0, 0.0, 1))
	colors.append((0,0, 0.0, 0.0, 1))
	cmap = LinearSegmentedColormap.from_list('flag', colors, 4)
else:
	cmap = cm.jet
plt.imshow(Z.reshape((len(Y),len(X))), extent=(X[0],X[-1],Y[0],Y[-1]), vmin=0, vmax=Z.max(), cmap=cmap, origin='lower', interpolation='gaussian')
CR = plt.contour(Xm, Ym, Z, int(round(Z.max())), colors='black', linewidths=[2])
if int(round(Z.max())) == 1: CR.clabel(fontsize=12, fmt='%.2f')
if int(sys.argv[1]) == 0:
	ax.scatter([1,0,1], [0,1,1], marker='v', facecolor='red',   edgecolor='black', s=50)
	ax.scatter([0],     [0],     marker='^', facecolor='azure', edgecolor='black', s=50)
elif int(sys.argv[1]) in [1,2]:
	ax.scatter([1,0], [0,1], marker='v', facecolor='red',   edgecolor='black', s=50)
	ax.scatter([0,1], [0,1], marker='^', facecolor='azure', edgecolor='black', s=50)
else:
	ax.scatter([+0], [+1], marker='^', facecolor='azure',  edgecolor='black', s=50)
	ax.scatter([+1], [+0], marker='>', facecolor='white',  edgecolor='black', s=50)
	ax.scatter([-0], [-1], marker='v', facecolor='red',    edgecolor='black', s=50)
	ax.scatter([-1], [-0], marker='<', facecolor='yellow', edgecolor='black', s=50)
plt.grid()
plt.xlim(X[0], X[-1])
plt.ylim(Y[0], Y[-1])
plt.xlabel("")
plt.ylabel("")
plt.savefig('plot%i.svg' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot%i.eps' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
