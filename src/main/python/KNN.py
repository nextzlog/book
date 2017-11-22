import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pylab as plt
from matplotlib.colors import LinearSegmentedColormap
import sys

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
ax = fig.add_subplot(111, aspect='equal')
data0 = np.loadtxt('data0.dat', delimiter=',')
data1 = np.loadtxt('data1.dat', delimiter=',')
data2 = np.loadtxt('data2.dat', delimiter=',')
paint = np.loadtxt('class.dat', delimiter=',')
X = np.arange(-10, 11)
Y = np.arange(-10, 11)
Xm, Ym = np.meshgrid(X, Y)
colors = []
colors.append((0.6, 0.6, 1.0, 1.0))
colors.append((1.0, 1.0, 1.0, 1.0))
colors.append((1.0, 0.6, 0.6, 1.0))
cmap = LinearSegmentedColormap.from_list('pafe', colors, 3)
ax.pcolorfast(X, Y, paint, cmap=cmap)
ax.scatter(data0[:,0], data0[:,1], marker='^', facecolor=map(lambda c:0 if c < 1 else c, colors[0][:3]), edgecolor='black', s=20, lw=1)
ax.scatter(data1[:,0], data1[:,1], marker='o', facecolor=map(lambda c:0 if c < 1 else c, colors[1][:3]), edgecolor='black', s=20, lw=1)
ax.scatter(data2[:,0], data2[:,1], marker='v', facecolor=map(lambda c:0 if c < 1 else c, colors[2][:3]), edgecolor='black', s=20, lw=1)
plt.xlim(X[0], X[-1])
plt.ylim(Y[0], Y[-1])
plt.xlabel("")
plt.ylabel("")
plt.grid()
plt.savefig('plot.svg', bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot.eps', bbox_inches='tight', pad_inches=0.1)
