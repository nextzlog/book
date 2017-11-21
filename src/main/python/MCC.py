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
plt.imshow(Z, extent=[0, Z.shape[1], 0, Z.shape[0]], vmin=0, vmax=Z.max(), cmap=cm.jet, origin='upper', interpolation=None) #'gaussian')
plt.grid()
plt.axis('off')
plt.xlabel("")
plt.ylabel("")
plt.savefig('plot.svg', bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot.eps', bbox_inches='tight', pad_inches=0.1)
