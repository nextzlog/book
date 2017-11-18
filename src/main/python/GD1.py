import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt
import math
import sys

plt.rcParams['font.family'] = 'monospace'

fig = plt.figure()

loss = np.array(map(lambda sgd: np.loadtxt('%s.dat' % sgd, delimiter=','), sys.argv[1:]))
lmax = 0.45

rect = fig.add_subplot(111, aspect=loss.shape[1]/lmax)

plots = []
colors = ['#AA0000', '#0000AA', '#00AA00']

for i,sgd in enumerate(sys.argv[1:]):
	for stage in xrange(loss.shape[2]):
		plot, = plt.plot(loss[i,:,stage], color=colors[i], lw=2, label=sgd)
	plots.append(plot)

plt.grid()

plt.xlim(0, loss.shape[1])
plt.ylim(0, lmax)

plt.xlabel("#epoch")
plt.ylabel("loss")

plt.legend(plots, sys.argv[1:], markerscale=2, labelspacing=.5, borderpad=.8, handletextpad=.5)

plt.savefig('plot.svg', bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot.eps', bbox_inches='tight', pad_inches=0.1)
