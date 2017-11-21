import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt
import math
import sys

plt.rcParams['font.family'] = 'monospace'

fig = plt.figure()

if int(sys.argv[1]) == 1:
	ax = fig.add_subplot(111, aspect='equal')
	data = map(lambda sgd: np.loadtxt('%s.dat' % sgd, delimiter=','), sys.argv[2:])
	data = np.array(map(lambda d:d[:min(map(lambda d:len(d), data))], data))
	res = 200
	X = np.linspace(-2.0, 2.0, res)
	Y = np.linspace(-2.0, 2.0, res)
	Z = np.zeros((res,res))
	for j,y in enumerate(Y):
		for i,x in enumerate(X):
			Z[j,i] = x**2 - y**2
	Xm, Ym = np.meshgrid(X, Y)
	levels = 10
	ax.imshow(Z, extent=(X[0], X[-1], Y[0], Y[-1]), vmin=-4, vmax=4, cmap=cm.jet, origin='lower', interpolation='gaussian', zorder=0)
	CR = plt.contour(Xm, Ym, Z, levels, colors='black', linewidths=[2], zorder=1)
	CR.clabel(fontsize=12, fmt='%.2f')
	colors = ['#AA0000', '#0000AA', '#00AA00']
	for i,sgd in enumerate(sys.argv[2:]):
		plt.plot(data[i,:,0], data[i,:,1], '-o', color=colors[i], lw=2, label=sgd, zorder=2+len(sys.argv[2:])-i)
	plt.text(data[0,0,0], data[0,0,1] + 0.1, 'start point', va='bottom', ha='center', bbox={'fc':'w'}, zorder=2)
	plt.grid()
	plt.xlim(X[0], X[-1])
	plt.ylim(Y[0], Y[-1])
	plt.xlabel("$x$")
	plt.ylabel("$y$")
	plt.legend(loc='lower right', markerscale=2, labelspacing=.5, borderpad=.8, handletextpad=.5)
	plt.savefig('plot1.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot1.eps', bbox_inches='tight', pad_inches=0.1)

else:
	loss = np.array(map(lambda sgd: np.loadtxt('%s.dat' % sgd, delimiter=','), sys.argv[2:]))
	lmax = 0.45
	ax = fig.add_subplot(111, aspect=loss.shape[1]/lmax)
	plots = []
	colors = ['#AA0000', '#0000AA', '#00AA00']
	for i,sgd in enumerate(sys.argv[2:]):
		for stage in xrange(loss.shape[2]):
			plot, = plt.plot(loss[i,:,stage], color=colors[i], lw=2, label=sgd)
		plots.append(plot)
	plt.grid()
	plt.xlim(0, loss.shape[1])
	plt.ylim(0, lmax)
	plt.xlabel("#epoch")
	plt.ylabel("loss")
	plt.legend(plots, sys.argv[2:], markerscale=2, labelspacing=.5, borderpad=.8, handletextpad=.5)
	plt.savefig('plot2.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot2.eps', bbox_inches='tight', pad_inches=0.1)
