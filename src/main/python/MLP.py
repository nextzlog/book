import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt
import math
import sys

plt.rcParams['font.family'] = 'monospace'

fig = plt.figure()

if sys.argv[1] == 'MLP':
	rect = fig.add_subplot(111, aspect='equal')
	dist = np.loadtxt('dist.dat', delimiter=',')
	
	X = np.arange(-1.0, 2.005, 0.005)
	Y = np.arange(-1.0, 2.005, 0.005)
	
	Xm, Ym = np.meshgrid(X, Y)
	
	levels = 1
	
	rect.imshow(dist.reshape((len(Y), len(X))), extent=(X[0], X[-1], Y[0], Y[-1]), vmin=0, vmax=1, cmap=cm.jet, origin='lower', interpolation='gaussian')
	CR = plt.contour(Xm, Ym, dist, levels, colors='black', linewidths=[2])
	CR.clabel(fontsize=12, fmt='%.2f')
	
	if int(sys.argv[1]) == 0:
		rect.scatter([1,0,1], [0,1,1], marker='v', facecolor='red',   edgecolor='black', s=50)
		rect.scatter([0],     [0],     marker='^', facecolor='azure', edgecolor='black', s=50)
	else:
		rect.scatter([1,0], [0,1], marker='v', facecolor='red',   edgecolor='black', s=50)
		rect.scatter([0,1], [0,1], marker='^', facecolor='azure', edgecolor='black', s=50)
	
	plt.grid()
	
	plt.xlim(-1.0, 2.0)
	plt.ylim(-1.0, 2.0)
	
	plt.xlabel("")
	plt.ylabel("")
	
	plt.savefig('plot%i.svg' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot%i.eps' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)

elif sys.argv[1] == 'GD1':
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

else:
	rect = fig.add_subplot(111, aspect='equal')
	
	data = map(lambda sgd: np.loadtxt('%s.dat' % sgd, delimiter=','), sys.argv[1:])
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
	
	rect.imshow(Z, extent=(X[0], X[-1], Y[0], Y[-1]), vmin=-4, vmax=4, cmap=cm.jet, origin='lower', interpolation='gaussian', zorder=0)
	CR = plt.contour(Xm, Ym, Z, levels, colors='black', linewidths=[2], zorder=1)
	CR.clabel(fontsize=12, fmt='%.2f')
	
	colors = ['#AA0000', '#0000AA', '#00AA00']
	
	for i,sgd in enumerate(sys.argv[1:]):
		plt.plot(data[i,:,0], data[i,:,1], '-o', color=colors[i], lw=2, label=sgd, zorder=2+len(sys.argv[1:])-i)
	plt.text(data[0,0,0], data[0,0,1] + 0.1, 'start point', va='bottom', ha='center', bbox={'fc':'w'}, zorder=2)
	
	plt.grid()
	
	plt.xlim(X[0], X[-1])
	plt.ylim(Y[0], Y[-1])
	
	plt.xlabel("$x$")
	plt.ylabel("$y$")
	
	plt.legend(loc='lower right', markerscale=2, labelspacing=.5, borderpad=.8, handletextpad=.5)
	
	plt.savefig('plot.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot.eps', bbox_inches='tight', pad_inches=0.1)
