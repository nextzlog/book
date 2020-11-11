import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
if int(sys.argv[1]) == 1:
	ax = fig.add_subplot(111, aspect='equal')
	data = [np.loadtxt('{}.dat'.format(sgd), delimiter=',') for sgd in sys.argv[2:]]
	res = 200
	X = np.linspace(-2.0, 2.0, res)
	Y = np.linspace(-2.0, 2.0, res)
	Z = np.zeros((res,res))
	for j,y in enumerate(Y):
		for i,x in enumerate(X):
			Z[j,i] = x**2 - y**2
	Xm, Ym = np.meshgrid(X, Y)
	levels = 10
	ax.imshow(Z, extent=(X[0], X[-1], Y[0], Y[-1]), vmin=-4, vmax=4, cmap=cm.jet, origin='lower', interpolation='gaussian')
	CR = plt.contour(Xm, Ym, Z, levels, colors='black', linewidths=[2])
	CR.clabel(fontsize=12, fmt='%.2f')
	colors = ['#AA0000', '#0000AA', '#00AA00']
	for i,sgd in enumerate(sys.argv[2:]):
		plt.plot(data[i][:,0], data[i][:,1], '-o', color=colors[i], lw=2, label=sgd)
	x,y = data[0][0,0], data[0][0,1]
	plt.annotate('start', xy=(x+.05,y), xytext=(x+.2,y), va='center', ha='left', arrowprops={'arrowstyle':'wedge'})
	plt.grid(ls='dotted')
	plt.xlim(X[0], X[-1])
	plt.ylim(Y[0], Y[-1])
	plt.xlabel('$x$')
	plt.ylabel('$y$')
	plt.legend(loc='lower right', markerscale=2, labelspacing=.5, borderpad=.8, handletextpad=.5)
	plt.savefig('plot1.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot1.eps', bbox_inches='tight', pad_inches=0.1)
else:
	loss = np.array([np.loadtxt('{}.dat'.format(sgd), delimiter=',') for sgd in sys.argv[2:]])
	lmax = int(np.max(loss[:,:,1:]) / 0.05 + 1) * 0.05
	ax = fig.add_subplot(111, aspect=np.log10(loss[0][-1,0])/lmax)
	plots = []
	colors = ['#AA0000', '#0000AA', '#00AA00']
	for i,sgd in enumerate(sys.argv[2:]):
		for stage in range(1,loss.shape[2]):
			plot, = plt.plot(np.log10(loss[i,:,0]), loss[i,:,stage], color=colors[i], lw=2, label=sgd)
		plots.append(plot)
	plt.grid(ls='dotted')
	plt.xlim(0, np.log10(loss[0][-1,0]))
	plt.ylim(0, lmax)
	plt.xlabel('#epoch')
	plt.ylabel('loss')
	xticks = np.arange(0, np.log10(loss[0][-1,0])+1)
	plt.xticks(xticks, ['1e{}'.format(tick) for tick in xticks])
	plt.yticks(np.linspace(0, lmax, int(lmax * 20 + 1)))
	plt.legend(plots, sys.argv[2:], loc='lower left', markerscale=2, labelspacing=.5, borderpad=.8, handletextpad=.5)
	plt.savefig('plot2.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot2.eps', bbox_inches='tight', pad_inches=0.1)
for sgd in sys.argv[2:]: os.remove('{}.dat'.format(sgd))
webbrowser.open('file://{}'.format(os.path.realpath('plot{}.svg'.format(sys.argv[1]))))
