import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
C0, C1 = 'red', 'blue'
if sys.argv[1] == 'KM':
	mixt0 = np.loadtxt('mixt0.dat', delimiter=',')
	mixt1 = np.loadtxt('mixt1.dat', delimiter=',')
	cents = np.loadtxt('cents.dat', delimiter=',')
	ax = fig.add_subplot(111, aspect='equal')
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	plt.xlabel("")
	plt.ylabel("")
	plt.grid(ls='dotted')
	ax.scatter(mixt0[:,0], mixt0[:,1], marker='.', color=C0, s=10)
	ax.scatter(mixt1[:,0], mixt1[:,1], marker='.', color=C1, s=10)
	ax.scatter([cents[0,0]], [cents[0,1]], marker='o', facecolor=C0, edgecolor='black', lw=2, s=30)
	ax.scatter([cents[1,0]], [cents[1,1]], marker='o', facecolor=C1, edgecolor='black', lw=2, s=30)
	plt.savefig('plot0.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot0.eps', bbox_inches='tight', pad_inches=0.1)
	webbrowser.open('file://{}'.format(os.path.realpath('plot0.svg')))

else:
	mixt0 = np.loadtxt('mixt0.dat', delimiter=',')
	mixt1 = np.loadtxt('mixt1.dat', delimiter=',')
	dense = np.loadtxt('dense.dat', delimiter=',')
	train = np.loadtxt('train.dat', delimiter=',')
	cents = np.loadtxt('cents.dat', delimiter=',')
	if sys.argv[2] == 'reverse': C0, C1 = C1, C0
	ax = fig.add_subplot(111, aspect='equal')
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	plt.xlabel("")
	plt.ylabel("")
	plt.grid(ls='dotted')
	X = np.arange(-3.0, 3.05, 0.05)
	Y = np.arange(-3.0, 3.05, 0.05)
	Xt, Yt = np.hsplit(train, 2)
	Xm, Ym = np.meshgrid(X, Y)
	levels = 8
	ax.contourf(Xm, Ym, dense, levels, alpha=1, cmap=cm.jet)
	CR = plt.contour(Xm, Ym, dense, levels, colors='black')
	CR.clabel(fontsize=12, fmt='%.2f')
	ax.scatter(Xt, Yt, marker='.', color='white', s=10)
	plt.savefig('plot1.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot1.eps', bbox_inches='tight', pad_inches=0.1)
	plt.delaxes(ax)
	ax = fig.add_subplot(111, aspect='equal')
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	plt.xlabel("")
	plt.ylabel("")
	plt.grid(ls='dotted')
	CR = plt.contour(Xm, Ym, dense, levels, colors='black')
	ax.scatter(mixt0[:,0], mixt0[:,1], marker='.', color=C0, s=10)
	ax.scatter(mixt1[:,0], mixt1[:,1], marker='.', color=C1, s=10)
	ax.scatter([cents[0,0]], [cents[0,1]], marker='o', facecolor=C0, edgecolor='black', lw=2, s=30)
	ax.scatter([cents[1,0]], [cents[1,1]], marker='o', facecolor=C1, edgecolor='black', lw=2, s=30)
	plt.savefig('plot2.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot2.eps', bbox_inches='tight', pad_inches=0.1)
	os.remove('train.dat')
	os.remove('dense.dat')
	os.remove('mixt0.dat')
	os.remove('mixt1.dat')
	os.remove('cents.dat')
	webbrowser.open('file://{}'.format(os.path.realpath('plot1.svg')))
	webbrowser.open('file://{}'.format(os.path.realpath('plot2.svg')))
