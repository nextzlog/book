import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt
import math
import sys

plt.rcParams['font.family'] = 'monospace'

fig = plt.figure()

C0, C1 = 'red', 'blue'

if sys.argv[1] == 'KM':
	mixt0 = np.loadtxt('mixt0.dat', delimiter=',')
	mixt1 = np.loadtxt('mixt1.dat', delimiter=',')
	cents = np.loadtxt('cents.dat', delimiter=',')

	rect = fig.add_subplot(111, aspect='equal')
	
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	
	plt.xlabel("")
	plt.ylabel("")
	
	plt.grid()

	rect.scatter(mixt0[:,0], mixt0[:,1], marker='.', color=C0, s=10)
	rect.scatter(mixt1[:,0], mixt1[:,1], marker='.', color=C1, s=10)
	
	rect.scatter([cents[0,0]], [cents[0,1]], marker='o', facecolor=C0, edgecolor='black', lw=2, s=30)
	rect.scatter([cents[1,0]], [cents[1,1]], marker='o', facecolor=C1, edgecolor='black', lw=2, s=30)
	
	plt.savefig('plot0.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot0.eps', bbox_inches='tight', pad_inches=0.1)

else:
	mixt0 = np.loadtxt('mixt0.dat', delimiter=',')
	mixt1 = np.loadtxt('mixt1.dat', delimiter=',')
	dense = np.loadtxt('dense.dat', delimiter=',')
	train = np.loadtxt('train.dat', delimiter=',')
	cents = np.loadtxt('cents.dat', delimiter=',')
	
	if sys.argv[2] == 'reverse': C0, C1 = C1, C0

	rect = fig.add_subplot(111, aspect='equal')
	
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	
	plt.xlabel("")
	plt.ylabel("")
	
	plt.grid()

	X = np.arange(-3.0, 3.05, 0.05)
	Y = np.arange(-3.0, 3.05, 0.05)
	
	(Xt, Yt) = np.hsplit(train, 2)
	
	Xm, Ym = np.meshgrid(X, Y)
	
	levels = 8
	
	rect.contourf(Xm, Ym, dense, levels, alpha=1, cmap=cm.jet)
	CR = plt.contour(Xm, Ym, dense, levels, colors='black')
	CR.clabel(fontsize=12, fmt='%.2f')
	
	rect.scatter(Xt, Yt, marker='.', color='white', s=10)
	
	plt.savefig('plot1.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot1.eps', bbox_inches='tight', pad_inches=0.1)
	plt.delaxes(rect)

	rect = fig.add_subplot(111, aspect='equal')
	
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	
	plt.xlabel("")
	plt.ylabel("")
	
	plt.grid()

	CR = plt.contour(Xm, Ym, dense, levels, colors='black')
	rect.scatter(mixt0[:,0], mixt0[:,1], marker='.', color=C0, s=10)
	rect.scatter(mixt1[:,0], mixt1[:,1], marker='.', color=C1, s=10)
	
	rect.scatter([cents[0,0]], [cents[0,1]], marker='o', facecolor=C0, edgecolor='black', lw=2, s=30)
	rect.scatter([cents[1,0]], [cents[1,1]], marker='o', facecolor=C1, edgecolor='black', lw=2, s=30)
		
	plt.savefig('plot2.svg', bbox_inches='tight', pad_inches=0.1)
	plt.savefig('plot2.eps', bbox_inches='tight', pad_inches=0.1)
