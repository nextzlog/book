import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pylab as plt

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
ax = fig.add_subplot(111, aspect=0.01)
X, T = np.loadtxt('dist{}.dat'.format(sys.argv[1]), delimiter=',', unpack=True)
plt.scatter(X, T, marker='*', facecolor='w', edgecolor='k')
W = [float(w) for w in sys.argv[2:]]
if sys.argv[1] == '1':
	Y = [sum([i_w[1] * (x ** i_w[0]) for i_w in enumerate(W)]) for x in X]
else:
	def gauss(x, m, s): return np.exp(-(x - m) * (x - m) / 2 / s / s)
	Y = [W[0] * gauss(x, -5, 1) + W[1] * gauss(x, 5, 1) for x in X]
plt.plot(X, Y, 'r-', lw=2)
plt.grid(ls='dotted')
plt.xlim(-10.0, 10.0)
plt.ylim(-1000, 1000)
plt.xlabel("")
plt.ylabel("")
plt.savefig('plot{}.svg'.format(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot{}.eps'.format(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
os.remove('dist{}.dat'.format(sys.argv[1]))
webbrowser.open('file://{}'.format(os.path.realpath('plot{}.svg'.format(sys.argv[1]))))
