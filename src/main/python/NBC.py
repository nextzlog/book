# -*- coding: utf-8 -*-

import numpy as np
import math
import sys

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pylab as plt
from matplotlib import cm,ticker

import cartopy.crs as ccrs
import cartopy.io.shapereader as shp

plt.rcParams['font.family'] = 'monospace'

nlim =  46 # 50 + 55.0/60 + 30.0/3600
slim =  30 # 20 + 25.0/60 + 31.0/3600
elim = 146 #156 + 19.0/60 + 00.0/3600
wlim = 128 #122 + 56.0/60 + 01.0/3600

fig = plt.figure(figsize=(10.4,7.8), dpi=80)
ax = fig.add_subplot(111, projection=ccrs.AlbersEqualArea((elim+wlim)/2, (nlim+slim)/2))
ax.outline_patch.set_visible(False)

prefs = dict(np.loadtxt('pref.dat', delimiter=',', dtype='str'))

shape = shp.Reader(shp.natural_earth(resolution='10m', category='cultural', name='admin_1_states_provinces'))
japan = filter(lambda p:p.attributes['admin'] == 'Japan', shape.records())
north = filter(lambda p:p.attributes['name'] == 'Sakhalin', shape.records())

for geom,pref in map(lambda p:(p.geometry,p.attributes['name']), japan):
	ax.add_geometries(geom, crs=ccrs.PlateCarree(), facecolor=prefs[pref], edgecolor='black', lw=1)
ax.add_geometries(north[0].geometry, crs=ccrs.PlateCarree(), facecolor=prefs['Hokkaido'], edgecolor='black', lw=1)

ax.set_extent((wlim, elim, slim, nlim))

plt.savefig('plot%i.svg' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
plt.savefig('plot%i.eps' % int(sys.argv[1]), bbox_inches='tight', pad_inches=0.1)
