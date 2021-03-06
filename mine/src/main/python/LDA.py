import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pylab as plt
import cartopy.crs as ccrs
import cartopy.io.shapereader as shp

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure(figsize=(10.4,7.8), dpi=80)
ax = fig.add_subplot(111, projection=ccrs.AlbersEqualArea((elim+wlim)/2, (nlim+slim)/2))
ax.outline_patch.set_visible(False)
prefs = dict(np.loadtxt('pref.dat', delimiter=',', dtype='str'))
nlim =  50 + 55.0/60 + 30.0/3600
slim =  20 + 25.0/60 + 31.0/3600
elim = 156 + 19.0/60 + 00.0/3600
wlim = 122 + 56.0/60 + 01.0/3600
ax.set_extent((wlim, elim, slim, nlim))
colors = ['red', 'green', 'yellow', 'navy', 'orange']
country = shp.Reader(shp.natural_earth(resolution='10m', category='cultural', name='admin_1_states_provinces'))
invaded = shp.Reader(shp.natural_earth(resolution='10m', category='cultural', name='admin_0_disputed_areas'))
for pref in [p for p in country.records() if p.attributes['admin'] == 'Japan']:
	ax.add_geometries(pref.geometry, crs=ccrs.PlateCarree(), facecolor=colors[int(prefs[pref])], edgecolor='black', lw=1)
for land in [p for p in invaded.records() if 'Claimed by Japan' in p.attributes['NOTE_BRK']]:
	if pref.attributes['ADMIN'] == 'Russia':
		ax.add_geometries(pref.geometry, crs=ccrs.PlateCarree(), facecolor=colors[int(prefs['Hokkaido'])], edgecolor='black', lw=1)
	else:
		ax.add_geometries(pref.geometry, crs=ccrs.PlateCarree(), facecolor=colors[int(prefs['Shimane'])], edgecolor='black', lw=1)
plt.savefig('plot{}.svg'.format(sys.argv[1]), bbox_inches='tight', pad_inches=0)
plt.savefig('plot{}.eps'.format(sys.argv[1]), bbox_inches='tight', pad_inches=0)
os.remove('pref.dat')
webbrowser.open('file://{}'.format(os.path.realpath('plot{}.svg'.format(sys.argv[1]))))
