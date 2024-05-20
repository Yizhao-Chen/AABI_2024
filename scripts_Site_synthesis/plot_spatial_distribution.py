#########################################################################################################
#plot the spatial distribution only used the lat output for Fig.S2c 
#for the latitude distribution of BEPS, EC-LUE, GPPinf and ensemble output
#########################################################################################################

import csv
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

root = ".\\"

file_name1 = ".\\Site_synthesis\\stat_summary_Global_ensemble_manu_toshp.csv"

#remove sites without cor result
infile = pd.read_csv(root+file_name1)
#infile1 = infile[~infile['cor'].isin([-999])]

#remove sites without lat
#infile1.dropna(subset=['lat'],how='any',inplace=True)
#infile1.index = range(len(infile1))

#infile1.to_csv(root+file_name1)


#get a df for p < 0.05 positive
infile2 = infile[(infile.out_flag == 1)]
#get a df for p < 0.05 negative
infile3 = infile[(infile.out_flag == -1)]

infile2.index = range(len(infile2))
infile3.index = range(len(infile3))

kk_lat = []
for k in range(0,len(infile.lat)):
    kk_lat.append(int(infile.lat[k]))


kk_lat2 = []
for k in range(0,len(infile2.lat)):
    kk_lat2.append(int(infile2.lat[k]))

kk_lat3 = []
for k in range(0,len(infile3.lat)):
    kk_lat3.append(int(infile3.lat[k]))

#here use the cor from BEPS since only the lat(right) panel is used
#needs to be corrected later if full figure used
x = np.array(infile.cor_BEPS)
y = np.array(kk_lat)

x2 = np.array(infile2.cor_BEPS)
y2 = np.array(kk_lat2)

x3 = np.array(infile3.cor_BEPS)
y3 = np.array(kk_lat3)

# definitions for the axes
left, width = 0.1, 0.65
bottom, height = 0.1, 0.65
spacing = 0.02

rect_scatter = [left, bottom, width, height]
rect_histx = [left, bottom + height + spacing, width, 0.2]
rect_histy = [left + width + spacing, bottom, 0.2, height]

# start with a rectangular Figure
plt.figure(figsize=(8, 8))

ax_scatter = plt.axes(rect_scatter)
ax_scatter.tick_params(direction='in', top=True, right=True)

# the scatter plot:
sc1 = ax_scatter.scatter(x, y,c = 'limegreen',label="all sites")
sc2 = ax_scatter.scatter(x2,y2,c='navy',label="p < 0.05,r > 0")
sc3 = ax_scatter.scatter(x3,y3,c='darkgoldenrod',label="p < 0.05, r < 0")
plt.legend(handles = [sc1,sc2,sc3], loc='lower left')

ax_histx = plt.axes(rect_histx)
ax_histx.tick_params(direction='in', labelbottom=False)
ax_histy = plt.axes(rect_histy)
ax_histy.tick_params(direction='in', labelleft=False)

plt.text(0.5, -0.075, 'r', ha='center', va='center',transform=ax_scatter.transAxes,size = 12)
plt.text(-0.075, 0.5, 'Lat', ha='center', va='center',transform=ax_scatter.transAxes,size = 12,
         rotation=90)
plt.text(-0.075, 0.5, 'Site_num', ha='center', va='center',transform=ax_histx.transAxes,size = 12,
         rotation=90)
plt.text(0.5, -0.075, 'Site_num', ha='center', va='center',transform=ax_histy.transAxes,size = 12)
# plt.text(0.05, 0.9, 'num :', ha='center', va='center', transform=ax.transAxes)
# plt.text(0.05, 0.85, 'corr  :', ha='center', va='center', transform=ax.transAxes)


#now determine nice limits by hand:
#1 entire dataset
binwidth1 = 1
binwidth2 = 0.1
#2 significant postive relationship
binwidth1_2 = 1
binwidth2_2 = 0.1
#3 significant negative relationship
binwidth1_3 = 1
binwidth2_3 = 0.1

#1
y_low = min(kk_lat) - 2
y_high = max(kk_lat) + 18
x_low = infile['cor_BEPS'].min()
x_high = infile['cor_BEPS'].max()


#1
lim1 = np.ceil(np.abs([x, y]).max() / binwidth1) * binwidth1
lim2 = np.ceil(np.abs([x, y]).max() / binwidth2) * binwidth2
ax_scatter.set_xlim((-0.8, 0.8))
ax_scatter.set_ylim((y_low,y_high))

#1
bins1 = np.arange(-lim1, lim1 + binwidth1, binwidth1)
bins2 = np.arange(-lim2, lim2 + binwidth2, binwidth2)
ax_histx.hist(x, bins=bins2)
ax_histy.hist(y, bins=bins1,color =[(0.961,0.478,0.478)],orientation='horizontal')

#2
ax_histx.hist(x2, bins=bins2)
ax_histy.hist(y2, bins=bins1,color =[(0.149,0.451,0)], orientation='horizontal')

#3
ax_histx.hist(x3, bins=bins2)
ax_histy.hist(y3, bins=bins1,color = [(0.66,0,0)],orientation='horizontal')

ax_histx.set_xlim(ax_scatter.get_xlim())
ax_histy.set_ylim(ax_scatter.get_ylim())

ax_histx.grid(False)
ax_histy.grid(False)


#ax_histy.splines[]


plt.rcParams['figure.dpi'] = 300
plt.savefig(root+out_name)
plt.show()



