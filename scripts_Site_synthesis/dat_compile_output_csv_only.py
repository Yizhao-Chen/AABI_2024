#########################################################################################################
#Perform site level correlation analysis between detrended GPP and tree ring width
#########################################################################################################

import os
import csv
import pandas as pd
from scipy import stats
import numpy as np
import pylab
import GFNWE

from sklearn.linear_model import LinearRegression
from sklearn import linear_model

from matplotlib import pyplot

from pylab import figure,text,scatter,show,plot

#ITRDB data source
path = ".\\Site_synthesis\\ITRDB_de\\europe_ITRDB_de\\"
#Gentree data source
#path = ".\\Site_synthesis\\GenTree_de\\"

#GPP datasets
#BEPS as an example
out_dir = ".\\Site_synthesis\\"
out_csv = "stat_summary_europe_ITRDB_de_GPP_BEPS.csv"
GPP_csv = ".\\Site_synthesis\\BEPS_annual_all_Tras.csv"

input_data = pd.read_csv(GPP_csv)

input_df = pd.DataFrame(input_data)
col_names = input_df.columns.values

files = os.listdir(path)
flength = len(files)

stat_summary = pd.DataFrame(columns=['name', 'cor', 'r2','p','num'])

for i in range(0,flength):
    # get the stat summary output

    fname = GFNWE.getFileNameWithoutExtension(files[i]) #without suffix
    fname1 = files[i]
    sname = fname.split('_')[0]

    csv_data = pd.read_csv(path + fname1, low_memory=False)

    csv_df = pd.DataFrame(csv_data)
    csv_df = csv_df.drop(csv_df.columns[0], axis=1)  #drop the first column without index

    for j in range(0,len(col_names)):
        if sname in col_names[j]:
            landsat_df1 = pd.DataFrame(landsat_df,columns=['Year',col_names[j]])

            list1 = csv_df['Year']
            list2 = landsat_df1['Year']

            test = set(list1)&set(list2)

            if len(test) >= 5:
                TR_GPP = pd.merge(csv_df, landsat_df1)
            else:
                continue
                break
            #remove linear trend
            X = [i for i in range(0, len(TR_GPP))]
            X = np.reshape(X, (len(X), 1))
            y1 = TR_GPP.TRW_mean
            y2 = TR_GPP[col_names[j]]
            model1 = LinearRegression()
            model2 = LinearRegression()
            model1.fit(X, y1)
            model2.fit(X, y2)
            trend1 = model1.predict(X)
            trend2 = model2.predict(X)
            detrend1 = [y1[i]-trend1[i] for i in range(0, len(X))]
            detrend2 = [y2[i]-trend2[i] for i in range(0, len(X))]

            #get the linear regression model
            reg = linear_model.LinearRegression()
            ar1 = np.reshape(detrend1,(-1,1))
            ar2 = np.reshape(detrend2,(-1,1))
            reg.fit(ar1,ar2)

            TR_GPP['TRW_detrend'] = detrend1
            TR_GPP['GPP_detrend'] = detrend2

           #get Pearson r and sign
            num = len(TR_GPP.TRW_detrend)
            #corr = TR_GPP.TRW_detrend.corr(TR_GPP.GPP_detrend)
            r, p = stats.pearsonr(TR_GPP.TRW_detrend, TR_GPP.GPP_detrend)
            r2 = r * r
            #sort data to 3 digits
            r = round(r,3)
            p = round(p,3)
            r2 = round(r2,3)


            stat_summary.loc[sname] = sname,r,r2,p,num

        else:
            continue
            break

print("finish")
stat_summary.to_csv(out_dir+out_csv,sep=',', na_rep="-999")






