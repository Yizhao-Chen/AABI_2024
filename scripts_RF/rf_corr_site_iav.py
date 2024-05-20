##################################################################################
#Script to evaluate the correlationship between IAV of rf del_bio output and obs.
##################################################################################
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

#delta_bio input
path_obs = ".\\RF\\IAV_test\\Target\\"
#predict input path
path_pred = ".\\RF\\IAV_test\\Predict\\"
#output
out_csv = ".\\RF\\Test\\stat_summary_uscanada_site_delta_bio_iav.csv"

files_pred = os.listdir(path_pred)
flength_pred = len(files_pred)

files_obs = os.listdir(path_obs)
flength_obs = len(files_obs)
flist_obs = list()

for j in range(0,flength_obs):
    fname_obs = GFNWE.getFileNameWithoutExtension(files_obs[j]) #without suffix
    sname_obs = fname_obs.split('_')[0]
    flist_obs.append(sname_obs)

#create the output dataframe
stat_summary = pd.DataFrame(columns=['name','r_nt','rsqu_nt','p_nt','num'])

for i in range(0,flength_pred):
    # get the stat summary output
    fname_pred = GFNWE.getFileNameWithoutExtension(files_pred[i]) #without suffix
    #fname1_pred = files_pred[i]
    sname_pred = fname_pred.split('_')[0]

    if sname_pred in flist_obs:
    #read the delta_bio obs. data
        csv_data_obs = pd.read_csv(path_obs + sname_pred + ".csv", low_memory=False)
        #csv_data_obs = pd.read_csv(path_obs + sname_pred + "_delta_bio_mean_test_trim.csv", low_memory=False)
        csv_data_sub_obs = csv_data_obs.drop(columns=['site'],axis=1)

    #read the pred. data
        csv_data_pred = pd.read_csv(path_pred + sname_pred + "_pred.csv",low_memory=False)
        csv_data_sub_pred = pd.DataFrame({'Year':csv_data_pred['Year'], 'Pred':csv_data_pred['predict']})

        list1 = csv_data_pred['Year']         #the year column in FLUXNET data
        list2 = csv_data_obs['Year']        #the year column in LAI data

        test = set(list1)&set(list2)   #combine the two lists, test the relationship

        if len(test) >= 5:
            df_process = pd.merge(csv_data_sub_obs, csv_data_sub_pred)
        else:
            continue
            break

        X = [i for i in range(0, len(df_process))]
        X = np.reshape(X, (len(X), 1))
        y1 = df_process.Target_1_age_cor
        y2 = df_process.Pred
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

        df_process['d_bio_detrend'] = detrend1
        df_process['d_bio_rf_detrend'] = detrend2

           #get Pearson r and sign
        num = len(df_process.d_bio_detrend)
        r1, p1 = stats.pearsonr(df_process.d_bio_detrend, df_process.d_bio_rf_detrend)

        rsqu1 = r1 * r1
            #sort data to 3 digits
        r1 = round(r1,3)    
        p1 = round(p1,3)
            #p2 = round(p2,3)
        rsqu1 = round(rsqu1,3)

        stat_summary.loc[sname_pred] = sname_pred,r1,rsqu1,p1,num
    else:
        print("no")
        print(sname_pred)

print("finish")
stat_summary.to_csv(out_dir+out_csv,sep=',', na_rep="-999")






