#########################################################################################################
#The script is to extract sites with low CVs
#########################################################################################################

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import rcParams, cycler
from matplotlib import cm
from collections import OrderedDict

#site csv input
site = pd.read_csv(".\\Site_synthesis\\"
                   "Global_sites_toshp.csv")

#cv input
GPP_EC_LUE_input_82_10 = pd.read_csv(".\\Site_synthesis\\"
                               "EC_LUE_82_10_0.05_cleaned_focal_33_cv_Tras.csv")
GPP_BEPS_input_82_10 = pd.read_csv(".\\Site_synthesis\\"
                               "BEPS_82_10_0.07272_cleaned_focal_33_cv_Tras.csv")
GPPinf_input_82_10 = pd.read_csv(".\\Site_synthesis\\"
                               "GPPinf_82_10_0.05_cleaned_focal_33_cv_Tras.csv")


GPP_EC_LUE_cv_82_10 = pd.DataFrame(GPP_EC_LUE_input_82_10)
GPP_BEPS_cv_82_10 = pd.DataFrame(GPP_BEPS_input_82_10)
GPPinf_cv_82_10 = pd.DataFrame(GPPinf_input_82_10)

GPP_EC_LUE_cv_corr_82_10 = GPP_EC_LUE_cv_82_10.replace([-9999],np.nan)
GPP_BEPS_cv_corr_82_10 = GPP_BEPS_cv_82_10.replace([-9999],np.nan)
GPPinf_cv_corr_82_10 = GPPinf_cv_82_10.replace([-9999],np.nan)

#get mean for each column
test_EC_LUE_82_10 = GPP_EC_LUE_cv_corr_82_10.mean()
test_BEPS_82_10 = GPP_BEPS_cv_corr_82_10.mean()
test_GPPinf_82_10 = GPPinf_cv_corr_82_10.mean()

#put it in the dataframe
GPP_EC_LUE_cv_corr_82_10.loc['mean'] = test_EC_LUE_82_10
GPP_BEPS_cv_corr_82_10.loc['mean'] = test_BEPS_82_10
GPPinf_cv_corr_82_10.loc['mean'] = test_GPPinf_82_10

#sort data decending
GPP_EC_LUE_cv_corr_82_10 = GPP_EC_LUE_cv_corr_82_10.sort_values(axis=1,by='mean',ascending=False)
GPP_BEPS_cv_corr_82_10 = GPP_BEPS_cv_corr_82_10.sort_values(axis=1,by='mean',ascending=False)
GPPinf_cv_corr_82_10 = GPPinf_cv_corr_82_10.sort_values(axis=1,by='mean',ascending=False)

#drop the mean line
GPP_EC_LUE_cv_corr_82_10 = GPP_EC_LUE_cv_corr_82_10.drop(['mean'],axis=0)
GPP_BEPS_cv_corr_82_10 = GPP_BEPS_cv_corr_82_10.drop(['mean'],axis=0)
GPPinf_cv_corr_82_10 = GPPinf_cv_corr_82_10.drop(['mean'],axis=0)

#use mean values
mean_EC_LUE_cv_corr_82_10 = GPP_EC_LUE_cv_corr_82_10.mean()
mean_BEPS_cv_corr_82_10 = GPP_BEPS_cv_corr_82_10.mean()
mean_GPPinf_cv_corr_82_10 = GPPinf_cv_corr_82_10.mean()

#mean cv > 0.4 list
EC_LUE_list_mean_82_10 = list()
BEPS_list_mean_82_10 = list()
GPPinf_list_mean_82_10= list()

#use mean values
for i in range(0,len(mean_EC_LUE_cv_corr_82_10)):
    if mean_EC_LUE_cv_corr_82_10.iloc[i] > 0.4 or mean_EC_LUE_cv_corr_82_10.iloc[i] < 0:
        EC_LUE_list_mean_82_10.append(mean_EC_LUE_cv_corr_82_10.index[i])

for i in range(0,len(mean_BEPS_cv_corr_82_10)):
    if mean_BEPS_cv_corr_82_10.iloc[i] > 0.4 or mean_BEPS_cv_corr_82_10.iloc[i] < 0:
        BEPS_list_mean_82_10.append(mean_BEPS_cv_corr_82_10.index[i])

for i in range(0,len(mean_GPPinf_cv_corr_82_10)):
    if mean_GPPinf_cv_corr_82_10.iloc[i] > 0.4 or mean_GPPinf_cv_corr_82_10.iloc[i] < 0:
        GPPinf_list_mean_82_10.append(mean_GPPinf_cv_corr_82_10.index[i])


#consider 2 from the 3 datasets
output_list1_mean1 = list(set(EC_LUE_list_mean_82_10) & set(BEPS_list_mean_82_10))
output_list1_mean2 = list(set(EC_LUE_list_mean_82_10) & set(GPPinf_list_mean_82_10))
output_list1_mean3 = list(set(BEPS_list_mean_82_10) & set(GPPinf_list_mean_82_10))
out_list1_mean4 = list(set(output_list1_mean1) | set(output_list1_mean2) | set(output_list1_mean3))

#To clean the env_corr files
#extract the sites
#since the raw sites are the same, only calculate one and output
sname_input_82_10 = list(GPP_EC_LUE_input_82_10.columns[1:len(GPP_EC_LUE_input_82_10.columns)])

site.index = site.id

for i in range(0,len(out_list1_mean4)):
    if out_list1_mean4[i] in sname_input_82_10:
        site.drop(out_list1_mean4[i],axis=0,inplace=True)

site.to_csv(".\\Site_synthesis\\"
                       "cv_2_3.csv")


