####################################################################################################
#Script to find sites with both young and old trees in North America
####################################################################################################

import pandas as pd
import numpy as np

root = ".\\Age_bias_correction\\"
age_input = pd.read_csv(root + "us_age_tree_1982_1986.csv")

site = list()
#split the column names
for i in range(2,(len(age_input.columns))):
    site.append(age_input.columns[i].split("_")[0])

#list for site
site2 = list(set(site))
#create a df for age information
age_df = pd.DataFrame(columns=site2,index=['mean_age','sd_age','max_age','min_age'])


for j in range(len(site2)):
    age_list = list()
    for k in range(2,len(age_input.columns)):
        site3 = age_input.columns[k].split("_")[0]
        if site3 == site2[j]:
            age_list.append(age_input[age_input.columns[k]][0])
    print(site2[j])
    age_df.at['mean_age',site2[j]] = np.mean(age_list)
    age_df.at['sd_age',site2[j]] = np.std(age_list)
    age_df.at['max_age',site2[j]] = np.max(age_list)
    age_df.at['min_age',site2[j]] = np.min(age_list)
    age_df.at['num_tree',site2[j]] = len(age_list)
    #print(age_df[site3][0])

age_df.to_csv(".\\Age_bias_correction\\site_age_mean_sd.csv")
age_df_T = age_df.T
age_df_T.to_csv(".\\Age_bias_correction\\site_age_mean_sd_T.csv")



