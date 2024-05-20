#########################################################################################################
#Script to match the pixels from env_corr to lat/lon file for mapping
#########################################################################################################
import pandas as pd
import numpy as np


#reference point csv
shp_csv = pd.read_csv(".\\Clim_response\\data\\"
                      "reference_point.CSV")
shp_csv.index = shp_csv['POINTID']


#root_temp = "D:\\MEGA\\Git\\Growth_GPP\\Clim_response\\data\\Output_temp\\"
#root_prep = "D:\\MEGA\\Git\\Growth_GPP\\Clim_response\\data\\Output_prep\\"
root_temp = ".\\Clim_response\\Output_temp\\"
root_prep = ".\\Clim_response\\Output_prep\\"

root_out = ".\\Clim_response\\"

#temp
#trendy S2
temp_trendyS2_csv = pd.read_csv(root_temp + "GPP_TRENDYS2_temp_pcorr_tras_processed.csv")
#trendy S3
temp_trendyS3_csv = pd.read_csv(root_temp + "GPP_TRENDYS3_temp_pcorr_tras_processed.csv")
#fluxcom
temp_fluxcom_csv = pd.read_csv(root_temp + "GPP_FLUXCOM_temp_pcorr_tras_processed.csv")
#sink age correct
temp_sink_age_correct_csv = pd.read_csv(root_temp + "AABI_per_area_temp_pcorr_tras_processed.csv")
#RS_mean
temp_RS_mean_csv = pd.read_csv(root_temp + "GPP_RS_mean_temp_pcorr_tras_processed.csv")
#sink per tree
temp_sink_per_tree_csv = pd.read_csv(root_temp + "AABI_per_tree_temp_pcorr_tras_processed.csv")


#convert index to point id, i.e., Variable in the corr files
temp_trendyS2_csv.index = temp_trendyS2_csv['Variable']
temp_trendyS3_csv.index = temp_trendyS3_csv['Variable']
temp_fluxcom_csv.index = temp_fluxcom_csv['Variable']
temp_sink_age_correct_csv.index = temp_sink_age_correct_csv['Variable']
temp_RS_mean_csv.index = temp_RS_mean_csv['Variable']
temp_sink_per_tree_csv.index = temp_sink_per_tree_csv['Variable']

#prep
#trendy S2
prep_trendyS2_csv = pd.read_csv(root_prep + "GPP_TRENDYS2_prep_pcorr_tras_processed.csv")
#trendy S3
prep_trendyS3_csv = pd.read_csv(root_prep + "GPP_TRENDYS3_prep_pcorr_tras_processed.csv")
#fluxcom
prep_fluxcom_csv = pd.read_csv(root_prep + "GPP_FLUXCOM_prep_pcorr_tras_processed.csv")
#sink age correct
prep_sink_age_correct_csv = pd.read_csv(root_prep + "AABI_per_area_prep_pcorr_tras_processed.csv")
#RS_mean
prep_RS_mean_csv = pd.read_csv(root_prep + "GPP_RS_mean_prep_pcorr_tras_processed.csv")
#sink per tree
prep_sink_per_tree_csv = pd.read_csv(root_prep + "AABI_per_tree_prep_pcorr_tras_processed.csv")


#convert index to point id, i.e., Variable in the corr files
prep_trendyS2_csv.index = prep_trendyS2_csv['Variable']
prep_trendyS3_csv.index = prep_trendyS3_csv['Variable']
prep_fluxcom_csv.index = prep_fluxcom_csv['Variable']
prep_sink_age_correct_csv.index = prep_sink_age_correct_csv['Variable']
prep_RS_mean_csv.index = prep_RS_mean_csv['Variable']
prep_sink_per_tree_csv.index = prep_sink_per_tree_csv['Variable']


#create lists
list_temp_trendyS2 = list(temp_trendyS2_csv['Variable'])
list_temp_trendyS3 = list(temp_trendyS3_csv['Variable'])
list_temp_fluxcom =  list(temp_fluxcom_csv['Variable'])
list_temp_sink_age = list(temp_sink_age_correct_csv['Variable'])
list_temp_RS_mean = list(temp_RS_mean_csv['Variable'])
list_temp_sink_per_tree = list(temp_sink_per_tree_csv['Variable'])

list_prep_trendyS2 = list(prep_trendyS2_csv['Variable'])
list_prep_trendyS3 = list(prep_trendyS3_csv['Variable'])
list_prep_fluxcom =  list(prep_fluxcom_csv['Variable'])
list_prep_sink_age = list(prep_sink_age_correct_csv['Variable'])
list_prep_RS_mean = list(prep_RS_mean_csv['Variable'])
list_prep_sink_per_tree = list(prep_sink_per_tree_csv['Variable'])

#list_shp = list(shp_csv['Point_id'])
list_shp = list(shp_csv['POINTID'])


#temp
#trendyS2
shp_csv_trendyS2 = shp_csv
shp_csv_trendyS2['Corr'] = np.NaN
shp_csv_trendyS2['T_start'] = np.NaN
shp_csv_trendyS2['T_end'] = np.NaN
for i in range(len(list_temp_trendyS2)):
    if list_temp_trendyS2[i] in list_shp:
        shp_csv_trendyS2['Corr'].at[list_temp_trendyS2[i]] = temp_trendyS2_csv['maximal_calculated_metric'].at[list_temp_trendyS2[i]]
        shp_csv_trendyS2['T_start'].at[list_temp_trendyS2[i]] = temp_trendyS2_csv['start'].at[list_temp_trendyS2[i]]
        shp_csv_trendyS2['T_end'].at[list_temp_trendyS2[i]] = temp_trendyS2_csv['end'].at[list_temp_trendyS2[i]]

shp_csv_trendyS2.to_csv(root_out + "GPP_TRENDYS2_temp_pcorr_tras_processed_toshp.csv")

shp_csv_trendyS2['Corr'] = np.NaN
shp_csv_trendyS2['T_start'] = np.NaN
shp_csv_trendyS2['T_end'] = np.NaN

#trendyS3
shp_csv_trendyS3 = shp_csv
shp_csv_trendyS3['Corr'] = np.NaN
shp_csv_trendyS3['T_start'] = np.NaN
shp_csv_trendyS3['T_end'] = np.NaN
for i in range(len(list_temp_trendyS3)):
    if list_temp_trendyS3[i] in list_shp:
        shp_csv_trendyS3['Corr'].at[list_temp_trendyS3[i]] = temp_trendyS3_csv['maximal_calculated_metric'].at[list_temp_trendyS3[i]]
        shp_csv_trendyS3['T_start'].at[list_temp_trendyS3[i]] = temp_trendyS3_csv['start'].at[list_temp_trendyS3[i]]
        shp_csv_trendyS3['T_end'].at[list_temp_trendyS3[i]] = temp_trendyS3_csv['end'].at[list_temp_trendyS3[i]]

shp_csv_trendyS3.to_csv(root_out + "GPP_TRENDYS3_temp_pcorr_tras_processed_toshp.csv")

shp_csv_trendyS3['Corr'] = np.NaN
shp_csv_trendyS3['T_start'] = np.NaN
shp_csv_trendyS3['T_end'] = np.NaN

#fluxcom
shp_csv_fluxcom = shp_csv
shp_csv_fluxcom['Corr'] = np.NaN
shp_csv_fluxcom['T_start'] = np.NaN
shp_csv_fluxcom['T_end'] = np.NaN
for i in range(len(list_temp_fluxcom)):
    if list_temp_fluxcom[i] in list_shp:
        shp_csv_fluxcom['Corr'].at[list_temp_fluxcom[i]] = temp_fluxcom_csv['maximal_calculated_metric'].at[list_temp_fluxcom[i]]
        shp_csv_fluxcom['T_start'].at[list_temp_fluxcom[i]] = temp_fluxcom_csv['start'].at[list_temp_fluxcom[i]]
        shp_csv_fluxcom['T_end'].at[list_temp_fluxcom[i]] = temp_fluxcom_csv['end'].at[list_temp_fluxcom[i]]

shp_csv_fluxcom.to_csv(root_out + "GPP_FLUXCOM_temp_pcorr_tras_processed_toshp.csv")

shp_csv_fluxcom['Corr'] = np.NaN
shp_csv_fluxcom['T_start'] = np.NaN
shp_csv_fluxcom['T_end'] = np.NaN

#sink per tree
shp_csv_sink_per_tree = shp_csv
shp_csv_sink_per_tree['Corr'] = np.NaN
shp_csv_sink_per_tree['T_start'] = np.NaN
shp_csv_sink_per_tree['T_end'] = np.NaN
for i in range(len(list_temp_sink_per_tree)):
    if list_temp_sink_per_tree[i] in list_shp:
        shp_csv_sink_per_tree['Corr'].at[list_temp_sink_per_tree[i]] = temp_sink_per_tree_csv['maximal_calculated_metric'].at[list_temp_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_start'].at[list_temp_sink_per_tree[i]] = temp_sink_per_tree_csv['start'].at[list_temp_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_end'].at[list_temp_sink_per_tree[i]] = temp_sink_per_tree_csv['end'].at[list_temp_sink_per_tree[i]]

shp_csv_sink_per_tree.to_csv(root_out + "AABI_per_tree_temp_pcorr_tras_processed_toshp.csv")

shp_csv_sink_per_tree['Corr'] = np.NaN
shp_csv_sink_per_tree['T_start'] = np.NaN
shp_csv_sink_per_tree['T_end'] = np.NaN

#RS_mean
shp_csv_RS_mean = shp_csv
shp_csv_RS_mean['Corr'] = np.NaN
shp_csv_RS_mean['T_start'] = np.NaN
shp_csv_RS_mean['T_end'] = np.NaN
for i in range(len(list_temp_RS_mean)):
    if list_temp_RS_mean[i] in list_shp:
        shp_csv_RS_mean['Corr'].at[list_temp_RS_mean[i]] = temp_RS_mean_csv['maximal_calculated_metric'].at[list_temp_RS_mean[i]]
        shp_csv_RS_mean['T_start'].at[list_temp_RS_mean[i]] = temp_RS_mean_csv['start'].at[list_temp_RS_mean[i]]
        shp_csv_RS_mean['T_end'].at[list_temp_RS_mean[i]] = temp_RS_mean_csv['end'].at[list_temp_RS_mean[i]]

shp_csv_RS_mean.to_csv(root_out + "GPP_RS_mean_temp_pcorr_tras_processed_toshp.csv")

shp_csv_RS_mean['Corr'] = np.NaN
shp_csv_RS_mean['T_start'] = np.NaN
shp_csv_RS_mean['T_end'] = np.NaN

#sink per area
shp_csv_sink_age = shp_csv
shp_csv_sink_age['Corr'] = np.NaN
shp_csv_sink_age['T_start'] = np.NaN
shp_csv_sink_age['T_end'] = np.NaN
for i in range(len(list_temp_sink_age)):
    if list_temp_sink_age[i] in list_shp:
        shp_csv_sink_age['Corr'].at[list_temp_sink_age[i]] = temp_sink_age_correct_csv['maximal_calculated_metric'].at[list_temp_sink_age[i]]
        shp_csv_sink_age['T_start'].at[list_temp_sink_age[i]] = temp_sink_age_correct_csv['start'].at[list_temp_sink_age[i]]
        shp_csv_sink_age['T_end'].at[list_temp_sink_age[i]] = temp_sink_age_correct_csv['end'].at[list_temp_sink_age[i]]

shp_csv_sink_age.to_csv(root_out + "AABI_per_area_temp_pcorr_tras_processed_toshp.csv")

shp_csv_sink_age['Corr'] = np.NaN
shp_csv_sink_age['T_start'] = np.NaN
shp_csv_sink_age['T_end'] = np.NaN

#prep
#trendyS2
shp_csv_trendyS2 = shp_csv
shp_csv_trendyS2['Corr'] = np.NaN
shp_csv_trendyS2['T_start'] = np.NaN
shp_csv_trendyS2['T_end'] = np.NaN
for i in range(len(list_prep_trendyS2)):
    if list_prep_trendyS2[i] in list_shp:
        shp_csv_trendyS2['Corr'].at[list_prep_trendyS2[i]] = prep_trendyS2_csv['maximal_calculated_metric'].at[list_prep_trendyS2[i]]
        shp_csv_trendyS2['T_start'].at[list_prep_trendyS2[i]] = prep_trendyS2_csv['start'].at[list_prep_trendyS2[i]]
        shp_csv_trendyS2['T_end'].at[list_prep_trendyS2[i]] = prep_trendyS2_csv['end'].at[list_prep_trendyS2[i]]

shp_csv_trendyS2.to_csv(root_out + "GPP_TRENDYS2_prep_pcorr_tras_processed_toshp.csv")

shp_csv_trendyS2['Corr'] = np.NaN
shp_csv_trendyS2['T_start'] = np.NaN
shp_csv_trendyS2['T_end'] = np.NaN

#trendyS3
shp_csv_trendyS3 = shp_csv
shp_csv_trendyS3['Corr'] = np.NaN
shp_csv_trendyS3['T_start'] = np.NaN
shp_csv_trendyS3['T_end'] = np.NaN
for i in range(len(list_prep_trendyS3)):
    if list_prep_trendyS3[i] in list_shp:
        shp_csv_trendyS3['Corr'].at[list_prep_trendyS3[i]] = prep_trendyS3_csv['maximal_calculated_metric'].at[list_prep_trendyS3[i]]
        shp_csv_trendyS3['T_start'].at[list_prep_trendyS3[i]] = prep_trendyS3_csv['start'].at[list_prep_trendyS3[i]]
        shp_csv_trendyS3['T_end'].at[list_prep_trendyS3[i]] = prep_trendyS3_csv['end'].at[list_prep_trendyS3[i]]

shp_csv_trendyS3.to_csv(root_out + "GPP_TRENDYS3_prep_pcorr_tras_processed_toshp.csv")

shp_csv_trendyS3['Corr'] = np.NaN
shp_csv_trendyS3['T_start'] = np.NaN
shp_csv_trendyS3['T_end'] = np.NaN

#fluxcom
shp_csv_fluxcom = shp_csv
shp_csv_fluxcom['Corr'] = np.NaN
shp_csv_fluxcom['T_start'] = np.NaN
shp_csv_fluxcom['T_end'] = np.NaN
for i in range(len(list_prep_fluxcom)):
    if list_prep_fluxcom[i] in list_shp:
        shp_csv_fluxcom['Corr'].at[list_prep_fluxcom[i]] = prep_fluxcom_csv['maximal_calculated_metric'].at[list_prep_fluxcom[i]]
        shp_csv_fluxcom['T_start'].at[list_prep_fluxcom[i]] = prep_fluxcom_csv['start'].at[list_prep_fluxcom[i]]
        shp_csv_fluxcom['T_end'].at[list_prep_fluxcom[i]] = prep_fluxcom_csv['end'].at[list_prep_fluxcom[i]]

shp_csv_fluxcom.to_csv(root_out + "GPP_FLUXCOM_prep_pcorr_tras_processed_toshp.csv")

shp_csv_fluxcom['Corr'] = np.NaN
shp_csv_fluxcom['T_start'] = np.NaN
shp_csv_fluxcom['T_end'] = np.NaN

#sink per tree
shp_csv_sink_per_tree = shp_csv
shp_csv_sink_per_tree['Corr'] = np.NaN
shp_csv_sink_per_tree['T_start'] = np.NaN
shp_csv_sink_per_tree['T_end'] = np.NaN
for i in range(len(list_prep_sink_per_tree)):
    if list_prep_sink_per_tree[i] in list_shp:
        shp_csv_sink_per_tree['Corr'].at[list_prep_sink_per_tree[i]] = prep_sink_per_tree_csv['maximal_calculated_metric'].at[list_prep_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_start'].at[list_prep_sink_per_tree[i]] = prep_sink_per_tree_csv['start'].at[list_prep_sink_per_tree[i]]
        shp_csv_sink_per_tree['T_end'].at[list_prep_sink_per_tree[i]] = prep_sink_per_tree_csv['end'].at[list_prep_sink_per_tree[i]]

shp_csv_sink_per_tree.to_csv(root_out + "AABI_per_tree_prep_pcorr_tras_processed_toshp.csv")

shp_csv_sink_per_tree['Corr'] = np.NaN
shp_csv_sink_per_tree['T_start'] = np.NaN
shp_csv_sink_per_tree['T_end'] = np.NaN

#RS_mean
shp_csv_RS_mean = shp_csv
shp_csv_RS_mean['Corr'] = np.NaN
shp_csv_RS_mean['T_start'] = np.NaN
shp_csv_RS_mean['T_end'] = np.NaN
for i in range(len(list_prep_RS_mean)):
    if list_prep_RS_mean[i] in list_shp:
        shp_csv_RS_mean['Corr'].at[list_prep_RS_mean[i]] = prep_RS_mean_csv['maximal_calculated_metric'].at[list_prep_RS_mean[i]]
        shp_csv_RS_mean['T_start'].at[list_prep_RS_mean[i]] = prep_RS_mean_csv['start'].at[list_prep_RS_mean[i]]
        shp_csv_RS_mean['T_end'].at[list_prep_RS_mean[i]] = prep_RS_mean_csv['end'].at[list_prep_RS_mean[i]]

shp_csv_RS_mean.to_csv(root_out + "GPP_RS_mean_prep_pcorr_tras_processed_toshp.csv")

shp_csv_RS_mean['Corr'] = np.NaN
shp_csv_RS_mean['T_start'] = np.NaN
shp_csv_RS_mean['T_end'] = np.NaN

#sink per area
shp_csv_sink_age = shp_csv
shp_csv_sink_age['Corr'] = np.NaN
shp_csv_sink_age['T_start'] = np.NaN
shp_csv_sink_age['T_end'] = np.NaN
for i in range(len(list_prep_sink_age)):
    if list_prep_sink_age[i] in list_shp:
        shp_csv_sink_age['Corr'].at[list_prep_sink_age[i]] = prep_sink_age_correct_csv['maximal_calculated_metric'].at[list_prep_sink_age[i]]
        shp_csv_sink_age['T_start'].at[list_prep_sink_age[i]] = prep_sink_age_correct_csv['start'].at[list_prep_sink_age[i]]
        shp_csv_sink_age['T_end'].at[list_prep_sink_age[i]] = prep_sink_age_correct_csv['end'].at[list_prep_sink_age[i]]

shp_csv_sink_age.to_csv(root_out + "AABI_per_area_prep_pcorr_tras_processed_toshp.csv")

shp_csv_sink_age['Corr'] = np.NaN
shp_csv_sink_age['T_start'] = np.NaN
shp_csv_sink_age['T_end'] = np.NaN
