#####################################################################################################################
#The script is to test the control fraction of pixels to IAV for both GPP and AABI
#####################################################################################################################

import pandas as pd

root = ".\\IAV_contribution\\"

input_con = pd.read_csv(root +"IAV_contribution_combined_summary_toshp.CSV")
sub_AABI = pd.DataFrame(input_con,columns=["lat","lon","POINTID","IAV_con_aabi_mean"])
sub_RS = pd.DataFrame(input_con,columns=["lat","lon","POINTID","IAV_con_RS_mean_GPP"])
sub_FLUXCOM = pd.DataFrame(input_con,columns=["lat","lon","POINTID","IAV_con_FLUXCOM_GPP"])
sub_TRENDY = pd.DataFrame(input_con,columns=["lat","lon","POINTID","IAV_con_TRENDYmean_GPP"])

#point data file input
data_AABI_all_mean = pd.read_csv("point_AABI_per_area_1984_2010_all_mean_detrend.csv")
data_trendys2 = pd.read_csv("point_trendys2_gpp_1984_2010_detrend.csv")
data_trendys3 = pd.read_csv("point_trendys3_gpp_1984_2010_detrend.csv")
data_RS = pd.read_csv("point_RS_mean_gpp_1984_2010_detrend.csv")
data_FLUXCOM = pd.read_csv("point_FLUXCOM_gpp_1984_2010_detrend.csv")

#data sort
sub_AABI.sort_values("IAV_con_aabi_mean",ascending=False,inplace=True)
sub_RS.sort_values("IAV_con_RS_mean_GPP",ascending=False,inplace=True)
sub_FLUXCOM.sort_values("IAV_con_FLUXCOM_GPP",ascending=False,inplace=True)
sub_TRENDY.sort_values("IAV_con_TRENDYmean_GPP",ascending=False,inplace=True)

#get number for corresponding percentage
#0.9 as an example
perc = 0.9 #0.25 0.3 0.4 ...1.0
num_AABI = int(len(sub_AABI.IAV_con_aabi_mean)*perc)
num_RS = int(len(sub_RS.IAV_con_RS_mean_GPP)*perc)
num_FLUXCOM = int(len(sub_FLUXCOM.IAV_con_FLUXCOM_GPP)*perc)
num_TRENDY = int(len(sub_TRENDY.IAV_con_TRENDYmean_GPP)*perc)

#get the subset df
AABI_con_sub = sub_AABI.head(num_AABI)
AABI_list_sub = list(AABI_con_sub.POINTID)

RS_con_sub = sub_RS.head(num_RS)
RS_list_sub = list(RS_con_sub.POINTID)

FLUXCOM_con_sub = sub_FLUXCOM.head(num_FLUXCOM)
FLUXCOM_list_sub = list(FLUXCOM_con_sub.POINTID)

TRENDY_con_sub = sub_TRENDY.head(num_TRENDY)
TRENDY_list_sub = list(TRENDY_con_sub.POINTID)

#mean data
data_AABI_all_mean_top = data_AABI_all_mean.drop(data_AABI_all_mean.columns[1:],axis=1)
data_trendys2_top = data_trendys2.drop(data_trendys2.columns[1:],axis=1)
data_trendys3_top = data_trendys3.drop(data_trendys3.columns[1:],axis=1)
data_RS_top = data_RS.drop(data_RS.columns[1:],axis=1)
data_FLUXCOM_top = data_FLUXCOM.drop(data_FLUXCOM.columns[1:],axis=1)

data_AABI_all_mean_list = list(data_AABI_all_mean.columns)
data_trendys2_list = list(data_trendys2.columns)
data_trendys3_list = list(data_trendys3.columns)
data_RS_list = list(data_RS.columns)
data_FLUXCOM_list = list(data_FLUXCOM.columns)

for i in range(len(AABI_list_sub)):
    str_num = str("X"+ str(AABI_list_sub[i]))
    if str_num in data_AABI_all_mean_list:
        data_AABI_all_mean_top = pd.concat([data_AABI_all_mean_top, data_AABI_all_mean[str_num]], axis=1)
        data_AABI_all_mean.drop(columns=str_num,inplace=True)

for i in range(len(TRENDY_list_sub)):
    str_num = str("X"+ str(TRENDY_list_sub[i]))
    if str_num in data_trendys2_list:
        data_trendys2_top = pd.concat([data_trendys2_top, data_trendys2[str_num]], axis=1)
        data_trendys2.drop(columns=str_num,inplace=True)

for i in range(len(TRENDY_list_sub)):
    str_num = str("X"+ str(TRENDY_list_sub[i]))
    if str_num in data_trendys3_list:
        data_trendys3_top = pd.concat([data_trendys3_top, data_trendys3[str_num]], axis=1)
        data_trendys3.drop(columns=str_num,inplace=True)

for i in range(len(RS_list_sub)):
    str_num = str("X"+ str(RS_list_sub[i]))
    if str_num in data_RS_list:
        data_RS_top = pd.concat([data_RS_top, data_RS[str_num]], axis=1)
        data_RS.drop(columns=str_num,inplace=True)

for i in range(len(FLUXCOM_list_sub)):
    str_num = str("X"+ str(FLUXCOM_list_sub[i]))
    if str_num in data_FLUXCOM_list:
        data_FLUXCOM_top = pd.concat([data_FLUXCOM_top, data_FLUXCOM[str_num]], axis=1)
        data_FLUXCOM.drop(columns=str_num,inplace=True)

#drop the original SUM column
data_AABI_all_mean = data_AABI_all_mean.drop(columns = ["SUM"])

data_trendys2 = data_trendys2.drop(columns = ["SUM"])
data_trendys3 = data_trendys3.drop(columns = ["SUM"])
data_FLUXCOM = data_FLUXCOM.drop(columns = ["SUM"])
data_RS = data_RS.drop(columns = ["SUM"])

#recalculate the sum
data_AABI_all_mean['SUM'] = data_AABI_all_mean.iloc[:,1:len(data_AABI_all_mean.columns)].sum(axis = 1)
data_trendys2['SUM'] = data_trendys2.iloc[:,1:len(data_trendys2.columns)].sum(axis = 1)
data_trendys3['SUM'] = data_trendys3.iloc[:,1:len(data_trendys3.columns)].sum(axis = 1)
data_FLUXCOM['SUM'] = data_FLUXCOM.iloc[:,1:len(data_FLUXCOM.columns)].sum(axis = 1)
data_RS['SUM'] = data_RS.iloc[:,1:len(data_RS.columns)].sum(axis = 1)

data_AABI_all_mean_top['SUM'] = data_AABI_all_mean_top.iloc[:,1:len(data_AABI_all_mean_top.columns)].sum(axis = 1)
data_trendys2_top['SUM'] = data_trendys2_top.iloc[:,1:len(data_trendys2_top.columns)].sum(axis = 1)
data_trendys3_top['SUM'] = data_trendys3_top.iloc[:,1:len(data_trendys3_top.columns)].sum(axis = 1)
data_FLUXCOM_top['SUM'] = data_FLUXCOM_top.iloc[:,1:len(data_FLUXCOM_top.columns)].sum(axis = 1)
data_RS_top['SUM'] = data_RS_top.iloc[:,1:len(data_RS_top.columns)].sum(axis = 1)

out_all_df = pd.DataFrame({"Year":data_trendys2_top['Year'],"trendys2_sum":data_trendys2_top['SUM'],
                           "trendys3_sum":data_trendys3_top['SUM'],"FLUXCOM_sum":data_FLUXCOM_top['SUM'],
                           "RS_sum":data_RS_top['SUM'],"AABI_all_mean_sum":data_AABI_all_mean_top['SUM']})

out_all_df.to_csv("point_sum_summary_1984_2010_detrend_top"+str(int(perc * 100))+"perc.csv",index=False)

#data_AABI_all_mean.to_csv("point_AABI_all_mean_1984_2010_detrend_"+str(int(100 - perc * 100))+"perc.csv",index=False)
#data_AABI_all_mean_top.to_csv("point_AABI_all_mean_1984_2010_detrend_top"+str(int(perc * 100))+"perc.csv",index=False)

#data_trendys2.to_csv("point_trendys2_1984_2010_detrend_"+str(int(100 - perc * 100))+"perc.csv",index=False)
#data_trendys2_top.to_csv("point_trendys2_1984_2010_detrend_top"+str(int(perc * 100))+"perc.csv",index=False)
#data_trendys3.to_csv("point_trendys3_1984_2010_detrend_"+str(int(100 - perc * 100))+"perc.csv",index=False)
#data_trendys3_top.to_csv("point_trendys3_1984_2010_detrend_top"+str(int(perc * 100))+"perc.csv",index=False)
#data_RS.to_csv("point_RS_1984_2010_detrend_"+str(int(100 - perc * 100))+"perc.csv",index=False)
#data_RS_top.to_csv("point_RS_1984_2010_detrend_top"+str(int(perc * 100))+"perc.csv",index=False)
#data_FLUXCOM.to_csv("point_FLUXCOM_1984_2010_detrend_"+str(int(100 - perc * 100))+"perc.csv",index=False)
#data_FLUXCOM_top.to_csv("point_FLUXCOM_1984_2010_detrend_top"+str(int(perc * 100))+"perc.csv",index=False)



