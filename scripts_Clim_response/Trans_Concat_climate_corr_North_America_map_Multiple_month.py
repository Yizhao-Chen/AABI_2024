#############################################################################################################
#Script to transpose and tranlate multi-month corr of North America map into a usable form
#############################################################################################################

import os
import csv
import pandas as pd
import numpy as np


root_temp = ".\\Clim_response\\Output_temp\\"
root_prep = ".\\Clim_response\\Output_prep\\"

file_name1 = "AABI_per_tree_temp_pcorr.csv"
file_name2 = "AABI_per_tree_prep_pcorr.csv"

out_name1 = "AABI_per_tree_temp_pcorr_tras.csv"
out_name2 = "AABI_per_tree_prep_pcorr_tras.csv"

index_list1 = list(['Variable'])
index_list2 = list(['Variable'])

infile_1 = pd.read_csv(root_temp+file_name1)
infile_2 = pd.read_csv(root_prep+file_name2)

#transpose first
infile_1 = infile_1.T
infile_2 = infile_2.T

out1 = infile_1 #pd.concat([infile_1,infile_2,infile_3,infile_4,infile_5,infile_6,infile_7,infile_8])
out2 = infile_2
#out3 = infile_3

out1.drop(['Unnamed: 0'],inplace = True)
out2.drop(['Unnamed: 0'],inplace = True)

out1.drop([0,1,2,5,6,7,8],axis = 1, inplace=True)
out2.drop([0,1,2,5,6,7,8],axis = 1, inplace=True)

#out3.drop(['Unnamed: 0'],inplace = True)
out1.drop_duplicates(inplace = True)
out2.drop_duplicates(inplace = True)

for i in range(1,len(out1.index)):
    index_list1.append(int(out1.index[i].strip('X')))

for i in range(1,len(out2.index)):
    index_list2.append(int(out2.index[i].strip('X')))

out1.index = index_list1
out2.index = index_list2

out1.to_csv(root_temp+out_name1,header=False)
out2.to_csv(root_prep+out_name2,header=False)

####################################################################################################################
#part two: manually translate the strings to number and output processed dataset
####################################################################################################################
input_temp = pd.read_csv(root_temp + "AABI_per_area_temp_pcorr_tras.csv")
input_prep = pd.read_csv(root_prep + "AABI_per_area_prep_pcorr_tras.csv")

out_name11 = "AABI_per_area_temp_pcorr_tras_processed.csv"
out_name22 =  "AABI_per_area_prep_pcorr_tras_processed.csv"

#temp
out_list = list()
out_list1 = list()
out_list2 = list()

for i in range(0,(input_temp.shape[0])):
    if "Jan*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-12)

    elif "Feb*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-11)

    elif "Mar*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-10)

    elif "Apr*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-9)

    elif "May*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-8)

    elif "Jun*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-7)

    elif "Jul*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-6)

    elif "Aug*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-5)

    elif "Sep*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-4)

    elif "Oct*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-3)

    elif "Nov*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-2)

    elif "Dec*" in input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(-1)

    elif "Jan" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(0)

    elif "Feb" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(1)

    elif "Mar" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(2)

    elif "Apr" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(3)

    elif "May" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(4)

    elif "Jun" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(5)

    elif "Jul" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(6)

    elif "Aug" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(7)

    elif "Sep" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(8)

    elif "Oct" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(9)

    elif "Nov" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(10)

    elif "Dec" in input_temp[input_temp.columns[3]][i][0:4] and "*" not in\
            input_temp[input_temp.columns[3]][i][0:4]:
        out_list.append(11)

    out_list1.append(input_temp[input_temp.columns[1]][i][0:4])
    out_list2.append(input_temp[input_temp.columns[1]][i][6:11])

    #would suffer a error here but just continue to work

start = pd.DataFrame({"start":out_list})

input_temp = pd.concat([input_temp,start],axis=1)
#input['optimal_time_window_length'].astype('int')

input_temp["end"] = input_temp.apply(lambda x: x['start'] + x['optimal_time_window_length'], axis=1)
input_temp["end"] = input_temp["end"] - 1

start_y = pd.DataFrame({"start_y":out_list1})
end_y = pd.DataFrame({"end_y":out_list2})

input_temp = pd.concat([input_temp,start_y],axis=1)
input_temp = pd.concat([input_temp,end_y],axis=1)

#translate the column from string to int
input_temp['start_y'] = pd.to_numeric(input_temp['start_y'], errors='ignore')
input_temp['end_y'] = pd.to_numeric(input_temp['end_y'], errors='ignore')

input_temp["y_num"] = input_temp.apply(lambda x: x['end_y'] - x['start_y'], axis=1)
input_temp["y_num"] = input_temp["y_num"] + 1

input_temp.to_csv(root_temp+ out_name11)



#prep
out_list = list()
out_list1 = list()
out_list2 = list()

for i in range(0,(input_prep.shape[0])):
    if "Jan*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-12)

    elif "Feb*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-11)

    elif "Mar*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-10)

    elif "Apr*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-9)

    elif "May*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-8)

    elif "Jun*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-7)

    elif "Jul*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-6)

    elif "Aug*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-5)

    elif "Sep*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-4)

    elif "Oct*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-3)

    elif "Nov*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-2)

    elif "Dec*" in input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(-1)

    elif "Jan" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(0)

    elif "Feb" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(1)

    elif "Mar" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(2)

    elif "Apr" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(3)

    elif "May" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(4)

    elif "Jun" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(5)

    elif "Jul" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(6)

    elif "Aug" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(7)

    elif "Sep" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(8)

    elif "Oct" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(9)

    elif "Nov" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(10)

    elif "Dec" in input_prep[input_prep.columns[3]][i][0:4] and "*" not in\
            input_prep[input_prep.columns[3]][i][0:4]:
        out_list.append(11)
    out_list1.append(input_prep[input_prep.columns[1]][i][0:4])
    out_list2.append(input_prep[input_prep.columns[1]][i][6:11])
start = pd.DataFrame({"start":out_list})
input_prep = pd.concat([input_prep,start],axis=1)

input_prep["end"] = input_prep.apply(lambda x: x['start'] + x['optimal_time_window_length'], axis=1)
input_prep["end"] = input_prep["end"] - 1

start_y = pd.DataFrame({"start_y":out_list1})
end_y = pd.DataFrame({"end_y":out_list2})

input_prep = pd.concat([input_prep,start_y],axis=1)
input_prep = pd.concat([input_prep,end_y],axis=1)

#translate the column from string to int
input_prep['start_y'] = pd.to_numeric(input_prep['start_y'], errors='ignore')
input_prep['end_y'] = pd.to_numeric(input_prep['end_y'], errors='ignore')

input_prep["y_num"] = input_prep.apply(lambda x: x['end_y'] - x['start_y'], axis=1)
input_prep["y_num"] = input_prep["y_num"] + 1

input_prep.to_csv(root_prep + out_name22)
