######################################################################################################
#Script to get the multi-site mean at the same pixel
######################################################################################################

import pandas as pd
import os
#us and canada
input = pd.read_csv(".\\RF\\"
                    "ITRDB_us_canada_s1982_multiple_sites.csv")

path = ".\\RF\\delta_bio_mean\\" \
       "delta_bio_mean_bias_corr\\"


#drop the -9999 site first us canada
#in the case of us and canada the input column name is ['Raster_value']
nan_site = input[input['Raster_value'].isin([-9999])]
input=input[~input['Raster_value'].isin([-9999])]

#drop -21474836 global
# nan_site = input[input['europe_seq'].isin([-21474836])]
# input=input[~input['europe_seq'].isin([-21474836])]

#list of the pixel
# slist = input['europe_seq'].unique()
#us_canada
slist = input['Raster_value'].unique()

namelist = list()
# for i in range(0,len(input['europe_seq'].unique())):
#     input_sub = input.loc[input['europe_seq']==slist[i]]
for i in range(0,len(input['Raster_value'].unique())):
    input_sub = input.loc[input['Raster_value']==slist[i]]
    input_sub.index = input_sub.name
    check = 0

    for k in range(0,len(input_sub.name)):
        try:
        #read the first file
            input_sub1 = pd.read_csv(path + input_sub.name.iloc[k] +
                                         "_delta_bio_mean_bias_corr.csv")
        except IOError:
            check = 1
                #give a specific value to input_sub1 to judge if there is an IOError
            continue

        #if input_sub1

            #drop the first unnamed column
        input_sub1.drop(columns=['Unnamed: 0'],inplace=True)
            #rename hte columns
        input_sub1.columns = ['Year_'+str(k),'d_bio_mean_'+str(k),'samp.depth_'+str(k)]
        input_sub1.index = input_sub1['Year_'+str(k)]
                #
                #combine all the data together first
        if k == 0:
            output_sub = input_sub1
        else:
            output_sub = pd.concat([output_sub,input_sub1],axis=1,join='inner')
        #os.remove(path + input_sub.name.iloc[k] + "_delta_bio_mean.csv")
    if check == 1:
        continue

    namelist.append(input_sub.name.iloc[k])

    #get the site number in output_sub
    tt = len(input_sub.name)
    #set up a new column to receive outputs
    output_sub['delta_bio_wm'] = 0
    output_sub['samp'] = 0
    #calcuate the weighted mean
    for j in range(0,len(output_sub.index)):
        sum = 0
        samp = 0
        for t in range(0,tt):
            sum = sum + output_sub['d_bio_mean_'+str(t)].iloc[j] * output_sub['samp.depth_'+str(t)].iloc[j]
            samp = samp + output_sub['samp.depth_'+str(t)].iloc[j]
        output_sub['delta_bio_wm'].iloc[j] = sum / samp
        output_sub['samp'].iloc[j] = samp

        #output to a file

    output_sub.to_csv(path + input_sub.name.iloc[k] +
                                         "_delta_bio_mean_ms_convert.csv")
