#########################################################################################################
#Extract the pixel values of GPP datasets
#GPP data links:
#BEPS: https://drive.google.com/file/d/1frf4QXMd--95Bn4VR1t9HRVo9KN5d8U8/view?usp=share_link
#EC-LUE:http://glass-product.bnu.edu.cn/introduction1/gpp.html
#GPPinf:https://figshare.com/articles/dataset/Long-term_1982-2018_global_gross_primary_production_dataset_based_on_NIRv/12981977/2
#########################################################################################################

from osgeo import gdal,ogr
import struct
import csv
import pandas as pd
import os

import gc


#for .tif
#driver = gdal.GetDriverByName('GTiff')
#for .dat .raw
driver = gdal.GetDriverByName('ENVI')
#for .hdf GPP EC_LUE
#driver = gdal.GetDriverByName('HDF4')

#shp file
shp_filename = '.\\Site_synthesis\\Global_sites.shp'
#shp_filename = '.\\Site_synthesis\\Gen_tree.shp'
#shp_filename = '.\\Site_synthesis\\Tropical_sites.shp'
# open shp file and get geoinfo
ds = ogr.Open(shp_filename)
lyr = ds.GetLayer()

#get the inital framework for output1
initial_list = list()
for feat in lyr:
    geom = feat.GetGeometryRef()
    feat_id = feat.GetField('id')
    initial_list.append([feat_id])


output1 = pd.DataFrame(initial_list)
output1.columns = ['id']

#Specify local raster file input from BEPS EC-LUE and GPPinf
#BEPS as an example
filepath = ".\\Site_synthesis\\RS_GPP\\BEPS_GPP\\"

f_list = os.listdir(filepath)

for i in f_list:
    # os.path.splitext():Seperate filename and surfix
    #if os.path.splitext(i)[1] == '.raw':
    if os.path.splitext(i)[1] == '.dat':        #for EC-LUE and GPPinf
    #if os.path.splitext(i)[1] == '.tif':         #for BEPS
        #if 'cv' in os.path.splitext(i)[0]:
        name1 = os.path.splitext(i)[0]   
            #name2 = name1.split('_')[1]      
        #name2 = name1.split('.')[4]     #for EC-LUE
        name2 = name1.split('_')[1]     #for GPPinf
        #name2 = name1.split('_')[3]     #for BEPS
        raster_input = filepath + i
            #open raster file and get geoinfo
        src_ds=gdal.Open(raster_input)
        gt=src_ds.GetGeoTransform()
        rb=src_ds.GetRasterBand(1)


        list_values = list()
        for feat in lyr:
            geom = feat.GetGeometryRef()
            feat_id = feat.GetField('id')
            mx,my=geom.GetX(), geom.GetY()  #coord in map units

            #Convert from map to pixel coordinates.
            #Only works for geotransforms with no rotation.
            px = int((mx - gt[0]) / gt[1]) #x pixel
            py = int((my - gt[3]) / gt[5]) #y pixel

            structval=rb.ReadRaster(px,py,1,1,buf_type=gdal.GDT_Float32) #float size
            intval = struct.unpack('f', structval)
            list_values.append([feat_id,intval[0]])

        #locals()['output_'+str(name2)] = pd.DataFrame(list_values)
        output = pd.DataFrame(list_values)
        output.columns = ['id',str(name2)]

        #locals()['output_'+str(name2)].columns = ['id', str(name2)]

        output1 = pd.concat([output1, output],axis=1)
        #close the current file
        #src_ds = None

output2 = output1.T.drop_duplicates()
output2 = output2.dropna(axis=1,how='any')
output2 = output2.rename(index = {'id':'Year'})

#output
output2.to_csv(".\\Site_synthesis\\BEPS_annual_all_Tras.csv")



