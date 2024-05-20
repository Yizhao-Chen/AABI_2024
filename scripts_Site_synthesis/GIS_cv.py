##########################################################################################################
#Script to get pixel values based on gdal
#Get site cv values from RS GPPs 
##########################################################################################################
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
#for .hdf
#driver = gdal.GetDriverByName('HDF4')

#shp file
#cleaned global sites
shp_filename = ".\\Site_synthesis\\Global_sites.shp"
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

#specify raster file input
filepath = ".\\Site_synthesis\\cv\\"

f_list = os.listdir(filepath)

for i in f_list:
    # os.path.splitext():Seperate filename and surfix
    #if os.path.splitext(i)[1] == '.raw':
    if os.path.splitext(i)[1] == '.dat':
        #if 'cv' in os.path.splitext(i)[0]:
        name1 = os.path.splitext(i)[0]   
        #name2 = name1.split('_')[1]      
        #name2 = name1.split('_')[6]
        name2 = "1982_2010_mean"
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

# output2.to_csv(".\\Site_synthesis\\"
#               "GPPinf_82_10_0.05_cleaned_focal_33_cv_Tras.csv",header = None)
# output2.to_csv(".\\Site_synthesis\\"
#               "EC_LUE_82_10_0.05_cleaned_focal_33_cv_Tras.csv",header = None)
output2.to_csv(".\\Site_synthesis\\"
               "BEPS_82_10_0.07272_focal_33_cv_Tras.csv",header = None)

