Spatial analysis to get maps of correlations

1. Git_GIS_spatial_detrend_correlation.R: detrend the rasters and perform correlation analysis

Inputs: .\\Spatial_analysis\\TRENDYv7_S2_ensemble_dVegC_1984_2010\\
            .\\Spatial_analysis\\TRENDYv7_S2_ensemble_GPP_1984_2010\\
            .\\Spatial_analysis\\TRENDYv7_S2_ensemble_NPP_1984_2010\\
            .\\Spatial_analysis\\TRENDYv7_S2_ensemble_NBP_1984_2010\\
            .\\Spatial_analysis\\TRENDYv7_S3_ensemble_GPP_1984_2010\\
            .\\Spatial_analysis\\FLUXCOM_GPP_1_North_America_GLC_1984_2010\\
            .\\Spatial_analysis\\RS_GPP_mean_1_North_America_GLC_1984_2010\\
            .\\Spatial_analysis\\AABI_per_area_age_bias_correct_mean_1_North_America_GLC_1984_2010\\

Outputs:  .\\Spatial_analysis\\detrend\\TRENDYv7_S2_ensemble_dVegC_1984_2010_detrend\\
            	.\\Spatial_analysis\\detrend\\TRENDYv7_S2_ensemble_GPP_1984_2010_detrend\\
            	.\\Spatial_analysis\\detrend\\TRENDYv7_S2_ensemble_NPP_1984_2010_detrend\\
            	.\\Spatial_analysis\\detrend\\TRENDYv7_S2_ensemble_NBP_1984_2010_detrend\\
            	.\\Spatial_analysis\\detrend\\TRENDYv7_S3_ensemble_GPP_1984_2010_detrend\\
            	.\\Spatial_analysis\\detrend\\FLUXCOM_GPP_1_North_America_GLC_1984_2010_detrend\\
            	.\\Spatial_analysis\\detrend\\RS_GPP_mean_1_North_America_GLC_1984_2010_detrend\\
            	.\\Spatial_analysis\\detrend\\AABI_per_area_age_bias_correct_mean_1_North_America_GLC_1984_2010_detrend\\
            	.\\Spatial_analysis\\corr\\corr_Trendy_s3_GPP_AABI_per_area_bias_correct_north_america_GLC.tif 
            	.\\Spatial_analysis\\corr\\corr_Trendy_s2_GPP_AABI_per_area_bias_correct_north_america_GLC.tif
            	.\\Spatial_analysis\\corr\\corr_Trendy_mean_AABI_per_area_bias_correct_north_america_GLC.tif    *Fig.1b*
            	.\\Spatial_analysis\\corr\\corr_trendy_gpp_npp_s2_ensemble_1984_2010_north_america_GLC.tif      *Fig.1a*
            	.\\Spatial_analysis\\corr\\corr_RS_GPP_mean_AABI_per_area_bias_correct_mean_GLC.tif                 *Fig.1d*
            	.\\Spatial_analysis\\corr\\corr_FLUXCOM_GPP_AABI_per_area_bias_correct_north_america_GLC.tif  *Fig.1c*
            	.\\Spatial_analysis\\corr\\corr_trendy_gpp_npp_s2_ensemble_1984_2010_GLC.tif   *ED Fig.1a*
            	.\\Spatial_analysis\\corr\\corr_trendy_gpp_nbp_s2_ensemble_1984_2010_GLC.tif   *ED Fig.1c*
            	.\\Spatial_analysis\\corr\\corr_trendy_gpp_dVegC_s2_ensemble_1984_2010_GLC.tif *ED Fig.1b*

pft analysis were performed by Arc GIS 10.5 and then plotted using sigmaplot 14.0, the pft map is from GLC2000  
Data summary: pft_output.xlsx *Fig.1e*

2. Git_zonal_plot.R: perform continental level analysis and plot
Inputs: 	.\\Spatial_analysis\\Zonal_statistics\\Global_gpp_s2_GLC_mask_summary_1984_2010.csv
	.\\Spatial_analysis\\Zonal_statistics\\Global_npp_s2_GLC_mask_summary_1984_2010.csv
	.\\Spatial_analysis\\Zonal_statistics\\Global_delta_VegC_s2_GLC_mask_summary_1984_2010.csv
	.\\Spatial_analysis\\Zonal_statistics\\Global_nbp_s2_GLC_mask_summary_1984_2010.csv
	.\\Spatial_analysis\\Zonal_statistics\\North_America_FLUXCOM_gpp_GLC_mask_summary_area_correct.csv
	.\\Spatial_analysis\\Zonal_statistics\\North_America_gpps2_GLC_mask_summary_area_correct.csv
	.\\Spatial_analysis\\Zonal_statistics\\North_America_gpps3_GLC_mask_summary_area_correct.csv
	.\\Spatial_analysis\\Zonal_statistics\\North_America_mask_per_area_GLC_area_ratio_correct_all_mean.csv
	.\\Spatial_analysis\\Zonal_statistics\\North_America_mask_per_tree_GLC_area_ratio_correct.csv
	.\\Spatial_analysis\\Zonal_statistics\\North_America_RS_gpp_mean_GLC_mask_summary_area_correct.csv

Outputs: .\\Spatial_analysis\\Fig1_P31.pdf Fig1_P32.pdf FigS1_d.pdf， i.e., *Fig.1f* *ED Fig.1d*   
	




