Steps to perform the site-level synthesis:

1. Collect and compare FLUXNET sites GPP data and the corresponding growth observations from various sources (FLUXNET comparison), as summarized in Data_synthesis_summary.xlsx.
2. Compare spatial explicit GPP data with ITRDB, Gentree and Tropical sites output (RS comparison).

Data involved: Spatial explicit GPP datasets: BEPS,EC-LUE,GPPinf; Tree ring data from ITRDB and Gentree; Inventory data from six Tropical sites.

Data processing:
Spatial homogenous test: 
Get the multi-year mean maps of the GPP datasets and calculate the spatial explicit CV values (focal statistics in Arc GIS 10.5); extract the pixels of of the growth observation sites and select the spatially homogenous ones (CV < 0.4). 
Extract the pixels: GIS_cv.py: 
Inputs: .\\Site_synthesis\\Global_sites.shp
            .\\Site_synthesis\\cv\\GLASS_GPP_0.05_1982_2010_mean_focal_33_cv.dat
            .\\Site_synthesis\\cv\\GLobal_GPP_0.05_1982_2010_mean_focal_33_cv.dat
            .\\Site_synthesis\\cv\\GPPinf_GPP_0.05_1982_2010_mean_focal_33_cv.dat
Outputs: .\\Site_synthesis\\GPPinf_82_10_0.05_cleaned_focal_33_cv_Tras.csv
	.\\Site_synthesis\\EC_LUE_82_10_0.05_cleaned_focal_33_cv_Tras.csv
	.\\Site_synthesis\\BEPS_82_10_0.07272_focal_33_cv_Tras.csv

Extract the low cv sites: Extract_lowCV_sites.py:
Inputs: .\\Site_synthesis\\Global_sites.shp
	.\\Site_synthesis\\GPPinf_82_10_0.05_cleaned_focal_33_cv_Tras.csv
	.\\Site_synthesis\\EC_LUE_82_10_0.05_cleaned_focal_33_cv_Tras.csv
	.\\Site_synthesis\\BEPS_82_10_0.07272_focal_33_cv_Tras.csv
Outupts:.\\Site_synthesis\\cv_2_3.csv

Correlation analysis:
1). Get the detrended correlationships between GPP and tree ring width. 
Extract the pixel values of GPP datasets: GIS_GPP_indexes.py:
Inputs: .\\Site_synthesis\\Global_sites.shp
	.\\Site_synthesis\\RS_GPP\\BEPS_GPP\\
	*BEPS GPP as example here
	EC-LUE GPP is available at: http://glass-product.bnu.edu.cn/introduction1/gpp.html
	GPPinf is available at: https://figshare.com/articles/dataset/Long-term_1982-2018_global_gross_primary_production_dataset_based_on_NIRv/12981977/2
Outputs: .\\Site_synthesis\\BEPS_annual_all_Tras.csv
	.\\Site_synthesis\\EC_LUE_annual_all_Tras.csv
	.\\Site_synthesis\\GPPinf_annual_all_Tras.csv

Correlation analysis: dat_compile_output_csv_only.py:
Inputs: .\\Site_synthesis\\ITRDB_de\\
	.\\Site_synthesis\\Gen_tree_de\\
	.\\Site_synthesis\\BEPS_annual_all_Tras.csv
	.\\Site_synthesis\\EC_LUE_annual_all_Tras.csv
	.\\Site_synthesis\\GPPinf_annual_all_Tras.csv

Outputs: .\\Site_synthesis\\stat_summary_BEPS\\
	.\\Site_synthesis\\stat_summary_EC_LUE\\
	.\\Site_synthesis\\stat_summary_GPPinf\\


2). The continental files were grouped, cleaned to cv < 0.4 sites using cv_2_3.csv and then add flags for significance as a summary file: stat_summary_Global_ensemble_manu_toshp.csv. The summary file was converted to .shp for *ED Fig.2a*. 


Output summary:  
Plot FLUXNET comparison output using Sigmaplot 14.0  *ED Fig.2b* 
Plot global map using Arc GIS 10.5. *ED Fig.2a*  
Plot code for the lat distribution: plot_spatial_distribution.py.  *ED Fig.2c*
