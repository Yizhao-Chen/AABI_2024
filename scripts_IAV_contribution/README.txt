IAV contribution analysis and plot

1. Git_IAV_contribution_analysis.R: get pixel contributions

Inputs: 	.\\IAV_contribution\\point_FLUXCOM_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_RS_mean_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_trendys2_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_trendys3_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_AABI_per_area_1984_2010_all_mean_detrend.csv

Outputs:	.\\IAV_contribution\\IAV_contribution*.csv
Files were converted into rasters as *ED Fig.5abcd*

Data were summarized in .\\IAV_contribution\\IAV_contribution_combined_summary_toshp.CSV

2.  Test_IAV_contribution_North_America_map.py
Inputs:	.\\IAV_contribution\\point_FLUXCOM_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_RS_mean_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_trendys2_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_trendys3_gpp_1984_2010_detrend.csv
	.\\IAV_contribution\\point_AABI_per_area_1984_2010_all_mean_detrend.csv
	.\\IAV_contribution\\IAV_contribution_combined_summary_toshp.CSV

Outputs:	.\\IAV_contribution\\point_sum_summary_1984_2010_detrend_top90perc.csv (example)

TRENDY and AABI data outputs were summarized in .\\IAV_contribution\\Trendy_AABI_1984_2010_detrend_IAV_all_ratio_analysis.csv. The file was used to plot *ED Fig.3e* in Git_IAV_contribution_analysis.R






