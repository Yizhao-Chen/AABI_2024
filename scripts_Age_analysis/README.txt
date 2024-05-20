Age_based_analysis and plot

1. Tree_age_young_old.py: find sites with both young and old trees in North America

Inputs: .\\Age_analysis\\us_age_tree_1982_1986.csv

Outputs:	.\\Age_analysis\\site_age_mean_sd.csv
	.\\Age_analysis\\site_age_mean_sd_T.csv

site_age_mean_sd_T_tree_25.csv: site with at least 25 samples
site_age_mean_sd_T_tree_25_final_list.csv: final site list

2.  site_age_IAV_comparison.py: analysis and plot the site-level age effect on inter-annual variability
Inputs: 	.\\Age_analysis\\us_age\\
	.\\Age_analysis\\us_delta_bio\\

Outputs:	.\\Age_analysis\\site_corr\\

The summarized file Corr_25_100_for_plot.csv was used to plot *ED Fig.9a*

3. The comparisons of the continental AABIper_tree predictions using age bias corrected and age bias non-corrected data: .\\Age_analysis\\
The forest age from the gridded data (NACP and GFAD, 2005 were used as the reference year) were dated back to ages of a specific year as the ecosystem forest age.
The age-AABI functions from Tang et al. (2014), i.e., y = 56.202*age^0.505*e^(-0.00521*age) for Boreal forest, and y = 339.612*age^0.243*e^(-0.00364*age) for Temperate forest, 
were used to calculate the AABI ratio between age of site and the ecosystem age.
Negative age from the gridded data means disturbance happened at a pixel, so we keep the biomass increment from the site. 

Output: .\\Age_analysis\\age_correction.csv

Map creations were performed in .\\RF\\Git_RF.R




