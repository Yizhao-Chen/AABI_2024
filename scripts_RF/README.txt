Scripts to perform random forest model fitting, validation, map creation and other related analysis

1. The raw site target AABI for RF training were created using TR-SNP v1.0: https://github.com/Yizhao-Chen/TR-SNP

2. calculate_multi_site_mean.py: get the multi-site mean at the same pixel

Inputs: .\\RF\\ITRDB_us_canada_s1982_multiple_sites.csv *This file defines the sites located at the same pixel of the 0.08333 map
	.\\RF\\delta_bio_mean\\delta_bio_mean\\

Outputs: .\\RF\\delta_bio_mean\\delta_bio_mean_ms_convert\\

3. Git_RF.R: perform random forest model fitting, validation, map creation  and others
Inputs: .\\RF\\input_data_age_corrected.csv *summarized from process 1-3. 
	.\\RF\\map_input\\

Outputs: .\\RF\\FigS6c-6g.pdf
	.\\RF\\FigS9a.pdf

The AABI products were deposited at figshare (https://figshare.com/projects/Tree-ring_based_AABI_in_North_America/153072).
The outputs were summarized as .\\RF\\FigS6a-b.pdf *ED Fig6a-b*

Data for CV related analysis were summarized as .\\RF\\FigS8a-c.tif *ED Fig8a-c*

Data for uncertainty analysis: 
.\\RF\\map_out_boot_full\\       complete output for bootstrap analysis
.\\RF\\map_out_var_all_full\\     complete output for predictor analysis
.\\RF\\map_out_boot_mean\\    data to produce FigS12b 
.\\RF\\map_out_var_all_mean\\      data to produce FigS12c
.\\RF\\FigS11b.tif *ED Fig11b*
.\\RF\\FigS11c.tif *ED Fig11c*
