###########################################################################################
#script to quantify the 24 months climate response of GPP and sink
###########################################################################################

library("dplyr")
library("dendroTools")
library("tidyr")
library("foreign")

#process the climate data and detrend
{
  setwd(".\\Clim_response\\")
  CRU_temp = read.csv("CRU_NCEP_temp_1_1984_2010.csv")
  CRU_prep = read.csv("CRU_NCEP_prep_1_1984_2010.csv")
  
  temp_df = base::subset(CRU_temp,Month == 1)
  prep_df = base::subset(CRU_prep,Month == 1)
  
  temp_detrend = temp_df
  for (i in 3:(ncol(temp_df))){
    temp_lm <- lm(temp_df[,i]~temp_df[,1])
    temp_ret <- residuals(temp_lm)
    temp_detrend[,i] <- temp_ret
  }
  
  prep_detrend = prep_df
  for (i in 3:(ncol(prep_df))){
    prep_lm <- lm(prep_df[,i]~prep_df[,1])
    prep_ret <- residuals(prep_lm)
    prep_detrend[,i] <- prep_ret
  }
  
  for (i in 2:12){
    data_temp = base::subset(CRU_temp,Month == i)
    data_prep = base::subset(CRU_prep,Month == i)
    
    temp_detrend_sub = data_temp
    for (i in 3:(ncol(data_temp))){
      temp_lm <- lm(data_temp[,i]~data_temp[,1])
      temp_ret <- residuals(temp_lm)
      temp_detrend_sub[,i] <- temp_ret
    }
    
    prep_detrend_sub = data_prep
    for (i in 3:(ncol(data_prep))){
      prep_lm <- lm(data_prep[,i]~data_prep[,1])
      prep_ret <- residuals(prep_lm)
      prep_detrend_sub[,i] <- prep_ret
    }
    
    temp_detrend = merge(temp_detrend,temp_detrend_sub,all=T)
    prep_detrend = merge(prep_detrend,prep_detrend_sub,all=T)
  }
  
  
  write.csv(temp_detrend,"CRU_NCEP_temp_1_1984_2010_detrend.csv")
  write.csv(prep_detrend,"CRU_NCEP_prep_1_1984_2010_detrend.csv")

}
###########################################################################################
#The detrended tifs of GPP and AABI were converted into point shps in Arc GIS 10.5
###########################################################################################
#dbf file processing - gpp and sink detrended data
{
#AABI per tree as an example
root = ".\\Clim_response\\AABI_age_bias_corrected_GLC_detrend\\"
#root = ".\\Clim_response\\GPP_TRENDYS2_GLC_detrend\\"
dbf_input = read.dbf(paste0(root,"pred_north_america_s804_1_",1984,"_bias_correct_per_area_all_mean_glc_detrend_827.dbf"))
names(dbf_input) = c("Point_ID",1984)


for (i in 1985:2010){
  dbf_input1 = read.dbf(paste0(root,"pred_North_America_s804_1_",i,"_bias_correct_per_area_all_mean_glc_detrend_827.dbf"))
  names(dbf_input1) = c("Point_ID",i)
  dbf_input1 = dbf_input1[-548,]
  dbf_input = merge(dbf_input,dbf_input1,by="Point_ID")
}

dbf_input_t = data.frame(t(dbf_input))
dbf_input_t = dbf_input_t[-1,]
X = rownames(dbf_input_t)
dbf_input_t =cbind(X,dbf_input_t)
}

#Climate response quantification
{
#summary = c()
out_df = NULL

#partial correlation
#TEMP
{
  CRU_temp_data = read.csv("CRU_NCEP_temp_1_1984_2010_detrend.csv")
  CRU_prep_data = read.csv("CRU_NCEP_prep_1_1984_2010_detrend.csv")
  input = dbf_input_t
  colnames(input)[1] = "Year"
  row.names(input) = input$Year
  #the first point,i =1
  {
    i = 1
    CRU_temp_sub = data.frame(CRU_temp_data[,2:3],CRU_temp_data[,(i+3)])
    CRU_prep_sub = data.frame(CRU_prep_data[,2:3],CRU_prep_data[,(i+3)])
    names(CRU_temp_sub) = c("Year","Month","Temperature")
    names(CRU_prep_sub) = c("Year","Month","Precipitation")  
    CRU_temp_prep_sub = merge(CRU_temp_sub,CRU_prep_sub)
    input_sub = data.frame(input[,(i+1)])
    row.names(input_sub) = input$Year
    
    #convert CRU_temp_sub to wide format
    CRU_temp_sub_wide = spread(CRU_temp_sub,Month,Temperature)
    row.names(CRU_temp_sub_wide) = CRU_temp_sub_wide$Year
    CRU_temp_sub_wide = CRU_temp_sub_wide[,-1]
    
    p_corr <- monthly_response_seascorr(response = input_sub,
                                        lower_limit = 1,upper_limit = 12,
                                        env_data_primary = CRU_temp_sub_wide,
                                        env_data_control = CRU_prep_sub,
                                        #row_names_subset = TRUE,
                                        remove_insignificant = FALSE,
                                        aggregate_function_env_data_primary = 'mean',
                                        aggregate_function_env_data_control = 'mean',
                                        alpha = 0.05, #pcor_method = "spearman",
                                        tidy_env_data_primary = FALSE,
                                        tidy_env_data_control = TRUE,
                                        previous_year = TRUE)
    
    out_df = summary(p_corr)
    names(out_df) = c("Variable",paste0("X",i))
  }
  
  #:(length(CRU_temp_data)-2)
  for (i in 2:(length(CRU_temp_data)-3)){
    try({
      print(i)
      CRU_temp_sub = data.frame(CRU_temp_data[,2:3],CRU_temp_data[,(i+3)])
      CRU_prep_sub = data.frame(CRU_prep_data[,2:3],CRU_prep_data[,(i+3)])
      names(CRU_temp_sub) = c("Year","Month","Temperature")
      names(CRU_prep_sub) = c("Year","Month","Precipitation")  
      CRU_temp_prep_sub = merge(CRU_temp_sub,CRU_prep_sub)
      input_sub = data.frame(input[,(i+1)])
      row.names(input_sub) = input$Year
      
      #convert CRU_temp_sub to wide format
      CRU_temp_sub_wide = spread(CRU_temp_sub,Month,Temperature)
      row.names(CRU_temp_sub_wide) = CRU_temp_sub_wide$Year
      CRU_temp_sub_wide = CRU_temp_sub_wide[,-1]
      
      p_corr <- monthly_response_seascorr(response = input_sub,
                                          lower_limit = 1,upper_limit = 12,
                                          env_data_primary = CRU_temp_sub_wide,
                                          env_data_control = CRU_prep_sub,
                                          #row_names_subset = TRUE,
                                          remove_insignificant = FALSE,
                                          aggregate_function_env_data_primary = 'mean',
                                          aggregate_function_env_data_control = 'mean',
                                          alpha = 0.05, #pcor_method = "spearman",
                                          tidy_env_data_primary = FALSE,
                                          tidy_env_data_control = TRUE,
                                          previous_year = TRUE) 
      out_df1 = summary(p_corr)
      names(out_df1) = c("Variable",paste0("X",i))
      
      out_df = inner_join(out_df,out_df1,by = c("Variable"))
      
    })
    
    if(inherits(p_corr,"try-error")) next
    
    
  }
}

write.csv(out_df,".\\Clim_response\\Output_temp\\AABI_age_correct_per_tree_temp_pcorr.csv")


#Prep
#summary = c()
out_df = NULL


#sink - PREP
{
  CRU_temp_data = read.csv("CRU_NCEP_temp_1_1984_2010_detrend.csv")
  CRU_prep_data = read.csv("CRU_NCEP_prep_1_1984_2010_detrend.csv")
  input = dbf_input_t
  
  colnames(input)[1] = "Year"
  row.names(input) = input$Year
  
  #the first point,i =1
  {
    i = 1
    CRU_temp_sub = data.frame(CRU_temp_data[,2:3],CRU_temp_data[,(i+3)])
    CRU_prep_sub = data.frame(CRU_prep_data[,2:3],CRU_prep_data[,(i+3)])
    names(CRU_temp_sub) = c("Year","Month","Temperature")
    names(CRU_prep_sub) = c("Year","Month","Precipitation")  
    CRU_temp_prep_sub = merge(CRU_prep_sub,CRU_temp_sub)
    input_sub = data.frame(input[,(i+1)])
    row.names(input_sub) = input$Year
    
    #convert CRU_temp_sub to wide format
    CRU_prep_sub_wide = spread(CRU_prep_sub,Month,Precipitation)
    row.names(CRU_prep_sub_wide) = CRU_prep_sub_wide$Year
    CRU_prep_sub_wide = CRU_prep_sub_wide[,-1]
    
    p_corr <- monthly_response_seascorr(response = input_sub,
                                        lower_limit = 1,upper_limit = 12,
                                        env_data_primary = CRU_prep_sub_wide,
                                        env_data_control = CRU_temp_sub,
                                        #row_names_subset = TRUE,
                                        remove_insignificant = FALSE,
                                        aggregate_function_env_data_primary = 'median',
                                        aggregate_function_env_data_control = 'median',
                                        alpha = 0.05, #pcor_method = "spearman",
                                        tidy_env_data_primary = FALSE,
                                        tidy_env_data_control = TRUE,
                                        previous_year = TRUE)
    
    out_df = summary(p_corr)
    names(out_df) = c("Variable",paste0("X",i))
  }
  
  for (i in 2:(length(CRU_prep_data)-3)){
    try({
      print(i)
      CRU_temp_sub = data.frame(CRU_temp_data[,2:3],CRU_temp_data[,(i+3)])
      CRU_prep_sub = data.frame(CRU_prep_data[,2:3],CRU_prep_data[,(i+3)])
      names(CRU_temp_sub) = c("Year","Month","Temperature")
      names(CRU_prep_sub) = c("Year","Month","Precipitation")  
      CRU_temp_prep_sub = merge(CRU_prep_sub,CRU_temp_sub)
      input_sub = data.frame(input[,(i+1)])
      row.names(input_sub) = input$Year
      
      #convert CRU_temp_sub to wide format
      CRU_prep_sub_wide = spread(CRU_prep_sub,Month,Precipitation)
      row.names(CRU_prep_sub_wide) = CRU_prep_sub_wide$Year
      CRU_prep_sub_wide = CRU_prep_sub_wide[,-1]
      
      p_corr <- monthly_response_seascorr(response = input_sub,
                                          lower_limit = 1,upper_limit = 12,
                                          env_data_primary = CRU_prep_sub_wide,
                                          env_data_control = CRU_temp_sub,
                                          #row_names_subset = TRUE,
                                          remove_insignificant = FALSE,
                                          aggregate_function_env_data_primary = 'mean',
                                          aggregate_function_env_data_control = 'mean',
                                          alpha = 0.05, #pcor_method = "spearman",
                                          tidy_env_data_primary = FALSE,
                                          tidy_env_data_control = TRUE,
                                          previous_year = TRUE) 
      out_df1 = summary(p_corr)
      names(out_df1) = c("Variable",paste0("X",i))
      
      out_df = inner_join(out_df,out_df1,by = c("Variable"))
      
    })
    
    if(inherits(p_corr,"try-error")) next
    
    
  }
}
write.csv(out_df,".\\Clim_response\\Output_prep\\AABI_age_correct_per_tree_prep_pcorr.csv")
}