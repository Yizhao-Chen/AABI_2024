###########################################################################################
#Pre-process and plot the zonal outputs
#Continental level .csv data were prepared using Arc GIS 10.5
###########################################################################################
library("ggplot2")
library("dplR")
library("ggthemes")
library("RColorBrewer")
library("ggcorrplot")

#Pre-process
{
  setwd(".\\Spatial_analysis\\Zonal_statistics\\")
  
  input_AABI_glc_all_mean = read.csv("North_America_mask_per_area_GLC_area_ratio_correct_all_mean.csv")
  input_AABI_glc_per_tree = read.csv("North_America_mask_per_tree_GLC_area_ratio_correct.csv")
  input_gpp_FLUXCOM = read.csv("North_America_FLUXCOM_gpp_GLC_mask_summary_area_correct.csv")
  input_gpp_RS_mean = read.csv("North_America_RS_gpp_mean_GLC_mask_summary_area_correct.csv")
  input_gpp_TRENDYS2 = read.csv("North_America_gpps2_GLC_mask_summary_area_correct.csv")
  input_gpp_TRENDYS3 = read.csv("North_America_gpps3_GLC_mask_summary_area_correct.csv")
  input_gpp_TRENDY_combined = merge(input_gpp_TRENDYS2,input_gpp_TRENDYS3,by = "Year")
  
  input_gpp_TRENDYS2_global = read.csv("Global_gpp_s2_GLC_mask_summary_1984_2010.csv")
  input_dVegC_TRENDYS2_global = read.csv("Global_npp_s2_GLC_mask_summary_1984_2010.csv")
  input_dVegC_TRENDYS2_global = read.csv("Global_delta_VegC_s2_GLC_mask_summary_1984_2010.csv")
  input_nbp_TRENDYS2_global = read.csv("Global_nbp_s2_GLC_mask_summary_1984_2010.csv")

  
  #TRENDY data detrend and normalization
{
  #detrend outputs
  input_gpp_TRENDYS2_de <- input_gpp_TRENDYS2
  for (i in 2:(ncol(input_gpp_TRENDYS2_de))){
    output_lm <- lm(input_gpp_TRENDYS2_de[,i]~input_gpp_TRENDYS2_de[,1],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_gpp_TRENDYS2_de[,i] <- output_ret
  }  

  input_gpp_TRENDYS3_de <- input_gpp_TRENDYS3
  for (i in 2:(ncol(input_gpp_TRENDYS3_de))){
    output_lm <- lm(input_gpp_TRENDYS3_de[,i]~input_gpp_TRENDYS3_de[,1],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_gpp_TRENDYS3_de[,i] <- output_ret
  }    
  
  input_gpp_TRENDY_combined_de <- input_gpp_TRENDY_combined
  for (i in 2:(ncol(input_gpp_TRENDY_combined_de))){
    output_lm <- lm(input_gpp_TRENDY_combined_de[,i]~input_gpp_TRENDY_combined_de[,1],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_gpp_TRENDY_combined_de[,i] <- output_ret
  }    
  
  input_gpp_global_de <- input_gpp_TRENDYS2_global
  for (i in 2:(ncol(input_gpp_global_de))){
    output_lm <- lm(input_gpp_global_de[,i]~input_gpp_global_de[,1],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_gpp_global_de[,i] <- output_ret
  }  
  
  input_npp_global_de <- input_npp_TRENDYS2_global
  for (i in 2:(ncol(input_npp_global_de))){
    output_lm <- lm(input_npp_global_de[,i]~input_npp_global_de[,1],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_npp_global_de[,i] <- output_ret
  }  
  
  input_dVegC_global_de <- input_dVegC_TRENDYS2_global
  for (i in 2:(ncol(input_dVegC_global_de))){
    output_lm <- lm(input_dVegC_global_de[,i]~input_dVegC_global_de[,1],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_dVegC_global_de[,i] <- output_ret
  }  
  
  input_nbp_global_de <- input_nbp_TRENDYS2_global
  for (i in 2:(ncol(input_nbp_global_de))){
    output_lm <- lm(input_nbp_global_de[,i]~input_nbp_global_de[,1],na.action=na.exclude)
    output_ret <- residuals(output_lm)
    input_nbp_global_de[,i] <- output_ret
  }  

  input_gpp_TRENDYS2_de_scaled = scale(input_gpp_TRENDYS2_de[,2:length(input_gpp_TRENDYS3_de)],center = TRUE,scale = TRUE)
  input_gpp_TRENDYS3_de_scaled = scale(input_gpp_TRENDYS3_de[,2:length(input_gpp_TRENDYS3_de)],center = TRUE,scale = TRUE)
  input_gpp_TRENDY_combined_de_scaled = scale(input_gpp_TRENDY_combined_de[,2:length(input_gpp_TRENDY_combined_de)],center = TRUE,scale = TRUE)
  input_gpp_global_de_scaled = scale(input_gpp_global_de[,2:length(input_gpp_global_de)],center = TRUE,scale = TRUE)
  input_npp_global_de_scaled = scale(input_npp_global_de[,2:length(input_npp_global_de)],center = TRUE,scale = TRUE)
  input_dVegC_global_de_scaled = scale(input_dVegC_global_de[,2:length(input_dVegC_global_de)],center = TRUE,scale = TRUE)
  input_nbp_global_de_scaled = scale(input_nbp_global_de[,2:length(input_nbp_global_de)],center = TRUE,scale = TRUE)
  
  #convert to dfs
  gpp_TRENDYS2_de_scaled_df = data.frame(input_gpp_TRENDYS2_de_scaled)
  gpp_TRENDYS3_de_scaled_df = data.frame(input_gpp_TRENDYS3_de_scaled)
  gpp_TRENDY_combined_de_scaled_df = data.frame(input_gpp_TRENDY_combined_de_scaled)
  gpp_de_scaled_global_df = data.frame(input_gpp_global_de_scaled) 
  npp_de_scaled_global_df = data.frame(input_npp_global_de_scaled)
  dVegC_de_scaled_global_df = data.frame(input_dVegC_global_de_scaled)
  nbp_de_scaled_global_df = data.frame(input_nbp_global_de_scaled)
  
  #calculate mean and sd
  #mean
  gpp_TRENDYS2_de_scaled_mean = apply(gpp_TRENDYS2_de_scaled_df[,1:ncol(gpp_TRENDYS2_de_scaled_df)],1,mean,na.rm=TRUE)
  gpp_TRENDYS3_de_scaled_mean = apply(gpp_TRENDYS3_de_scaled_df[,1:ncol(gpp_TRENDYS3_de_scaled_df)],1,mean,na.rm=TRUE)
  gpp_TRENDY_combined_de_scaled_mean = apply(gpp_TRENDY_combined_de_scaled_df[,1:ncol(gpp_TRENDY_combined_de_scaled_df)],1,mean,na.rm=TRUE)
  gpp_de_scaled_global_mean = apply(gpp_de_scaled_global_df[,1:ncol(gpp_de_scaled_global_df)],1,mean,na.rm=TRUE)
  npp_de_scaled_global_mean = apply(npp_de_scaled_global_df[,1:ncol(npp_de_scaled_global_df)],1,mean,na.rm=TRUE)
  dVegC_de_scaled_global_mean = apply(dVegC_de_scaled_global_df[,1:ncol(dVegC_de_scaled_global_df)],1,mean,na.rm=TRUE)
  nbp_de_scaled_global_mean = apply(nbp_de_scaled_global_df[,1:ncol(nbp_de_scaled_global_df)],1,mean,na.rm=TRUE)
  
  #sd
  gpp_TRENDYS2_de_scaled_rsd = apply(gpp_TRENDYS2_de_scaled_df[,1:ncol(gpp_TRENDYS2_de_scaled_df)],1,sd,na.rm=TRUE)
  gpp_TRENDYS3_de_scaled_rsd = apply(gpp_TRENDYS3_de_scaled_df[,1:ncol(gpp_TRENDYS3_de_scaled_df)],1,sd,na.rm=TRUE)
  gpp_TRENDY_combined_de_scaled_rsd = apply(gpp_TRENDY_combined_de_scaled_df[,1:ncol(gpp_TRENDY_combined_de_scaled_df)],1,sd,na.rm=TRUE)
  gpp_de_scaled_global_rsd = apply(gpp_de_scaled_global_df[,1:ncol(gpp_de_scaled_global_df)],1,sd,na.rm=TRUE)
  npp_de_scaled_global_rsd = apply(npp_de_scaled_global_df[,1:ncol(npp_de_scaled_global_df)],1,sd,na.rm=TRUE)
  dVegC_de_scaled_global_rsd = apply(dVegC_de_scaled_global_df[,1:ncol(dVegC_de_scaled_global_df)],1,sd,na.rm=TRUE)
  nbp_de_scaled_global_rsd = apply(nbp_de_scaled_global_df[,1:ncol(nbp_de_scaled_global_df)],1,sd,na.rm=TRUE)
  
  #add to dfs
  gpp_TRENDYS2_de_scaled_df$mean = gpp_TRENDYS2_de_scaled_mean 
  gpp_TRENDYS2_de_scaled_df$sd = gpp_TRENDYS2_de_scaled_rsd 
  gpp_TRENDYS2_de_scaled_df$min = gpp_TRENDYS2_de_scaled_mean - gpp_TRENDYS2_de_scaled_rsd   
  gpp_TRENDYS2_de_scaled_df$max = gpp_TRENDYS2_de_scaled_mean + gpp_TRENDYS2_de_scaled_rsd  
  
  gpp_TRENDYS3_de_scaled_df$mean = gpp_TRENDYS3_de_scaled_mean 
  gpp_TRENDYS3_de_scaled_df$sd = gpp_TRENDYS3_de_scaled_rsd 
  gpp_TRENDYS3_de_scaled_df$min = gpp_TRENDYS3_de_scaled_mean - gpp_TRENDYS3_de_scaled_rsd   
  gpp_TRENDYS3_de_scaled_df$max = gpp_TRENDYS3_de_scaled_mean + gpp_TRENDYS3_de_scaled_rsd 
  
  gpp_TRENDY_combined_de_scaled_df$mean = gpp_TRENDY_combined_de_scaled_mean 
  gpp_TRENDY_combined_de_scaled_df$sd = gpp_TRENDY_combined_de_scaled_rsd 
  gpp_TRENDY_combined_de_scaled_df$min = gpp_TRENDY_combined_de_scaled_mean - gpp_TRENDY_combined_de_scaled_rsd   
  gpp_TRENDY_combined_de_scaled_df$max = gpp_TRENDY_combined_de_scaled_mean + gpp_TRENDY_combined_de_scaled_rsd
  
  gpp_de_scaled_global_df$mean = gpp_de_scaled_global_mean 
  gpp_de_scaled_global_df$sd = gpp_de_scaled_global_rsd 
  gpp_de_scaled_global_df$min = gpp_de_scaled_global_mean - gpp_de_scaled_global_rsd   
  gpp_de_scaled_global_df$max = gpp_de_scaled_global_mean + gpp_de_scaled_global_rsd  
  
  npp_de_scaled_global_df$mean = npp_de_scaled_global_mean 
  npp_de_scaled_global_df$sd = npp_de_scaled_global_rsd 
  npp_de_scaled_global_df$min = npp_de_scaled_global_mean - npp_de_scaled_global_rsd   
  npp_de_scaled_global_df$max = npp_de_scaled_global_mean + npp_de_scaled_global_rsd  
  
  dVegC_de_scaled_global_df$mean = dVegC_de_scaled_global_mean 
  dVegC_de_scaled_global_df$sd = dVegC_de_scaled_global_rsd 
  dVegC_de_scaled_global_df$min = dVegC_de_scaled_global_mean - dVegC_de_scaled_global_rsd   
  dVegC_de_scaled_global_df$max = dVegC_de_scaled_global_mean + dVegC_de_scaled_global_rsd  
  
  nbp_de_scaled_global_df$mean = nbp_de_scaled_global_mean 
  nbp_de_scaled_global_df$sd = nbp_de_scaled_global_rsd 
  nbp_de_scaled_global_df$min = nbp_de_scaled_global_mean - nbp_de_scaled_global_rsd   
  nbp_de_scaled_global_df$max = nbp_de_scaled_global_mean + nbp_de_scaled_global_rsd  
  
  #add year to dfs
  gpp_TRENDYS2_de_scaled_df$Year = c(1984:2010)
  gpp_TRENDYS3_de_scaled_df$Year = c(1984:2010)
  gpp_TRENDY_combined_de_scaled_df$Year = c(1984:2010)
  gpp_de_scaled_global_df$Year = c(1984:2010)
  npp_de_scaled_global_df$Year = c(1984:2010)
  dVegC_de_scaled_global_df$Year = c(1984:2010)  
  nbp_de_scaled_global_df$Year = c(1984:2010) 
  }
  
  
  #detrend the other datasets
  {
    input_AABI_glc_all_mean_de <- input_AABI_glc_all_mean
    for (i in 2:(ncol(input_AABI_glc_all_mean_de))){
      output_lm <- lm(input_AABI_glc_all_mean_de[,i]~input_AABI_glc_all_mean_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_AABI_glc_all_mean_de[,i] <- output_ret
    } 
    
    input_AABI_glc_per_tree_de <- input_AABI_glc_per_tree
    for (i in 2:(ncol(input_AABI_glc_per_tree_de))){
      output_lm <- lm(input_AABI_glc_per_tree_de[,i]~input_AABI_glc_per_tree_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_AABI_glc_per_tree_de[,i] <- output_ret
    } 
    
    input_gpp_FLUXCOM_de <- input_gpp_FLUXCOM
    for (i in 2:(ncol(input_gpp_FLUXCOM_de))){
      output_lm <- lm(input_gpp_FLUXCOM_de[,i]~input_gpp_FLUXCOM_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_FLUXCOM_de[,i] <- output_ret
    }
    
    input_gpp_RS_mean_de <- input_gpp_RS_mean
    for (i in 2:(ncol(input_gpp_RS_mean_de))){
      output_lm <- lm(input_gpp_RS_mean_de[,i]~input_gpp_RS_mean_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_RS_mean_de[,i] <- output_ret
    }
    
    
     }
    
    input_AABI_glc_all_mean_de_scaled = scale(input_AABI_glc_all_mean_de[,2:length(input_AABI_glc_all_mean_de)],center = TRUE,scale = TRUE)
    input_AABI_glc_per_tree_de_scaled = scale(input_AABI_glc_per_tree_de[,2:length(input_AABI_glc_per_tree_de)],center = TRUE,scale = TRUE)
    input_gpp_FLUXCOM_de_scaled = scale(input_gpp_FLUXCOM_de[,2:length(input_gpp_FLUXCOM_de)],center = TRUE,scale = TRUE)
    input_gpp_RS_mean_de_scaled = scale(input_gpp_RS_mean_de[,2:length(input_gpp_RS_mean_de)],center = TRUE,scale = TRUE)
    
    input_AABI_glc_all_mean_de_scaled_df = data.frame(input_AABI_glc_all_mean_de_scaled)
    input_AABI_glc_per_tree_de_scaled_df = data.frame(input_AABI_glc_per_tree_de_scaled)
    input_gpp_FLUXCOM_de_scaled_df = data.frame(input_gpp_FLUXCOM_de_scaled)
    input_gpp_RS_mean_de_scaled_df = data.frame(input_gpp_RS_mean_de_scaled)
    
    year = c(1984:2010)
    input_AABI_glc_all_mean_de_scaled_df$Year = year
    input_AABI_glc_per_tree_de_scaled_df$Year = year
    input_gpp_FLUXCOM_de_scaled_df$Year = year
    input_gpp_RS_mean_de_scaled_df$Year = year
  }

#plot code for North America Fig4b1
{
  NPPMean = read.csv('NPP_modelMean_detrend.csv')
  VegCMean = read.csv('VegC_modelMean_detrend.csv')
  AABI_allmean = read.csv('AABI_allmean_detrend.csv')
  AABI_pertree = read.csv('AABI_pertree_detrend.csv')
  
  
  pdf(".\\Spatial_analysis\\Fig42.pdf",width = 12,height = 5.5)
  p42 <- ggplot()+
    geom_ribbon(data = NPPMean,aes(x=Year,ymin= min,ymax= max),fill = "green4",alpha = 0.2)+
    #geom_ribbon(data = VegCMean,aes(x=Year,ymin= min,ymax= max),fill = "green4",alpha = 0.1)+
    #geom_ribbon(data = AABI_allmean,aes(x=Year,ymin= min,ymax= max),fill = "lightcoral",alpha = 0.1)+
    #geom_ribbon(data = S3_NPP,aes(x=Year,ymin= min,ymax= max),fill = "gray21",alpha = 0.2)+
    geom_line(data = AABI_allmean,aes(x=Year,y= input_AABI_glc_all_mean_de_scaled,color = "aa"),linetype="solid",size = 1.5)+
    geom_line(data = NPPMean,aes(x=Year,y=mean,color = "cc"),linetype="solid",size =1.5)+
    #geom_line(data = VegCMean,aes(x=Year,y= mean,color = "bb"),linetype="solid",size = 1.5)+
    
    #geom_line(data = S3_NPP,aes(x=Year,y= mean,color = "dd"),linetype="solid",size = 1.5)+
    
    #geom_line(data = input_AABI_glc_all_mean_de_scaled_df,aes(x=Year,y= input_AABI_glc_per_tree_de_scaled,color = "gg"),linetype="solid",size = 1.5)+
    #geom_line(data = input_AABI_glc_per_tree_de_scaled_df,aes(x=Year,y= input_AABI_glc_per_tree_de_scaled,color = "hh"),linetype="solid",size = 1.5)+
    ylab("z-score")+
    xlab("Year")+
    scale_y_continuous(limits = c(-8.5,4),breaks= c(-4,-2,0,2,4),labels = sprintf("%.1f", c(-4,-2,0,2,4)))+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '',
                       values =c("cc"="green4",#"bb" = "green4",
                                 "aa"="orange"), labels = c(bquote("AABI"[per_tree]),bquote("NPP"[TRENDY])))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "bottom",legend.text = element_text(face="bold",size = 24))+
    theme(axis.text = element_text(face="bold",size =20),axis.text.x = element_text(face="bold",size =20,colour = "black"),axis.text.y = element_text(face="bold",size =20,colour = "black"),axis.title.x=element_text(face="bold",size=22),axis.title.y=element_text(face="bold.italic",size=22))+
    theme(
      legend.position = c(0.25, 0.1),
      legend.justification = c(1, 0),
      legend.box = "horizontal",
      legend.margin = margin(t = 0, l = 0, unit = "pt")
    )+
    theme(panel.border = element_rect(colour = "black", size = 1, fill = NA))
  
  #theme(legend.position = c(0.1, 0.1), legend.margin = margin(t = -30, l = 0, unit = "pt"))
  p42
  dev.off()
    
  }

#Plot code for Global ED Fig1d
{
  
  pdf(".\\Spatial_analysis\\FigS1_d.pdf",width = 10,height =6)
  p32 <- ggplot()+
    geom_ribbon(data = gpp_de_scaled_global_df,aes(x=Year,ymin= min,
                                                            ymax= max),fill = "gray21",alpha = 0.1)+
    geom_ribbon(data = npp_de_scaled_global_df,aes(x=Year,ymin= min,
                                                            ymax= max),fill = "royalblue",alpha = 0.1)+
    geom_ribbon(data = dVegC_de_scaled_global_df,aes(x=Year,ymin= min,
                                                            ymax= max),fill = "lightcoral",alpha = 0.1)+
    geom_ribbon(data = nbp_de_scaled_global_df,aes(x=Year,ymin= min,
                                                            ymax= max),fill = "green4",alpha = 0.1)+
    geom_line(data = gpp_de_scaled_global_df,aes(x=Year,
                                                              y=mean,color = "aa"),linetype="solid",size =1.5)+
    geom_line(data = npp_de_scaled_global_df,aes(x=Year,y= mean,color = "bb"),linetype="solid",size = 1.5)+
    geom_line(data = dVegC_de_scaled_global_df,aes(x=Year,y= mean,color = "cc"),linetype="solid",size = 1.5)+
    geom_line(data = nbp_de_scaled_global_df,aes(x=Year,y= mean,color = "dd"),linetype="solid",size = 1.5)+
    ylab("z-score")+
    xlab("Year")+
    scale_y_continuous(limits = c(-4,4),breaks= c(-4,-2,0,2,4),labels = sprintf("%.1f", c(-4,-2,0,2,4)))+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '',
                       values =c("aa"="gray21","bb" = "royalblue","cc"="lightcoral","dd"="green4"), labels = c(bquote("GPP"[TRENDY]),bquote("NPP"[TRENDY]),bquote("dVegC"[TRENDY]),bquote("NBP"[TRENDY])))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "bottom",legend.text = element_text(face="bold",size = 18))+
    theme(axis.text = element_text(face="bold",size =24),axis.text.x = element_text(face="bold",size =24,colour = "black"),axis.text.y = element_text(face="bold",size =22,colour = "black"),axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold.italic",size=24))
  p32
  dev.off()
  
}