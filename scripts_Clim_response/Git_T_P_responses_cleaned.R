##########################################################################################
#The script is to process and plot the T&P response for TRENDY&AABI outputs
#Input data extraction and preparations were performed in R and Arc GIS
##########################################################################################
library(ggplot2)
library(forcats)
library(magrittr)
library(hrbrthemes)
library(ggridges)
library(viridis)
library(dplyr)
library(tidyr)
library(dbscan)
library(akima)
  
#summary data of climate response for plot
Data_growth_NPP_VegC = read.csv(".\\Clim_response\\data\\Data_growth_NPP_VegC_response_summary.csv")

  Data_growth = subset(Data_growth_NPP_VegC,Type =="growth")  
  Data_NPP = subset(Data_growth_NPP_VegC,Type == "NPP_TRENDY")  

  
  
#growth cluster
  Data_growth1 <- dbscan(Data_growth_filled[, c("Temp_zscore", "Prep_zscore")], eps = 0.5, minPts = 40)
  Data_growth_filled$cluster <- Data_growth1$cluster
  Data_growth_filled_cluster <- Data_growth_filled %>% filter(cluster != -1)
  Data_growth_filled_cluster1 <- Data_growth_filled_cluster %>% filter(cluster == 1)
  
#NPP cluster
  Data_NPP1 <- dbscan(Data_NPP_filled[, c("Temp_zscore", "Prep_zscore")], eps = 0.5, minPts = 40)
  Data_NPP_filled$cluster <- Data_NPP1$cluster
  Data_NPP_filled_cluster <- Data_NPP_filled %>% filter(cluster != -1)
  Data_NPP_filled_cluster1 <- Data_NPP_filled_cluster %>% filter(cluster == 1)
  
  pdf(".\\Clim_response\\data\\Fig1a.pdf",width = 6,height = 6)
  
  p_11 <- ggplot(Data_growth_filled_cluster1, aes(x = Temp_zscore, y = Prep_zscore)) +
    stat_summary_hex(aes(z = Corr_Temp), fun = mean, bins = 30) +
    scale_fill_gradientn(colors = viridis::inferno(10), 
                         limits = c(-1, 1), 
                         breaks = c(-0.8,-0.4,0,0.4,0.8))+
    xlim(-2.5,2.5)+
    ylim(-2.5,2.5)+
    #oob = scales::squish, # Ensures values outside limits are squished into range
    #guide = guide_colorbar(title = "Avg Corr_Temp", barwidth = 10, barheight = .5)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 1,color = "black") + # Add vertical line at x = 0
    geom_hline(yintercept = 0, linetype = "dashed", size = 1,color = "black") +
    labs(x = "Temp_zscore", y = "Prep_zscore",fill = "pcor") +
    theme(axis.text = element_text(face="bold",size =24),axis.text.x = element_text(face="bold",size =24),axis.text.y = element_text(face="bold",size =24),axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold",size=24),legend.title = element_text(size = 16,face = "bold"),
          legend.text = element_text(size = 12,face = "bold"),legend.position = c(0.9,0.2))
  
  
  p_11
  dev.off()
  
  
  
  
  pdf(".\\Clim_response\\data\\Fig1b.pdf",width = 6,height = 6)
  
  p_12 <- ggplot(Data_growth_filled_cluster1, aes(x = Temp_zscore, y = Prep_zscore)) +
    stat_summary_hex(aes(z = Corr_Prep), fun = mean, bins = 30) +
    scale_fill_gradientn(colors = viridis::mako(10), 
                         limits = c(-1, 1), 
                         breaks = c(-0.8,-0.4,0,0.4,0.8))+
    xlim(-2.5,2.5)+
    ylim(-2.5,2.5)+
    #oob = scales::squish, # Ensures values outside limits are squished into range
    #guide = guide_colorbar(title = "Avg Corr_Temp", barwidth = 10, barheight = .5)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 1,color = "black") + # Add vertical line at x = 0
    geom_hline(yintercept = 0, linetype = "dashed", size = 1,color = "black") +
    labs(x = "Temp_zscore", y = "Prep_zscore",fill = "pcor") +
    theme(axis.text = element_text(face="bold",size =24),axis.text.x = element_text(face="bold",size =24),axis.text.y = element_text(face="bold",size =24),axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold",size=24),legend.title = element_text(size = 16,face = "bold"),
          legend.text = element_text(size = 12,face = "bold"),legend.position = c(0.9,0.2))
  
  
  p_12
  dev.off()

#NPP  
  
  pdf(".\\Clim_response\\data\\Fig1c.pdf",width = 6,height = 6)
  
  p_21 <- ggplot(Data_NPP_filled_cluster1, aes(x = Temp_zscore, y = Prep_zscore)) +
    stat_summary_hex(aes(z = Corr_Temp), fun = mean, bins = 30) +
    scale_fill_gradientn(colors = viridis::inferno(10), 
                         limits = c(-1, 1), 
                         breaks = c(-0.8,-0.4,0,0.4,0.8))+
    xlim(-2.5,2.5)+
    ylim(-2.5,2.5)+
    #oob = scales::squish, # Ensures values outside limits are squished into range
    #guide = guide_colorbar(title = "Avg Corr_Temp", barwidth = 10, barheight = .5)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 1,color = "black") + # Add vertical line at x = 0
    geom_hline(yintercept = 0, linetype = "dashed", size = 1,color = "black") +
    labs(x = "Temp_zscore", y = "Prep_zscore",fill = "pcor") +
    theme(axis.text = element_text(face="bold",size =24),axis.text.x = element_text(face="bold",size =24),axis.text.y = element_text(face="bold",size =24),axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold",size=24),legend.title = element_text(size = 16,face = "bold"),
          legend.text = element_text(size = 12,face = "bold"),legend.position = c(0.9,0.2))
  
  
  p_21
  dev.off()
  
  
  
  
  pdf(".\\Clim_response\\data\\Fig1d.pdf",width = 6,height = 6)
  
  p_22 <- ggplot(Data_NPP_filled_cluster1, aes(x = Temp_zscore, y = Prep_zscore)) +
    stat_summary_hex(aes(z = Corr_Prep), fun = mean, bins = 30) +
    scale_fill_gradientn(colors = viridis::mako(10), 
                         limits = c(-1, 1), 
                         breaks = c(-0.8,-0.4,0,0.4,0.8))+
    xlim(-2.5,2.5)+
    ylim(-2.5,2.5)+
    #oob = scales::squish, # Ensures values outside limits are squished into range
    #guide = guide_colorbar(title = "Avg Corr_Temp", barwidth = 10, barheight = .5)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 1,color = "black") + # Add vertical line at x = 0
    geom_hline(yintercept = 0, linetype = "dashed", size = 1,color = "black") +
    labs(x = "Temp_zscore", y = "Prep_zscore",fill = "pcor") +
    theme(axis.text = element_text(face="bold",size =24),axis.text.x = element_text(face="bold",size =24),axis.text.y = element_text(face="bold",size =24),axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold",size=24),legend.title = element_text(size = 16,face = "bold"),
          legend.text = element_text(size = 12,face = "bold"),legend.position = c(0.9,0.2))
  
  
  p_22
  dev.off()
  
  
  
