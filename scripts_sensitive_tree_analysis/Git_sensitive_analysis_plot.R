################################################################################
#script to plot the outputs for sensitive tree bias analysis
################################################################################
library(tidyverse)
library(ggplot2)
library(dendroTools)
library(dplyr)
library(dplR)

################################################################################
#Part 1: chronology 
################################################################################
setwd(".\\sensitive_tree_analysis\\")

#DEN
{
  #PIST
  #####project 39#####
  PIST_1 = read.csv(".\\den\\PIST_39\\core.csv")
  unique(PIST_1$Species)
  PIST_1 = subset(PIST_1,Species == 'PIST')
  PIST_39_plot = read.csv(".\\den\\PIST_39\\plot.csv")
  
  PIST_39_rwl = PIST_1[,20:ncol(PIST_1)]
  PIST_39_rwl = data.frame(rev(PIST_39_rwl))
  PIST_39_rwl = data.frame(t(PIST_39_rwl))
  rownames(PIST_39_rwl) = 1777:2017
  colnames(PIST_39_rwl) = PIST_1$TreeID
  for(i in 1:nrow(PIST_39_rwl)){
    for(j in 1:ncol(PIST_39_rwl)){
      if(PIST_39_rwl[i,j] == 0){
        PIST_39_rwl[i,j] = NA
        next
      }
    }
  }
  
  info1 = rwl.stats(PIST_39_rwl)
  info1 = subset(info1,first <= 1950)
  PIST_39_rwl = PIST_39_rwl[,which(colnames(PIST_39_rwl) %in% info1$series)]
  
  PIST_39_rwl = powt(PIST_39_rwl,rescale = TRUE)
  PIST_39_rwl <- PIST_39_rwl[, sapply(PIST_39_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_39_rwl = dplR::detrend(PIST_39_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_39_rwl = PIST_39_rwl[which(rownames(PIST_39_rwl) >= 1950 & rownames(PIST_39_rwl) <= 2010),]
  PIST_39_chron = chron(PIST_39_rwl,prefix = "CAM")
  PIST_39_chron = PIST_39_chron[which(rownames(PIST_39_chron) >= 1950 & rownames(PIST_39_chron) <= 2010),]
  #plot(PIST_39_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_39_rwl,"PIST_39_rwl_1950_2010.csv")
  #write.csv(PIST_39_chron,"PIST_39_chron_1950_2010.csv")
  
  
  #####Project_18#####
  PIST_1 = read.csv(".\\den\\PIST_18\\core.csv")
  PIST_1 = subset(PIST_1,Species == 'PIST')
  PIST_18_plot = read.csv(".\\den\\PIST_18\\plot.csv")
  #a
  PIST_18_a = subset(PIST_1,PlotID %in% PIST_18_plot$PlotID[8:9])
  PIST_18_a_rwl = PIST_18_a[,20:166]
  PIST_18_a_rwl = data.frame(rev(PIST_18_a_rwl))
  PIST_18_a_rwl = data.frame(t(PIST_18_a_rwl))
  rownames(PIST_18_a_rwl) = 1864:2010
  colnames(PIST_18_a_rwl) = PIST_18_a $TreeID
  unique_treeid = unique(colnames(PIST_18_a_rwl))
  PIST_18_a_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2010-1864+1))
  rownames(PIST_18_a_rwl_1) = 1864:2010
  colnames(PIST_18_a_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_18_a_rwl[,which(colnames(PIST_18_a_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_18_a_rwl_1[,i] = requried_col;next}
    PIST_18_a_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_18_a_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_18_a_rwl = PIST_18_a_rwl_1
  PIST_18_a_rwl = PIST_18_a_rwl[,which(colnames(PIST_18_a_rwl) %in% info1$series)]
  
  PIST_18_a_rwl = powt(PIST_18_a_rwl,rescale = TRUE)
  PIST_18_a_rwl <- PIST_18_a_rwl[, sapply(PIST_18_a_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_18_a_rwl = dplR::detrend(PIST_18_a_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_18_a_rwl = PIST_18_a_rwl[which(rownames(PIST_18_a_rwl) >= 1950 & rownames(PIST_18_a_rwl) <= 2010),]
  PIST_18_a_chron = chron(PIST_18_a_rwl,prefix = "CAM")
  PIST_18_a_chron = PIST_18_a_chron[which(rownames(PIST_18_a_chron) >= 1950 & rownames(PIST_18_a_chron) <= 2010),]
  #plot(PIST_18_a_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_18_a_rwl,"PIST_18_a_rwl_1950_2010.csv")
  #write.csv(PIST_18_a_chron,"PIST_18_a_chron_1950_2010.csv")
  
  #####project 33#####
  PIST_1 = read.csv(".\\den\\PIST_33\\core.csv",header = F)
  colnames(PIST_1) = PIST_1[1,]
  PIST_1 = PIST_1[2:nrow(PIST_1),]
  
  PIST_1 = subset(PIST_1,Species == 'PIST')
  PIST_33_plot = read.csv(".\\den\\PIST_33\\plot.csv")
  #ALC
  PIST_33_a = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[2])
  PIST_33_a_rwl = PIST_33_a[,20:301]
  PIST_33_a_rwl = data.frame(rev(PIST_33_a_rwl))
  PIST_33_a_rwl = data.frame(t(PIST_33_a_rwl))
  rownames(PIST_33_a_rwl) = 1735:2016
  colnames(PIST_33_a_rwl) = PIST_33_a $TreeID
  unique_treeid = unique(colnames(PIST_33_a_rwl))
  PIST_33_a_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_a_rwl_1) = 1735:2016
  colnames(PIST_33_a_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_a_rwl[,which(colnames(PIST_33_a_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_a_rwl_1[,i] = requried_col;next}
    PIST_33_a_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_a_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_a_rwl = PIST_33_a_rwl_1
  PIST_33_a_rwl = PIST_33_a_rwl[,which(colnames(PIST_33_a_rwl) %in% info1$series)]
  
  PIST_33_a_rwl = powt(PIST_33_a_rwl,rescale = TRUE)
  PIST_33_a_rwl <- PIST_33_a_rwl[, sapply(PIST_33_a_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_a_rwl = dplR::detrend(PIST_33_a_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_a_rwl = PIST_33_a_rwl[which(rownames(PIST_33_a_rwl) >= 1950 & rownames(PIST_33_a_rwl) <= 2010),]
  PIST_33_a_chron = chron(PIST_33_a_rwl,prefix = "CAM")
  PIST_33_a_chron = PIST_33_a_chron[which(rownames(PIST_33_a_chron) >= 1950 & rownames(PIST_33_a_chron) <= 2010),]
  #plot(PIST_33_a_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_a_rwl,"PIST_33_a_rwl_1950_2010.csv")
  #write.csv(PIST_33_a_chron,"PIST_33_a_chron_1950_2010.csv")
  
  
  #BB
  PIST_33_b = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[3])
  PIST_33_b_rwl = PIST_33_b[,20:301]
  PIST_33_b_rwl = data.frame(rev(PIST_33_b_rwl))
  PIST_33_b_rwl = data.frame(t(PIST_33_b_rwl))
  rownames(PIST_33_b_rwl) = 1735:2016
  colnames(PIST_33_b_rwl) = PIST_33_b $TreeID
  unique_treeid = unique(colnames(PIST_33_b_rwl))
  PIST_33_b_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_b_rwl_1) = 1735:2016
  colnames(PIST_33_b_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_b_rwl[,which(colnames(PIST_33_b_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_b_rwl_1[,i] = requried_col;next}
    PIST_33_b_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_b_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_b_rwl = PIST_33_b_rwl_1
  PIST_33_b_rwl = PIST_33_b_rwl[,which(colnames(PIST_33_b_rwl) %in% info1$series)]
  
  PIST_33_b_rwl = powt(PIST_33_b_rwl,rescale = TRUE)
  PIST_33_b_rwl <- PIST_33_b_rwl[, sapply(PIST_33_b_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_b_rwl = dplR::detrend(PIST_33_b_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_b_rwl = PIST_33_b_rwl[which(rownames(PIST_33_b_rwl) >= 1950 & rownames(PIST_33_b_rwl) <= 2010),]
  PIST_33_b_chron = chron(PIST_33_b_rwl,prefix = "CAM")
  PIST_33_b_chron = PIST_33_b_chron[which(rownames(PIST_33_b_chron) >= 1950 & rownames(PIST_33_b_chron) <= 2010),]
  #plot(PIST_33_b_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_b_rwl,"PIST_33_b_rwl_1950_2010.csv")
  #write.csv(PIST_33_b_chron,"PIST_33_b_chron_1950_2010.csv")
  
  #CNA
  PIST_33_c = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[4])
  PIST_33_c_rwl = PIST_33_c[,20:301]
  PIST_33_c_rwl = data.frame(rev(PIST_33_c_rwl))
  PIST_33_c_rwl = data.frame(t(PIST_33_c_rwl))
  rownames(PIST_33_c_rwl) = 1735:2016
  colnames(PIST_33_c_rwl) = PIST_33_c $TreeID
  unique_treeid = unique(colnames(PIST_33_c_rwl))
  PIST_33_c_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_c_rwl_1) = 1735:2016
  colnames(PIST_33_c_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_c_rwl[,which(colnames(PIST_33_c_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_c_rwl_1[,i] = requried_col;next}
    PIST_33_c_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_c_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_c_rwl = PIST_33_c_rwl_1
  PIST_33_c_rwl = PIST_33_c_rwl[,which(colnames(PIST_33_c_rwl) %in% info1$series)]
  
  PIST_33_c_rwl = powt(PIST_33_c_rwl,rescale = TRUE)
  PIST_33_c_rwl <- PIST_33_c_rwl[, sapply(PIST_33_c_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_c_rwl = dplR::detrend(PIST_33_c_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_c_rwl = PIST_33_c_rwl[which(rownames(PIST_33_c_rwl) >= 1950 & rownames(PIST_33_c_rwl) <= 2010),]
  PIST_33_c_chron = chron(PIST_33_c_rwl,prefix = "CAM")
  PIST_33_c_chron = PIST_33_c_chron[which(rownames(PIST_33_c_chron) >= 1950 & rownames(PIST_33_c_chron) <= 2010),]
  #plot(PIST_33_c_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_c_rwl,"PIST_33_c_rwl_1950_2010.csv")
  #write.csv(PIST_33_c_chron,"PIST_33_c_chron_1950_2010.csv")  
  
  #FMP
  PIST_33_f = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[6])
  PIST_33_f_rwl = PIST_33_f[,20:301]
  PIST_33_f_rwl = data.frame(rev(PIST_33_f_rwl))
  PIST_33_f_rwl = data.frame(t(PIST_33_f_rwl))
  rownames(PIST_33_f_rwl) = 1735:2016
  colnames(PIST_33_f_rwl) = PIST_33_f $TreeID
  unique_treeid = unique(colnames(PIST_33_f_rwl))
  PIST_33_f_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_f_rwl_1) = 1735:2016
  colnames(PIST_33_f_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_f_rwl[,which(colnames(PIST_33_f_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_f_rwl_1[,i] = requried_col;next}
    PIST_33_f_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_f_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_f_rwl = PIST_33_f_rwl_1
  PIST_33_f_rwl = PIST_33_f_rwl[,which(colnames(PIST_33_f_rwl) %in% info1$series)]
  
  PIST_33_f_rwl = powt(PIST_33_f_rwl,rescale = TRUE)
  PIST_33_f_rwl <- PIST_33_f_rwl[, sapply(PIST_33_f_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_f_rwl = dplR::detrend(PIST_33_f_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_f_rwl = PIST_33_f_rwl[which(rownames(PIST_33_f_rwl) >= 1950 & rownames(PIST_33_f_rwl) <= 2010),]
  PIST_33_f_chron = chron(PIST_33_f_rwl,prefix = "CAM")
  PIST_33_f_chron = PIST_33_f_chron[which(rownames(PIST_33_f_chron) >= 1950 & rownames(PIST_33_f_chron) <= 2010),]
  #plot(PIST_33_f_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_f_rwl,"PIST_33_f_rwl_1950_2010.csv")
  #write.csv(PIST_33_f_chron,"PIST_33_f_chron_1950_2010.csv")  
  
  #GOSH1
  PIST_33_g = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[9])
  PIST_33_g_rwl = PIST_33_g[,20:301]
  PIST_33_g_rwl = data.frame(rev(PIST_33_g_rwl))
  PIST_33_g_rwl = data.frame(t(PIST_33_g_rwl))
  rownames(PIST_33_g_rwl) = 1735:2016
  colnames(PIST_33_g_rwl) = PIST_33_g $TreeID
  unique_treeid = unique(colnames(PIST_33_g_rwl))
  PIST_33_g_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_g_rwl_1) = 1735:2016
  colnames(PIST_33_g_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_g_rwl[,which(colnames(PIST_33_g_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_g_rwl_1[,i] = requried_col;next}
    PIST_33_g_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_g_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_g_rwl = PIST_33_g_rwl_1
  PIST_33_g_rwl = PIST_33_g_rwl[,which(colnames(PIST_33_g_rwl) %in% info1$series)]
  
  PIST_33_g_rwl = powt(PIST_33_g_rwl,rescale = TRUE)
  PIST_33_g_rwl <- PIST_33_g_rwl[, sapply(PIST_33_g_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_g_rwl = dplR::detrend(PIST_33_g_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_g_rwl = PIST_33_g_rwl[which(rownames(PIST_33_g_rwl) >= 1950 & rownames(PIST_33_g_rwl) <= 2010),]
  PIST_33_g_chron = chron(PIST_33_g_rwl,prefix = "CAM")
  PIST_33_g_chron = PIST_33_g_chron[which(rownames(PIST_33_g_chron) >= 1950 & rownames(PIST_33_g_chron) <= 2010),]
  #plot(PIST_33_g_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_g_rwl,"PIST_33_g_rwl_1950_2010.csv")
  #write.csv(PIST_33_g_chron,"PIST_33_g_chron_1950_2010.csv")  
  
  #IBR
  PIST_33_i = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[13])
  PIST_33_i_rwl = PIST_33_i[,20:301]
  PIST_33_i_rwl = data.frame(rev(PIST_33_i_rwl))
  PIST_33_i_rwl = data.frame(t(PIST_33_i_rwl))
  rownames(PIST_33_i_rwl) = 1735:2016
  colnames(PIST_33_i_rwl) = PIST_33_i $TreeID
  unique_treeid = unique(colnames(PIST_33_i_rwl))
  PIST_33_i_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_i_rwl_1) = 1735:2016
  colnames(PIST_33_i_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_i_rwl[,which(colnames(PIST_33_i_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_i_rwl_1[,i] = requried_col;next}
    PIST_33_i_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_i_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_i_rwl = PIST_33_i_rwl_1
  PIST_33_i_rwl = PIST_33_i_rwl[,which(colnames(PIST_33_i_rwl) %in% info1$series)]
  
  PIST_33_i_rwl = powt(PIST_33_i_rwl,rescale = TRUE)
  PIST_33_i_rwl <- PIST_33_i_rwl[, sapply(PIST_33_i_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_i_rwl = dplR::detrend(PIST_33_i_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_i_rwl = PIST_33_i_rwl[which(rownames(PIST_33_i_rwl) >= 1950 & rownames(PIST_33_i_rwl) <= 2010),]
  PIST_33_i_chron = chron(PIST_33_i_rwl,prefix = "CAM")
  PIST_33_i_chron = PIST_33_i_chron[which(rownames(PIST_33_i_chron) >= 1950 & rownames(PIST_33_i_chron) <= 2010),]
  #plot(PIST_33_i_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_i_rwl,"PIST_33_i_rwl_1950_2010.csv")
  #write.csv(PIST_33_i_chron,"PIST_33_i_chron_1950_2010.csv")  
  
  #NB
  PIST_33_n = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[17])
  PIST_33_n_rwl = PIST_33_n[,20:301]
  PIST_33_n_rwl = data.frame(rev(PIST_33_n_rwl))
  PIST_33_n_rwl = data.frame(t(PIST_33_n_rwl))
  rownames(PIST_33_n_rwl) = 1735:2016
  colnames(PIST_33_n_rwl) = PIST_33_n $TreeID
  unique_treeid = unique(colnames(PIST_33_n_rwl))
  PIST_33_n_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_n_rwl_1) = 1735:2016
  colnames(PIST_33_n_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_n_rwl[,which(colnames(PIST_33_n_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_n_rwl_1[,i] = requried_col;next}
    PIST_33_n_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_n_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_n_rwl = PIST_33_n_rwl_1
  PIST_33_n_rwl = PIST_33_n_rwl[,which(colnames(PIST_33_n_rwl) %in% info1$series)]
  
  PIST_33_n_rwl = powt(PIST_33_n_rwl,rescale = TRUE)
  PIST_33_n_rwl <- PIST_33_n_rwl[, sapply(PIST_33_n_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_n_rwl = dplR::detrend(PIST_33_n_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_n_rwl = PIST_33_n_rwl[which(rownames(PIST_33_n_rwl) >= 1950 & rownames(PIST_33_n_rwl) <= 2010),]
  PIST_33_n_chron = chron(PIST_33_n_rwl,prefix = "CAM")
  PIST_33_n_chron = PIST_33_n_chron[which(rownames(PIST_33_n_chron) >= 1950 & rownames(PIST_33_n_chron) <= 2010),]
  #plot(PIST_33_n_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_n_rwl,"PIST_33_n_rwl_1950_2010.csv")
  #write.csv(PIST_33_n_chron,"PIST_33_n_chron_1950_2010.csv")    
  
  #PERU
  PIST_33_p = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[18])
  PIST_33_p_rwl = PIST_33_p[,20:301]
  PIST_33_p_rwl = data.frame(rev(PIST_33_p_rwl))
  PIST_33_p_rwl = data.frame(t(PIST_33_p_rwl))
  rownames(PIST_33_p_rwl) = 1735:2016
  colnames(PIST_33_p_rwl) = PIST_33_p $TreeID
  unique_treeid = unique(colnames(PIST_33_p_rwl))
  PIST_33_p_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_p_rwl_1) = 1735:2016
  colnames(PIST_33_p_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_p_rwl[,which(colnames(PIST_33_p_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_p_rwl_1[,i] = requried_col;next}
    PIST_33_p_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_p_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_p_rwl = PIST_33_p_rwl_1
  PIST_33_p_rwl = PIST_33_p_rwl[,which(colnames(PIST_33_p_rwl) %in% info1$series)]
  
  PIST_33_p_rwl = powt(PIST_33_p_rwl,rescale = TRUE)
  PIST_33_p_rwl <- PIST_33_p_rwl[, sapply(PIST_33_p_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_p_rwl = dplR::detrend(PIST_33_p_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_p_rwl = PIST_33_p_rwl[which(rownames(PIST_33_p_rwl) >= 1950 & rownames(PIST_33_p_rwl) <= 2010),]
  PIST_33_p_chron = chron(PIST_33_p_rwl,prefix = "CAM")
  PIST_33_p_chron = PIST_33_p_chron[which(rownames(PIST_33_p_chron) >= 1950 & rownames(PIST_33_p_chron) <= 2010),]
  #plot(PIST_33_p_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_p_rwl,"PIST_33_p_rwl_1950_2010.csv")
  #write.csv(PIST_33_p_chron,"PIST_33_p_chron_1950_2010.csv")    
  
  #RR
  PIST_33_r = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[19:20])
  PIST_33_r_rwl = PIST_33_r[,20:301]
  PIST_33_r_rwl = data.frame(rev(PIST_33_r_rwl))
  PIST_33_r_rwl = data.frame(t(PIST_33_r_rwl))
  rownames(PIST_33_r_rwl) = 1735:2016
  colnames(PIST_33_r_rwl) = PIST_33_r $TreeID
  unique_treeid = unique(colnames(PIST_33_r_rwl))
  PIST_33_r_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_r_rwl_1) = 1735:2016
  colnames(PIST_33_r_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_r_rwl[,which(colnames(PIST_33_r_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_r_rwl_1[,i] = requried_col;next}
    PIST_33_r_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_r_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_r_rwl = PIST_33_r_rwl_1
  PIST_33_r_rwl = PIST_33_r_rwl[,which(colnames(PIST_33_r_rwl) %in% info1$series)]
  
  PIST_33_r_rwl = powt(PIST_33_r_rwl,rescale = TRUE)
  PIST_33_r_rwl <- PIST_33_r_rwl[, sapply(PIST_33_r_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_r_rwl = dplR::detrend(PIST_33_r_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_r_rwl = PIST_33_r_rwl[which(rownames(PIST_33_r_rwl) >= 1950 & rownames(PIST_33_r_rwl) <= 2010),]
  PIST_33_r_chron = chron(PIST_33_r_rwl,prefix = "CAM")
  PIST_33_r_chron = PIST_33_r_chron[which(rownames(PIST_33_r_chron) >= 1950 & rownames(PIST_33_r_chron) <= 2010),]
  #plot(PIST_33_r_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_r_rwl,"PIST_33_r_rwl_1950_2010.csv")
  #write.csv(PIST_33_r_chron,"PIST_33_r_chron_1950_2010.csv")    
  
  #UFR
  PIST_33_u = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[21])
  PIST_33_u_rwl = PIST_33_u[,20:301]
  PIST_33_u_rwl = data.frame(rev(PIST_33_u_rwl))
  PIST_33_u_rwl = data.frame(t(PIST_33_u_rwl))
  rownames(PIST_33_u_rwl) = 1735:2016
  colnames(PIST_33_u_rwl) = PIST_33_u $TreeID
  unique_treeid = unique(colnames(PIST_33_u_rwl))
  PIST_33_u_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_u_rwl_1) = 1735:2016
  colnames(PIST_33_u_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_u_rwl[,which(colnames(PIST_33_u_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_u_rwl_1[,i] = requried_col;next}
    PIST_33_u_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_u_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_u_rwl = PIST_33_u_rwl_1
  PIST_33_u_rwl = PIST_33_u_rwl[,which(colnames(PIST_33_u_rwl) %in% info1$series)]
  
  PIST_33_u_rwl = powt(PIST_33_u_rwl,rescale = TRUE)
  PIST_33_u_rwl <- PIST_33_u_rwl[, sapply(PIST_33_u_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_u_rwl = dplR::detrend(PIST_33_u_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_u_rwl = PIST_33_u_rwl[which(rownames(PIST_33_u_rwl) >= 1950 & rownames(PIST_33_u_rwl) <= 2010),]
  PIST_33_u_chron = chron(PIST_33_u_rwl,prefix = "CAM")
  PIST_33_u_chron = PIST_33_u_chron[which(rownames(PIST_33_u_chron) >= 1950 & rownames(PIST_33_u_chron) <= 2010),]
  #plot(PIST_33_u_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_u_rwl,"PIST_33_u_rwl_1950_2010.csv")
  #write.csv(PIST_33_u_chron,"PIST_33_u_chron_1950_2010.csv")  
  
  #VMC812 1130
  PIST_33_v = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[22:23])
  PIST_33_v_rwl = PIST_33_v[,20:301]
  PIST_33_v_rwl = data.frame(rev(PIST_33_v_rwl))
  PIST_33_v_rwl = data.frame(t(PIST_33_v_rwl))
  rownames(PIST_33_v_rwl) = 1735:2016
  colnames(PIST_33_v_rwl) = PIST_33_v $TreeID
  unique_treeid = unique(colnames(PIST_33_v_rwl))
  PIST_33_v_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_v_rwl_1) = 1735:2016
  colnames(PIST_33_v_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_v_rwl[,which(colnames(PIST_33_v_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_v_rwl_1[,i] = requried_col;next}
    PIST_33_v_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_v_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_v_rwl = PIST_33_v_rwl_1
  PIST_33_v_rwl = PIST_33_v_rwl[,which(colnames(PIST_33_v_rwl) %in% info1$series)]
  
  PIST_33_v_rwl = powt(PIST_33_v_rwl,rescale = TRUE)
  PIST_33_v_rwl <- PIST_33_v_rwl[, sapply(PIST_33_v_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_v_rwl = dplR::detrend(PIST_33_v_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_v_rwl = PIST_33_v_rwl[which(rownames(PIST_33_v_rwl) >= 1950 & rownames(PIST_33_v_rwl) <= 2010),]
  PIST_33_v_chron = chron(PIST_33_v_rwl,prefix = "CAM")
  PIST_33_v_chron = PIST_33_v_chron[which(rownames(PIST_33_v_chron) >= 1950 & rownames(PIST_33_v_chron) <= 2010),]
  #plot(PIST_33_v_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_v_rwl,"PIST_33_v_rwl_1950_2010.csv")
  #write.csv(PIST_33_v_chron,"PIST_33_v_chron_1950_2010.csv")    
  
  #WLWO
  PIST_33_w = subset(PIST_1,PlotID %in% PIST_33_plot$PlotID[25:26])
  PIST_33_w_rwl = PIST_33_w[,20:301]
  PIST_33_w_rwl = data.frame(rev(PIST_33_w_rwl))
  PIST_33_w_rwl = data.frame(t(PIST_33_w_rwl))
  rownames(PIST_33_w_rwl) = 1735:2016
  colnames(PIST_33_w_rwl) = PIST_33_w $TreeID
  unique_treeid = unique(colnames(PIST_33_w_rwl))
  PIST_33_w_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(PIST_33_w_rwl_1) = 1735:2016
  colnames(PIST_33_w_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = PIST_33_w_rwl[,which(colnames(PIST_33_w_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){PIST_33_w_rwl_1[,i] = requried_col;next}
    PIST_33_w_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(PIST_33_w_rwl_1)
  info1 = subset(info1,first <= 1950)
  PIST_33_w_rwl = PIST_33_w_rwl_1
  PIST_33_w_rwl = PIST_33_w_rwl[,which(colnames(PIST_33_w_rwl) %in% info1$series)]
  
  PIST_33_w_rwl = powt(PIST_33_w_rwl,rescale = TRUE)
  PIST_33_w_rwl <- PIST_33_w_rwl[, sapply(PIST_33_w_rwl, function(x) all(is.na(x) | x >= 0))]
  PIST_33_w_rwl = dplR::detrend(PIST_33_w_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  PIST_33_w_rwl = PIST_33_w_rwl[which(rownames(PIST_33_w_rwl) >= 1950 & rownames(PIST_33_w_rwl) <= 2010),]
  PIST_33_w_chron = chron(PIST_33_w_rwl,prefix = "CAM")
  PIST_33_w_chron = PIST_33_w_chron[which(rownames(PIST_33_w_chron) >= 1950 & rownames(PIST_33_w_chron) <= 2010),]
  #plot(PIST_33_w_chron, add.spline=FALSE, nyrs=60)
  #write.csv(PIST_33_w_rwl,"PIST_33_w_rwl_1950_2010.csv")
  #write.csv(PIST_33_w_chron,"PIST_33_w_chron_1950_2010.csv")  

  #TSCA
  #####Project_33#####
  TSCA_1 = read.csv(".\\den\\TSCA_33\\core.csv",header = F)
  colnames(TSCA_1) = TSCA_1[1,]
  TSCA_1 = TSCA_1[2:nrow(TSCA_1),]
  
  TSCA_1 = subset(TSCA_1,Species == 'TSCA')
  TSCA_33_plot = read.csv(".\\den\\TSCA_33\\plot.csv")
  #a
  TSCA_33_a = subset(TSCA_1,PlotID %in% TSCA_33_plot$PlotID[1:2])
  TSCA_33_a_rwl = TSCA_33_a[,20:301]
  TSCA_33_a_rwl = data.frame(rev(TSCA_33_a_rwl))
  TSCA_33_a_rwl = data.frame(t(TSCA_33_a_rwl))
  rownames(TSCA_33_a_rwl) = 1735:2016
  colnames(TSCA_33_a_rwl) = TSCA_33_a $TreeID
  unique_treeid = unique(colnames(TSCA_33_a_rwl))
  TSCA_33_a_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(TSCA_33_a_rwl_1) = 1735:2016
  colnames(TSCA_33_a_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = TSCA_33_a_rwl[,which(colnames(TSCA_33_a_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){TSCA_33_a_rwl_1[,i] = requried_col;next}
    TSCA_33_a_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(TSCA_33_a_rwl_1)
  info1 = subset(info1,first <= 1950)
  TSCA_33_a_rwl = TSCA_33_a_rwl_1
  TSCA_33_a_rwl = TSCA_33_a_rwl[,which(colnames(TSCA_33_a_rwl) %in% info1$series)]
  
  TSCA_33_a_rwl = powt(TSCA_33_a_rwl,rescale = TRUE)
  TSCA_33_a_rwl <- TSCA_33_a_rwl[, sapply(TSCA_33_a_rwl, function(x) all(is.na(x) | x >= 0))]
  TSCA_33_a_rwl = dplR::detrend(TSCA_33_a_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  TSCA_33_a_rwl = TSCA_33_a_rwl[which(rownames(TSCA_33_a_rwl) >= 1950 & rownames(TSCA_33_a_rwl) <= 2010),]
  TSCA_33_a_chron = chron(TSCA_33_a_rwl,prefix = "CAM")
  TSCA_33_a_chron = TSCA_33_a_chron[which(rownames(TSCA_33_a_chron) >= 1950 & rownames(TSCA_33_a_chron) <= 2010),]
  #plot(TSCA_33_a_chron, add.spline=FALSE, nyrs=60)
  #write.csv(TSCA_33_a_rwl,"TSCA_33_a_rwl_1950_2010.csv")
  #write.csv(TSCA_33_a_chron,"TSCA_33_a_chron_1950_2010.csv")
  
  
  #b
  TSCA_33_b = subset(TSCA_1,PlotID %in% TSCA_33_plot$PlotID[3])
  TSCA_33_b_rwl = TSCA_33_b[,20:301]
  TSCA_33_b_rwl = data.frame(rev(TSCA_33_b_rwl))
  TSCA_33_b_rwl = data.frame(t(TSCA_33_b_rwl))
  rownames(TSCA_33_b_rwl) = 1735:2016
  colnames(TSCA_33_b_rwl) = TSCA_33_b $TreeID
  unique_treeid = unique(colnames(TSCA_33_b_rwl))
  TSCA_33_b_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(TSCA_33_b_rwl_1) = 1735:2016
  colnames(TSCA_33_b_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = TSCA_33_b_rwl[,which(colnames(TSCA_33_b_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){TSCA_33_b_rwl_1[,i] = requried_col;next}
    TSCA_33_b_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(TSCA_33_b_rwl_1)
  info1 = subset(info1,first <= 1950)
  TSCA_33_b_rwl = TSCA_33_b_rwl_1
  TSCA_33_b_rwl = TSCA_33_b_rwl[,which(colnames(TSCA_33_b_rwl) %in% info1$series)]
  
  TSCA_33_b_rwl = powt(TSCA_33_b_rwl,rescale = TRUE)
  TSCA_33_b_rwl <- TSCA_33_b_rwl[, sapply(TSCA_33_b_rwl, function(x) all(is.na(x) | x >= 0))]
  TSCA_33_b_rwl = dplR::detrend(TSCA_33_b_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  TSCA_33_b_rwl = TSCA_33_b_rwl[which(rownames(TSCA_33_b_rwl) >= 1950 & rownames(TSCA_33_b_rwl) <= 2010),]
  TSCA_33_b_chron = chron(TSCA_33_b_rwl,prefix = "CAM")
  TSCA_33_b_chron = TSCA_33_b_chron[which(rownames(TSCA_33_b_chron) >= 1950 & rownames(TSCA_33_b_chron) <= 2010),]
  #plot(TSCA_33_b_chron, add.spline=FALSE, nyrs=60)
  #write.csv(TSCA_33_b_rwl,"TSCA_33_b_rwl_1950_2010.csv")
  #write.csv(TSCA_33_b_chron,"TSCA_33_b_chron_1950_2010.csv")
  
  #g
  TSCA_33_g = subset(TSCA_1,PlotID %in% TSCA_33_plot$PlotID[7:9])
  TSCA_33_g_rwl = TSCA_33_g[,20:301]
  TSCA_33_g_rwl = data.frame(rev(TSCA_33_g_rwl))
  TSCA_33_g_rwl = data.frame(t(TSCA_33_g_rwl))
  rownames(TSCA_33_g_rwl) = 1735:2016
  colnames(TSCA_33_g_rwl) = TSCA_33_g $TreeID
  unique_treeid = unique(colnames(TSCA_33_g_rwl))
  TSCA_33_g_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(TSCA_33_g_rwl_1) = 1735:2016
  colnames(TSCA_33_g_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = TSCA_33_g_rwl[,which(colnames(TSCA_33_g_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){TSCA_33_g_rwl_1[,i] = requried_col;next}
    TSCA_33_g_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(TSCA_33_g_rwl_1)
  info1 = subset(info1,first <= 1950)
  TSCA_33_g_rwl = TSCA_33_g_rwl_1
  TSCA_33_g_rwl = TSCA_33_g_rwl[,which(colnames(TSCA_33_g_rwl) %in% info1$series)]
  
  TSCA_33_g_rwl = powt(TSCA_33_g_rwl,rescale = TRUE)
  TSCA_33_g_rwl <- TSCA_33_g_rwl[, sapply(TSCA_33_g_rwl, function(x) all(is.na(x) | x >= 0))]
  TSCA_33_g_rwl = dplR::detrend(TSCA_33_g_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  TSCA_33_g_rwl = TSCA_33_g_rwl[which(rownames(TSCA_33_g_rwl) >= 1950 & rownames(TSCA_33_g_rwl) <= 2010),]
  TSCA_33_g_chron = chron(TSCA_33_g_rwl,prefix = "CAM")
  TSCA_33_g_chron = TSCA_33_g_chron[which(rownames(TSCA_33_g_chron) >= 1950 & rownames(TSCA_33_g_chron) <= 2010),]
  #plot(TSCA_33_g_chron, add.spline=FALSE, nyrs=60)
  #write.csv(TSCA_33_g_rwl,"TSCA_33_g_rwl_1950_2010.csv")
  #write.csv(TSCA_33_g_chron,"TSCA_33_g_chron_1950_2010.csv")
  
  #h
  TSCA_33_h = subset(TSCA_1,PlotID %in% TSCA_33_plot$PlotID[10:12])
  TSCA_33_h_rwl = TSCA_33_h[,20:301]
  TSCA_33_h_rwl = data.frame(rev(TSCA_33_h_rwl))
  TSCA_33_h_rwl = data.frame(t(TSCA_33_h_rwl))
  rownames(TSCA_33_h_rwl) = 1735:2016
  colnames(TSCA_33_h_rwl) = TSCA_33_h $TreeID
  unique_treeid = unique(colnames(TSCA_33_h_rwl))
  TSCA_33_h_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(TSCA_33_h_rwl_1) = 1735:2016
  colnames(TSCA_33_h_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = TSCA_33_h_rwl[,which(colnames(TSCA_33_h_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){TSCA_33_h_rwl_1[,i] = requried_col;next}
    TSCA_33_h_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(TSCA_33_h_rwl_1)
  info1 = subset(info1,first <= 1950)
  TSCA_33_h_rwl = TSCA_33_h_rwl_1
  TSCA_33_h_rwl = TSCA_33_h_rwl[,which(colnames(TSCA_33_h_rwl) %in% info1$series)]
  
  TSCA_33_h_rwl = powt(TSCA_33_h_rwl,rescale = TRUE)
  TSCA_33_h_rwl <- TSCA_33_h_rwl[, sapply(TSCA_33_h_rwl, function(x) all(is.na(x) | x >= 0))]
  TSCA_33_h_rwl = dplR::detrend(TSCA_33_h_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  TSCA_33_h_rwl = TSCA_33_h_rwl[which(rownames(TSCA_33_h_rwl) >= 1950 & rownames(TSCA_33_h_rwl) <= 2010),]
  TSCA_33_h_chron = chron(TSCA_33_h_rwl,prefix = "CAM")
  TSCA_33_h_chron = TSCA_33_h_chron[which(rownames(TSCA_33_h_chron) >= 1950 & rownames(TSCA_33_h_chron) <= 2010),]
  #plot(TSCA_33_h_chron, add.spline=FALSE, nyrs=60)
  #write.csv(TSCA_33_h_rwl,"TSCA_33_h_rwl_1950_2010.csv")
  #write.csv(TSCA_33_h_chron,"TSCA_33_h_chron_1950_2010.csv")
  
  
  #l
  TSCA_33_l = subset(TSCA_1,PlotID %in% TSCA_33_plot$PlotID[14:15])
  TSCA_33_l_rwl = TSCA_33_l[,20:301]
  TSCA_33_l_rwl = data.frame(rev(TSCA_33_l_rwl))
  TSCA_33_l_rwl = data.frame(t(TSCA_33_l_rwl))
  rownames(TSCA_33_l_rwl) = 1735:2016
  colnames(TSCA_33_l_rwl) = TSCA_33_l $TreeID
  unique_treeid = unique(colnames(TSCA_33_l_rwl))
  TSCA_33_l_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(TSCA_33_l_rwl_1) = 1735:2016
  colnames(TSCA_33_l_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = TSCA_33_l_rwl[,which(colnames(TSCA_33_l_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){TSCA_33_l_rwl_1[,i] = requried_col;next}
    TSCA_33_l_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(TSCA_33_l_rwl_1)
  info1 = subset(info1,first <= 1950)
  TSCA_33_l_rwl = TSCA_33_l_rwl_1
  TSCA_33_l_rwl = TSCA_33_l_rwl[,which(colnames(TSCA_33_l_rwl) %in% info1$series)]
  
  TSCA_33_l_rwl = powt(TSCA_33_l_rwl,rescale = TRUE)
  TSCA_33_l_rwl <- TSCA_33_l_rwl[, sapply(TSCA_33_l_rwl, function(x) all(is.na(x) | x >= 0))]
  TSCA_33_l_rwl = dplR::detrend(TSCA_33_l_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  TSCA_33_l_rwl = TSCA_33_l_rwl[which(rownames(TSCA_33_l_rwl) >= 1950 & rownames(TSCA_33_l_rwl) <= 2010),]
  TSCA_33_l_chron = chron(TSCA_33_l_rwl,prefix = "CAM")
  TSCA_33_l_chron = TSCA_33_l_chron[which(rownames(TSCA_33_l_chron) >= 1950 & rownames(TSCA_33_l_chron) <= 2010),]
  #plot(TSCA_33_l_chron, add.spline=FALSE, nyrs=60)
  #write.csv(TSCA_33_l_rwl,"TSCA_33_l_rwl_1950_2010.csv")
  #write.csv(TSCA_33_l_chron,"TSCA_33_l_chron_1950_2010.csv")
  
  
  #m
  TSCA_33_m = subset(TSCA_1,PlotID %in% TSCA_33_plot$PlotID[16])
  TSCA_33_m_rwl = TSCA_33_m[,20:301]
  TSCA_33_m_rwl = data.frame(rev(TSCA_33_m_rwl))
  TSCA_33_m_rwl = data.frame(t(TSCA_33_m_rwl))
  rownames(TSCA_33_m_rwl) = 1735:2016
  colnames(TSCA_33_m_rwl) = TSCA_33_m $TreeID
  unique_treeid = unique(colnames(TSCA_33_m_rwl))
  TSCA_33_m_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(TSCA_33_m_rwl_1) = 1735:2016
  colnames(TSCA_33_m_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = TSCA_33_m_rwl[,which(colnames(TSCA_33_m_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){TSCA_33_m_rwl_1[,i] = requried_col;next}
    TSCA_33_m_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(TSCA_33_m_rwl_1)
  info1 = subset(info1,first <= 1950)
  TSCA_33_m_rwl = TSCA_33_m_rwl_1
  TSCA_33_m_rwl = TSCA_33_m_rwl[,which(colnames(TSCA_33_m_rwl) %in% info1$series)]
  
  TSCA_33_m_rwl = powt(TSCA_33_m_rwl,rescale = TRUE)
  TSCA_33_m_rwl <- TSCA_33_m_rwl[, sapply(TSCA_33_m_rwl, function(x) all(is.na(x) | x >= 0))]
  TSCA_33_m_rwl = dplR::detrend(TSCA_33_m_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  TSCA_33_m_rwl = TSCA_33_m_rwl[which(rownames(TSCA_33_m_rwl) >= 1950 & rownames(TSCA_33_m_rwl) <= 2010),]
  TSCA_33_m_chron = chron(TSCA_33_m_rwl,prefix = "CAM")
  TSCA_33_m_chron = TSCA_33_m_chron[which(rownames(TSCA_33_m_chron) >= 1950 & rownames(TSCA_33_m_chron) <= 2010),]
  #plot(TSCA_33_m_chron, add.spline=FALSE, nyrs=60)
  #write.csv(TSCA_33_m_rwl,"TSCA_33_m_rwl_1950_2010.csv")
  #write.csv(TSCA_33_m_chron,"TSCA_33_m_chron_1950_2010.csv")
  
  #w
  TSCA_33_w = subset(TSCA_1,PlotID %in% TSCA_33_plot$PlotID[25:26])
  TSCA_33_w_rwl = TSCA_33_w[,20:301]
  TSCA_33_w_rwl = data.frame(rev(TSCA_33_w_rwl))
  TSCA_33_w_rwl = data.frame(t(TSCA_33_w_rwl))
  rownames(TSCA_33_w_rwl) = 1735:2016
  colnames(TSCA_33_w_rwl) = TSCA_33_w $TreeID
  unique_treeid = unique(colnames(TSCA_33_w_rwl))
  TSCA_33_w_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1735+1))
  rownames(TSCA_33_w_rwl_1) = 1735:2016
  colnames(TSCA_33_w_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = TSCA_33_w_rwl[,which(colnames(TSCA_33_w_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){TSCA_33_w_rwl_1[,i] = requried_col;next}
    TSCA_33_w_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  
  info1 = rwl.stats(TSCA_33_w_rwl_1)
  info1 = subset(info1,first <= 1950)
  TSCA_33_w_rwl = TSCA_33_w_rwl_1
  TSCA_33_w_rwl = TSCA_33_w_rwl[,which(colnames(TSCA_33_w_rwl) %in% info1$series)]
  
  TSCA_33_w_rwl = powt(TSCA_33_w_rwl,rescale = TRUE)
  TSCA_33_w_rwl <- TSCA_33_w_rwl[, sapply(TSCA_33_w_rwl, function(x) all(is.na(x) | x >= 0))]
  TSCA_33_w_rwl = dplR::detrend(TSCA_33_w_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  TSCA_33_w_rwl = TSCA_33_w_rwl[which(rownames(TSCA_33_w_rwl) >= 1950 & rownames(TSCA_33_w_rwl) <= 2010),]
  TSCA_33_w_chron = chron(TSCA_33_w_rwl,prefix = "CAM")
  TSCA_33_w_chron = TSCA_33_w_chron[which(rownames(TSCA_33_w_chron) >= 1950 & rownames(TSCA_33_w_chron) <= 2010),]
  #plot(TSCA_33_w_chron, add.spline=FALSE, nyrs=60)
  #write.csv(TSCA_33_w_rwl,"TSCA_33_w_rwl_1950_2010.csv")
  #write.csv(TSCA_33_w_chron,"TSCA_33_w_chron_1950_2010.csv")
  
  #QURU
  #####Project_39#####
  QURU_1 = read.csv(".\\den\\QURU_39\\core.csv")
  unique(QURU_1$Species)
  QURU_1 = subset(QURU_1,Species == 'QURU')
  QURU_39_plot = read.csv(".\\den\\QURU_39\\plot.csv")
  
  QURU_39_rwl = QURU_1[,20:ncol(QURU_1)]
  QURU_39_rwl = data.frame(rev(QURU_39_rwl))
  QURU_39_rwl = data.frame(t(QURU_39_rwl))
  rownames(QURU_39_rwl) = 1777:2017
  colnames(QURU_39_rwl) = QURU_1$TreeID
  for(i in 1:nrow(QURU_39_rwl)){
    for(j in 1:ncol(QURU_39_rwl)){
      if(QURU_39_rwl[i,j] == 0){
        QURU_39_rwl[i,j] = NA
        next
      }
    }
  }
  
  info1 = rwl.stats(QURU_39_rwl)
  info1 = subset(info1,first <= 1950)
  QURU_39_rwl = QURU_39_rwl[,which(colnames(QURU_39_rwl) %in% info1$series)]
  
  QURU_39_rwl = powt(QURU_39_rwl,rescale = TRUE)
  QURU_39_rwl <- QURU_39_rwl[, sapply(QURU_39_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_39_rwl = dplR::detrend(QURU_39_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_39_rwl = QURU_39_rwl[which(rownames(QURU_39_rwl) >= 1950 & rownames(QURU_39_rwl) <= 2010),]
  QURU_39_chron = chron(QURU_39_rwl,prefix = "CAM")
  QURU_39_chron = QURU_39_chron[which(rownames(QURU_39_chron) >= 1950 & rownames(QURU_39_chron) <= 2010),]
  #plot(QURU_39_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_39_rwl,"QURU_39_rwl_1950_2010.csv")
  #write.csv(QURU_39_chron,"QURU_39_chron_1950_2010.csv")
  
  
  #####Project_31#####
  QURU_1 = read.csv(".\\den\\QURU_31\\core.csv",
                    row.names = NULL)
  colnames(QURU_1) = c("ProjectID","Project","PlotID","TreeID","Species","DBH","DBH_Units",
                       "Increment_Units","CoreID","MeasurementMethod","RingCount",
                       "AverageRingWidth","BarkPresence","BarkWidth","PithPresence",
                       "CorrectedYears","EstimatedAge","Analyst","Dendrochronologist",
                       rev(1865:2016))
  unique(QURU_1$Species)
  QURU_1 = subset(QURU_1,Species == 'Quercus rubra')
  QURU_31_plot = read.csv(".\\den\\QURU_31\\plot.csv")
  #d
  QURU_31_d = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[1:2])
  QURU_31_d_rwl = QURU_31_d[,20:ncol(QURU_31_d)]
  QURU_31_d_rwl = data.frame(rev(QURU_31_d_rwl))
  QURU_31_d_rwl = data.frame(t(QURU_31_d_rwl))
  rownames(QURU_31_d_rwl) = 1865:2016
  colnames(QURU_31_d_rwl) = QURU_31_d $TreeID
  
  unique_treeid = unique(QURU_31_d$TreeID)
  QURU_31_d_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_d_rwl_1) = 1865:2016
  colnames(QURU_31_d_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_d_rwl[,which(colnames(QURU_31_d_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_d_rwl_1[,i] = requried_col;next}
    QURU_31_d_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_d_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_d_rwl_1 = QURU_31_d_rwl_1[,which(colnames(QURU_31_d_rwl_1) %in% info1$series)]
  
  QURU_31_d_rwl = powt(QURU_31_d_rwl_1,rescale = TRUE)
  QURU_31_d_rwl <- QURU_31_d_rwl[, sapply(QURU_31_d_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_d_rwl = dplR::detrend(QURU_31_d_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_d_rwl = QURU_31_d_rwl[which(rownames(QURU_31_d_rwl) >= 1950 & rownames(QURU_31_d_rwl) <= 2010),]
  QURU_31_d_chron = chron(QURU_31_d_rwl,prefix = "CAM")
  QURU_31_d_chron = QURU_31_d_chron[which(rownames(QURU_31_d_chron) >= 1950 & rownames(QURU_31_d_chron) <= 2010),]
  #plot(QURU_31_d_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_d_rwl,"QURU_31_d_rwl_1950_2010.csv")
  #write.csv(QURU_31_d_chron,"QURU_31_d_chron_1950_2010.csv")
  
  #g
  QURU_31_g = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[3])
  QURU_31_g_rwl = QURU_31_g[,20:ncol(QURU_31_g)]
  QURU_31_g_rwl = data.frame(rev(QURU_31_g_rwl))
  QURU_31_g_rwl = data.frame(t(QURU_31_g_rwl))
  rownames(QURU_31_g_rwl) = 1865:2016
  colnames(QURU_31_g_rwl) = QURU_31_g $TreeID
  
  unique_treeid = unique(QURU_31_g $TreeID)
  QURU_31_g_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_g_rwl_1) = 1865:2016
  colnames(QURU_31_g_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_g_rwl[,which(colnames(QURU_31_g_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_g_rwl_1[,i] = requried_col;next}
    QURU_31_g_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_g_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_g_rwl_1 = QURU_31_g_rwl_1[,which(colnames(QURU_31_g_rwl_1) %in% info1$series)]
  
  QURU_31_g_rwl = powt(QURU_31_g_rwl_1,rescale = TRUE)
  QURU_31_g_rwl <- QURU_31_g_rwl[, sapply(QURU_31_g_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_g_rwl = dplR::detrend(QURU_31_g_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_g_rwl = QURU_31_g_rwl[which(rownames(QURU_31_g_rwl) >= 1950 & rownames(QURU_31_g_rwl) <= 2010),]
  QURU_31_g_chron = chron(QURU_31_g_rwl,prefix = "CAM")
  QURU_31_g_chron = QURU_31_g_chron[which(rownames(QURU_31_g_chron) >= 1950 & rownames(QURU_31_g_chron) <= 2010),]
  #plot(QURU_31_g_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_g_rwl,"QURU_31_g_rwl_1950_2010.csv")
  #write.csv(QURU_31_g_chron,"QURU_31_g_chron_1950_2010.csv")
  
  #h
  QURU_31_h = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[4:5])
  QURU_31_h_rwl = QURU_31_h[,20:ncol(QURU_31_h)]
  QURU_31_h_rwl = data.frame(rev(QURU_31_h_rwl))
  QURU_31_h_rwl = data.frame(t(QURU_31_h_rwl))
  rownames(QURU_31_h_rwl) = 1865:2016
  colnames(QURU_31_h_rwl) = QURU_31_h $TreeID
  
  unique_treeid = unique(QURU_31_h $TreeID)
  QURU_31_h_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_h_rwl_1) = 1865:2016
  colnames(QURU_31_h_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_h_rwl[,which(colnames(QURU_31_h_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_h_rwl_1[,i] = requried_col;next}
    QURU_31_h_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_h_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_h_rwl_1 = QURU_31_h_rwl_1[,which(colnames(QURU_31_h_rwl_1) %in% info1$series)]
  
  QURU_31_h_rwl = powt(QURU_31_h_rwl_1,rescale = TRUE)
  QURU_31_h_rwl <- QURU_31_h_rwl[, sapply(QURU_31_h_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_h_rwl = dplR::detrend(QURU_31_h_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_h_rwl = QURU_31_h_rwl[which(rownames(QURU_31_h_rwl) >= 1950 & rownames(QURU_31_h_rwl) <= 2010),]
  QURU_31_h_chron = chron(QURU_31_h_rwl,prefix = "CAM")
  QURU_31_h_chron = QURU_31_h_chron[which(rownames(QURU_31_h_chron) >= 1950 & rownames(QURU_31_h_chron) <= 2010),]
  #plot(QURU_31_h_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_h_rwl,"QURU_31_h_rwl_1950_2010.csv")
  #write.csv(QURU_31_h_chron,"QURU_31_h_chron_1950_2010.csv")
  
  #i
  QURU_31_i = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[6])
  QURU_31_i_rwl = QURU_31_i[,20:ncol(QURU_31_i)]
  QURU_31_i_rwl = data.frame(rev(QURU_31_i_rwl))
  QURU_31_i_rwl = data.frame(t(QURU_31_i_rwl))
  rownames(QURU_31_i_rwl) = 1865:2016
  colnames(QURU_31_i_rwl) = QURU_31_i $TreeID
  
  unique_treeid = unique(QURU_31_i $TreeID)
  QURU_31_i_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_i_rwl_1) = 1865:2016
  colnames(QURU_31_i_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_i_rwl[,which(colnames(QURU_31_i_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_i_rwl_1[,i] = requried_col;next}
    QURU_31_i_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_i_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_i_rwl_1 = QURU_31_i_rwl_1[,which(colnames(QURU_31_i_rwl_1) %in% info1$series)]
  
  QURU_31_i_rwl = powt(QURU_31_i_rwl_1,rescale = TRUE)
  QURU_31_i_rwl <- QURU_31_i_rwl[, sapply(QURU_31_i_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_i_rwl = dplR::detrend(QURU_31_i_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_i_rwl = QURU_31_i_rwl[which(rownames(QURU_31_i_rwl) >= 1950 & rownames(QURU_31_i_rwl) <= 2010),]
  QURU_31_i_chron = chron(QURU_31_i_rwl,prefix = "CAM")
  QURU_31_i_chron = QURU_31_i_chron[which(rownames(QURU_31_i_chron) >= 1950 & rownames(QURU_31_i_chron) <= 2010),]
  #plot(QURU_31_i_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_i_rwl,"QURU_31_i_rwl_1950_2010.csv")
  #write.csv(QURU_31_i_chron,"QURU_31_i_chron_1950_2010.csv")
  
  #l
  QURU_31_l = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[7:8])
  QURU_31_l_rwl = QURU_31_l[,20:ncol(QURU_31_l)]
  QURU_31_l_rwl = data.frame(rev(QURU_31_l_rwl))
  QURU_31_l_rwl = data.frame(t(QURU_31_l_rwl))
  rownames(QURU_31_l_rwl) = 1865:2016
  colnames(QURU_31_l_rwl) = QURU_31_l $TreeID
  
  unique_treeid = unique(QURU_31_l $TreeID)
  QURU_31_l_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_l_rwl_1) = 1865:2016
  colnames(QURU_31_l_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_l_rwl[,which(colnames(QURU_31_l_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_l_rwl_1[,i] = requried_col;next}
    QURU_31_l_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_l_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_l_rwl_1 = QURU_31_l_rwl_1[,which(colnames(QURU_31_l_rwl_1) %in% info1$series)]
  
  QURU_31_l_rwl = powt(QURU_31_l_rwl_1,rescale = TRUE)
  QURU_31_l_rwl <- QURU_31_l_rwl[, sapply(QURU_31_l_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_l_rwl = dplR::detrend(QURU_31_l_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_l_rwl = QURU_31_l_rwl[which(rownames(QURU_31_l_rwl) >= 1950 & rownames(QURU_31_l_rwl) <= 2010),]
  QURU_31_l_chron = chron(QURU_31_l_rwl,prefix = "CAM")
  QURU_31_l_chron = QURU_31_l_chron[which(rownames(QURU_31_l_chron) >= 1950 & rownames(QURU_31_l_chron) <= 2010),]
  #plot(QURU_31_l_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_l_rwl,"QURU_31_l_rwl_1950_2010.csv")
  #write.csv(QURU_31_l_chron,"QURU_31_l_chron_1950_2010.csv")
  
  #n
  QURU_31_n = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[9])
  QURU_31_n_rwl = QURU_31_n[,20:ncol(QURU_31_n)]
  QURU_31_n_rwl = data.frame(rev(QURU_31_n_rwl))
  QURU_31_n_rwl = data.frame(t(QURU_31_n_rwl))
  rownames(QURU_31_n_rwl) = 1865:2016
  colnames(QURU_31_n_rwl) = QURU_31_n $TreeID
  
  unique_treeid = unique(QURU_31_n $TreeID)
  QURU_31_n_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_n_rwl_1) = 1865:2016
  colnames(QURU_31_n_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_n_rwl[,which(colnames(QURU_31_n_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_n_rwl_1[,i] = requried_col;next}
    QURU_31_n_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_n_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_n_rwl_1 = QURU_31_n_rwl_1[,which(colnames(QURU_31_n_rwl_1) %in% info1$series)]
  
  QURU_31_n_rwl = powt(QURU_31_n_rwl_1,rescale = TRUE)
  QURU_31_n_rwl <- QURU_31_n_rwl[, sapply(QURU_31_n_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_n_rwl = dplR::detrend(QURU_31_n_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_n_rwl = QURU_31_n_rwl[which(rownames(QURU_31_n_rwl) >= 1950 & rownames(QURU_31_n_rwl) <= 2010),]
  QURU_31_n_chron = chron(QURU_31_n_rwl,prefix = "CAM")
  QURU_31_n_chron = QURU_31_n_chron[which(rownames(QURU_31_n_chron) >= 1950 & rownames(QURU_31_n_chron) <= 2010),]
  #plot(QURU_31_n_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_n_rwl,"QURU_31_n_rwl_1950_2010.csv")
  #write.csv(QURU_31_n_chron,"QURU_31_n_chron_1950_2010.csv")
  
  #r
  QURU_31_r = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[10])
  QURU_31_r_rwl = QURU_31_r[,20:ncol(QURU_31_r)]
  QURU_31_r_rwl = data.frame(rev(QURU_31_r_rwl))
  QURU_31_r_rwl = data.frame(t(QURU_31_r_rwl))
  rownames(QURU_31_r_rwl) = 1865:2016
  colnames(QURU_31_r_rwl) = QURU_31_r $TreeID
  
  unique_treeid = unique(QURU_31_r $TreeID)
  QURU_31_r_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_r_rwl_1) = 1865:2016
  colnames(QURU_31_r_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_r_rwl[,which(colnames(QURU_31_r_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_r_rwl_1[,i] = requried_col;next}
    QURU_31_r_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_r_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_r_rwl_1 = QURU_31_r_rwl_1[,which(colnames(QURU_31_r_rwl_1) %in% info1$series)]
  
  QURU_31_r_rwl = powt(QURU_31_r_rwl_1,rescale = TRUE)
  QURU_31_r_rwl <- QURU_31_r_rwl[, sapply(QURU_31_r_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_r_rwl = dplR::detrend(QURU_31_r_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_r_rwl = QURU_31_r_rwl[which(rownames(QURU_31_r_rwl) >= 1950 & rownames(QURU_31_r_rwl) <= 2010),]
  QURU_31_r_chron = chron(QURU_31_r_rwl,prefix = "CAM")
  QURU_31_r_chron = QURU_31_r_chron[which(rownames(QURU_31_r_chron) >= 1950 & rownames(QURU_31_r_chron) <= 2010),]
  #plot(QURU_31_r_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_r_rwl,"QURU_31_r_rwl_1950_2010.csv")
  #write.csv(QURU_31_r_chron,"QURU_31_r_chron_1950_2010.csv")
  
  #s
  QURU_31_s = subset(QURU_1,PlotID %in% QURU_31_plot$PlotID[11])
  QURU_31_s_rwl = QURU_31_s[,20:ncol(QURU_31_s)]
  QURU_31_s_rwl = data.frame(rev(QURU_31_s_rwl))
  QURU_31_s_rwl = data.frame(t(QURU_31_s_rwl))
  rownames(QURU_31_s_rwl) = 1865:2016
  colnames(QURU_31_s_rwl) = QURU_31_s $TreeID
  
  unique_treeid = unique(QURU_31_s $TreeID)
  QURU_31_s_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1865+1))
  rownames(QURU_31_s_rwl_1) = 1865:2016
  colnames(QURU_31_s_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_31_s_rwl[,which(colnames(QURU_31_s_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_31_s_rwl_1[,i] = requried_col;next}
    QURU_31_s_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_31_s_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_31_s_rwl_1 = QURU_31_s_rwl_1[,which(colnames(QURU_31_s_rwl_1) %in% info1$series)]
  
  QURU_31_s_rwl = powt(QURU_31_s_rwl_1,rescale = TRUE)
  QURU_31_s_rwl <- QURU_31_s_rwl[, sapply(QURU_31_s_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_31_s_rwl = dplR::detrend(QURU_31_s_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_31_s_rwl = QURU_31_s_rwl[which(rownames(QURU_31_s_rwl) >= 1950 & rownames(QURU_31_s_rwl) <= 2010),]
  QURU_31_s_chron = chron(QURU_31_s_rwl,prefix = "CAM")
  QURU_31_s_chron = QURU_31_s_chron[which(rownames(QURU_31_s_chron) >= 1950 & rownames(QURU_31_s_chron) <= 2010),]
  #plot(QURU_31_s_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_31_s_rwl,"QURU_31_s_rwl_1950_2010.csv")
  #write.csv(QURU_31_s_chron,"QURU_31_s_chron_1950_2010.csv")
  
  #####Project_30#####
  QURU_1 = read.csv(".\\den\\QURU_30\\core.csv")
  unique(QURU_1$Species)
  QURU_1 = subset(QURU_1,Species == 'QURU')
  QURU_30_plot = read.csv(".\\den\\QURU_30\\plot.csv")
  #a
  QURU_30_a = subset(QURU_1,PlotID %in% QURU_30_plot$PlotID[1])
  QURU_30_a_rwl = QURU_30_a[,20:ncol(QURU_30_a)]
  QURU_30_a_rwl = data.frame(rev(QURU_30_a_rwl))
  QURU_30_a_rwl = data.frame(t(QURU_30_a_rwl))
  rownames(QURU_30_a_rwl) = 1670:2016
  colnames(QURU_30_a_rwl) = QURU_30_a $TreeID
  
  unique_treeid = unique(QURU_30_a$TreeID)
  QURU_30_a_rwl_1 = data.frame(matrix(ncol = length(unique_treeid),nrow = 2016-1670+1))
  rownames(QURU_30_a_rwl_1) = 1670:2016
  colnames(QURU_30_a_rwl_1) = unique_treeid
  for(i in 1:length(unique_treeid)){
    requried_col = QURU_30_a_rwl[,which(colnames(QURU_30_a_rwl) == unique_treeid[i])]
    if(class(requried_col) == "numeric"){QURU_30_a_rwl_1[,i] = requried_col;next}
    QURU_30_a_rwl_1[,i] = rowMeans(requried_col,na.rm = TRUE)
  }
  
  info1 = rwl.stats(QURU_30_a_rwl_1)
  info1 = subset(info1,first <= 1950)
  QURU_30_a_rwl_1 = QURU_30_a_rwl_1[,which(colnames(QURU_30_a_rwl_1) %in% info1$series)]
  
  QURU_30_a_rwl = powt(QURU_30_a_rwl_1,rescale = TRUE)
  QURU_30_a_rwl <- QURU_30_a_rwl[, sapply(QURU_30_a_rwl, function(x) all(is.na(x) | x >= 0))]
  QURU_30_a_rwl = dplR::detrend(QURU_30_a_rwl,method = "Spline",difference = FALSE,nyrs = 30)
  QURU_30_a_rwl = QURU_30_a_rwl[which(rownames(QURU_30_a_rwl) >= 1950 & rownames(QURU_30_a_rwl) <= 2010),]
  QURU_30_a_chron = chron(QURU_30_a_rwl,prefix = "CAM")
  QURU_30_a_chron = QURU_30_a_chron[which(rownames(QURU_30_a_chron) >= 1950 & rownames(QURU_30_a_chron) <= 2010),]
  #plot(QURU_30_a_chron, add.spline=FALSE, nyrs=60)
  #write.csv(QURU_30_a_rwl,"QURU_30_a_rwl_1950_2010.csv")
  #write.csv(QURU_30_a_chron,"QURU_30_a_chron_1950_2010.csv")
  
}

#ITRDB
{
  #QURU
  {
    #QURU ma016
    QURU_ma016 = read.rwl(".\\itrdb\\ma016.rwl")
    info1 = rwl.stats(QURU_ma016)
    info1 = subset(info1,first <= 1950)
    QURU_ma016_rwl_1 = QURU_ma016[,which(colnames(QURU_ma016) %in% info1$series)]
    
    QURU_ma016_rwl = powt(QURU_ma016_rwl_1,rescale = TRUE)
    QURU_ma016_rwl <- QURU_ma016_rwl[, sapply(QURU_ma016_rwl, function(x) all(is.na(x) | x >= 0))]
    QURU_ma016_rwl = dplR::detrend(QURU_ma016_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    QURU_ma016_rwl = QURU_ma016_rwl[which(rownames(QURU_ma016_rwl) >= 1950 & rownames(QURU_ma016_rwl) <= 2010),]
    QURU_ma016_chron = chron(QURU_ma016_rwl,prefix = "CAM")
    QURU_ma016_chron = QURU_ma016_chron[which(rownames(QURU_ma016_chron) >= 1950 & rownames(QURU_ma016_chron) <= 2010),]
    #plot(QURU_ma016_chron, add.spline=FALSE, nyrs=60)
    #write.csv(QURU_ma016_rwl,"QURU_ma016_rwl_1950_2010.csv")
    #write.csv(QURU_ma016_chron,"QURU_ma016_chron_1950_2010.csv")
    
    #QURU nj006
    QURU_nj006 = read.rwl(".\\itrdb\\nj006.rwl")
    info1 = rwl.stats(QURU_nj006)
    info1 = subset(info1,first <= 1950)
    QURU_nj006_rwl_1 = QURU_nj006[,which(colnames(QURU_nj006) %in% info1$series)]
    
    QURU_nj006_rwl = powt(QURU_nj006_rwl_1,rescale = TRUE)
    QURU_nj006_rwl <- QURU_nj006_rwl[, sapply(QURU_nj006_rwl, function(x) all(is.na(x) | x >= 0))]
    QURU_nj006_rwl = dplR::detrend(QURU_nj006_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    QURU_nj006_rwl = QURU_nj006_rwl[which(rownames(QURU_nj006_rwl) >= 1950 & rownames(QURU_nj006_rwl) <= 2010),]
    QURU_nj006_chron = chron(QURU_nj006_rwl,prefix = "CAM")
    QURU_nj006_chron = QURU_nj006_chron[which(rownames(QURU_nj006_chron) >= 1950 & rownames(QURU_nj006_chron) <= 2010),]
    #plot(QURU_nj006_chron, add.spline=FALSE, nyrs=60)
    #write.csv(QURU_nj006_rwl,"QURU_nj006_rwl_1950_2010.csv")
    #write.csv(QURU_nj006_chron,"QURU_nj006_chron_1950_2010.csv")
    
    #QURU ny018
    QURU_ny018 = read.rwl(".\\itrdb\\ny018.rwl")
    info1 = rwl.stats(QURU_ny018)
    info1 = subset(info1,first <= 1950)
    QURU_ny018_rwl_1 = QURU_ny018[,which(colnames(QURU_ny018) %in% info1$series)]
    
    QURU_ny018_rwl = powt(QURU_ny018_rwl_1,rescale = TRUE)
    QURU_ny018_rwl <- QURU_ny018_rwl[, sapply(QURU_ny018_rwl, function(x) all(is.na(x) | x >= 0))]
    QURU_ny018_rwl = dplR::detrend(QURU_ny018_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    QURU_ny018_rwl = QURU_ny018_rwl[which(rownames(QURU_ny018_rwl) >= 1950 & rownames(QURU_ny018_rwl) <= 2010),]
    QURU_ny018_chron = chron(QURU_ny018_rwl,prefix = "CAM")
    QURU_ny018_chron = QURU_ny018_chron[which(rownames(QURU_ny018_chron) >= 1950 & rownames(QURU_ny018_chron) <= 2010),]
    #plot(QURU_ny018_chron, add.spline=FALSE, nyrs=60)
    #write.csv(QURU_ny018_rwl,"QURU_ny018_rwl_1950_2010.csv")
    #write.csv(QURU_ny018_chron,"QURU_ny018_chron_1950_2010.csv")
    
    #QURU ny032
    QURU_ny032 = read.rwl(".\\itrdb\\ny032.rwl")
    info1 = rwl.stats(QURU_ny032)
    info1 = subset(info1,first <= 1950)
    QURU_ny032_rwl_1 = QURU_ny032[,which(colnames(QURU_ny032) %in% info1$series)]
    
    QURU_ny032_rwl = powt(QURU_ny032_rwl_1,rescale = TRUE)
    QURU_ny032_rwl <- QURU_ny032_rwl[, sapply(QURU_ny032_rwl, function(x) all(is.na(x) | x >= 0))]
    QURU_ny032_rwl = dplR::detrend(QURU_ny032_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    QURU_ny032_rwl = QURU_ny032_rwl[which(rownames(QURU_ny032_rwl) >= 1950 & rownames(QURU_ny032_rwl) <= 2010),]
    QURU_ny032_chron = chron(QURU_ny032_rwl,prefix = "CAM")
    QURU_ny032_chron = QURU_ny032_chron[which(rownames(QURU_ny032_chron) >= 1950 & rownames(QURU_ny032_chron) <= 2010),]
    #plot(QURU_ny032_chron, add.spline=FALSE, nyrs=60)
    #write.csv(QURU_ny032_rwl,"QURU_ny032_rwl_1950_2010.csv")
    #write.csv(QURU_ny032_chron,"QURU_ny032_chron_1950_2010.csv")
    
    #QURU ny036
    QURU_ny036 = read.rwl(".\\itrdb\\ny036.rwl")
    info1 = rwl.stats(QURU_ny036)
    info1 = subset(info1,first <= 1950)
    QURU_ny036_rwl_1 = QURU_ny036[,which(colnames(QURU_ny036) %in% info1$series)]
    
    QURU_ny036_rwl = powt(QURU_ny036_rwl_1,rescale = TRUE)
    QURU_ny036_rwl <- QURU_ny036_rwl[, sapply(QURU_ny036_rwl, function(x) all(is.na(x) | x >= 0))]
    QURU_ny036_rwl = dplR::detrend(QURU_ny036_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    QURU_ny036_rwl = QURU_ny036_rwl[which(rownames(QURU_ny036_rwl) >= 1950 & rownames(QURU_ny036_rwl) <= 2010),]
    QURU_ny036_chron = chron(QURU_ny036_rwl,prefix = "CAM")
    QURU_ny036_chron = QURU_ny036_chron[which(rownames(QURU_ny036_chron) >= 1950 & rownames(QURU_ny036_chron) <= 2010),]
    #plot(QURU_ny036_chron, add.spline=FALSE, nyrs=60)
    #write.csv(QURU_ny036_rwl,"QURU_ny036_rwl_1950_2010.csv")
    #write.csv(QURU_ny036_chron,"QURU_ny036_chron_1950_2010.csv")
  }
  #TSCA
  {
    #TSCA ma019
    TSCA_ma019 = read.rwl(".\\itrdb\\ma019.rwl")
    info1 = rwl.stats(TSCA_ma019)
    info1 = subset(info1,first <= 1950)
    TSCA_ma019_rwl_1 = TSCA_ma019[,which(colnames(TSCA_ma019) %in% info1$series)]
    
    TSCA_ma019_rwl = powt(TSCA_ma019_rwl_1,rescale = TRUE)
    TSCA_ma019_rwl <- TSCA_ma019_rwl[, sapply(TSCA_ma019_rwl, function(x) all(is.na(x) | x >= 0))]
    TSCA_ma019_rwl = dplR::detrend(TSCA_ma019_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    TSCA_ma019_rwl = TSCA_ma019_rwl[which(rownames(TSCA_ma019_rwl) >= 1950 & rownames(TSCA_ma019_rwl) <= 2010),]
    TSCA_ma019_chron = chron(TSCA_ma019_rwl,prefix = "CAM")
    TSCA_ma019_chron = TSCA_ma019_chron[which(rownames(TSCA_ma019_chron) >= 1950 & rownames(TSCA_ma019_chron) <= 2010),]
    #plot(TSCA_ma019_chron, add.spline=FALSE, nyrs=60)
    #write.csv(TSCA_ma019_rwl,"TSCA_ma019_rwl_1950_2010.csv")
    #write.csv(TSCA_ma019_chron,"TSCA_ma019_chron_1950_2010.csv")
    
    #TSCA ny029
    TSCA_ny029 = read.rwl(".\\itrdb\\ny029.rwl")
    info1 = rwl.stats(TSCA_ny029)
    info1 = subset(info1,first <= 1950)
    TSCA_ny029_rwl_1 = TSCA_ny029[,which(colnames(TSCA_ny029) %in% info1$series)]
    
    TSCA_ny029_rwl = powt(TSCA_ny029_rwl_1,rescale = TRUE)
    TSCA_ny029_rwl <- TSCA_ny029_rwl[, sapply(TSCA_ny029_rwl, function(x) all(is.na(x) | x >= 0))]
    TSCA_ny029_rwl = dplR::detrend(TSCA_ny029_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    TSCA_ny029_rwl = TSCA_ny029_rwl[which(rownames(TSCA_ny029_rwl) >= 1950 & rownames(TSCA_ny029_rwl) <= 2010),]
    TSCA_ny029_chron = chron(TSCA_ny029_rwl,prefix = "CAM")
    TSCA_ny029_chron = TSCA_ny029_chron[which(rownames(TSCA_ny029_chron) >= 1950 & rownames(TSCA_ny029_chron) <= 2010),]
    #plot(TSCA_ny029_chron, add.spline=FALSE, nyrs=60)
    #write.csv(TSCA_ny029_rwl,"TSCA_ny029_rwl_1950_2010.csv")
    #write.csv(TSCA_ny029_chron,"TSCA_ny029_chron_1950_2010.csv")
    
    #TSCA ny047
    TSCA_ny047 = read.rwl(".\\itrdb\\ny047.rwl")
    info1 = rwl.stats(TSCA_ny047)
    info1 = subset(info1,first <= 1950)
    TSCA_ny047_rwl_1 = TSCA_ny047[,which(colnames(TSCA_ny047) %in% info1$series)]
    
    TSCA_ny047_rwl = powt(TSCA_ny047_rwl_1,rescale = TRUE)
    TSCA_ny047_rwl <- TSCA_ny047_rwl[, sapply(TSCA_ny047_rwl, function(x) all(is.na(x) | x >= 0))]
    TSCA_ny047_rwl = dplR::detrend(TSCA_ny047_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    TSCA_ny047_rwl = TSCA_ny047_rwl[which(rownames(TSCA_ny047_rwl) >= 1950 & rownames(TSCA_ny047_rwl) <= 2010),]
    TSCA_ny047_chron = chron(TSCA_ny047_rwl,prefix = "CAM")
    TSCA_ny047_chron = TSCA_ny047_chron[which(rownames(TSCA_ny047_chron) >= 1950 & rownames(TSCA_ny047_chron) <= 2010),]
    #plot(TSCA_ny047_chron, add.spline=FALSE, nyrs=60)
    #write.csv(TSCA_ny047_rwl,"TSCA_ny047_rwl_1950_2010.csv")
    #write.csv(TSCA_ny047_chron,"TSCA_ny047_chron_1950_2010.csv")
    
    #TSCA pa016
    TSCA_pa016 = read.rwl(".\\itrdb\\pa016.rwl")
    info1 = rwl.stats(TSCA_pa016)
    info1 = subset(info1,first <= 1950)
    TSCA_pa016_rwl_1 = TSCA_pa016[,which(colnames(TSCA_pa016) %in% info1$series)]
    
    TSCA_pa016_rwl = powt(TSCA_pa016_rwl_1,rescale = TRUE)
    TSCA_pa016_rwl <- TSCA_pa016_rwl[, sapply(TSCA_pa016_rwl, function(x) all(is.na(x) | x >= 0))]
    TSCA_pa016_rwl = dplR::detrend(TSCA_pa016_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    TSCA_pa016_rwl = TSCA_pa016_rwl[which(rownames(TSCA_pa016_rwl) >= 1950 & rownames(TSCA_pa016_rwl) <= 2010),]
    TSCA_pa016_chron = chron(TSCA_pa016_rwl,prefix = "CAM")
    TSCA_pa016_chron = TSCA_pa016_chron[which(rownames(TSCA_pa016_chron) >= 1950 & rownames(TSCA_pa016_chron) <= 2010),]
    #plot(TSCA_pa016_chron, add.spline=FALSE, nyrs=60)
    #write.csv(TSCA_pa016_rwl,"TSCA_pa016_rwl_1950_2010.csv")
    #write.csv(TSCA_pa016_chron,"TSCA_pa016_chron_1950_2010.csv")
    
    #TSCA vt007
    TSCA_vt007 = read.rwl(".\\itrdb\\vt007.rwl")
    info1 = rwl.stats(TSCA_vt007)
    info1 = subset(info1,first <= 1950)
    TSCA_vt007_rwl_1 = TSCA_vt007[,which(colnames(TSCA_vt007) %in% info1$series)]
    
    TSCA_vt007_rwl = powt(TSCA_vt007_rwl_1,rescale = TRUE)
    TSCA_vt007_rwl <- TSCA_vt007_rwl[, sapply(TSCA_vt007_rwl, function(x) all(is.na(x) | x >= 0))]
    TSCA_vt007_rwl = dplR::detrend(TSCA_vt007_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    TSCA_vt007_rwl = TSCA_vt007_rwl[which(rownames(TSCA_vt007_rwl) >= 1950 & rownames(TSCA_vt007_rwl) <= 2010),]
    TSCA_vt007_chron = chron(TSCA_vt007_rwl,prefix = "CAM")
    TSCA_vt007_chron = TSCA_vt007_chron[which(rownames(TSCA_vt007_chron) >= 1950 & rownames(TSCA_vt007_chron) <= 2010),]
    #plot(TSCA_vt007_chron, add.spline=FALSE, nyrs=60)
    #write.csv(TSCA_vt007_rwl,"TSCA_vt007_rwl_1950_2010.csv")
    #write.csv(TSCA_vt007_chron,"TSCA_vt007_chron_1950_2010.csv")
  }
  #PIST
  {
    #PIST me034
    PIST_me034 = read.rwl(".\\itrdb\\me034.rwl")
    info1 = rwl.stats(PIST_me034)
    info1 = subset(info1,first <= 1950)
    PIST_me034_rwl_1 = PIST_me034[,which(colnames(PIST_me034) %in% info1$series)]
    
    PIST_me034_rwl = powt(PIST_me034_rwl_1,rescale = TRUE)
    PIST_me034_rwl <- PIST_me034_rwl[, sapply(PIST_me034_rwl, function(x) all(is.na(x) | x >= 0))]
    PIST_me034_rwl = dplR::detrend(PIST_me034_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    PIST_me034_rwl = PIST_me034_rwl[which(rownames(PIST_me034_rwl) >= 1950 & rownames(PIST_me034_rwl) <= 2010),]
    PIST_me034_chron = chron(PIST_me034_rwl,prefix = "CAM")
    PIST_me034_chron = PIST_me034_chron[which(rownames(PIST_me034_chron) >= 1950 & rownames(PIST_me034_chron) <= 2010),]
    #plot(PIST_me034_chron, add.spline=FALSE, nyrs=60)
    #write.csv(PIST_me034_rwl,"PIST_me034_rwl_1950_2010.csv")
    #write.csv(PIST_me034_chron,"PIST_me034_chron_1950_2010.csv")  
    
    
    #PIST me033
    PIST_me033 = read.rwl(".\\itrdb\\me033.rwl")
    info1 = rwl.stats(PIST_me033)
    info1 = subset(info1,first <= 1950)
    PIST_me033_rwl_1 = PIST_me033[,which(colnames(PIST_me033) %in% info1$series)]
    
    PIST_me033_rwl = powt(PIST_me033_rwl_1,rescale = TRUE)
    PIST_me033_rwl <- PIST_me033_rwl[, sapply(PIST_me033_rwl, function(x) all(is.na(x) | x >= 0))]
    PIST_me033_rwl = dplR::detrend(PIST_me033_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    PIST_me033_rwl = PIST_me033_rwl[which(rownames(PIST_me033_rwl) >= 1950 & rownames(PIST_me033_rwl) <= 2010),]
    PIST_me033_chron = chron(PIST_me033_rwl,prefix = "CAM")
    PIST_me033_chron = PIST_me033_chron[which(rownames(PIST_me033_chron) >= 1950 & rownames(PIST_me033_chron) <= 2010),]
    #plot(PIST_me033_chron, add.spline=FALSE, nyrs=60)
    #write.csv(PIST_me033_rwl,"PIST_me033_rwl_1950_2010.csv")
    #write.csv(PIST_me033_chron,"PIST_me033_chron_1950_2010.csv")  
    
    #PIST me035
    PIST_me035 = read.rwl(".\\itrdb\\me035.rwl")
    info1 = rwl.stats(PIST_me035)
    info1 = subset(info1,first <= 1950)
    PIST_me035_rwl_1 = PIST_me035[,which(colnames(PIST_me035) %in% info1$series)]
    
    PIST_me035_rwl = powt(PIST_me035_rwl_1,rescale = TRUE)
    PIST_me035_rwl <- PIST_me035_rwl[, sapply(PIST_me035_rwl, function(x) all(is.na(x) | x >= 0))]
    PIST_me035_rwl = dplR::detrend(PIST_me035_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    PIST_me035_rwl = PIST_me035_rwl[which(rownames(PIST_me035_rwl) >= 1950 & rownames(PIST_me035_rwl) <= 2010),]
    PIST_me035_chron = chron(PIST_me035_rwl,prefix = "CAM")
    PIST_me035_chron = PIST_me035_chron[which(rownames(PIST_me035_chron) >= 1950 & rownames(PIST_me035_chron) <= 2010),]
    #plot(PIST_me035_chron, add.spline=FALSE, nyrs=60)
    #write.csv(PIST_me035_rwl,"PIST_me035_rwl_1950_2010.csv")
    #write.csv(PIST_me035_chron,"PIST_me035_chron_1950_2010.csv")  
    
    #PIST ny043
    PIST_ny043 = read.rwl(".\\itrdb\\ny043.rwl")
    info1 = rwl.stats(PIST_ny043)
    info1 = subset(info1,first <= 1950)
    PIST_ny043_rwl_1 = PIST_ny043[,which(colnames(PIST_ny043) %in% info1$series)]
    
    PIST_ny043_rwl = powt(PIST_ny043_rwl_1,rescale = TRUE)
    PIST_ny043_rwl <- PIST_ny043_rwl[, sapply(PIST_ny043_rwl, function(x) all(is.na(x) | x >= 0))]
    PIST_ny043_rwl = dplR::detrend(PIST_ny043_rwl,method = "Spline",difference = FALSE,nyrs = 30)
    PIST_ny043_rwl = PIST_ny043_rwl[which(rownames(PIST_ny043_rwl) >= 1950 & rownames(PIST_ny043_rwl) <= 2010),]
    PIST_ny043_chron = chron(PIST_ny043_rwl,prefix = "CAM")
    PIST_ny043_chron = PIST_ny043_chron[which(rownames(PIST_ny043_chron) >= 1950 & rownames(PIST_ny043_chron) <= 2010),]
    #plot(PIST_ny043_chron, add.spline=FALSE, nyrs=60)
    #write.csv(PIST_ny043_rwl,"PIST_ny043_rwl_1950_2010.csv")
    #write.csv(PIST_ny043_chron,"PIST_ny043_chron_1950_2010.csv")  
  }
}

#Summary
#QURU
QURU_den <- cbind(QURU_39_rwl,QURU_31_d_rwl,QURU_31_g_rwl,QURU_31_h_rwl,QURU_31_i_rwl,QURU_31_l_rwl,QURU_31_n_rwl,QURU_31_r_rwl,QURU_31_s_rwl,QURU_30_a_rwl)
#TSCA
TSCA_den <- cbind(TSCA_33_a_rwl,TSCA_33_b_rwl,TSCA_33_g_rwl,TSCA_33_h_rwl,TSCA_33_l_rwl,TSCA_33_m_rwl,TSCA_33_w_rwl)
#PIST
PIST_den <- cbind(PIST_33_a_rwl,PIST_33_b_rwl,PIST_33_c_rwl,PIST_33_f_rwl,PIST_33_g_rwl,PIST_33_i_rwl,PIST_33_n_rwl,PIST_33_p_rwl,PIST_33_r_rwl,PIST_33_u_rwl,PIST_33_v_rwl,PIST_33_w_rwl)


# List of data frames
TSCA_itrdb_list <- list(TSCA_ma019_rwl,TSCA_ny029_rwl,TSCA_ny047_rwl,TSCA_pa016_rwl,TSCA_vt007_rwl)
QURU_itrdb_list <- list(QURU_ma016_rwl,QURU_nj006_rwl,QURU_ny018_rwl,QURU_ny032_rwl,QURU_ny036_rwl)
PIST_itrdb_list <- list(PIST_me033_rwl,PIST_me034_rwl,PIST_me035_rwl,PIST_ny043_rwl)
# Calculate the max number of rows
max_rows_TSCA <- max(sapply(TSCA_itrdb_list, nrow))
max_rows_QURU <- max(sapply(QURU_itrdb_list, nrow))
max_rows_PIST <- max(sapply(PIST_itrdb_list, nrow))
Rowname_TSCA <-  row.names(TSCA_itrdb_list[[which.max(sapply(TSCA_itrdb_list, nrow))]])
Rowname_QURU <-  row.names(QURU_itrdb_list[[which.max(sapply(QURU_itrdb_list, nrow))]])
Rowname_PIST <-  row.names(PIST_itrdb_list[[which.max(sapply(PIST_itrdb_list, nrow))]])
# Make all data frames have the same number of rows
TSCA_itrdb_list_modified <- lapply(TSCA_itrdb_list, function(x) { 
  x[nrow(x):max_rows_TSCA, ] <- NA
  row.names(x) <- Rowname_TSCA
  return(x)
})

QURU_itrdb_list_modified <- lapply(QURU_itrdb_list, function(x) { 
  x[nrow(x):max_rows_QURU, ] <- NA
  row.names(x) <- Rowname_QURU
  return(x)
})

PIST_itrdb_list_modified <- lapply(PIST_itrdb_list, function(x) { 
  x[nrow(x):max_rows_PIST, ] <- NA
  row.names(x) <- Rowname_PIST
  return(x)
})

TSCA_itrdb = do.call(cbind, TSCA_itrdb_list_modified)

QURU_itrdb = do.call(cbind, QURU_itrdb_list_modified)

PIST_itrdb = do.call(cbind, PIST_itrdb_list_modified)

TSCA_itrdb_chron =  chron(TSCA_itrdb,prefix = "CAM")
QURU_itrdb_chron =  chron(QURU_itrdb,prefix = "CAM")
#remove years with significant reduction of sampling depth
QURU_itrdb_chron = QURU_itrdb_chron[1:51,]
PIST_itrdb_chron = chron(PIST_itrdb,prefix = "CAM")

TSCA_den_chron = chron(TSCA_den,prefix="CAM")
QURU_den_chron = chron(QURU_den,prefix="CAM")
PIST_den_chron = chron(PIST_den,prefix="CAM")

TSCA_itrdb = data.frame(chron = TSCA_itrdb_chron$std,data = "ITRDB",year = seq(1950,2010,1))
TSCA_den = data.frame(chron = TSCA_den_chron$std,data = "DEN",year = seq(1950,2010,1))
TSCA_long = rbind(TSCA_itrdb,TSCA_den)

QURU_itrdb = data.frame(chron = QURU_itrdb_chron$std,data = "ITRDB",year = seq(1950,2000,1))
QURU_den = data.frame(chron = QURU_den_chron$std,data = "DEN",year = seq(1950,2010,1))
QURU_long = rbind(QURU_itrdb,QURU_den)

PIST_itrdb = data.frame(chron = PIST_itrdb_chron$std,data = "ITRDB",year = seq(1950,2010,1))
PIST_den = data.frame(chron = PIST_den_chron$std,data = "DEN",year = seq(1950,2010,1))
PIST_long = rbind(PIST_itrdb,PIST_den)



pdf(".\\graph_output\\Fig_p_TSCA_chron.pdf",width = 6,height = 4)

p_TSCA_chron <- ggplot(TSCA_long, aes(x = year, y = chron, color = data)) +
  geom_line(size = 1) +
  scale_color_brewer(palette="Paired")+
  labs(x= NULL,y= "Ring index")+
  scale_y_continuous(limits = c(0.5,1.5))+
  theme_bw()+
  theme(legend.title = element_blank()) +
  theme(axis.text = element_text(size =20),axis.text.x = element_text(size =20),axis.text.y = element_text(size =20),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(legend.position = c(0.75,0.065),legend.direction = "horizontal",legend.text = element_text(size =14,face="bold"))

p_TSCA_chron
dev.off()

pdf(".\\graph_output\\Fig_p_QURU_chron.pdf",width = 6,height = 4)

p_QURU_chron <- ggplot(QURU_long, aes(x = year, y = chron, color = data)) +
  geom_line(size = 1) +
  scale_color_brewer(palette="Paired")+
  labs(x= NULL,y= "Ring index")+
  scale_y_continuous(limits = c(0.5,1.5))+
  theme_bw()+
  theme(legend.title = element_blank()) +
  theme(axis.text = element_text(size =20),axis.text.x = element_text(size =20),axis.text.y = element_text(size =20),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(legend.position = c(0.75,0.065),legend.direction = "horizontal",legend.text = element_text(size =14,face="bold"))

p_QURU_chron
dev.off()

pdf(".\\graph_output\\Fig_p_PIST_chron.pdf",width = 6,height = 4)

p_PIST_chron <- ggplot(PIST_long, aes(x = year, y = chron, color = data)) +
  geom_line(size = 1) +
  scale_color_brewer(palette="Paired")+
  labs(x= NULL,y= "Ring index")+
  scale_y_continuous(limits = c(0.5,1.5))+
  theme_bw()+
  theme(legend.title = element_blank()) +
  theme(axis.text = element_text(size =20),axis.text.x = element_text(size =20),axis.text.y = element_text(size =20),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(legend.position = c(0.25,0.065),legend.direction = "horizontal",legend.text = element_text(size =14,face="bold"))

p_PIST_chron
dev.off()

################################################################################
#Part 2: climatic sensitivity analysis
################################################################################
setwd(".\\prepNtemp\\prepNtemp\\")

#PIST input
DEN_temp_PIST = read.csv(".\\DEN_temp\\DEN_temp_PIST.csv")
ITRDB_temp_PIST = read.csv(".\\ITRDB_temp\\ITRDB_temp_PIST.csv")
DEN_prep_PIST = read.csv(".\\DEN_prep\\DEN_prep_PIST.csv")
ITRDB_prep_PIST = read.csv(".\\ITRDB_prep\\ITRDB_prep_PIST.csv")

#QURU input
DEN_temp_QURU = read.csv(".\\DEN_temp\\DEN_temp_QURU.csv")
ITRDB_temp_QURU = read.csv(".\\ITRDB_temp\\ITRDB_temp_QURU.csv")
DEN_prep_QURU = read.csv(".\\DEN_prep\\DEN_prep_QURU.csv")
ITRDB_prep_QURU = read.csv(".\\ITRDB_prep\\ITRDB_prep_QURU.csv")

#TSCA input
DEN_temp_TSCA = read.csv(".\\DEN_temp\\DEN_temp_TSCA.csv")
ITRDB_temp_TSCA = read.csv(".\\ITRDB_temp\\ITRDB_temp_TSCA.csv")
DEN_prep_TSCA = read.csv(".\\DEN_prep\\DEN_prep_TSCA.csv")
ITRDB_prep_TSCA = read.csv(".\\ITRDB_prep\\ITRDB_prep_TSCA.csv")

# Gather the columns into one column, and add a new column with all values as "DEN"
#####QURU#####
DEN_temp_QURU$X = NULL
DEN_temp_QURU_long <- DEN_temp_QURU %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "DEN")

ITRDB_temp_QURU$X = NULL
ITRDB_temp_QURU_long <- ITRDB_temp_QURU %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "ITRDB")

QURU_temp_long = rbind(DEN_temp_QURU_long, ITRDB_temp_QURU_long )
QURU_temp_long$value = abs(QURU_temp_long$value)


DEN_prep_QURU$X = NULL
DEN_prep_QURU_long <- DEN_prep_QURU %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "DEN")

ITRDB_prep_QURU$X = NULL
ITRDB_prep_QURU_long <- ITRDB_prep_QURU %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "ITRDB")

QURU_prep_long = rbind(DEN_prep_QURU_long, ITRDB_prep_QURU_long )
QURU_prep_long$value = abs(QURU_prep_long$value)

#####PIST#####
DEN_temp_PIST$X = NULL
DEN_temp_PIST_long <- DEN_temp_PIST %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "DEN")

ITRDB_temp_PIST$X = NULL
ITRDB_temp_PIST_long <- ITRDB_temp_PIST %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "ITRDB")

PIST_temp_long = rbind(DEN_temp_PIST_long, ITRDB_temp_PIST_long )
PIST_temp_long$value = abs(PIST_temp_long$value)


DEN_prep_PIST$X = NULL
DEN_prep_PIST_long <- DEN_prep_PIST %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "DEN")

ITRDB_prep_PIST$X = NULL
ITRDB_prep_PIST_long <- ITRDB_prep_PIST %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "ITRDB")

PIST_prep_long = rbind(DEN_prep_PIST_long, ITRDB_prep_PIST_long )
PIST_prep_long$value = abs(PIST_prep_long$value)

#####TSCA#####
DEN_temp_TSCA$X = NULL
DEN_temp_TSCA_long <- DEN_temp_TSCA %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "DEN")

ITRDB_temp_TSCA$X = NULL
ITRDB_temp_TSCA_long <- ITRDB_temp_TSCA %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "ITRDB")

TSCA_temp_long = rbind(DEN_temp_TSCA_long, ITRDB_temp_TSCA_long )
TSCA_temp_long$value = abs(TSCA_temp_long$value)


DEN_prep_TSCA$X = NULL
DEN_prep_TSCA_long <- DEN_prep_TSCA %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "DEN")

ITRDB_prep_TSCA$X = NULL
ITRDB_prep_TSCA_long <- ITRDB_prep_TSCA %>%
  gather(key = "site", value = "value") %>%
  mutate(data = "ITRDB")

TSCA_prep_long = rbind(DEN_prep_TSCA_long, ITRDB_prep_TSCA_long )
TSCA_prep_long$value = abs(TSCA_prep_long$value)

#####PLOT#####
#####TEMP#####
pdf(".\\Fig_p_QURU_temp.pdf",width = 6,height = 8)
p_QURU_temp <- ggplot(QURU_temp_long, aes(x = data, y = value,fill = data)) +
  geom_boxplot() +
  labs(x= NULL,y= "r")+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p_QURU_temp
dev.off()

pdf(".\\Fig_p_PIST_temp.pdf",width = 6,height = 8)

p_PIST_temp <- ggplot(PIST_temp_long, aes(x = data, y = value,fill = data)) +
  geom_boxplot() +
  labs(x= NULL,y= "r")+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p_PIST_temp
dev.off()

pdf(".\\Fig_p_TSCA_temp.pdf",width = 6,height = 8)

p_TSCA_temp <- ggplot(TSCA_temp_long, aes(x = data, y = value,fill = data)) +
  geom_boxplot() +
  labs(x= NULL,y= "r")+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p_TSCA_temp
dev.off()

#####PREP#####
pdf(".\\Fig_p_QURU_prep.pdf",width = 6,height = 8)

p_QURU_prep <- ggplot(QURU_prep_long, aes(x = data, y = value,fill = data)) +
  geom_boxplot() +
  labs(x= NULL,y= "r")+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  scale_fill_manual(values = c("DEN" = "#C77CFF", "ITRDB" = "#619CFF"))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p_QURU_prep
dev.off()


pdf(".\\Fig_p_PIST_prep.pdf",width = 6,height = 8)

p_PIST_prep <- ggplot(PIST_prep_long, aes(x = data, y = value,fill = data)) +
  geom_boxplot() +
  labs(x= NULL,y= "r")+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  scale_fill_manual(values = c("DEN" = "#C77CFF", "ITRDB" = "#619CFF"))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p_PIST_prep
dev.off()

pdf(".\\Fig_p_TSCA_prep.pdf",width = 6,height = 8)

p_TSCA_prep <- ggplot(TSCA_prep_long, aes(x = data, y = value,fill = data)) +
  geom_boxplot() +
  labs(x= NULL,y= "r")+
  scale_y_continuous(limits = c(0,0.8),breaks = c(0,0.2,0.4,0.6,0.8))+
  scale_fill_manual(values = c("DEN" = "#C77CFF", "ITRDB" = "#619CFF"))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

p_TSCA_prep
dev.off()
