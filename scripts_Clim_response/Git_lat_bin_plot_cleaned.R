#######################################################################################
#lat distribution of clim response and period Fig.1 efgh
#######################################################################################

library("ggplot2")
library(tidyverse)
library(tidyr)
library(modeest)

#data input
{
  #AABI
  data_temp_per_tree = read.csv(".\\Clim_response\\data\\Output_temp\\AABI_per_tree_temp_pcorr_tras_processed_toshp.csv")
  data_prep_per_tree = read.csv(".\\Clim_response\\data\\Clim_response\\Output_prep\\AABI_per_tree_prep_pcorr_tras_processed_toshp.csv")
  #NPP
  data_temp_NPP_mean = read.csv(".\\Clim_response\\data\\Clim_response\\Output_temp\\NPP_TRENDY_temp_pcorr_tras_processed_toshp.csv")
  data_prep_NPP_mean = read.csv(".\\Clim_response\\data\\Clim_response\\Output_prep\\NPP_TRENDY_prep_pcorr_tras_processed_toshp.csv")
  #VegC
  data_temp_VegC_mean = read.csv(".\\Clim_response\\data\\Clim_response\\Output_temp\\VegC_TRENDY_temp_pcorr_tras_processed_toshp.csv")
  data_prep_VegC_mean = read.csv(".\\Clim_response\\data\\Clim_response\\Output_prep\\VegC_TRENDY_prep_pcorr_tras_processed_toshp.csv")

}

#remove NA & -999 lines
{
  
  #AABI
  data1_temp_per_tree = data_temp_per_tree[!is.na(data_temp_per_tree$T_mark),]
  data1_prep_per_tree = data_prep_per_tree[!is.na(data_prep_per_tree$T_mark),]
  
  #NPP
  data1_temp_NPP_mean = data_temp_NPP_mean[!is.na(data_temp_NPP_mean$T_mark),]
  data1_prep_NPP_mean = data_prep_NPP_mean[!is.na(data_prep_NPP_mean$T_mark),] 
  
  #VegC
  data1_temp_VegC_mean = data_temp_VegC_mean[!is.na(data_temp_VegC_mean$T_mark),]
  data1_prep_VegC_mean = data_prep_VegC_mean[!is.na(data_prep_VegC_mean$T_mark),] 
  
}


#bulid subsets for north america only
#subset for period
subset_period <- function(x){
  list = list()
  for (i in 1:21){
    sub1 = base::subset(x,lat > (26+2*(i-1)) & lat <= (28+2*i))
    sub1_list = c()
    for (j in 1:length(sub1$lat)){
      sub1_list = append(sub1_list,c(sub1$T_start[j]:sub1$T_end[j]))
    }
    list[[i]] = sub1_list
  }
  
  bin_df = setNames(do.call(cbind.data.frame, lapply(lapply(list, unlist), `length<-`, max(lengths(list)))), paste0("V", 1:21))
  
  bin_df_long = gather(bin_df,bin,out)
  
  outlist = list(bin_df,bin_df_long)
  return(outlist)
}

#subset for corr
subset_corr <- function(x){
  list_corr = list()
  for (i in 1:21){
    sub1_corr = base::subset(x,lat > (26+2*(i-1)) & lat <= (28+2*i))
    sub1_list_corr = c()
    for (j in 1:length(sub1_corr$lat)){
      sub1_list_corr = append(sub1_list_corr,sub1_corr$Corr)
    }
    list_corr[[i]] = sub1_list_corr
  }
  
  bin_df_corr = setNames(do.call(cbind.data.frame, lapply(lapply(list_corr, unlist), `length<-`, max(lengths(list_corr)))), paste0("V", 1:21))
  
  bin_df_corr_long = gather(bin_df_corr,bin_corr,out_corr)
  
  outlist = list(bin_df_corr,bin_df_corr_long)
  return(outlist)
}

#get plotable dfs
#period bin_dfs
{
  
  #sink per tree
  bin_df_temp_sink_per_tree_period = data.frame(subset_period(data1_temp_per_tree)[1])
  bin_df_prep_sink_per_tree_period = data.frame(subset_period(data1_prep_per_tree)[1])
  bin_df_long_temp_sink_per_tree_period = data.frame(subset_period(data1_temp_per_tree)[2])
  bin_df_long_prep_sink_per_tree_period = data.frame(subset_period(data1_prep_per_tree)[2])
  
  
  #NPP
  bin_df_temp_NPP_mean_period = data.frame(subset_period(data1_temp_NPP_mean)[1])
  bin_df_prep_NPP_mean_period = data.frame(subset_period(data1_prep_NPP_mean)[1])
  bin_df_long_temp_NPP_mean_period = data.frame(subset_period(data1_temp_NPP_mean)[2])
  bin_df_long_prep_NPP_mean_period = data.frame(subset_period(data1_prep_NPP_mean)[2])
  
  #VegC
  bin_df_temp_VegC_mean_period = data.frame(subset_period(data1_temp_VegC_mean)[1])
  bin_df_prep_VegC_mean_period = data.frame(subset_period(data1_prep_VegC_mean)[1])
  bin_df_long_temp_VegC_mean_period = data.frame(subset_period(data1_temp_VegC_mean)[2])
  bin_df_long_prep_VegC_mean_period = data.frame(subset_period(data1_prep_VegC_mean)[2])
  

}

#corr bin_dfs 
{
  
  #sink per tree
  bin_df_temp_sink_per_tree_corr = data.frame(subset_corr(data1_temp_per_tree)[1])
  bin_df_prep_sink_per_tree_corr = data.frame(subset_corr(data1_prep_per_tree)[1])
  bin_df_long_temp_sink_per_tree_corr = data.frame(subset_corr(data1_temp_per_tree)[2])
  bin_df_long_prep_sink_per_tree_corr = data.frame(subset_corr(data1_prep_per_tree)[2])
  
  #NPP
  bin_df_temp_NPP_mean_corr = data.frame(subset_corr(data1_temp_NPP_mean)[1])
  bin_df_prep_NPP_mean_corr = data.frame(subset_corr(data1_prep_NPP_mean)[1])
  bin_df_long_temp_NPP_mean_corr = data.frame(subset_corr(data1_temp_NPP_mean)[2])
  bin_df_long_prep_NPP_mean_corr = data.frame(subset_corr(data1_prep_NPP_mean)[2])
  
  #VegC
  bin_df_temp_VegC_mean_corr = data.frame(subset_corr(data1_temp_VegC_mean)[1])
  bin_df_prep_VegC_mean_corr = data.frame(subset_corr(data1_prep_VegC_mean)[1])
  bin_df_long_temp_VegC_mean_corr = data.frame(subset_corr(data1_temp_VegC_mean)[2])
  bin_df_long_prep_VegC_mean_corr = data.frame(subset_corr(data1_prep_VegC_mean)[2])
  
}

#calculate stats.
stats = function(x){
  
  col_mean = apply(x,2,mean,na.rm=TRUE)
  col_sd = apply(x,2,sd,na.rm=TRUE) 
  col_max = col_mean + col_sd
  col_min = col_mean - col_sd
  bin_df_stats = data.frame(col_mean,col_sd,col_max,col_min)  
  return(bin_df_stats)
}

#period stats
{
  
  #sink per tree
  bin_stats_temp_sink_per_tree_period = stats(bin_df_temp_sink_per_tree_period)
  bin_stats_prep_sink_per_tree_period = stats(bin_df_prep_sink_per_tree_period)
  
  #NPP
  bin_stats_temp_NPP_mean_period = stats(bin_df_temp_NPP_mean_period)
  bin_stats_prep_NPP_mean_period = stats(bin_df_prep_NPP_mean_period)  
  
  #VegC
  bin_stats_temp_VegC_mean_period = stats(bin_df_temp_VegC_mean_period)
  bin_stats_prep_VegC_mean_period = stats(bin_df_prep_VegC_mean_period) 
  
}

#corr stats
{
  
  #sink per tree AABI
  bin_stats_temp_sink_per_tree_corr = stats(bin_df_temp_sink_per_tree_corr)
  bin_stats_prep_sink_per_tree_corr = stats(bin_df_prep_sink_per_tree_corr)
  
  #NPP
  bin_stats_temp_NPP_mean_corr = stats(bin_df_temp_NPP_mean_corr)
  bin_stats_prep_NPP_mean_corr = stats(bin_df_prep_NPP_mean_corr)
  
  #VegC
  bin_stats_temp_VegC_mean_corr = stats(bin_df_temp_VegC_mean_corr)
  bin_stats_prep_VegC_mean_corr = stats(bin_df_prep_VegC_mean_corr)

}

#lines
{
  #parameters for plots
  {
    txt_size1 = 50
    txt_size2 = 40
    txt_size3 = 50
    txt_size4 = 40
    line_size1 = 4  #main lines
    line_size2 = 2  #hline 
    line_size3 = 2  #axis lines
  }
  
  #all period bin_stats_temp_
  #temp all
  {
   ppp11 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_sink_per_tree_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                                 ymax= col_max),fill = "#F9A825",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_NPP_mean_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#1565C0",alpha = 0.3)+
      #geom_ribbon(data = bin_stats_temp_VegC_mean_period,aes(x=seq(28,68,2),ymin= col_min, ymax= col_max),fill = "#0D2B4F",alpha = 0.2)+
      #geom_ribbon(data = bin_stats_temp_WoodC_mean_period,aes(x=seq(28,68,2),ymin= col_min, 
      #ymax= col_max),fill = "#245496",alpha = 0.3)+
      geom_line(data = bin_stats_temp_sink_per_tree_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_temp_NPP_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
     # geom_line(data = bin_stats_temp_VegC_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      #geom_line(data = bin_stats_temp_WoodC_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '',values =c(aa="#F9A825",bb = "#1565C0"), labels = c(bquote("AABI"[per_tree]),bquote("NPP"[TRENDY])))+
      scale_x_continuous(limits = c(12.5,75),breaks = c(30,50,70),labels = c(expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-Jan","-Apr","-Jul","-Oct","Jan","Apr","Jul","Oct"))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    ggsave(".\\Clim_response\\data\\Fig1g.pdf", ppp11, device = "pdf",width =14,height = 20) 
  } 
  #prep all
  {
    ppp22 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_sink_per_tree_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                                 ymax= col_max),fill = "#F9A825",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_NPP_mean_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#1565C0",alpha = 0.3)+
      #geom_ribbon(data = bin_stats_prep_VegC_mean_period,aes(x=seq(28,68,2),ymin= col_min, ymax= col_max),fill = "#0D2B4F",alpha = 0.2)+
      geom_line(data = bin_stats_prep_sink_per_tree_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_prep_NPP_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      #geom_line(data = bin_stats_prep_VegC_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '',values =c(aa="#F9A825",bb = "#1565C0"), labels = c(bquote("AABI"[per_tree]),bquote("NPP"[TRENDY])))+
      scale_x_continuous(limits = c(12.5,75),breaks = c(30,50,70),labels = c(expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-Jan","-Apr","-Jul","-Oct","Jan","Apr","Jul","Oct"))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    ggsave(".\\Clim_response\\data\\Fig1h.pdf", ppp22, device = "pdf",width =14,height = 20)

    
  }  
  #all pcor
  #temp all pcor
  {
    ppp33 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_sink_per_tree_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                               ymax= col_max),fill = "#FFD76A",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_NPP_mean_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#6ECBD0",alpha = 0.3)+
    #  geom_ribbon(data = bin_stats_temp_VegC_mean_corr,aes(x=seq(28,68,2),ymin= col_min, ymax= col_max),fill = "#481567FF",alpha = 0.2)+
      geom_line(data = bin_stats_temp_sink_per_tree_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_temp_NPP_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
     # geom_line(data = bin_stats_temp_VegC_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylim(-1.12,1.12)+
      xlab("")+
      ylab("pcor")+
      scale_color_manual(name = '', 
                         values = c(aa="#F57f17",bb = "#55C667FF"), labels = c(bquote("AABI"[per_tree]),bquote("NPP"[TRENDY])))+
      scale_x_continuous(limits = c(12.5,75),breaks = c(30,50,70),labels = c(expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    ggsave(".\\Clim_response\\data\\Fig1e.pdf", ppp33, device = "pdf",width =14,height = 20)
    
    
  }
  #prep all pcor
  {
    ppp44 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_sink_per_tree_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                               ymax= col_max),fill = "#FFD76A",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_NPP_mean_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#6ECBD0",alpha = 0.3)+
    #  geom_ribbon(data = bin_stats_prep_VegC_mean_corr,aes(x=seq(28,68,2),ymin= col_min,ymax= col_max),fill = "#481567FF",alpha = 0.2)+
      geom_line(data = bin_stats_prep_sink_per_tree_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_prep_NPP_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
     # geom_line(data = bin_stats_prep_VegC_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylim(-1.12,1.12)+
      xlab("")+
      ylab("pcor")+
      scale_color_manual(name = '', 
                         values = c(aa="#F57f17",bb = "#55C667FF"), labels = c(bquote("AABI"[per_tree]),bquote("NPP"[TRENDY])))+
      scale_x_continuous(limits = c(12.5,75),breaks = c(30,50,70),labels = c(expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.175),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size3),axis.title.x=element_text(face="bold",size=txt_size4),axis.title.y=element_text(face="bold",size=txt_size4))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    

    ggsave(".\\Clim_response\\data\\Fig1f.pdf", ppp44, device = "pdf",width =14,height = 20)

  }
}