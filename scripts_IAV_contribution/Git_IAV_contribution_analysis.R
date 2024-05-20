##########################################################################################
#IAV contribution analysis
##########################################################################################
setwd(".\\IAV_contribution\\")

#PART 1 get the contribution of each pixel
{
#TRENDY S2
input_trendyS2 = read.csv("point_trendys2_gpp_1984_2010_detrend.csv")
#TRENDY S3
input_trendyS3 = read.csv("point_trendys3_gpp_1984_2010_detrend.csv")
#fluxcom
input_fluxcom = read.csv("point_FLUXCOM_gpp_1984_2010_detrend.csv")
#eclue
input_RS_mean = read.csv("point_RS_mean_gpp_1984_2010_detrend.csv")

input_AABI_all_mean = read.csv("point_AABI_per_area_1984_2010_all_mean_detrend.csv",header = T)

#Equation form Anders et al 2015
#fj = sum((xjt*|Xt|)/Xt)/sum(|Xt|) t:year j:pixel

#AABI
f_AABI = numeric()

for (m in 2:(length(input_AABI_all_mean)-1)){
  sum1 = 0
  sum2 = 0
  for (i in 1:length(input_AABI_all_mean$Year)){
    sum_sub1 = ((input_AABI_all_mean[i,m])*abs(input_AABI_all_mean$SUM[i]))/(input_AABI_all_mean$SUM[i])
    sum_sub2 = abs(input_AABI_all_mean$SUM[i])
    sum1 = sum1 + sum_sub1
    sum2 = sum2 + sum_sub2
  }
  f_AABI[m-1] = sum1/sum2
}

write.csv(f_AABI,"\\IAV_contribution\\IAV_contribution_AABI_all_mean.csv")



#trendy  
f_trendyS2 = numeric()

for (m in 2:(length(input_trendyS2)-1)){
  sum1 = 0
  sum2 = 0
  for (i in 1:length(input_trendyS2$Year)){
    sum_sub1 = ((input_trendyS2[i,m])*abs(input_trendyS2$SUM[i]))/input_trendyS2$SUM[i]
    sum_sub2 = abs(input_trendy$SUM[i])
    sum1 = sum1 + sum_sub1
    sum2 = sum2 + sum_sub2
  }
  f_trendy[m-1] = sum1/sum2
}

write.csv(f_trendyS2,"\\IAV_contribution\\IAV_contribution_TRENDYS2_GPP.csv")

#trendy  
f_trendyS3 = numeric()

for (m in 2:(length(input_trendyS3)-1)){
  sum1 = 0
  sum2 = 0
  for (i in 1:length(input_trendyS3$Year)){
    sum_sub1 = ((input_trendyS3[i,m])*abs(input_trendyS3$SUM[i]))/input_trendyS3$SUM[i]
    sum_sub2 = abs(input_trendy$SUM[i])
    sum1 = sum1 + sum_sub1
    sum2 = sum2 + sum_sub2
  }
  f_trendy[m-1] = sum1/sum2
}

write.csv(f_trendyS3,"\\IAV_contribution\\IAV_contribution_TRENDYS3_GPP.csv")

#fluxcom
f_fluxcom = numeric()

for (m in 2:(length(input_fluxcom)-1)){
  sum1 = 0
  sum2 = 0
  for (i in 1:length(input_fluxcom$Year)){
    sum_sub1 = ((input_fluxcom[i,m])*abs(input_fluxcom$SUM[i]))/input_fluxcom$SUM[i]
    sum_sub2 = abs(input_fluxcom$SUM[i])
    sum1 = sum1 + sum_sub1
    sum2 = sum2 + sum_sub2
  }
  f_fluxcom[m-1] = sum1/sum2
} 

write.csv(f_AABI,"\\IAV_contribution\\IAV_contribution_FLUXCOM_GPP.csv")


#RS_mean
f_RS_mean = numeric()

for (m in 2:(length(input_RS_mean)-1)){
  sum1 = 0
  sum2 = 0
  for (i in 1:length(input_RS_mean$X)){
    sum_sub1 = ((input_RS_mean[i,m])*abs(input_RS_mean$SUM[i]))/input_RS_mean$SUM[i]
    sum_sub2 = abs(input_RS_mean$SUM[i])
    sum1 = sum1 + sum_sub1
    sum2 = sum2 + sum_sub2
  }
  f_RS_mean[m-1] = sum1/sum2
} 

write.csv(f_AABI,"\\IAV_contribution\\IAV_contribution_RS_mean_GPP.csv")
}
#PART 2 compare the main contributing pixels
########################################################################################################
#Test_IAV_contribution_North_America_map.py
########################################################################################################

#Plot
{
  input_all_ratio = read.csv(".\\IAV_contribution\\Trendy_AABI_1984_2010_detrend_IAV_all_ratio_analysis.csv")
  input_all_ratio_scaled = scale(input_all_ratio[,2:length(input_all_ratio)],center = TRUE,scale = TRUE)
  input_scaled_all_ratio_df = data.frame(input_all_ratio_scaled)
  #r2 calculation and record
  # rsall = summary(lm(input_scaled_all_ratio_df$AABI_all~input_scaled_all_ratio_df$trendy_all))[8]$r.squared
  # rs25 = summary(lm(input_scaled_all_ratio_df$AABI_25~input_scaled_all_ratio_df$trendy_25))[8]$r.squared
  # rs30 = summary(lm(input_scaled_all_ratio_df$AABI_30~input_scaled_all_ratio_df$trendy_30))[8]$r.squared
  # rs35 = summary(lm(input_scaled_all_ratio_df$AABI_35~input_scaled_all_ratio_df$trendy_35))[8]$r.squared
  # rs40 = summary(lm(input_scaled_all_ratio_df$AABI_40~input_scaled_all_ratio_df$trendy_40))[8]$r.squared
  # rs50 = summary(lm(input_scaled_all_ratio_df$AABI_50~input_scaled_all_ratio_df$trendy_50))[8]$r.squared
  # rs60 = summary(lm(input_scaled_all_ratio_df$AABI_60~input_scaled_all_ratio_df$trendy_60))[8]$r.squared
  # rs70 = summary(lm(input_scaled_all_ratio_df$AABI_70~input_scaled_all_ratio_df$trendy_70))[8]$r.squared
  # rs80 = summary(lm(input_scaled_all_ratio_df$AABI_80~input_scaled_all_ratio_df$trendy_80))[8]$r.squared
  # rs90 = summary(lm(input_scaled_all_ratio_df$AABI_90~input_scaled_all_ratio_df$trendy_90))[8]$r.squared
  
  #r calculation and record
  rs25 = cor(input_scaled_all_ratio_df$trendy_top25_mean,input_scaled_all_ratio_df$AABI_top25_mean)
  rs30 = cor(input_scaled_all_ratio_df$trendy_top30_mean,input_scaled_all_ratio_df$AABI_top30_mean)
  #rs35 = cor(input_scaled_all_ratio_df$trendy_top35_mean,input_scaled_all_ratio_df$AABI_top35_mean)
  rs40 = cor(input_scaled_all_ratio_df$trendy_top40_mean,input_scaled_all_ratio_df$AABI_top40_mean)
  rs50 = cor(input_scaled_all_ratio_df$trendy_top50_mean,input_scaled_all_ratio_df$AABI_top50_mean)
  rs60 = cor(input_scaled_all_ratio_df$trendy_top60_mean,input_scaled_all_ratio_df$AABI_top60_mean)
  rs70 = cor(input_scaled_all_ratio_df$trendy_top70_mean,input_scaled_all_ratio_df$AABI_top70_mean)
  rs80 = cor(input_scaled_all_ratio_df$trendy_top80_mean,input_scaled_all_ratio_df$AABI_top80_mean)
  rs90 = cor(input_scaled_all_ratio_df$trendy_top90_mean,input_scaled_all_ratio_df$AABI_top90_mean)
  rsall = cor(input_scaled_all_ratio_df$trendy_top100_mean,input_scaled_all_ratio_df$AABI_top100_mean)
  
  rs_sum = data.frame(perc = c(0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),rs = c(rs25,rs30,rs40,rs50,rs60,rs70,rs80,rs90,rsall))
  #rs_sum = data.frame(perc = c(0.25,0.3,0.4,0.5,0.6,0.7,0.8),rs = c(rs25,rs30,rs40,rs50,rs60,rs70,rs80))
  
  
  pdf(".\\IAV_contribution\\Fig.S3e.pdf",width =5,height = 5)
  
  p10 = ggplot(rs_sum,aes(x=perc,y=rs)) + 
    geom_line(aes(size = 1))+
    ylim(0,0.65)+
    xlab("Fraction of area")+
    ylab(expression(r))+
    theme(legend.position = "none")+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16)) 

  p10
  dev.off()
}
