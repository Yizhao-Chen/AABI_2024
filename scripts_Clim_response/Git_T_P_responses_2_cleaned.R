library(ggplot2)
library(tidyverse)
library(Metrics)
library(mgcv)

#Comparison of absolute per tree growth and TRENDY GPP
#Tem pClim_response
{
  Summary_data = read.csv(".\\Clim_response\\data\\summary_GLC_ecosystem_Temp_fitted.CSV")

  Summary_data[665,1] = NA
  Summary_data$TRENDY_VegC = abs(Summary_data$TRENDY_VegC)
  Summary_data$TRENDY_WoodC = abs(Summary_data$TRENDY_WoodC)
  
  # set up cut-off values 
  #breaks <- seq(260,306,2)
  #breaks for 2 degree
  #breaks <- seq(-2,28,2)
  breaks <- c(-2,seq(2,24,2),28)
  
  #breaks for 2 degree
  # tags <- c("[-2-0)","[0-2)", "[2-4)", "[4-6)", "[6-8)", "[8-10)","[10-12)", "[12-14)","[14-16)","[16-18)","[18-20)","[20-22)","[22-24)","[24-26)","[26-28)")
  # 
  # tags1 <- c("-1","1","3","5","7","9","11","13","15","17","19","21","23","25","27")
  
  tags_T <- c("[-2-2)","[2-4)", "[4-6)", "[6-8)", "[8-10)", "[10-12)","[12-14)", "[14-16)","[16-18)","[18-20)","[20-22)","[22-24)","[24-28)")
  
  tags1_T <- c("0","3","5","7","9","11","13","15","17","19","21","23","26")
  
  #breaks for 4 degree
  #tags <- c("[-4-0)","[0-4)", "[4-8)", "[8-12)", "[12-16)", "[16-20)","[20-24)", "[24-28)","[28-32)")
  
  #tags1 <- c("-2","2","6","10","14","18","22","26","30")
  
  # bucketing values into bins
  group_tags_T <- cut(Summary_data$T_mean_C, 
                      breaks=breaks, 
                      include.lowest=FALSE, 
                      right=FALSE, 
                      labels=tags_T)
  # inspect bins
  summary(group_tags_T)
  
  #Temperature TRENDY GPP
  {
    v_TRENDY_GPP_T <- Summary_data %>% select(TRENDY_GPP,T_mean_C)
    vgroup_TRENDY_GPP_T <- as_tibble(v_TRENDY_GPP_T) %>%
      mutate(tag = case_when(
        T_mean_C >= -2 & T_mean_C < 2 ~ tags1_T[1],
        T_mean_C >= 2 & T_mean_C < 4 ~ tags1_T[2],
        T_mean_C >= 4 & T_mean_C < 6 ~ tags1_T[3],
        T_mean_C >= 6 & T_mean_C < 8 ~ tags1_T[4],
        T_mean_C >= 8 & T_mean_C < 10 ~ tags1_T[5],
        T_mean_C >= 10 & T_mean_C < 12 ~ tags1_T[6],
        T_mean_C >= 12 & T_mean_C < 14 ~ tags1_T[7],
        T_mean_C >= 14 & T_mean_C < 16  ~ tags1_T[8],
        T_mean_C >= 16 & T_mean_C < 18 ~ tags1_T[9],
        T_mean_C >= 18 & T_mean_C < 20 ~ tags1_T[10],
        T_mean_C >= 20 & T_mean_C < 22 ~ tags1_T[11],
        T_mean_C >= 22 & T_mean_C < 24 ~ tags1_T[12],
        T_mean_C >= 24 & T_mean_C < 28 ~ tags1_T[13]
      ))
  }
  #Temperature FLUXCOM GPP
  {
    v_FLUXCOM_GPP_T <- Summary_data %>% select(FLUXCOM_GPP,T_mean_C)
    vgroup_FLUXCOM_GPP_T <- as_tibble(v_FLUXCOM_GPP_T) %>%
      mutate(tag = case_when(
        T_mean_C >= -2 & T_mean_C < 2 ~ tags1_T[1],
        T_mean_C >= 2 & T_mean_C < 4 ~ tags1_T[2],
        T_mean_C >= 4 & T_mean_C < 6 ~ tags1_T[3],
        T_mean_C >= 6 & T_mean_C < 8 ~ tags1_T[4],
        T_mean_C >= 8 & T_mean_C < 10 ~ tags1_T[5],
        T_mean_C >= 10 & T_mean_C < 12 ~ tags1_T[6],
        T_mean_C >= 12 & T_mean_C < 14 ~ tags1_T[7],
        T_mean_C >= 14 & T_mean_C < 16  ~ tags1_T[8],
        T_mean_C >= 16 & T_mean_C < 18 ~ tags1_T[9],
        T_mean_C >= 18 & T_mean_C < 20 ~ tags1_T[10],
        T_mean_C >= 20 & T_mean_C < 22 ~ tags1_T[11],
        T_mean_C >= 22 & T_mean_C < 24 ~ tags1_T[12],
        T_mean_C >= 24 & T_mean_C < 28 ~ tags1_T[13]
      ))
  }
  #Temperature RS GPP
  {
    v_RS_GPP_T <- Summary_data %>% select(RS_mean,T_mean_C)
    vgroup_RS_GPP_T <- as_tibble(v_RS_GPP_T) %>%
      mutate(tag = case_when(
        T_mean_C >= -2 & T_mean_C < 2 ~ tags1_T[1],
        T_mean_C >= 2 & T_mean_C < 4 ~ tags1_T[2],
        T_mean_C >= 4 & T_mean_C < 6 ~ tags1_T[3],
        T_mean_C >= 6 & T_mean_C < 8 ~ tags1_T[4],
        T_mean_C >= 8 & T_mean_C < 10 ~ tags1_T[5],
        T_mean_C >= 10 & T_mean_C < 12 ~ tags1_T[6],
        T_mean_C >= 12 & T_mean_C < 14 ~ tags1_T[7],
        T_mean_C >= 14 & T_mean_C < 16  ~ tags1_T[8],
        T_mean_C >= 16 & T_mean_C < 18 ~ tags1_T[9],
        T_mean_C >= 18 & T_mean_C < 20 ~ tags1_T[10],
        T_mean_C >= 20 & T_mean_C < 22 ~ tags1_T[11],
        T_mean_C >= 22 & T_mean_C < 24 ~ tags1_T[12],
        T_mean_C >= 24 & T_mean_C < 28 ~ tags1_T[13]
      ))
  }
  #Temperature TRENDY NPP
  {
    v_NPP_T <- Summary_data %>% select(TRENDY_NPP,T_mean_C)
    vgroup_NPP_T <- as_tibble(v_NPP_T) %>%
      mutate(tag = case_when(
        T_mean_C >= -2 & T_mean_C < 2 ~ tags1_T[1],
        T_mean_C >= 2 & T_mean_C < 4 ~ tags1_T[2],
        T_mean_C >= 4 & T_mean_C < 6 ~ tags1_T[3],
        T_mean_C >= 6 & T_mean_C < 8 ~ tags1_T[4],
        T_mean_C >= 8 & T_mean_C < 10 ~ tags1_T[5],
        T_mean_C >= 10 & T_mean_C < 12 ~ tags1_T[6],
        T_mean_C >= 12 & T_mean_C < 14 ~ tags1_T[7],
        T_mean_C >= 14 & T_mean_C < 16  ~ tags1_T[8],
        T_mean_C >= 16 & T_mean_C < 18 ~ tags1_T[9],
        T_mean_C >= 18 & T_mean_C < 20 ~ tags1_T[10],
        T_mean_C >= 20 & T_mean_C < 22 ~ tags1_T[11],
        T_mean_C >= 22 & T_mean_C < 24 ~ tags1_T[12],
        T_mean_C >= 24 & T_mean_C < 28 ~ tags1_T[13]
      ))
  }
  #Temperature TRENDY dVegC
  {
    v_VegC_T <- Summary_data %>% select(TRENDY_VegC,T_mean_C)
    vgroup_VegC_T <- as_tibble(v_VegC_T) %>% 
      mutate(tag = case_when(
        T_mean_C >= -2 & T_mean_C < 2 ~ tags1_T[1],
        T_mean_C >= 2 & T_mean_C < 4 ~ tags1_T[2],
        T_mean_C >= 4 & T_mean_C < 6 ~ tags1_T[3],
        T_mean_C >= 6 & T_mean_C < 8 ~ tags1_T[4],
        T_mean_C >= 8 & T_mean_C < 10 ~ tags1_T[5],
        T_mean_C >= 10 & T_mean_C < 12 ~ tags1_T[6],
        T_mean_C >= 12 & T_mean_C < 14 ~ tags1_T[7],
        T_mean_C >= 14 & T_mean_C < 16  ~ tags1_T[8],
        T_mean_C >= 16 & T_mean_C < 18 ~ tags1_T[9],
        T_mean_C >= 18 & T_mean_C < 20 ~ tags1_T[10],
        T_mean_C >= 20 & T_mean_C < 22 ~ tags1_T[11],
        T_mean_C >= 22 & T_mean_C < 24 ~ tags1_T[12],
        T_mean_C >= 24 & T_mean_C < 28 ~ tags1_T[13]
      )) 
  }
  #Temperature growth
  {
    v_growth_T <- Summary_data %>% select(North_America_per_tree1,T_mean_C)
    v_growth_T = v_growth_T[-665,]
    vgroup_growth_T <- as_tibble(v_growth_T) %>% 
      mutate(tag = case_when(
        T_mean_C >= -2 & T_mean_C < 2 ~ tags1_T[1],
        T_mean_C >= 2 & T_mean_C < 4 ~ tags1_T[2],
        T_mean_C >= 4 & T_mean_C < 6 ~ tags1_T[3],
        T_mean_C >= 6 & T_mean_C < 8 ~ tags1_T[4],
        T_mean_C >= 8 & T_mean_C < 10 ~ tags1_T[5],
        T_mean_C >= 10 & T_mean_C < 12 ~ tags1_T[6],
        T_mean_C >= 12 & T_mean_C < 14 ~ tags1_T[7],
        T_mean_C >= 14 & T_mean_C < 16  ~ tags1_T[8],
        T_mean_C >= 16 & T_mean_C < 18 ~ tags1_T[9],
        T_mean_C >= 18 & T_mean_C < 20 ~ tags1_T[10],
        T_mean_C >= 20 & T_mean_C < 22 ~ tags1_T[11],
        T_mean_C >= 22 & T_mean_C < 24 ~ tags1_T[12],
        T_mean_C >= 24 & T_mean_C < 28 ~ tags1_T[13]
      ))
    
  }

  # vgroup_TRENDY_GPP_T_order <- vgroup_TRENDY_GPP_T %>% arrange(T_mean_C)
  # vgroup_FLUXCOM_GPP_T_order <- vgroup_FLUXCOM_GPP_T %>% arrange(T_mean_C)
  # vgroup_RS_GPP_T_order <- vgroup_RS_GPP_T %>% arrange(T_mean_C)
  vgroup_TRENDY_GPP_T$Normalization_GPP_raw <- (vgroup_TRENDY_GPP_T$TRENDY_GPP - min(vgroup_TRENDY_GPP_T$TRENDY_GPP)) / (max(vgroup_TRENDY_GPP_T$TRENDY_GPP) - min(vgroup_TRENDY_GPP_T$TRENDY_GPP))
  
  vgroup_FLUXCOM_GPP_T$Normalization_GPP_raw <- (vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP - min(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP)) / (max(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP) - min(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP))
  
  vgroup_RS_GPP_T$Normalization_GPP_raw <- (vgroup_RS_GPP_T$RS_mean - min(vgroup_RS_GPP_T$RS_mean)) / (max(vgroup_RS_GPP_T$RS_mean) - min(vgroup_RS_GPP_T$RS_mean))
  
  vgroup_growth_T$Normalization_growth_raw <- (vgroup_growth_T$North_America_per_tree1 - min(vgroup_growth_T$North_America_per_tree1)) / (max(vgroup_growth_T$North_America_per_tree1) - min(vgroup_growth_T$North_America_per_tree1))
  
  vgroup_NPP_T$Normalization_NPP_raw <- (vgroup_NPP_T$TRENDY_NPP - min(vgroup_NPP_T$TRENDY_NPP)) / (max(vgroup_NPP_T$TRENDY_NPP) - min(vgroup_NPP_T$TRENDY_NPP))
  vgroup_VegC_T$Normalization_VegC_raw <- (vgroup_VegC_T$TRENDY_VegC - min(vgroup_VegC_T$TRENDY_VegC)) / (max(vgroup_VegC_T$TRENDY_VegC) - min(vgroup_VegC_T$TRENDY_VegC))

  
}

#Prep
{
  Summary_data1 = read.csv(".\\Clim_response\\data\\summary_GLC_ecosystem_Prep_fitted.CSV")

  Summary_data1[665,1] = NA
  Summary_data1$TRENDY_VegC = abs(Summary_data1$TRENDY_VegC)
  Summary_data1$TRENDY_WoodC = abs(Summary_data1$TRENDY_WoodC)

  #data binning
  # set up cut-off values 
  breaks <- c(0,seq(120,780,60),900)
  
  tags_P <- c("[0-120)","[120-180)", "[180-240)", "[240-300)", "[300-360)", "[360-420)","[420-480)", "[480-540)","[540-600)","[600-660)","[660-720)","[720-780)","[780-900)")
  
  tags1_P <- c("60","150","210","270","330","390","450","510","570","630","690","750","840")
  
  group_tags_P <- cut(Summary_data1$P_4n183, 
                      breaks=breaks, 
                      include.lowest=FALSE, 
                      right=FALSE, 
                      labels=tags_P)
  # inspect bins
  summary(group_tags_P)
  
  #Prep TRENDY GPP
  {
    v_GPP_P <- Summary_data1 %>% select(TRENDY_GPP,P_4n183)
    vgroup_TRENDY_GPP_P <- as_tibble(v_GPP_P) %>% 
      mutate(tag = case_when(
        P_4n183 >= 0 & P_4n183 < 120 ~ tags1_P[1],
        P_4n183 >= 120 & P_4n183 < 180 ~ tags1_P[2],
        P_4n183 >= 180 & P_4n183 < 240 ~ tags1_P[3],
        P_4n183 >= 240 & P_4n183 < 300 ~ tags1_P[4],
        P_4n183 >= 300 & P_4n183 < 360 ~ tags1_P[5],
        P_4n183 >= 360 & P_4n183 < 420 ~ tags1_P[6],
        P_4n183 >= 420 & P_4n183 < 480 ~ tags1_P[7],
        P_4n183 >= 480 & P_4n183 < 540 ~ tags1_P[8],
        P_4n183 >= 540 & P_4n183 < 600  ~ tags1_P[9],
        P_4n183 >= 600 & P_4n183 < 660 ~ tags1_P[10],
        P_4n183 >= 660 & P_4n183 < 720 ~ tags1_P[11],
        P_4n183 >= 720 & P_4n183 < 780 ~ tags1_P[12],
        P_4n183 >= 780 & P_4n183 < 900 ~ tags1_P[13]
      ))
    
  }
  #Prep FLUXCOM GPP
  {
    v_FLUXCOM_GPP_P <- Summary_data1 %>% select(FLUXCOM_GPP,P_4n183)
    vgroup_FLUXCOM_GPP_P <- as_tibble(v_FLUXCOM_GPP_P) %>% 
      mutate(tag = case_when(
        P_4n183 >= 0 & P_4n183 < 120 ~ tags1_P[1],
        P_4n183 >= 120 & P_4n183 < 180 ~ tags1_P[2],
        P_4n183 >= 180 & P_4n183 < 240 ~ tags1_P[3],
        P_4n183 >= 240 & P_4n183 < 300 ~ tags1_P[4],
        P_4n183 >= 300 & P_4n183 < 360 ~ tags1_P[5],
        P_4n183 >= 360 & P_4n183 < 420 ~ tags1_P[6],
        P_4n183 >= 420 & P_4n183 < 480 ~ tags1_P[7],
        P_4n183 >= 480 & P_4n183 < 540 ~ tags1_P[8],
        P_4n183 >= 540 & P_4n183 < 600  ~ tags1_P[9],
        P_4n183 >= 600 & P_4n183 < 660 ~ tags1_P[10],
        P_4n183 >= 660 & P_4n183 < 720 ~ tags1_P[11],
        P_4n183 >= 720 & P_4n183 < 780 ~ tags1_P[12],
        P_4n183 >= 780 & P_4n183 < 900 ~ tags1_P[13]
      ))
    
  }
  #Prep RS GPP
  {
    v_RS_GPP_P <- Summary_data1 %>% select(RS_mean,P_4n183)
    vgroup_RS_GPP_P <- as_tibble(v_RS_GPP_P) %>% 
      mutate(tag = case_when(
        P_4n183 >= 0 & P_4n183 < 120 ~ tags1_P[1],
        P_4n183 >= 120 & P_4n183 < 180 ~ tags1_P[2],
        P_4n183 >= 180 & P_4n183 < 240 ~ tags1_P[3],
        P_4n183 >= 240 & P_4n183 < 300 ~ tags1_P[4],
        P_4n183 >= 300 & P_4n183 < 360 ~ tags1_P[5],
        P_4n183 >= 360 & P_4n183 < 420 ~ tags1_P[6],
        P_4n183 >= 420 & P_4n183 < 480 ~ tags1_P[7],
        P_4n183 >= 480 & P_4n183 < 540 ~ tags1_P[8],
        P_4n183 >= 540 & P_4n183 < 600  ~ tags1_P[9],
        P_4n183 >= 600 & P_4n183 < 660 ~ tags1_P[10],
        P_4n183 >= 660 & P_4n183 < 720 ~ tags1_P[11],
        P_4n183 >= 720 & P_4n183 < 780 ~ tags1_P[12],
        P_4n183 >= 780 & P_4n183 < 900 ~ tags1_P[13]
      ))
    
  }
  #Prep TRENDY NPP
  {
    v_NPP_P <- Summary_data1 %>% select(TRENDY_NPP,P_4n183)
    vgroup_NPP_P <- as_tibble(v_NPP_P) %>% 
      mutate(tag = case_when(
        P_4n183 >= 0 & P_4n183 < 120 ~ tags1_P[1],
        P_4n183 >= 120 & P_4n183 < 180 ~ tags1_P[2],
        P_4n183 >= 180 & P_4n183 < 240 ~ tags1_P[3],
        P_4n183 >= 240 & P_4n183 < 300 ~ tags1_P[4],
        P_4n183 >= 300 & P_4n183 < 360 ~ tags1_P[5],
        P_4n183 >= 360 & P_4n183 < 420 ~ tags1_P[6],
        P_4n183 >= 420 & P_4n183 < 480 ~ tags1_P[7],
        P_4n183 >= 480 & P_4n183 < 540 ~ tags1_P[8],
        P_4n183 >= 540 & P_4n183 < 600  ~ tags1_P[9],
        P_4n183 >= 600 & P_4n183 < 660 ~ tags1_P[10],
        P_4n183 >= 660 & P_4n183 < 720 ~ tags1_P[11],
        P_4n183 >= 720 & P_4n183 < 780 ~ tags1_P[12],
        P_4n183 >= 780 & P_4n183 < 900 ~ tags1_P[13]
      ))
    
  }
  #Prep growth
  {
    v_growth_P <- Summary_data1 %>% select(North_America_per_tree1,P_4n183)
    v_growth_P = v_growth_P[-665,]
    vgroup_growth_P <- as_tibble(v_growth_P) %>% 
      mutate(tag = case_when(
        P_4n183 >= 0 & P_4n183 < 120 ~ tags1_P[1],
        P_4n183 >= 120 & P_4n183 < 180 ~ tags1_P[2],
        P_4n183 >= 180 & P_4n183 < 240 ~ tags1_P[3],
        P_4n183 >= 240 & P_4n183 < 300 ~ tags1_P[4],
        P_4n183 >= 300 & P_4n183 < 360 ~ tags1_P[5],
        P_4n183 >= 360 & P_4n183 < 420 ~ tags1_P[6],
        P_4n183 >= 420 & P_4n183 < 480 ~ tags1_P[7],
        P_4n183 >= 480 & P_4n183 < 540 ~ tags1_P[8],
        P_4n183 >= 540 & P_4n183 < 600  ~ tags1_P[9],
        P_4n183 >= 600 & P_4n183 < 660 ~ tags1_P[10],
        P_4n183 >= 660 & P_4n183 < 720 ~ tags1_P[11],
        P_4n183 >= 720 & P_4n183 < 780 ~ tags1_P[12],
        P_4n183 >= 780 & P_4n183 < 900 ~ tags1_P[13]
        
      ))
    
  }
  #Prep VegC
  {
    v_VegC_P <- Summary_data1 %>% select(TRENDY_VegC,P_4n183)
    vgroup_VegC_P <- as_tibble(v_VegC_P) %>% 
      mutate(tag = case_when(
        P_4n183 >= 0 & P_4n183 < 120 ~ tags1_P[1],
        P_4n183 >= 120 & P_4n183 < 180 ~ tags1_P[2],
        P_4n183 >= 180 & P_4n183 < 240 ~ tags1_P[3],
        P_4n183 >= 240 & P_4n183 < 300 ~ tags1_P[4],
        P_4n183 >= 300 & P_4n183 < 360 ~ tags1_P[5],
        P_4n183 >= 360 & P_4n183 < 420 ~ tags1_P[6],
        P_4n183 >= 420 & P_4n183 < 480 ~ tags1_P[7],
        P_4n183 >= 480 & P_4n183 < 540 ~ tags1_P[8],
        P_4n183 >= 540 & P_4n183 < 600  ~ tags1_P[9],
        P_4n183 >= 600 & P_4n183 < 660 ~ tags1_P[10],
        P_4n183 >= 660 & P_4n183 < 720 ~ tags1_P[11],
        P_4n183 >= 720 & P_4n183 < 780 ~ tags1_P[12],
        P_4n183 >= 780 & P_4n183 < 900 ~ tags1_P[13]
      ))
    
  }    
  
  
  vgroup_TRENDY_GPP_P$Normalization_GPP_raw <- (vgroup_TRENDY_GPP_P$TRENDY_GPP - min(vgroup_TRENDY_GPP_P$TRENDY_GPP)) / (max(vgroup_TRENDY_GPP_P$TRENDY_GPP) - min(vgroup_TRENDY_GPP_P$TRENDY_GPP))
  
  vgroup_FLUXCOM_GPP_P$Normalization_GPP_raw <- (vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP - min(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP)) / (max(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP) - min(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP))
  
  vgroup_RS_GPP_P$Normalization_GPP_raw <- (vgroup_RS_GPP_P$RS_mean - min(vgroup_RS_GPP_P$RS_mean)) / (max(vgroup_RS_GPP_P$RS_mean) - min(vgroup_RS_GPP_P$RS_mean))
  
  vgroup_growth_P$Normalization_growth_raw <- (vgroup_growth_P$North_America_per_tree1 - min(vgroup_growth_P$North_America_per_tree1)) / (max(vgroup_growth_P$North_America_per_tree1) - min(vgroup_growth_P$North_America_per_tree1))
  
  vgroup_NPP_P$Normalization_NPP_raw <- (vgroup_NPP_P$TRENDY_NPP - min(vgroup_NPP_P$TRENDY_NPP)) / (max(vgroup_NPP_P$TRENDY_NPP) - min(vgroup_NPP_P$TRENDY_NPP))
  vgroup_VegC_P$Normalization_VegC_raw <- (vgroup_VegC_P$TRENDY_VegC - min(vgroup_VegC_P$TRENDY_VegC)) / (max(vgroup_VegC_P$TRENDY_VegC) - min(vgroup_VegC_P$TRENDY_VegC))
}



#plot

p111 <- ggplot() + 
  geom_point(data = vgroup_TRENDY_GPP_T,aes(x = as.numeric(T_mean_C),y = Normalization_GPP_raw,color = "c1"),alpha =0.1)+
  geom_point(data = vgroup_FLUXCOM_GPP_T,aes(x = as.numeric(T_mean_C),y = Normalization_GPP_raw,color = "c2"),alpha =0.1)+
  geom_point(data = vgroup_RS_GPP_T,aes(x = as.numeric(T_mean_C),y = Normalization_GPP_raw,color = "c3"),alpha =0.1)+
  #geom_point(data = vgroup_VegC_T,aes(x = as.numeric(T_mean_C),y = Normalization_VegC_raw,color = "d"),alpha =0.1)+
  geom_point(data = vgroup_NPP_T,aes(x = as.numeric(T_mean_C),y = Normalization_NPP_raw,color = "b"),alpha =0.1)+
  geom_point(data = vgroup_growth_T,aes(x = as.numeric(T_mean_C),y = Normalization_growth_raw,color = "a"),alpha =0.1)+
  stat_smooth(data = vgroup_TRENDY_GPP_T, aes(x = as.numeric(tag),y=Normalization_GPP_raw,color ="c1",linetype = "cc1"),n=827,alpha = 0.3,size = 0.8, show.legend =TRUE)+
  stat_smooth(data = vgroup_FLUXCOM_GPP_T, aes(x = as.numeric(tag),y=Normalization_GPP_raw,color ="c2",linetype = "cc2"),n=827,alpha = 0.3,size = 0.8, show.legend =TRUE)+
  stat_smooth(data = vgroup_RS_GPP_T, aes(x = as.numeric(tag),y=Normalization_GPP_raw,color ="c3",linetype = "cc3"),n=827,alpha = 0.2,size = 0.8, show.legend =TRUE)+
  #stat_smooth(data = vgroup_VegC_T, aes(x = as.numeric(tag),y=Normalization_VegC_raw,color="d"),n=827,alpha = 0.5,size = 0.8, show.legend =TRUE)+
  stat_smooth(data = vgroup_NPP_T, aes(x = as.numeric(tag),y=Normalization_NPP_raw,color ="b"),n=827,alpha = 0.5,size = 0.8, show.legend =TRUE)+
  stat_smooth(data = vgroup_growth_T, aes(x = as.numeric(tag),y=Normalization_growth_raw,color ="a"),n=826,alpha = 0.5,size = 0.8, show.legend =TRUE)+
  scale_x_continuous(breaks = c(0,5,10,15,20,25))+
  scale_y_continuous(limits = c(0,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(x= expression("T"["grow"]~"/"~degree*"C"),y="Normalised index")+
  scale_colour_manual(name="", values=c("a" = "#F57f17", "b" = "#7AD151FF", "c1" = "#B24745FF","c2" ="#481567FF" ,"c3" = "#00A1D5FF"
                                        #"d" = "#481567FF"
                                        ),labels = c(expression("AABI"["per_tree"]),expression("NPP"["TRENDY"]),expression("GPP"["TRENDY"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"])
                                                     #,expression("dVegC"["TRENDY"])
                                                     ))+
  scale_linetype_manual(values = c(rep("longdash", 3)),guide = "none")+
  theme_bw()+
  theme(legend.position = c(0.8,0.2),legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = "bold"))+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

ggsave(".\\Clim_response\\data\\Fig3a.pdf", p111, device = "pdf",width =12,height = 6)

p222 <- ggplot() + 
  geom_point(data = vgroup_TRENDY_GPP_P,aes(x = as.numeric(P_4n183),y = Normalization_GPP_raw,color = "c1"),alpha =0.1)+
  geom_point(data = vgroup_FLUXCOM_GPP_P,aes(x = as.numeric(P_4n183),y = Normalization_GPP_raw,color = "c2"),alpha =0.1)+
  geom_point(data = vgroup_RS_GPP_P,aes(x = as.numeric(P_4n183),y = Normalization_GPP_raw,color = "c3"),alpha =0.1)+
 # geom_point(data = vgroup_VegC_P,aes(x = as.numeric(P_4n183),y = Normalization_VegC_raw,color = "d"),alpha =0.1)+
  geom_point(data = vgroup_NPP_P,aes(x = as.numeric(P_4n183),y = Normalization_NPP_raw,color = "b"),alpha =0.1)+
  geom_point(data = vgroup_growth_P,aes(x = as.numeric(P_4n183),y = Normalization_growth_raw,color = "a"),alpha =0.1)+
  stat_smooth(data = vgroup_TRENDY_GPP_P, aes(x = as.numeric(tag),y=Normalization_GPP_raw,color ="c1",linetype = "cc1"),n=827,alpha = 0.3,size = 0.8, show.legend =TRUE)+
  stat_smooth(data = vgroup_FLUXCOM_GPP_P, aes(x = as.numeric(tag),y=Normalization_GPP_raw,color ="c2",linetype = "cc2"),n=827,alpha = 0.3,size = 0.8, show.legend =TRUE)+
  stat_smooth(data = vgroup_RS_GPP_P, aes(x = as.numeric(tag),y=Normalization_GPP_raw,color ="c3",linetype = "cc3"),n=827,alpha = 0.3,size = 0.8, show.legend =TRUE)+
 # stat_smooth(data = vgroup_VegC_P, aes(x = as.numeric(tag),y=Normalization_VegC_raw,color="d"),n=827,alpha = 0.5,size = 0.8, show.legend =FALSE)+
  stat_smooth(data = vgroup_NPP_P, aes(x = as.numeric(tag),y=Normalization_NPP_raw,color ="b"),n=827,alpha = 0.5,size = 0.8, show.legend =TRUE)+
  stat_smooth(data = vgroup_growth_P, aes(x = as.numeric(tag),y=Normalization_growth_raw,color ="a"),n=826,alpha = 0.5,size = 0.8, show.legend =TRUE)+
  scale_x_continuous(breaks = c(50,200,400,600,800))+
  scale_y_continuous(limits = c(0,1.02),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(x= expression("P"["grow"]~"/"~"mm"),y="Normalised index")+
  scale_colour_manual(name="", values=c("a" = "#F57f17", "b" = "#7AD151FF", "c1" = "#B24745FF","c2" ="#481567FF" ,"c3" = "#00A1D5FF"
                                        #,"d" = "#481567FF"
                                        ),labels = c(expression("AABI"["per_tree"]),expression("NPP"["TRENDY"]),expression("GPP"["TRENDY"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"])
                                                     #,expression("dVegC"["TRENDY"])
                                                     ))+
  scale_linetype_manual(values = c(rep("longdash", 3)),guide = "none")+
  theme_bw()+
  theme(legend.position = c(0.8,0.2),legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = "bold"))+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

ggsave(".\\Clim_response\\data\\Fig3b.pdf", p222, device = "pdf",width =12,height = 6)
p222
dev.off()



normalize <- function(x, min, max) {
  (x - min) / (max - min)
}

#Temp
{
  
  #AABI
  ggplot_build(line_growth_T)
  vgroup_growth_T = vgroup_growth_T[order(vgroup_growth_T$T_mean_C),]
  vgroup_growth_T$AABI_fitted = ggplot_build(line_growth_T)$data[[1]]$y
  vgroup_growth_T$AABI_fitted_ymin = ggplot_build(line_growth_T)$data[[1]]$ymin
  vgroup_growth_T$AABI_fitted_ymax = ggplot_build(line_growth_T)$data[[1]]$ymax
  cor(vgroup_growth_T$AABI_fitted,vgroup_growth_T$North_America_per_tree1)
  rmse(vgroup_growth_T$AABI_fitted,vgroup_growth_T$North_America_per_tree1)
  
  normalized_y <- normalize(y, global_min, global_max)
  normalized_ci_lower <- normalize(ci_lower, global_min, global_max)
  normalized_ci_upper <- normalize(ci_upper, global_min, global_max)
  
  #AABI exclude high value
  ggplot_build(line)
  vgroup_growth_T1 = vgroup_growth_T1[order(vgroup_growth_T1$T_mean_C),]
  vgroup_growth_T1$AABI_fitted = ggplot_build(line)$data[[1]]$y
  cor(vgroup_growth_T1$AABI_fitted,vgroup_growth_T1$North_America_per_tree1)
  rmse(vgroup_growth_T1$AABI_fitted,vgroup_growth_T1$North_America_per_tree1)
  #TRENDY GPP
  ggplot_build(line_TRENDY_GPP_T)
  vgroup_TRENDY_GPP_T = vgroup_TRENDY_GPP_T[order(vgroup_TRENDY_GPP_T$T_mean_C),]
  vgroup_TRENDY_GPP_T$TRENDY_GPP_fitted = ggplot_build(line_TRENDY_GPP_T)$data[[1]]$y
  vgroup_TRENDY_GPP_T$TRENDY_GPP_ymin = ggplot_build(line_TRENDY_GPP_T)$data[[1]]$ymin
  vgroup_TRENDY_GPP_T$TRENDY_GPP_ymax = ggplot_build(line_TRENDY_GPP_T)$data[[1]]$ymax
  cor(vgroup_TRENDY_GPP_T$TRENDY_GPP_fitted,vgroup_TRENDY_GPP_T$TRENDY_GPP)
  rmse(vgroup_TRENDY_GPP_T$TRENDY_GPP_fitted,vgroup_TRENDY_GPP_T$TRENDY_GPP)
  #FLUXCOM GPP
  ggplot_build(line_FLUXCOM_GPP_T)
  vgroup_FLUXCOM_GPP_T = vgroup_FLUXCOM_GPP_T[order(vgroup_FLUXCOM_GPP_T$T_mean_C),]
  vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_fitted = ggplot_build(line_FLUXCOM_GPP_T)$data[[1]]$y
  vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_ymin = ggplot_build(line_FLUXCOM_GPP_T)$data[[1]]$ymin
  vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_ymax = ggplot_build(line_FLUXCOM_GPP_T)$data[[1]]$ymax
  cor(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP)
  rmse(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP)
  #RS GPP
  ggplot_build(line_RS_GPP_T)
  vgroup_RS_GPP_T = vgroup_RS_GPP_T[order(vgroup_RS_GPP_T$T_mean_C),]
  vgroup_RS_GPP_T$RS_GPP_fitted = ggplot_build(line_RS_GPP_T)$data[[1]]$y
  vgroup_RS_GPP_T$RS_GPP_ymin = ggplot_build(line_RS_GPP_T)$data[[1]]$ymin
  vgroup_RS_GPP_T$RS_GPP_ymax = ggplot_build(line_RS_GPP_T)$data[[1]]$ymax
  cor(vgroup_RS_GPP_T$RS_GPP_fitted,vgroup_RS_GPP_T$RS_mean)
  rmse(vgroup_RS_GPP_T$RS_GPP_fitted,vgroup_RS_GPP_T$RS_mean)
  #TRENDY NPP
  ggplot_build(line_TRENDY_NPP_T)
  vgroup_NPP_T = vgroup_NPP_T[order(vgroup_NPP_T$T_mean_C),]
  vgroup_NPP_T$TRENDY_NPP_fitted = ggplot_build(line_TRENDY_NPP_T)$data[[1]]$y
  vgroup_NPP_T$NPP_ymin = ggplot_build(line_TRENDY_NPP_T)$data[[1]]$ymin
  vgroup_NPP_T$NPP_ymax = ggplot_build(line_TRENDY_NPP_T)$data[[1]]$ymax
  cor(vgroup_NPP_T$TRENDY_NPP_fitted,vgroup_NPP_T$TRENDY_NPP)
  rmse(vgroup_NPP_T$TRENDY_NPP_fitted,vgroup_NPP_T$TRENDY_NPP)
  #TRENDY VegC
  ggplot_build(line_TRENDY_VegC_T)
  vgroup_VegC_T = vgroup_VegC_T[order(vgroup_VegC_T$T_mean_C),]
  vgroup_VegC_T$TRENDY_VegC_fitted = ggplot_build(line_TRENDY_VegC_T)$data[[1]]$y
  vgroup_VegC_T$VegC_ymin = ggplot_build(line_TRENDY_VegC_T)$data[[1]]$ymin
  vgroup_VegC_T$VegC_ymax = ggplot_build(line_TRENDY_VegC_T)$data[[1]]$ymax
  cor(vgroup_VegC_T$TRENDY_VegC_fitted,vgroup_VegC_T$TRENDY_VegC)
  rmse(vgroup_VegC_T$TRENDY_VegC_fitted,vgroup_VegC_T$TRENDY_VegC)
}
#prep
{
  
  #AABI
  ggplot_build(line_growth_P)
  vgroup_growth_P = vgroup_growth_P[order(vgroup_growth_P$P_4n183),]
  vgroup_growth_P$AABI_fitted = ggplot_build(line_growth_P)$data[[1]]$y
  cor(vgroup_growth_P$AABI_fitted,vgroup_growth_P$North_America_per_tree1)
  rmse(vgroup_growth_P$AABI_fitted,vgroup_growth_P$North_America_per_tree1)
  #TRENDY GPP
  ggplot_build(line_TRENDY_GPP_P)
  vgroup_TRENDY_GPP_P = vgroup_TRENDY_GPP_P[order(vgroup_TRENDY_GPP_P$P_4n183),]
  vgroup_TRENDY_GPP_P$TRENDY_GPP_fitted = ggplot_build(line_TRENDY_GPP_P)$data[[1]]$y
  cor(vgroup_TRENDY_GPP_P$TRENDY_GPP_fitted,vgroup_TRENDY_GPP_P$TRENDY_GPP)
  rmse(vgroup_TRENDY_GPP_P$TRENDY_GPP_fitted,vgroup_TRENDY_GPP_P$TRENDY_GPP)
  #FLUXCOM GPP
  ggplot_build(line_FLUXCOM_GPP_P)
  vgroup_FLUXCOM_GPP_P = vgroup_FLUXCOM_GPP_P[order(vgroup_FLUXCOM_GPP_P$P_4n183),]
  vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP_fitted = ggplot_build(line_FLUXCOM_GPP_P)$data[[1]]$y
  cor(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP)
  rmse(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP_fitted,vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP)
  #RS GPP
  ggplot_build(line_RS_GPP_P)
  vgroup_RS_GPP_P = vgroup_RS_GPP_P[order(vgroup_RS_GPP_P$P_4n183),]
  vgroup_RS_GPP_P$RS_GPP_fitted = ggplot_build(line_RS_GPP_P)$data[[1]]$y
  cor(vgroup_RS_GPP_P$RS_GPP_fitted,vgroup_RS_GPP_P$RS_mean)
  rmse(vgroup_RS_GPP_P$RS_GPP_fitted,vgroup_RS_GPP_P$RS_mean)
  #TRENDY NPP
  ggplot_build(line_TRENDY_NPP_P)
  vgroup_NPP_P = vgroup_NPP_P[order(vgroup_NPP_P$P_4n183),]
  vgroup_NPP_P$TRENDY_NPP_fitted = ggplot_build(line_TRENDY_NPP_P)$data[[1]]$y
  cor(vgroup_NPP_P$TRENDY_NPP_fitted,vgroup_NPP_P$TRENDY_NPP)
  rmse(vgroup_NPP_P$TRENDY_NPP_fitted,vgroup_NPP_P$TRENDY_NPP)
  #TRENDY VegC
  ggplot_build(line_TRENDY_VegC_P)
  vgroup_VegC_P = vgroup_VegC_P[order(vgroup_VegC_P$P_4n183),]
  vgroup_VegC_P$TRENDY_VegC_fitted = ggplot_build(line_TRENDY_VegC_P)$data[[1]]$y
  vgroup_VegC_P$VegC_ymin = ggplot_build(line_TRENDY_VegC_P)$data[[1]]$ymin
  vgroup_VegC_P$VegC_ymax = ggplot_build(line_TRENDY_VegC_P)$data[[1]]$ymax
  cor(vgroup_VegC_P$TRENDY_VegC_fitted,vgroup_VegC_P$TRENDY_VegC)
  rmse(vgroup_VegC_P$TRENDY_VegC_fitted,vgroup_VegC_P$TRENDY_VegC)
}


#normalize the fitted data to 0-1

vgroup_TRENDY_GPP_T$Normalization_GPP <- (vgroup_TRENDY_GPP_T$TRENDY_GPP_fitted - min(vgroup_TRENDY_GPP_T$TRENDY_GPP_fitted)) / (max(vgroup_TRENDY_GPP_T$TRENDY_GPP_fitted) - min(vgroup_TRENDY_GPP_T$TRENDY_GPP_fitted))

vgroup_FLUXCOM_GPP_T$Normalization_GPP <- (vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_fitted - min(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_fitted)) / (max(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_fitted) - min(vgroup_FLUXCOM_GPP_T$FLUXCOM_GPP_fitted))

vgroup_RS_GPP_T$Normalization_GPP <- (vgroup_RS_GPP_T$RS_GPP_fitted - min(vgroup_RS_GPP_T$RS_GPP_fitted)) / (max(vgroup_RS_GPP_T$RS_GPP_fitted) - min(vgroup_RS_GPP_T$RS_GPP_fitted))

vgroup_RS_GPP_T$Normalization_GPP_ymin <- vgroup_growth_T$Normalization_growth/vgroup_RS_GPP_T$AABI_fitted * vgroup_growth_T$AABI_fitted_ymin

vgroup_growth_T$Normalization_growth_ymax <- vgroup_growth_T$Normalization_growth/vgroup_growth_T$AABI_fitted * vgroup_growth_T$AABI_fitted_ymax


vgroup_growth_T$Normalization_growth <- (vgroup_growth_T$AABI_fitted - min(vgroup_growth_T$AABI_fitted)) / (max(vgroup_growth_T$AABI_fitted) - min(vgroup_growth_T$AABI_fitted))

vgroup_growth_T$Normalization_growth_ymin <- vgroup_growth_T$Normalization_growth/vgroup_growth_T$AABI_fitted * vgroup_growth_T$AABI_fitted_ymin

vgroup_growth_T$Normalization_growth_ymax <- vgroup_growth_T$Normalization_growth/vgroup_growth_T$AABI_fitted * vgroup_growth_T$AABI_fitted_ymax

vgroup_NPP_T$Normalization_NPP <- (vgroup_NPP_T$TRENDY_NPP_fitted - min(vgroup_NPP_T$TRENDY_NPP_fitted)) / (max(vgroup_NPP_T$TRENDY_NPP_fitted) - min(vgroup_NPP_T$TRENDY_NPP_fitted))
vgroup_VegC_T$Normalization_VegC <- (vgroup_VegC_T$TRENDY_VegC_fitted - min(vgroup_VegC_T$TRENDY_VegC_fitted)) / (max(vgroup_VegC_T$TRENDY_VegC_fitted) - min(vgroup_VegC_T$TRENDY_VegC_fitted))

vgroup_TRENDY_GPP_P$Normalization_GPP <- (vgroup_TRENDY_GPP_P$TRENDY_GPP_fitted - min(vgroup_TRENDY_GPP_P$TRENDY_GPP_fitted)) / (max(vgroup_TRENDY_GPP_P$TRENDY_GPP_fitted) - min(vgroup_TRENDY_GPP_P$TRENDY_GPP_fitted))
vgroup_FLUXCOM_GPP_P$Normalization_GPP <- (vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP_fitted - min(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP_fitted)) / (max(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP_fitted) - min(vgroup_FLUXCOM_GPP_P$FLUXCOM_GPP_fitted))
vgroup_RS_GPP_P$Normalization_GPP <- (vgroup_RS_GPP_P$RS_GPP_fitted - min(vgroup_RS_GPP_P$RS_GPP_fitted)) / (max(vgroup_RS_GPP_P$RS_GPP_fitted) - min(vgroup_RS_GPP_P$RS_GPP_fitted))

vgroup_growth_P$Normalization_growth <- (vgroup_growth_P$AABI_fitted - min(vgroup_growth_P$AABI_fitted)) / (max(vgroup_growth_P$AABI_fitted) - min(vgroup_growth_P$AABI_fitted))

vgroup_NPP_P$Normalization_NPP <- (vgroup_NPP_P$TRENDY_NPP_fitted - min(vgroup_NPP_P$TRENDY_NPP_fitted)) / (max(vgroup_NPP_P$TRENDY_NPP_fitted) - min(vgroup_NPP_P$TRENDY_NPP_fitted))
vgroup_VegC_P$Normalization_VegC <- (vgroup_VegC_P$TRENDY_VegC_fitted - min(vgroup_VegC_P$TRENDY_VegC_fitted)) / (max(vgroup_VegC_P$TRENDY_VegC_fitted) - min(vgroup_VegC_P$TRENDY_VegC_fitted))


#write out dataframes for subplots
write.csv(vgroup_TRENDY_GPP_T,".\\Clim_response\\data\\vgroup_TRENDY_GPP_T.csv")
write.csv(vgroup_FLUXCOM_GPP_T,".\\Clim_response\\data\\vgroup_FLUXCOM_GPP_T.csv")
write.csv(vgroup_RS_GPP_T,".\\Clim_response\\data\\vgroup_RS_GPP_T.csv")
write.csv(vgroup_growth_T,".\\Clim_response\\data\\vgroup_growth_T1.csv")
write.csv(vgroup_NPP_T,".\\Clim_response\\data\\vgroup_NPP_T1.csv")
write.csv(vgroup_VegC_T,".\\Clim_response\\data\\vgroup_VegC_T1.csv")

write.csv(vgroup_TRENDY_GPP_P,".\\Clim_response\\data\\vgroup_TRENDY_GPP_P.csv")
write.csv(vgroup_FLUXCOM_GPP_P,".\\Clim_response\\data\\vgroup_FLUXCOM_GPP_P.csv")
write.csv(vgroup_RS_GPP_P,".\\Clim_response\\data\\vgroup_RS_GPP_P.csv")
write.csv(vgroup_growth_P,".\\Clim_response\\data\\vgroup_growth_P1.csv")
write.csv(vgroup_NPP_P,".\\Clim_response\\data\\vgroup_NPP_P1.csv")
write.csv(vgroup_VegC_P,".\\Clim_response\\data\\vgroup_VegC_P1.csv")

