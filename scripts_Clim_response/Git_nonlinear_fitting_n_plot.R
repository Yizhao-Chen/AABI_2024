library(patchwork)
library(ggplot2)
library(climwin)


Temp_df = read.csv(".\\Clim_response\\data\\CRU_NCEP_temp_1_1984_2010_detrend.csv")

Prep_df = read.csv(".\\Clim_response\\data\\CRU_NCEP_prep_1_1984_2010_detrend.csv")

AABI_df = read.csv(".\\Clim_response\\data\\point_AABI_per_tree_1984_2010_age_correct_detrend_827.csv")

NPP_df = read.csv(".\\Clim_response\\data\\point_NPP_Trendy_1984_2010_age_correct_detrend_827.csv")

VegC_df = read.csv(".\\Clim_response\\data\\point_VegC_Trendy_1984_2010_age_correct_detrend_827.csv")

lat_df = read.csv(".\\Clim_response\\data\\AABI_per_tree_temp_prep_pcorr_toshp.csv")
lat_df = data.frame("point_ID" = c(1:827),"lat" = lat_df$lat)

lat_df <- lat_df %>%
  mutate(lat_group = cut(lat, 
                         breaks = 6, 
                         labels = c("L6", "L5", "L4", "L3", "L2", "L1"),
                         include.lowest = TRUE))


Temp_df$date <- as.Date(paste(Temp_df$Year, Temp_df$Month, "01", sep = "-"), "%Y-%m-%d")
Temp_df$formatted_date <- format(Temp_df$date, "%d/%m/%Y")

Prep_df$date <- as.Date(paste(Prep_df$Year, Prep_df$Month, "01", sep = "-"), "%Y-%m-%d")
Prep_df$formatted_date <- format(Prep_df$date, "%d/%m/%Y")

AABI_df$date <- as.Date(paste(AABI_df$Year, "12", "01", sep = "-"), "%Y-%m-%d")
AABI_df$formatted_date <- format(AABI_df$date, "%d/%m/%Y")

NPP_df$date <- as.Date(paste(NPP_df$Year, "12", "01", sep = "-"), "%Y-%m-%d")
NPP_df$formatted_date <- format(NPP_df$date, "%d/%m/%Y")

VegC_df$date <- as.Date(paste(VegC_df$Year, "12", "01", sep = "-"), "%Y-%m-%d")
VegC_df$formatted_date <- format(VegC_df$date, "%d/%m/%Y")

MassWin_Temp_AABI = list()
MassWin_Temp_NPP = list()
MassWin_Temp_VegC = list()
bestmodel_Temp_AABI = list()
bestmodel_Temp_NPP = list()
bestmodel_Temp_VegC = list()
bestmodel_Temp_AABI_pred = list()
bestmodel_Temp_NPP_pred = list()
bestmodel_Temp_VegC_pred = list()

MassWin_Prep_AABI = list()
MassWin_Prep_NPP = list()
MassWin_Prep_VegC = list()
bestmodel_Prep_AABI = list()
bestmodel_Prep_NPP = list()
bestmodel_Prep_VegC = list()
bestmodel_Prep_AABI_pred = list()
bestmodel_Prep_NPP_pred = list()
bestmodel_Prep_VegC_pred = list()

for (i in 1:827){
Temp_sub_df = data.frame("formatted_date" = Temp_df$formatted_date, "Temp" = Temp_df[,(3+i)])
Prep_sub_df = data.frame("formatted_date" = Prep_df$formatted_date, "Prep" = Prep_df[,(3+i)])
AABI_sub_df = data.frame("formatted_date" = AABI_df$formatted_date, "AABI" = AABI_df[,(2+i)])
NPP_sub_df = data.frame("formatted_date" = NPP_df$formatted_date, "NPP" = NPP_df[,(2+i)])
VegC_sub_df = data.frame("formatted_date" = VegC_df$formatted_date, "VegC" = VegC_df[,(2+i)])

AABI_sub_df = AABI_sub_df[-1,]
NPP_sub_df = NPP_sub_df[-1,]
VegC_sub_df = VegC_sub_df[-1,]

MassWin_Temp_AABI[[i]] <- slidingwin(xvar = list(Temp = Temp_sub_df$Temp),
                      cdate = as.factor(Temp_sub_df$formatted_date),
                      bdate = as.factor(AABI_sub_df$formatted_date),
                      baseline = lm(AABI ~ 1, data = AABI_sub_df),
                      cinterval = "month",
                      range = c(23, 0),
                      type = "absolute", refday = c(1, 12),
                      stat = "mean",
                      func = c("lin","quad"))
# 
MassWin_Temp_NPP[[i]] <- slidingwin(xvar = list(Temp = Temp_sub_df$Temp),
                      cdate = as.factor(Temp_sub_df$formatted_date),
                      bdate = as.factor(NPP_sub_df$formatted_date),
                      baseline = lm(NPP ~ 1, data = NPP_sub_df),
                      cinterval = "month",
                      range = c(23, 0),
                      type = "absolute", refday = c(1, 12),
                      stat = "mean",
                      func = c("lin","quad"))

MassWin_Temp_VegC[[i]] <- slidingwin(xvar = list(Temp = Temp_sub_df$Temp),
                                    cdate = as.factor(Temp_sub_df$formatted_date),
                                    bdate = as.factor(VegC_sub_df$formatted_date),
                                    baseline = lm(VegC ~ 1, data = VegC_sub_df),
                                    cinterval = "month",
                                    range = c(23, 0),
                                    type = "absolute", refday = c(1, 12),
                                    stat = "mean",
                                    func = c("lin","quad"))

MassWin_Prep_AABI[[i]] <- slidingwin(xvar = list(Prep = Prep_sub_df$Prep),
                                     cdate = as.factor(Prep_sub_df$formatted_date),
                                     bdate = as.factor(AABI_sub_df$formatted_date),
                                     baseline = lm(AABI ~ 1, data = AABI_sub_df),
                                     cinterval = "month",
                                     range = c(23, 0),
                                     type = "absolute", refday = c(1, 12),
                                     stat = "mean",
                                     func = c("lin","quad"))

MassWin_Prep_NPP[[i]] <- slidingwin(xvar = list(Prep = Prep_sub_df$Prep),
                                    cdate = as.factor(Prep_sub_df$formatted_date),
                                    bdate = as.factor(NPP_sub_df$formatted_date),
                                    baseline = lm(NPP ~ 1, data = NPP_sub_df),
                                    cinterval = "month",
                                    range = c(23, 0),
                                    type = "absolute", refday = c(1, 12),
                                    stat = "mean",
                                    func = c("lin","quad"))

MassWin_Prep_VegC[[i]] <- slidingwin(xvar = list(Prep = Prep_sub_df$Prep),
                                    cdate = as.factor(Prep_sub_df$formatted_date),
                                    bdate = as.factor(VegC_sub_df$formatted_date),
                                    baseline = lm(VegC ~ 1, data = VegC_sub_df),
                                    cinterval = "month",
                                    range = c(23, 0),
                                    type = "absolute", refday = c(1, 12),
                                    stat = "mean",
                                    func = c("lin","quad"))

}

Temp_count_lin_NPP = 0
Temp_count_quad_NPP = 0
Temp_count_lin_VegC = 0
Temp_count_quad_VegC = 0
Temp_count_lin_AABI = 0
Temp_count_quad_AABI = 0
Prep_count_lin_NPP = 0
Prep_count_quad_NPP = 0
Prep_count_lin_VegC = 0
Prep_count_quad_VegC = 0
Prep_count_lin_AABI = 0
Prep_count_quad_AABI = 0

for (j in 1:827){

  if(which.min(c(MassWin_Temp_NPP[[j]]$combos$DeltaAICc[1],MassWin_Temp_NPP[[j]]$combos$DeltaAICc[2],MassWin_Temp_NPP[[j]]$combos$DeltaAICc[3])) == 1){
    bestmodel_Temp_NPP[[j]] = MassWin_Temp_NPP[[j]][[1]]$BestModelData[,1:2]
    bestmodel_Temp_NPP_pred[[j]] = MassWin_Temp_NPP[[j]][[1]]$BestModel
  }else if(which.min(c(MassWin_Temp_NPP[[j]]$combos$DeltaAICc[1],MassWin_Temp_NPP[[j]]$combos$DeltaAICc[2],MassWin_Temp_NPP[[j]]$combos$DeltaAICc[3])) == 2){
    bestmodel_Temp_NPP[[j]] = MassWin_Temp_NPP[[j]][[2]]$BestModelData[,1:2]
    bestmodel_Temp_NPP_pred[[j]] = MassWin_Temp_NPP[[j]][[2]]$BestModel
  }else{
    bestmodel_Temp_NPP[[j]] = MassWin_Temp_NPP[[j]][[3]]$BestModelData[,1:2]
    bestmodel_Temp_NPP_pred[[j]] = MassWin_Temp_NPP[[j]][[3]]$BestModel
  }
  
  if(which.min(c(MassWin_Temp_VegC[[j]]$combos$DeltaAICc[1],MassWin_Temp_VegC[[j]]$combos$DeltaAICc[2],MassWin_Temp_VegC[[j]]$combos$DeltaAICc[3])) == 1){
    bestmodel_Temp_VegC[[j]] = MassWin_Temp_VegC[[j]][[1]]$BestModelData[,1:2]
    bestmodel_Temp_VegC_pred[[j]] = MassWin_Temp_VegC[[j]][[1]]$BestModel
  }else if(which.min(c(MassWin_Temp_VegC[[j]]$combos$DeltaAICc[1],MassWin_Temp_VegC[[j]]$combos$DeltaAICc[2],MassWin_Temp_VegC[[j]]$combos$DeltaAICc[3])) == 2){
    bestmodel_Temp_VegC[[j]] = MassWin_Temp_VegC[[j]][[2]]$BestModelData[,1:2]
    bestmodel_Temp_VegC_pred[[j]] = MassWin_Temp_VegC[[j]][[2]]$BestModel
  }else{
    bestmodel_Temp_VegC[[j]] = MassWin_Temp_VegC[[j]][[3]]$BestModelData[,1:2]
    bestmodel_Temp_VegC_pred[[j]] = MassWin_Temp_VegC[[j]][[3]]$BestModel
  }
  
  if(which.min(c(MassWin_Temp_AABI[[j]]$combos$DeltaAICc[1],MassWin_Temp_AABI[[j]]$combos$DeltaAICc[2],MassWin_Temp_AABI[[j]]$combos$DeltaAICc[3])) == 1){
    bestmodel_Temp_AABI[[j]] = MassWin_Temp_AABI[[j]][[1]]$BestModelData[,1:2]
    bestmodel_Temp_AABI_pred[[j]] = MassWin_Temp_AABI[[j]][[1]]$BestModel
  }else if(which.min(c(MassWin_Temp_AABI[[j]]$combos$DeltaAICc[1],MassWin_Temp_AABI[[j]]$combos$DeltaAICc[2],MassWin_Temp_AABI[[j]]$combos$DeltaAICc[3])) == 2){
    bestmodel_Temp_AABI[[j]] = MassWin_Temp_AABI[[j]][[2]]$BestModelData[,1:2]
    bestmodel_Temp_AABI_pred[[j]] = MassWin_Temp_AABI[[j]][[2]]$BestModel
  }else{
    bestmodel_Temp_AABI[[j]] = MassWin_Temp_AABI[[j]][[3]]$BestModelData[,1:2]
    bestmodel_Temp_AABI_pred[[j]] = MassWin_Temp_AABI[[j]][[3]]$BestModel
  }
  

  if(which.min(c(MassWin_Prep_NPP[[j]]$combos$DeltaAICc[1],MassWin_Prep_NPP[[j]]$combos$DeltaAICc[2],MassWin_Prep_NPP[[j]]$combos$DeltaAICc[3])) == 1){
    bestmodel_Prep_NPP[[j]] = MassWin_Prep_NPP[[j]][[1]]$BestModelData[,1:2]
    bestmodel_Prep_NPP_pred[[j]] = MassWin_Prep_NPP[[j]][[1]]$BestModel
  }else if(which.min(c(MassWin_Prep_NPP[[j]]$combos$DeltaAICc[1],MassWin_Prep_NPP[[j]]$combos$DeltaAICc[2],MassWin_Prep_NPP[[j]]$combos$DeltaAICc[3])) == 2){
    bestmodel_Prep_NPP[[j]] = MassWin_Prep_NPP[[j]][[2]]$BestModelData[,1:2]
    bestmodel_Prep_NPP_pred[[j]] = MassWin_Prep_NPP[[j]][[2]]$BestModel
  }else{
    bestmodel_Prep_NPP[[j]] = MassWin_Prep_NPP[[j]][[3]]$BestModelData[,1:2]
    bestmodel_Prep_NPP_pred[[j]] = MassWin_Prep_NPP[[j]][[3]]$BestModel
  }
  
  if(which.min(c(MassWin_Prep_VegC[[j]]$combos$DeltaAICc[1],MassWin_Prep_VegC[[j]]$combos$DeltaAICc[2],MassWin_Prep_VegC[[j]]$combos$DeltaAICc[3])) == 1){
    bestmodel_Prep_VegC[[j]] = MassWin_Prep_VegC[[j]][[1]]$BestModelData[,1:2]
    bestmodel_Prep_VegC_pred[[j]] = MassWin_Prep_VegC[[j]][[1]]$BestModel
  }else if(which.min(c(MassWin_Prep_VegC[[j]]$combos$DeltaAICc[1],MassWin_Prep_VegC[[j]]$combos$DeltaAICc[2],MassWin_Prep_VegC[[j]]$combos$DeltaAICc[3])) == 2){
    bestmodel_Prep_VegC[[j]] = MassWin_Prep_VegC[[j]][[2]]$BestModelData[,1:2]
    bestmodel_Prep_VegC_pred[[j]] = MassWin_Prep_VegC[[j]][[2]]$BestModel
  }else{
    bestmodel_Prep_VegC[[j]] = MassWin_Prep_VegC[[j]][[3]]$BestModelData[,1:2]
    bestmodel_Prep_VegC_pred[[j]] = MassWin_Prep_VegC[[j]][[3]]$BestModel
  }
  
  if(which.min(c(MassWin_Prep_AABI[[j]]$combos$DeltaAICc[1],MassWin_Prep_AABI[[j]]$combos$DeltaAICc[2],MassWin_Prep_AABI[[j]]$combos$DeltaAICc[3])) == 1){
    bestmodel_Prep_AABI[[j]] = MassWin_Prep_AABI[[j]][[1]]$BestModelData[,1:2]
    bestmodel_Prep_AABI_pred[[j]] = MassWin_Prep_AABI[[j]][[1]]$BestModel
  }else if(which.min(c(MassWin_Prep_AABI[[j]]$combos$DeltaAICc[1],MassWin_Prep_AABI[[j]]$combos$DeltaAICc[2],MassWin_Prep_AABI[[j]]$combos$DeltaAICc[3])) == 2){
    bestmodel_Prep_AABI[[j]] = MassWin_Prep_AABI[[j]][[2]]$BestModelData[,1:2]
    bestmodel_Prep_AABI_pred[[j]] = MassWin_Prep_AABI[[j]][[2]]$BestModel
  }else{
    bestmodel_Prep_AABI[[j]] = MassWin_Prep_AABI[[j]][[3]]$BestModelData[,1:2]
    bestmodel_Prep_AABI_pred[[j]] = MassWin_Prep_AABI[[j]][[3]]$BestModel
  }

}

#Temp dataframes
  {
  bestmodel_Temp_NPP_df = bind_rows(bestmodel_Temp_NPP, .id = "point_ID")
  bestmodel_Temp_VegC_df = bind_rows(bestmodel_Temp_VegC, .id = "point_ID")
  bestmodel_Temp_AABI_df = bind_rows(bestmodel_Temp_AABI, .id = "point_ID")
  
  bestmodel_Temp_NPP_df$point_ID = as.numeric(bestmodel_Temp_NPP_df$point_ID)
  bestmodel_Temp_NPP_df = left_join(bestmodel_Temp_NPP_df,lat_df, by = "point_ID")
  
  bestmodel_Temp_VegC_df$point_ID = as.numeric(bestmodel_Temp_VegC_df$point_ID)
  bestmodel_Temp_VegC_df = left_join(bestmodel_Temp_VegC_df,lat_df, by = "point_ID")
  
  bestmodel_Temp_AABI_df$point_ID = as.numeric(bestmodel_Temp_AABI_df$point_ID)
  bestmodel_Temp_AABI_df = left_join(bestmodel_Temp_AABI_df,lat_df, by = "point_ID")

  bestmodel_Temp_AABI_L1_df = subset(bestmodel_Temp_AABI_df, lat_group == "L1")
  bestmodel_Temp_AABI_L2_df = subset(bestmodel_Temp_AABI_df, lat_group == "L2")
  bestmodel_Temp_AABI_L3_df = subset(bestmodel_Temp_AABI_df, lat_group == "L3")
  bestmodel_Temp_AABI_L4_df = subset(bestmodel_Temp_AABI_df, lat_group == "L4")
  bestmodel_Temp_AABI_L5_df = subset(bestmodel_Temp_AABI_df, lat_group == "L5")
  bestmodel_Temp_AABI_L6_df = subset(bestmodel_Temp_AABI_df, lat_group == "L6")
  
  bestmodel_Temp_NPP_L1_df = subset(bestmodel_Temp_NPP_df, lat_group == "L1")
  bestmodel_Temp_NPP_L2_df = subset(bestmodel_Temp_NPP_df, lat_group == "L2")
  bestmodel_Temp_NPP_L3_df = subset(bestmodel_Temp_NPP_df, lat_group == "L3")
  bestmodel_Temp_NPP_L4_df = subset(bestmodel_Temp_NPP_df, lat_group == "L4")
  bestmodel_Temp_NPP_L5_df = subset(bestmodel_Temp_NPP_df, lat_group == "L5")
  bestmodel_Temp_NPP_L6_df = subset(bestmodel_Temp_NPP_df, lat_group == "L6")
  
  bestmodel_Temp_VegC_L1_df = subset(bestmodel_Temp_VegC_df, lat_group == "L1")
  bestmodel_Temp_VegC_L2_df = subset(bestmodel_Temp_VegC_df, lat_group == "L2")
  bestmodel_Temp_VegC_L3_df = subset(bestmodel_Temp_VegC_df, lat_group == "L3")
  bestmodel_Temp_VegC_L4_df = subset(bestmodel_Temp_VegC_df, lat_group == "L4")
  bestmodel_Temp_VegC_L5_df = subset(bestmodel_Temp_VegC_df, lat_group == "L5")
  bestmodel_Temp_VegC_L6_df = subset(bestmodel_Temp_VegC_df, lat_group == "L6")
  
  bestmodel_Temp_AABI_L1_df$climate = bestmodel_Temp_AABI_L1_df$climate/2
  bestmodel_Temp_AABI_L2_df$climate = bestmodel_Temp_AABI_L2_df$climate/2
  bestmodel_Temp_AABI_L3_df$climate = bestmodel_Temp_AABI_L3_df$climate/2
  bestmodel_Temp_AABI_L4_df$climate = bestmodel_Temp_AABI_L4_df$climate/2
  bestmodel_Temp_AABI_L5_df$climate = bestmodel_Temp_AABI_L5_df$climate/2
  bestmodel_Temp_AABI_L6_df$climate = bestmodel_Temp_AABI_L6_df$climate/2

  bestmodel_Temp_NPP_L1_df$climate = bestmodel_Temp_NPP_L1_df$climate/2
  bestmodel_Temp_NPP_L2_df$climate = bestmodel_Temp_NPP_L2_df$climate/2
  bestmodel_Temp_NPP_L3_df$climate = bestmodel_Temp_NPP_L3_df$climate/2
  bestmodel_Temp_NPP_L4_df$climate = bestmodel_Temp_NPP_L4_df$climate/2
  bestmodel_Temp_NPP_L5_df$climate = bestmodel_Temp_NPP_L5_df$climate/2
  bestmodel_Temp_NPP_L6_df$climate = bestmodel_Temp_NPP_L6_df$climate/2

  bestmodel_Temp_VegC_L1_df$climate = bestmodel_Temp_VegC_L1_df$climate/2
  bestmodel_Temp_VegC_L2_df$climate = bestmodel_Temp_VegC_L2_df$climate/2
  bestmodel_Temp_VegC_L3_df$climate = bestmodel_Temp_VegC_L3_df$climate/2
  bestmodel_Temp_VegC_L4_df$climate = bestmodel_Temp_VegC_L4_df$climate/2
  bestmodel_Temp_VegC_L5_df$climate = bestmodel_Temp_VegC_L5_df$climate/2
  bestmodel_Temp_VegC_L6_df$climate = bestmodel_Temp_VegC_L6_df$climate/2
}
#Prep dataframes
  {
  bestmodel_Prep_NPP_df = bind_rows(bestmodel_Prep_NPP, .id = "point_ID")
  bestmodel_Prep_VegC_df = bind_rows(bestmodel_Prep_VegC, .id = "point_ID")
  bestmodel_Prep_AABI_df = bind_rows(bestmodel_Prep_AABI, .id = "point_ID")
  
  bestmodel_Prep_NPP_df$point_ID = as.numeric(bestmodel_Prep_NPP_df$point_ID)
  bestmodel_Prep_NPP_df = left_join(bestmodel_Prep_NPP_df,lat_df, by = "point_ID")
  
  bestmodel_Prep_VegC_df$point_ID = as.numeric(bestmodel_Prep_VegC_df$point_ID)
  bestmodel_Prep_VegC_df = left_join(bestmodel_Prep_VegC_df,lat_df, by = "point_ID")
  
  bestmodel_Prep_AABI_df$point_ID = as.numeric(bestmodel_Prep_AABI_df$point_ID)
  bestmodel_Prep_AABI_df = left_join(bestmodel_Prep_AABI_df,lat_df, by = "point_ID")

  # bestmodel_Prep_AABI_df1 = db(bestmodel_Prep_AABI_df)
  # bestmodel_Prep_NPP_df1 = db(bestmodel_Prep_NPP_df)
  # bestmodel_Prep_VegC_df1 = db(bestmodel_Prep_VegC_df)
  
  bestmodel_Prep_AABI_L1_df = subset(bestmodel_Prep_AABI_df, lat_group == "L1")
  bestmodel_Prep_AABI_L2_df = subset(bestmodel_Prep_AABI_df, lat_group == "L2")
  bestmodel_Prep_AABI_L3_df = subset(bestmodel_Prep_AABI_df, lat_group == "L3")
  bestmodel_Prep_AABI_L4_df = subset(bestmodel_Prep_AABI_df, lat_group == "L4")
  bestmodel_Prep_AABI_L5_df = subset(bestmodel_Prep_AABI_df, lat_group == "L5")
  bestmodel_Prep_AABI_L6_df = subset(bestmodel_Prep_AABI_df, lat_group == "L6")
  
  bestmodel_Prep_NPP_L1_df = subset(bestmodel_Prep_NPP_df, lat_group == "L1")
  bestmodel_Prep_NPP_L2_df = subset(bestmodel_Prep_NPP_df, lat_group == "L2")
  bestmodel_Prep_NPP_L3_df = subset(bestmodel_Prep_NPP_df, lat_group == "L3")
  bestmodel_Prep_NPP_L4_df = subset(bestmodel_Prep_NPP_df, lat_group == "L4")
  bestmodel_Prep_NPP_L5_df = subset(bestmodel_Prep_NPP_df, lat_group == "L5")
  bestmodel_Prep_NPP_L6_df = subset(bestmodel_Prep_NPP_df, lat_group == "L6")
  
  bestmodel_Prep_VegC_L1_df = subset(bestmodel_Prep_VegC_df, lat_group == "L1")
  bestmodel_Prep_VegC_L2_df = subset(bestmodel_Prep_VegC_df, lat_group == "L2")
  bestmodel_Prep_VegC_L3_df = subset(bestmodel_Prep_VegC_df, lat_group == "L3")
  bestmodel_Prep_VegC_L4_df = subset(bestmodel_Prep_VegC_df, lat_group == "L4")
  bestmodel_Prep_VegC_L5_df = subset(bestmodel_Prep_VegC_df, lat_group == "L5")
  bestmodel_Prep_VegC_L6_df = subset(bestmodel_Prep_VegC_df, lat_group == "L6")
  
  bestmodel_Prep_AABI_L1_df$climate = bestmodel_Prep_AABI_L1_df$climate*30
  bestmodel_Prep_AABI_L2_df$climate = bestmodel_Prep_AABI_L2_df$climate*30
  bestmodel_Prep_AABI_L3_df$climate = bestmodel_Prep_AABI_L3_df$climate*30
  bestmodel_Prep_AABI_L4_df$climate = bestmodel_Prep_AABI_L4_df$climate*30
  bestmodel_Prep_AABI_L5_df$climate = bestmodel_Prep_AABI_L5_df$climate*30
  bestmodel_Prep_AABI_L6_df$climate = bestmodel_Prep_AABI_L6_df$climate*30
  
  bestmodel_Prep_NPP_L1_df$climate = bestmodel_Prep_NPP_L1_df$climate*30
  bestmodel_Prep_NPP_L2_df$climate = bestmodel_Prep_NPP_L2_df$climate*30
  bestmodel_Prep_NPP_L3_df$climate = bestmodel_Prep_NPP_L3_df$climate*30
  bestmodel_Prep_NPP_L4_df$climate = bestmodel_Prep_NPP_L4_df$climate*30
  bestmodel_Prep_NPP_L5_df$climate = bestmodel_Prep_NPP_L5_df$climate*30
  bestmodel_Prep_NPP_L6_df$climate = bestmodel_Prep_NPP_L6_df$climate*30
  
  bestmodel_Prep_VegC_L1_df$climate = bestmodel_Prep_VegC_L1_df$climate*30
  bestmodel_Prep_VegC_L2_df$climate = bestmodel_Prep_VegC_L2_df$climate*30
  bestmodel_Prep_VegC_L3_df$climate = bestmodel_Prep_VegC_L3_df$climate*30
  bestmodel_Prep_VegC_L4_df$climate = bestmodel_Prep_VegC_L4_df$climate*30
  bestmodel_Prep_VegC_L5_df$climate = bestmodel_Prep_VegC_L5_df$climate*30
  bestmodel_Prep_VegC_L6_df$climate = bestmodel_Prep_VegC_L6_df$climate*30
  
}

  #linear model test
{
  lm_AABI_Temp_L1_model <- lm(yvar ~climate ,data = bestmodel_Temp_AABI_L1_df)
  lm_AABI_Temp_L2_model <- lm(yvar ~climate ,data = bestmodel_Temp_AABI_L2_df)
  lm_AABI_Temp_L3_model <- lm(yvar ~climate ,data = bestmodel_Temp_AABI_L3_df)
  lm_AABI_Temp_L4_model <- lm(yvar ~climate ,data = bestmodel_Temp_AABI_L4_df)
  lm_AABI_Temp_L5_model <- lm(yvar ~climate ,data = bestmodel_Temp_AABI_L5_df)
  lm_AABI_Temp_L6_model <- lm(yvar ~climate ,data = bestmodel_Temp_AABI_L6_df)
  
  lm_NPP_Temp_L1_model <- lm(yvar ~climate ,data = bestmodel_Temp_NPP_L1_df)
  lm_NPP_Temp_L2_model <- lm(yvar ~climate ,data = bestmodel_Temp_NPP_L2_df)
  lm_NPP_Temp_L3_model <- lm(yvar ~climate ,data = bestmodel_Temp_NPP_L3_df)
  lm_NPP_Temp_L4_model <- lm(yvar ~climate ,data = bestmodel_Temp_NPP_L4_df)
  lm_NPP_Temp_L5_model <- lm(yvar ~climate ,data = bestmodel_Temp_NPP_L5_df)
  lm_NPP_Temp_L6_model <- lm(yvar ~climate ,data = bestmodel_Temp_NPP_L6_df)
  
  lm_VegC_Temp_L1_model <- lm(yvar ~climate ,data = bestmodel_Temp_VegC_L1_df)
  lm_VegC_Temp_L2_model <- lm(yvar ~climate ,data = bestmodel_Temp_VegC_L2_df)
  lm_VegC_Temp_L3_model <- lm(yvar ~climate ,data = bestmodel_Temp_VegC_L3_df)
  lm_VegC_Temp_L4_model <- lm(yvar ~climate ,data = bestmodel_Temp_VegC_L4_df)
  lm_VegC_Temp_L5_model <- lm(yvar ~climate ,data = bestmodel_Temp_VegC_L5_df)
  lm_VegC_Temp_L6_model <- lm(yvar ~climate ,data = bestmodel_Temp_VegC_L6_df)
  
  lm_AABI_Prep_L1_model <- lm(yvar ~climate ,data = bestmodel_Prep_AABI_L1_df)
  lm_AABI_Prep_L2_model <- lm(yvar ~climate ,data = bestmodel_Prep_AABI_L2_df)
  lm_AABI_Prep_L3_model <- lm(yvar ~climate ,data = bestmodel_Prep_AABI_L3_df)
  lm_AABI_Prep_L4_model <- lm(yvar ~climate ,data = bestmodel_Prep_AABI_L4_df)
  lm_AABI_Prep_L5_model <- lm(yvar ~climate ,data = bestmodel_Prep_AABI_L5_df)
  lm_AABI_Prep_L6_model <- lm(yvar ~climate ,data = bestmodel_Prep_AABI_L6_df)
  
  lm_NPP_Prep_L1_model <- lm(yvar ~climate ,data = bestmodel_Prep_NPP_L1_df)
  lm_NPP_Prep_L2_model <- lm(yvar ~climate ,data = bestmodel_Prep_NPP_L2_df)
  lm_NPP_Prep_L3_model <- lm(yvar ~climate ,data = bestmodel_Prep_NPP_L3_df)
  lm_NPP_Prep_L4_model <- lm(yvar ~climate ,data = bestmodel_Prep_NPP_L4_df)
  lm_NPP_Prep_L5_model <- lm(yvar ~climate ,data = bestmodel_Prep_NPP_L5_df)
  lm_NPP_Prep_L6_model <- lm(yvar ~climate ,data = bestmodel_Prep_NPP_L6_df)
  
  lm_VegC_Prep_L1_model <- lm(yvar ~climate ,data = bestmodel_Prep_VegC_L1_df)
  lm_VegC_Prep_L2_model <- lm(yvar ~climate ,data = bestmodel_Prep_VegC_L2_df)
  lm_VegC_Prep_L3_model <- lm(yvar ~climate ,data = bestmodel_Prep_VegC_L3_df)
  lm_VegC_Prep_L4_model <- lm(yvar ~climate ,data = bestmodel_Prep_VegC_L4_df)
  lm_VegC_Prep_L5_model <- lm(yvar ~climate ,data = bestmodel_Prep_VegC_L5_df)
  lm_VegC_Prep_L6_model <- lm(yvar ~climate ,data = bestmodel_Prep_VegC_L6_df)
  

}  
  #quadratic model test
{

  quad_AABI_Temp_L1_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_AABI_L1_df)
  quad_AABI_Temp_L2_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_AABI_L2_df)
  quad_AABI_Temp_L3_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_AABI_L3_df)
  quad_AABI_Temp_L4_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_AABI_L4_df)
  quad_AABI_Temp_L5_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_AABI_L5_df)
  quad_AABI_Temp_L6_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_AABI_L6_df)
  
  quad_NPP_Temp_L1_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_NPP_L1_df)
  quad_NPP_Temp_L2_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_NPP_L2_df)
  quad_NPP_Temp_L3_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_NPP_L3_df)
  quad_NPP_Temp_L4_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_NPP_L4_df)
  quad_NPP_Temp_L5_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_NPP_L5_df)
  quad_NPP_Temp_L6_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_NPP_L6_df)
  
  quad_VegC_Temp_L1_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_VegC_L1_df)
  quad_VegC_Temp_L2_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_VegC_L2_df)
  quad_VegC_Temp_L3_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_VegC_L3_df)
  quad_VegC_Temp_L4_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_VegC_L4_df)
  quad_VegC_Temp_L5_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_VegC_L5_df)
  quad_VegC_Temp_L6_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Temp_VegC_L6_df)
  
  quad_AABI_Prep_L1_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_AABI_L1_df)
  quad_AABI_Prep_L2_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_AABI_L2_df)
  quad_AABI_Prep_L3_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_AABI_L3_df)
  quad_AABI_Prep_L4_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_AABI_L4_df)
  quad_AABI_Prep_L5_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_AABI_L5_df)
  quad_AABI_Prep_L6_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_AABI_L6_df)
  
  quad_NPP_Prep_L1_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_NPP_L1_df)
  quad_NPP_Prep_L2_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_NPP_L2_df)
  quad_NPP_Prep_L3_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_NPP_L3_df)
  quad_NPP_Prep_L4_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_NPP_L4_df)
  quad_NPP_Prep_L5_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_NPP_L5_df)
  quad_NPP_Prep_L6_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_NPP_L6_df)
  
  quad_VegC_Prep_L1_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_VegC_L1_df)
  quad_VegC_Prep_L2_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_VegC_L2_df)
  quad_VegC_Prep_L3_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_VegC_L3_df)
  quad_VegC_Prep_L4_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_VegC_L4_df)
  quad_VegC_Prep_L5_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_VegC_L5_df)
  quad_VegC_Prep_L6_model <- lm(yvar ~climate + I(climate^2),data = bestmodel_Prep_VegC_L6_df)
  

}

  pred_AABI_Temp_L1 <- predict(quad_AABI_Temp_L1_model, newdata = data.frame(climate = bestmodel_Temp_AABI_L1_df$climate), interval = "confidence")
  pred_AABI_Temp_L2 <- predict(quad_AABI_Temp_L2_model, newdata = data.frame(climate = bestmodel_Temp_AABI_L2_df$climate), interval = "confidence")
  pred_AABI_Temp_L3 <- predict(quad_AABI_Temp_L3_model, newdata = data.frame(climate = bestmodel_Temp_AABI_L3_df$climate), interval = "confidence")
  pred_AABI_Temp_L4 <- predict(quad_AABI_Temp_L4_model, newdata = data.frame(climate = bestmodel_Temp_AABI_L4_df$climate), interval = "confidence")
  pred_AABI_Temp_L5 <- predict(lm_AABI_Temp_L5_model, newdata = data.frame(climate = bestmodel_Temp_AABI_L5_df$climate), interval = "confidence")
  pred_AABI_Temp_L6 <- predict(quad_AABI_Temp_L6_model, newdata = data.frame(climate = bestmodel_Temp_AABI_L6_df$climate), interval = "confidence")
  
  pred_NPP_Temp_L1 <- predict(quad_NPP_Temp_L1_model, newdata = data.frame(climate = bestmodel_Temp_NPP_L1_df$climate), interval = "confidence")
  pred_NPP_Temp_L2 <- predict(quad_NPP_Temp_L2_model, newdata = data.frame(climate = bestmodel_Temp_NPP_L2_df$climate), interval = "confidence")
  pred_NPP_Temp_L3 <- predict(quad_NPP_Temp_L3_model, newdata = data.frame(climate = bestmodel_Temp_NPP_L3_df$climate), interval = "confidence")
  pred_NPP_Temp_L4 <- predict(quad_NPP_Temp_L4_model, newdata = data.frame(climate = bestmodel_Temp_NPP_L4_df$climate), interval = "confidence")
  pred_NPP_Temp_L5 <- predict(quad_NPP_Temp_L5_model, newdata = data.frame(climate = bestmodel_Temp_NPP_L5_df$climate), interval = "confidence")
  pred_NPP_Temp_L6 <- predict(quad_NPP_Temp_L6_model, newdata = data.frame(climate = bestmodel_Temp_NPP_L6_df$climate), interval = "confidence")
  
  pred_VegC_Temp_L1 <- predict(quad_VegC_Temp_L1_model, newdata = data.frame(climate = bestmodel_Temp_VegC_L1_df$climate), interval = "confidence")
  pred_VegC_Temp_L2 <- predict(quad_VegC_Temp_L2_model, newdata = data.frame(climate = bestmodel_Temp_VegC_L2_df$climate), interval = "confidence")
  pred_VegC_Temp_L3 <- predict(quad_VegC_Temp_L3_model, newdata = data.frame(climate = bestmodel_Temp_VegC_L3_df$climate), interval = "confidence")
  pred_VegC_Temp_L4 <- predict(quad_VegC_Temp_L4_model, newdata = data.frame(climate = bestmodel_Temp_VegC_L4_df$climate), interval = "confidence")
  pred_VegC_Temp_L5 <- predict(quad_VegC_Temp_L5_model, newdata = data.frame(climate = bestmodel_Temp_VegC_L5_df$climate), interval = "confidence")
  pred_VegC_Temp_L6 <- predict(quad_VegC_Temp_L6_model, newdata = data.frame(climate = bestmodel_Temp_VegC_L6_df$climate), interval = "confidence")
  
 
  pred_AABI_Prep_L1 <- predict(quad_AABI_Prep_L1_model, newdata = data.frame(climate = bestmodel_Prep_AABI_L1_df$climate), interval = "confidence")
  pred_AABI_Prep_L2 <- predict(quad_AABI_Prep_L2_model, newdata = data.frame(climate = bestmodel_Prep_AABI_L2_df$climate), interval = "confidence")
  pred_AABI_Prep_L3 <- predict(quad_AABI_Prep_L3_model, newdata = data.frame(climate = bestmodel_Prep_AABI_L3_df$climate), interval = "confidence")
  pred_AABI_Prep_L4 <- predict(quad_AABI_Prep_L4_model, newdata = data.frame(climate = bestmodel_Prep_AABI_L4_df$climate), interval = "confidence")
  pred_AABI_Prep_L5 <- predict(quad_AABI_Prep_L5_model, newdata = data.frame(climate = bestmodel_Prep_AABI_L5_df$climate), interval = "confidence")
  pred_AABI_Prep_L6 <- predict(quad_AABI_Prep_L6_model, newdata = data.frame(climate = bestmodel_Prep_AABI_L6_df$climate), interval = "confidence")
  
  pred_NPP_Prep_L1 <- predict(quad_NPP_Prep_L1_model, newdata = data.frame(climate = bestmodel_Prep_NPP_L1_df$climate), interval = "confidence")
  pred_NPP_Prep_L2 <- predict(quad_NPP_Prep_L2_model, newdata = data.frame(climate = bestmodel_Prep_NPP_L2_df$climate), interval = "confidence")
  pred_NPP_Prep_L3 <- predict(quad_NPP_Prep_L3_model, newdata = data.frame(climate = bestmodel_Prep_NPP_L3_df$climate), interval = "confidence")
  pred_NPP_Prep_L4 <- predict(quad_NPP_Prep_L4_model, newdata = data.frame(climate = bestmodel_Prep_NPP_L4_df$climate), interval = "confidence")
  pred_NPP_Prep_L5 <- predict(quad_NPP_Prep_L5_model, newdata = data.frame(climate = bestmodel_Prep_NPP_L5_df$climate), interval = "confidence")
  pred_NPP_Prep_L6 <- predict(quad_NPP_Prep_L6_model, newdata = data.frame(climate = bestmodel_Prep_NPP_L6_df$climate), interval = "confidence")
  
  pred_VegC_Prep_L1 <- predict(quad_VegC_Prep_L1_model, newdata = data.frame(climate = bestmodel_Prep_VegC_L1_df$climate), interval = "confidence")
  pred_VegC_Prep_L2 <- predict(quad_VegC_Prep_L2_model, newdata = data.frame(climate = bestmodel_Prep_VegC_L2_df$climate), interval = "confidence")
  pred_VegC_Prep_L3 <- predict(quad_VegC_Prep_L3_model, newdata = data.frame(climate = bestmodel_Prep_VegC_L3_df$climate), interval = "confidence")
  pred_VegC_Prep_L4 <- predict(quad_VegC_Prep_L4_model, newdata = data.frame(climate = bestmodel_Prep_VegC_L4_df$climate), interval = "confidence")
  pred_VegC_Prep_L5 <- predict(quad_VegC_Prep_L5_model, newdata = data.frame(climate = bestmodel_Prep_VegC_L5_df$climate), interval = "confidence")
  pred_VegC_Prep_L6 <- predict(quad_VegC_Prep_L6_model, newdata = data.frame(climate = bestmodel_Prep_VegC_L6_df$climate), interval = "confidence")
  
 
  #plot Temp dataframes
  {
  plot_AABI_Temp_L1 <- data.frame(
    xvar = bestmodel_Temp_AABI_L1_df$climate,
    yvar = bestmodel_Temp_AABI_L1_df$yvar,
    fitted = pred_AABI_Temp_L1[,1],
    lower = pred_AABI_Temp_L1[,2],
    upper = pred_AABI_Temp_L1[,3]
  )
  
  plot_AABI_Temp_L2 <- data.frame(
    xvar = bestmodel_Temp_AABI_L2_df$climate,
    yvar = bestmodel_Temp_AABI_L2_df$yvar,
    fitted = pred_AABI_Temp_L2[,1],
    lower = pred_AABI_Temp_L2[,2],
    upper = pred_AABI_Temp_L2[,3]
  )
  
  plot_AABI_Temp_L3 <- data.frame(
    xvar = bestmodel_Temp_AABI_L3_df$climate,
    yvar = bestmodel_Temp_AABI_L3_df$yvar,
    fitted = pred_AABI_Temp_L3[,1],
    lower = pred_AABI_Temp_L3[,2],
    upper = pred_AABI_Temp_L3[,3]
  )
  
  plot_AABI_Temp_L4 <- data.frame(
    xvar = bestmodel_Temp_AABI_L4_df$climate,
    yvar = bestmodel_Temp_AABI_L4_df$yvar,
    fitted = pred_AABI_Temp_L4[,1],
    lower = pred_AABI_Temp_L4[,2],
    upper = pred_AABI_Temp_L4[,3]
  )
  
  plot_AABI_Temp_L5 <- data.frame(
    xvar = bestmodel_Temp_AABI_L5_df$climate,
    yvar = bestmodel_Temp_AABI_L5_df$yvar,
    fitted = pred_AABI_Temp_L5[,1],
    lower = pred_AABI_Temp_L5[,2],
    upper = pred_AABI_Temp_L5[,3]
  )
  
  plot_AABI_Temp_L6 <- data.frame(
    xvar = bestmodel_Temp_AABI_L6_df$climate,
    yvar = bestmodel_Temp_AABI_L6_df$yvar,
    fitted = pred_AABI_Temp_L6[,1],
    lower = pred_AABI_Temp_L6[,2],
    upper = pred_AABI_Temp_L6[,3]
  )
    
    plot_NPP_Temp_L1 <- data.frame(
      xvar = bestmodel_Temp_NPP_L1_df$climate,
      yvar = bestmodel_Temp_NPP_L1_df$yvar,
      fitted = pred_NPP_Temp_L1[,1],
      lower = pred_NPP_Temp_L1[,2],
      upper = pred_NPP_Temp_L1[,3]
    )
    
    plot_NPP_Temp_L2 <- data.frame(
      xvar = bestmodel_Temp_NPP_L2_df$climate,
      yvar = bestmodel_Temp_NPP_L2_df$yvar,
      fitted = pred_NPP_Temp_L2[,1],
      lower = pred_NPP_Temp_L2[,2],
      upper = pred_NPP_Temp_L2[,3]
    )
    
    plot_NPP_Temp_L3 <- data.frame(
      xvar = bestmodel_Temp_NPP_L3_df$climate,
      yvar = bestmodel_Temp_NPP_L3_df$yvar,
      fitted = pred_NPP_Temp_L3[,1],
      lower = pred_NPP_Temp_L3[,2],
      upper = pred_NPP_Temp_L3[,3]
    )
    
    plot_NPP_Temp_L4 <- data.frame(
      xvar = bestmodel_Temp_NPP_L4_df$climate,
      yvar = bestmodel_Temp_NPP_L4_df$yvar,
      fitted = pred_NPP_Temp_L4[,1],
      lower = pred_NPP_Temp_L4[,2],
      upper = pred_NPP_Temp_L4[,3]
    )
    
    plot_NPP_Temp_L5 <- data.frame(
      xvar = bestmodel_Temp_NPP_L5_df$climate,
      yvar = bestmodel_Temp_NPP_L5_df$yvar,
      fitted = pred_NPP_Temp_L5[,1],
      lower = pred_NPP_Temp_L5[,2],
      upper = pred_NPP_Temp_L5[,3]
    )
    
    plot_NPP_Temp_L6 <- data.frame(
      xvar = bestmodel_Temp_NPP_L6_df$climate,
      yvar = bestmodel_Temp_NPP_L6_df$yvar,
      fitted = pred_NPP_Temp_L6[,1],
      lower = pred_NPP_Temp_L6[,2],
      upper = pred_NPP_Temp_L6[,3]
    )
      
      plot_VegC_Temp_L1 <- data.frame(
        xvar = bestmodel_Temp_VegC_L1_df$climate,
        yvar = bestmodel_Temp_VegC_L1_df$yvar,
        fitted = pred_VegC_Temp_L1[,1],
        lower = pred_VegC_Temp_L1[,2],
        upper = pred_VegC_Temp_L1[,3]
      )
      
      plot_VegC_Temp_L2 <- data.frame(
        xvar = bestmodel_Temp_VegC_L2_df$climate,
        yvar = bestmodel_Temp_VegC_L2_df$yvar,
        fitted = pred_VegC_Temp_L2[,1],
        lower = pred_VegC_Temp_L2[,2],
        upper = pred_VegC_Temp_L2[,3]
      )
      
      plot_VegC_Temp_L3 <- data.frame(
        xvar = bestmodel_Temp_VegC_L3_df$climate,
        yvar = bestmodel_Temp_VegC_L3_df$yvar,
        fitted = pred_VegC_Temp_L3[,1],
        lower = pred_VegC_Temp_L3[,2],
        upper = pred_VegC_Temp_L3[,3]
      )
      
      plot_VegC_Temp_L4 <- data.frame(
        xvar = bestmodel_Temp_VegC_L4_df$climate,
        yvar = bestmodel_Temp_VegC_L4_df$yvar,
        fitted = pred_VegC_Temp_L4[,1],
        lower = pred_VegC_Temp_L4[,2],
        upper = pred_VegC_Temp_L4[,3]
      )
      
      plot_VegC_Temp_L5 <- data.frame(
        xvar = bestmodel_Temp_VegC_L5_df$climate,
        yvar = bestmodel_Temp_VegC_L5_df$yvar,
        fitted = pred_VegC_Temp_L5[,1],
        lower = pred_VegC_Temp_L5[,2],
        upper = pred_VegC_Temp_L5[,3]
      )
      
      plot_VegC_Temp_L6 <- data.frame(
        xvar = bestmodel_Temp_VegC_L6_df$climate,
        yvar = bestmodel_Temp_VegC_L6_df$yvar,
        fitted = pred_VegC_Temp_L6[,1],
        lower = pred_VegC_Temp_L6[,2],
        upper = pred_VegC_Temp_L6[,3]
      )
    
  }
  #plot Prep dataframes
  {
  plot_AABI_Prep_L1 <- data.frame(
    xvar = bestmodel_Prep_AABI_L1_df$climate,
    yvar = bestmodel_Prep_AABI_L1_df$yvar,
    fitted = pred_AABI_Prep_L1[,1],
    lower = pred_AABI_Prep_L1[,2],
    upper = pred_AABI_Prep_L1[,3]
  )
  
  plot_AABI_Prep_L2 <- data.frame(
    xvar = bestmodel_Prep_AABI_L2_df$climate,
    yvar = bestmodel_Prep_AABI_L2_df$yvar,
    fitted = pred_AABI_Prep_L2[,1],
    lower = pred_AABI_Prep_L2[,2],
    upper = pred_AABI_Prep_L2[,3]
  )
  
  plot_AABI_Prep_L3 <- data.frame(
    xvar = bestmodel_Prep_AABI_L3_df$climate,
    yvar = bestmodel_Prep_AABI_L3_df$yvar,
    fitted = pred_AABI_Prep_L3[,1],
    lower = pred_AABI_Prep_L3[,2],
    upper = pred_AABI_Prep_L3[,3]
  )
  
  plot_AABI_Prep_L4 <- data.frame(
    xvar = bestmodel_Prep_AABI_L4_df$climate,
    yvar = bestmodel_Prep_AABI_L4_df$yvar,
    fitted = pred_AABI_Prep_L4[,1],
    lower = pred_AABI_Prep_L4[,2],
    upper = pred_AABI_Prep_L4[,3]
  )
  
  plot_AABI_Prep_L5 <- data.frame(
    xvar = bestmodel_Prep_AABI_L5_df$climate,
    yvar = bestmodel_Prep_AABI_L5_df$yvar,
    fitted = pred_AABI_Prep_L5[,1],
    lower = pred_AABI_Prep_L5[,2],
    upper = pred_AABI_Prep_L5[,3]
  )
  
  plot_AABI_Prep_L6 <- data.frame(
    xvar = bestmodel_Prep_AABI_L6_df$climate,
    yvar = bestmodel_Prep_AABI_L6_df$yvar,
    fitted = pred_AABI_Prep_L6[,1],
    lower = pred_AABI_Prep_L6[,2],
    upper = pred_AABI_Prep_L6[,3]
  )
  
    plot_NPP_Prep_L1 <- data.frame(
      xvar = bestmodel_Prep_NPP_L1_df$climate,
      yvar = bestmodel_Prep_NPP_L1_df$yvar,
      fitted = pred_NPP_Prep_L1[,1],
      lower = pred_NPP_Prep_L1[,2],
      upper = pred_NPP_Prep_L1[,3]
    )
    
    plot_NPP_Prep_L2 <- data.frame(
      xvar = bestmodel_Prep_NPP_L2_df$climate,
      yvar = bestmodel_Prep_NPP_L2_df$yvar,
      fitted = pred_NPP_Prep_L2[,1],
      lower = pred_NPP_Prep_L2[,2],
      upper = pred_NPP_Prep_L2[,3]
    )
    
    plot_NPP_Prep_L3 <- data.frame(
      xvar = bestmodel_Prep_NPP_L3_df$climate,
      yvar = bestmodel_Prep_NPP_L3_df$yvar,
      fitted = pred_NPP_Prep_L3[,1],
      lower = pred_NPP_Prep_L3[,2],
      upper = pred_NPP_Prep_L3[,3]
    )
    
    plot_NPP_Prep_L4 <- data.frame(
      xvar = bestmodel_Prep_NPP_L4_df$climate,
      yvar = bestmodel_Prep_NPP_L4_df$yvar,
      fitted = pred_NPP_Prep_L4[,1],
      lower = pred_NPP_Prep_L4[,2],
      upper = pred_NPP_Prep_L4[,3]
    )
    
    plot_NPP_Prep_L5 <- data.frame(
      xvar = bestmodel_Prep_NPP_L5_df$climate,
      yvar = bestmodel_Prep_NPP_L5_df$yvar,
      fitted = pred_NPP_Prep_L5[,1],
      lower = pred_NPP_Prep_L5[,2],
      upper = pred_NPP_Prep_L5[,3]
    )
    
    plot_NPP_Prep_L6 <- data.frame(
      xvar = bestmodel_Prep_NPP_L6_df$climate,
      yvar = bestmodel_Prep_NPP_L6_df$yvar,
      fitted = pred_NPP_Prep_L6[,1],
      lower = pred_NPP_Prep_L6[,2],
      upper = pred_NPP_Prep_L6[,3]
    )
      
      plot_VegC_Prep_L1 <- data.frame(
        xvar = bestmodel_Prep_VegC_L1_df$climate,
        yvar = bestmodel_Prep_VegC_L1_df$yvar,
        fitted = pred_VegC_Prep_L1[,1],
        lower = pred_VegC_Prep_L1[,2],
        upper = pred_VegC_Prep_L1[,3]
      )
      
      plot_VegC_Prep_L2 <- data.frame(
        xvar = bestmodel_Prep_VegC_L2_df$climate,
        yvar = bestmodel_Prep_VegC_L2_df$yvar,
        fitted = pred_VegC_Prep_L2[,1],
        lower = pred_VegC_Prep_L2[,2],
        upper = pred_VegC_Prep_L2[,3]
      )
      
      plot_VegC_Prep_L3 <- data.frame(
        xvar = bestmodel_Prep_VegC_L3_df$climate,
        yvar = bestmodel_Prep_VegC_L3_df$yvar,
        fitted = pred_VegC_Prep_L3[,1],
        lower = pred_VegC_Prep_L3[,2],
        upper = pred_VegC_Prep_L3[,3]
      )
      
      plot_VegC_Prep_L4 <- data.frame(
        xvar = bestmodel_Prep_VegC_L4_df$climate,
        yvar = bestmodel_Prep_VegC_L4_df$yvar,
        fitted = pred_VegC_Prep_L4[,1],
        lower = pred_VegC_Prep_L4[,2],
        upper = pred_VegC_Prep_L4[,3]
      )
      
      plot_VegC_Prep_L5 <- data.frame(
        xvar = bestmodel_Prep_VegC_L5_df$climate,
        yvar = bestmodel_Prep_VegC_L5_df$yvar,
        fitted = pred_VegC_Prep_L5[,1],
        lower = pred_VegC_Prep_L5[,2],
        upper = pred_VegC_Prep_L5[,3]
      )
      
      plot_VegC_Prep_L6 <- data.frame(
        xvar = bestmodel_Prep_VegC_L6_df$climate,
        yvar = bestmodel_Prep_VegC_L6_df$yvar,
        fitted = pred_VegC_Prep_L6[,1],
        lower = pred_VegC_Prep_L6[,2],
        upper = pred_VegC_Prep_L6[,3]
  )
  }
  
  pdf(".\\Clim_response\\data\\Fig21_1.pdf",width = 6,height = 2.2)
  
 p211 <- ggplot() +
    geom_line(data = plot_AABI_Temp_L1, aes(x = xvar, y = fitted), color = "#000004FF",size =1) +
    geom_ribbon(data = plot_AABI_Temp_L1, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#000004FF") +  # CI for Variable1
    geom_line(data = plot_NPP_Temp_L1, aes(x = xvar, y = (fitted*10)), color = "#000004FF",linetype = "dashed",size =1) +  # First y variable
    geom_ribbon(data = plot_NPP_Temp_L1, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill = "#000004FF") +  # CI for Variable1
    scale_y_continuous(
      name = "",
      limits = c(-0.6, 1.1),  # Set your primary axis limits here
      breaks = seq(-0.5,1,0.5),
      sec.axis = sec_axis(~ . /10, name = ""
                          )  # Adjust as necessary
    ) +  
    xlim(-5,5)+# Adjust scale as needed
   theme_minimal()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
     axis.title.x = element_blank(),  # Removes the x-axis label
     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
   )+
   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 # +
 #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  p211
  dev.off()
 

 pdf(".\\Clim_response\\data\\Fig21_2.pdf",width = 6,height = 2.2)
 
  p212 <- ggplot() +
    geom_line(data = plot_AABI_Temp_L2, aes(x = xvar, y = fitted), color = "#3A0963FF",size =1) +
    geom_ribbon(data = plot_AABI_Temp_L2, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#3A0963FF") +  # CI for Variable1
    geom_line(data = plot_NPP_Temp_L2, aes(x = xvar, y = (fitted*10)), color = "#3A0963FF",linetype = "dashed",size =1) +  # First y variable
    geom_ribbon(data = plot_NPP_Temp_L2, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill = "#3A0963FF") +  # CI for Variable1
    scale_y_continuous(
      name = "",
      limits = c(-1.1, 1.2),  # Set your primary axis limits here
      sec.axis = sec_axis(~ . /10, name = ""
      ),  # Adjust as necessary
    ) +  
    xlim(-5,5)+# Adjust scale as needed
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
      axis.title.x = element_blank(),  # Removes the x-axis label
      axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
    )+
    theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))

  # +
  #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  p212
  dev.off()
  
  
  
  pdf(".\\Clim_response\\data\\Fig21_3.pdf",width = 6,height = 2.2)

  
  p213 <- ggplot() +
    geom_line(data = plot_AABI_Temp_L3, aes(x = xvar, y = fitted), color = "#85216BFF",size =1) +
    geom_ribbon(data = plot_AABI_Temp_L3, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#85216BFF") +  # CI for Variable1
    geom_line(data = plot_NPP_Temp_L3, aes(x = xvar, y = (fitted*10)), color = "#85216BFF",linetype = "dashed",size =1) +  # First y variable
    geom_ribbon(data = plot_NPP_Temp_L3, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill = "#85216BFF") +  # CI for Variable1
    scale_y_continuous(
      name = "",
      limits = c(-1, 1.3),  # Set your primary axis limits here
      sec.axis = sec_axis(~ . /10, name = ""
      )  # Adjust as necessary
    ) +  
    xlim(-5,5)+# Adjust scale as needed
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
      axis.title.x = element_blank(),  # Removes the x-axis label
      axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
    )+
    theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
  # +
  #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  p213
  dev.off()
  
  
  pdf(".\\Clim_response\\data\\Fig21_4.pdf",width = 6,height = 2.2)
  
  p214 <- ggplot() +
    geom_line(data = plot_AABI_Temp_L4, aes(x = xvar, y = fitted), color = "#CB4149FF",size =1) +
    geom_ribbon(data = plot_AABI_Temp_L4, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#CB4149FF") +  # CI for Variable1
    geom_line(data = plot_NPP_Temp_L4, aes(x = xvar, y = (fitted*15)), color = "#CB4149FF",linetype = "dashed",size =1) +  # First y variable
    geom_ribbon(data = plot_NPP_Temp_L4, aes(x = xvar, ymin = (lower*15), ymax = (upper*15)), alpha = 0.2, fill = "#CB4149FF") +  # CI for Variable1
    scale_y_continuous(
      name = "",
      limits = c(-0.4, 1),  # Set your primary axis limits here
      
      sec.axis = sec_axis(~ . /15, name = ""
      )  # Adjust as necessary
    ) +  
    xlim(-5,5)+# Adjust scale as needed
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
      axis.title.x = element_blank(),  # Removes the x-axis label
      axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
    )+
    theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
  # +
  #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  p214
  dev.off()

  
  pdf(".\\Clim_response\\data\\Fig21_5.pdf",width = 6,height = 2.2)
  
  p215 <- ggplot() +
    geom_line(data = plot_AABI_Temp_L5, aes(x = xvar, y = fitted), color = "#F78311FF",size =1) +
    geom_ribbon(data = plot_AABI_Temp_L5, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#F78311FF") +  # CI for Variable1
    geom_line(data = plot_NPP_Temp_L5, aes(x = xvar, y = (fitted*5)), color = "#F78311FF",linetype = "dashed",size =1) +  # First y variable
    geom_ribbon(data = plot_NPP_Temp_L5, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#F78311FF") +  # CI for Variable1
    scale_y_continuous(
      name = "",
      limits = c(-1.3, 1),  # Set your primary axis limits here
      sec.axis = sec_axis(~ . /5, name = ""
      )  # Adjust as necessary
    ) +  
    xlim(-5,5)+# Adjust scale as needed
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
      axis.title.x = element_blank(),  # Removes the x-axis label
      axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
    )+
    theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
  # +
  #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  p215
  dev.off()
  
  
  
  pdf(".\\Clim_response\\data\\Fig21_6.pdf",width = 6,height = 2.2)
  
  p216 <- ggplot() +
    geom_line(data = plot_AABI_Temp_L6, aes(x = xvar, y = fitted), color = "#FCAD12FF",size =1) +
    geom_ribbon(data = plot_AABI_Temp_L6, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#FCAD12FF") +  # CI for Variable1
    geom_line(data = plot_NPP_Temp_L6, aes(x = xvar, y = (fitted*5)), color = "#FCAD12FF",linetype = "dashed",size =1) +  # First y variable
    geom_ribbon(data = plot_NPP_Temp_L6, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#FCAD12FF") +  # CI for Variable1
    scale_y_continuous(
      name = "",
      limits = c(-1, 1),  # Set your primary axis limits here
      sec.axis = sec_axis(~ . /10, name = ""
      )  # Adjust as necessary
    ) +  
    xlim(-5,5)+# Adjust scale as needed
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
      axis.title.x = element_blank(),  # Removes the x-axis label
      #axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
    )+
    theme(axis.text = element_text(size =16),axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
  # +
  #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
 p216
 dev.off()
 
 pdf(".\\Clim_response\\data\\Fig22_1.pdf",width = 6,height = 2.2)
 
 p221 <- ggplot() +
   geom_line(data = plot_AABI_Prep_L1, aes(x = xvar, y = fitted), color = "#35264CFF",size =1) +
   geom_ribbon(data = plot_AABI_Prep_L1, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#35264CFF") +  # CI for Variable1
   geom_line(data = plot_NPP_Prep_L1, aes(x = xvar, y = (fitted*10)), color = "#35264CFF",linetype = "dashed",size =1) +  # First y variable
   geom_ribbon(data = plot_NPP_Prep_L1, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill = "#35264CFF") +  # CI for Variable1
   scale_y_continuous(
     name = "",
     limits = c(-0.8, 1.8),  # Set your primary axis limits here
     sec.axis = sec_axis(~ . /10, name = ""
     )  # Adjust as necessary
   ) +  
   xlim(-15,15)+# Adjust scale as needed
   #geom_bar(stat = "identity") + facet_wrap(~ category, ncol = 1)+
   theme_minimal()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
     axis.title.x = element_blank(),  # Removes the x-axis label
     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
   )+
   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 
 p221
 dev.off()
 
 pdf(".\\Clim_response\\data\\Fig22_2.pdf",width = 6,height = 2.2)
 
 p222 <- ggplot() +
   geom_line(data = plot_AABI_Prep_L2, aes(x = xvar, y = fitted), color = "#403A75FF",size =1) +
   geom_ribbon(data = plot_AABI_Prep_L2, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#403A75FF") +  # CI for Variable1
   geom_line(data = plot_NPP_Prep_L2, aes(x = xvar, y = (fitted*5)), color = "#403A75FF",linetype = "dashed",size =1) +  # First y variable
   geom_ribbon(data = plot_NPP_Prep_L2, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#403A75FF") +  # CI for Variable1
   scale_y_continuous(
     name = "",
     limits = c(-0.6, 0.4),  # Set your primary axis limits here
     sec.axis = sec_axis(~ . /5, name = ""
     )  # Adjust as necessary
   ) +  
   xlim(-15,15)+# Adjust scale as needed
   #geom_bar(stat = "identity") + facet_wrap(~ category, ncol = 1)+
   theme_minimal()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
     axis.title.x = element_blank(),  # Removes the x-axis label
     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
   )+
   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 
 p222
 dev.off()
 
 
 pdf(".\\Clim_response\\data\\Fig22_3.pdf",width = 6,height = 2.2)
 
 p223 <- ggplot() +
   geom_line(data = plot_AABI_Prep_L3, aes(x = xvar, y = fitted), color = "#366DA0FF",size =1) +
   geom_ribbon(data = plot_AABI_Prep_L3, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#366DA0FF") +  # CI for Variable1
   geom_line(data = plot_NPP_Prep_L3, aes(x = xvar, y = (fitted*10)), color = "#366DA0FF",linetype = "dashed",size =1) +  # First y variable
   geom_ribbon(data = plot_NPP_Prep_L3, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill = "#366DA0FF") +  # CI for Variable1
   scale_y_continuous(
     name = "",
     limits = c(-0.8, 0.4),  # Set your primary axis limits here
     sec.axis = sec_axis(~ . /10, name = ""
     )  # Adjust as necessary
   ) +  
   xlim(-15,15)+# Adjust scale as needed
   #geom_bar(stat = "identity") + facet_wrap(~ category, ncol = 1)+
   theme_minimal()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
     axis.title.x = element_blank(),  # Removes the x-axis label
     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
   )+
   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 
 p223
 dev.off()
 
 
 pdf(".\\Clim_response\\data\\Fig22_4.pdf",width = 6,height = 2.2)
 
 p224 <- ggplot() +
   geom_line(data = plot_AABI_Prep_L4, aes(x = xvar, y = fitted), color = "#3487A6FF",size =1) +
   geom_ribbon(data = plot_AABI_Prep_L4, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#3487A6FF") +  # CI for Variable1
   geom_line(data = plot_NPP_Prep_L4, aes(x = xvar, y = (fitted*5)), color = "#3487A6FF",linetype = "dashed",size =1) +  # First y variable
   geom_ribbon(data = plot_NPP_Prep_L4, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#3487A6FF") +  # CI for Variable1
   scale_y_continuous(
     name = "",
     limits = c(-0.4, 0.4),  # Set your primary axis limits here
     sec.axis = sec_axis(~ . /5, name = ""
     )  # Adjust as necessary
   ) +  
   xlim(-15,15)+# Adjust scale as needed
   #geom_bar(stat = "identity") + facet_wrap(~ category, ncol = 1)+
   theme_minimal()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
     axis.title.x = element_blank(),  # Removes the x-axis label
     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
   )+
   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 
 p224
 dev.off()
 
 
 pdf(".\\Clim_response\\data\\Fig22_5.pdf",width = 6,height = 2.2)
 
 p225 <- ggplot() +
   geom_line(data = plot_AABI_Prep_L5, aes(x = xvar, y = fitted), color = "#35A1ABFF",size =1) +
   geom_ribbon(data = plot_AABI_Prep_L5, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#35A1ABFF") +  # CI for Variable1
   geom_line(data = plot_NPP_Prep_L5, aes(x = xvar, y = (fitted*5)), color = "#35A1ABFF",linetype = "dashed",size =1) +  # First y variable
   geom_ribbon(data = plot_NPP_Prep_L5, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#35A1ABFF") +  # CI for Variable1
   scale_y_continuous(
     name = "",
     limits = c(-0.75, 0.5),  # Set your primary axis limits here
     sec.axis = sec_axis(~ . /5, name = ""
     )  # Adjust as necessary
   ) +  
   xlim(-15,15)+# Adjust scale as needed
   #geom_bar(stat = "identity") + facet_wrap(~ category, ncol = 1)+
   theme_minimal()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
     axis.title.x = element_blank(),  # Removes the x-axis label
     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
   )+
   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 
 p225
 dev.off()
 
 
 
 pdf(".\\Clim_response\\data\\Fig22_6.pdf",width = 6,height = 2.2)
 
 p226 <- ggplot() +
   geom_line(data = plot_AABI_Prep_L6, aes(x = xvar, y = fitted), color = "#6CD3ADFF",size =1) +
   geom_ribbon(data = plot_AABI_Prep_L6, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#6CD3ADFF") +  # CI for Variable1
   geom_line(data = plot_NPP_Prep_L6, aes(x = xvar, y = (fitted*5)), color = "#6CD3ADFF",linetype = "dashed",size =1) +  # First y variable
   geom_ribbon(data = plot_NPP_Prep_L6, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#6CD3ADFF") +  # CI for Variable1
   scale_y_continuous(
     name = "",
     limits = c(-0.6, 0.45),  # Set your primary axis limits here
     sec.axis = sec_axis(~ . /5, name = ""
     )  # Adjust as necessary
   ) +  
   xlim(-15,15)+# Adjust scale as needed
   #geom_bar(stat = "identity") + facet_wrap(category ~ .)+
   theme_minimal()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
     axis.title.x = element_blank(),  # Removes the x-axis label
     #axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
   )+
   theme(axis.text = element_text(size =16),axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 
 p226
 dev.off()
  
 
 # pdf("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig21\\fig31_1.pdf",width = 6,height = 2.2)
 # 
 # p231 <- ggplot() +
 #   geom_line(data = plot_VegC_Temp_L1, aes(x = xvar, y = fitted), color = "#000004FF",size =1) +
 #   geom_ribbon(data = plot_VegC_Temp_L1, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#000004FF") +  # CI for Variable1
 #   geom_line(data = plot_GPP_TRENDY_Temp_L1, aes(x = xvar, y = (fitted)), color = "#000004FF",linetype = "dashed",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_TRENDY_Temp_L1, aes(x = xvar, ymin = (lower), ymax = (upper)), alpha = 0.2, fill = "#000004FF") +  # CI for Variable1+  # CI for Variable1
 #   geom_line(data = plot_GPP_FLUXCOM_Temp_L1, aes(x = xvar, y = (fitted*10)), color = "#000004FF",linetype = "dotted",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_FLUXCOM_Temp_L1, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill = "#000004FF") +  # CI for Variable1
 #   geom_line(data = plot_GPP_RS_Temp_L1, aes(x = xvar, y = (fitted/100)), color = "#000004FF",linetype = "longdash",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_RS_Temp_L1, aes(x = xvar, ymin = (lower/100), ymax = (upper/100)), alpha = 0.2, fill = "#000004FF") +  # CI for Variable1
 #   scale_y_continuous(
 #     name = "",
 #     limits = c(-0.8, 0.8),  # Set your primary axis limits here
 #     breaks = seq(-0.5,1,0.5)
 #       # Adjust as necessary
 #   ) +  
 #   xlim(-5,5)+# Adjust scale as needed
 #   theme_minimal()+
 #   theme(
 #     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
 #     axis.title.x = element_blank(),  # Removes the x-axis label
 #     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
 #   )+
 #   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 # # +
 # #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
 # 
 # p231
 # dev.off()
 # 
 # 
 # pdf("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig21\\fig23_2.pdf",width = 6,height = 2.2)
 # 
 # p232 <- ggplot() +
 #   geom_line(data = plot_VegC_Temp_L2, aes(x = xvar, y = fitted), color = "#3A0963FF",size =1) +
 #   geom_ribbon(data = plot_VegC_Temp_L2, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#3A0963FF") +  # CI for Variable1
 #   geom_line(data = plot_GPP_TRENDY_Temp_L2, aes(x = xvar, y = (fitted)), color = "#3A0963FF",linetype = "dashed",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_TRENDY_Temp_L2, aes(x = xvar, ymin = (lower), ymax = (upper)), alpha = 0.2, fill = "#3A0963FF") +  # CI for Variable1+  # CI for Variable1
 #   geom_line(data = plot_GPP_FLUXCOM_Temp_L2, aes(x = xvar, y = (fitted*10)), color = "#3A0963FF",linetype = "dotted",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_FLUXCOM_Temp_L2, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill = "#3A0963FF") +  # CI for Variable1
 #   geom_line(data = plot_GPP_RS_Temp_L2, aes(x = xvar, y = (fitted/100)), color = "#3A0963FF",linetype = "longdash",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_RS_Temp_L2, aes(x = xvar, ymin = (lower/100), ymax = (upper/100)), alpha = 0.2, fill = "#3A0963FF") +  # CI for Variable1
 #   scale_y_continuous(
 #     name = "",
 #     limits = c(-1.1, 1.2)  # Adjust as necessary
 #   ) +  
 #   xlim(-5,5)+# Adjust scale as needed
 #   theme_minimal()+
 #   theme(
 #     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
 #     axis.title.x = element_blank(),  # Removes the x-axis label
 #     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
 #   )+
 #   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 # 
 # # +
 # #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
 # 
 # p232
 # dev.off()
 # 
 # 
 # 
 # pdf("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig21\\fig23_3.pdf",width = 6,height = 2.2)
 # 
 # 
 # p233 <- ggplot() +
 #   geom_line(data = plot_VegC_Temp_L2, aes(x = xvar, y = fitted), color =  "#85216BFF",size =1) +
 #   geom_ribbon(data = plot_VegC_Temp_L2, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill =  "#85216BFF") +  # CI for Variable1
 #   geom_line(data = plot_GPP_TRENDY_Temp_L2, aes(x = xvar, y = (fitted)), color =  "#85216BFF",linetype = "dashed",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_TRENDY_Temp_L2, aes(x = xvar, ymin = (lower), ymax = (upper)), alpha = 0.2, fill =  "#85216BFF") +  # CI for Variable1+  # CI for Variable1
 #   geom_line(data = plot_GPP_FLUXCOM_Temp_L2, aes(x = xvar, y = (fitted*10)), color =  "#85216BFF",linetype = "dotted",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_FLUXCOM_Temp_L2, aes(x = xvar, ymin = (lower*10), ymax = (upper*10)), alpha = 0.2, fill =  "#85216BFF") +  # CI for Variable1
 #   geom_line(data = plot_GPP_RS_Temp_L2, aes(x = xvar, y = (fitted/100)), color = "#85216BFF",linetype = "longdash",size =1) +  # First y variable
 #   geom_ribbon(data = plot_GPP_RS_Temp_L2, aes(x = xvar, ymin = (lower/100), ymax = (upper/100)), alpha = 0.2, fill =  "#85216BFF") +  # CI for Variable1
 #   scale_y_continuous(
 #     name = "",
 #     limits = c(-1, 1.3)  # Adjust as necessary
 #   ) +  
 #   xlim(-5,5)+# Adjust scale as needed
 #   theme_minimal()+
 #   theme(
 #     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
 #     axis.title.x = element_blank(),  # Removes the x-axis label
 #     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
 #   )+
 #   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 # # +
 # #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
 # 
 # p233
 # dev.off()
 # 
 # 
 # pdf("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig21\\fig23_4.pdf",width = 6,height = 2.2)
 # 
 # p234 <- ggplot() +
 #   geom_line(data = plot_AABI_Temp_L4, aes(x = xvar, y = fitted), color = "#CB4149FF",size =1) +
 #   geom_ribbon(data = plot_AABI_Temp_L4, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#CB4149FF") +  # CI for Variable1
 #   geom_line(data = plot_VegC_Temp_L4, aes(x = xvar, y = (fitted*15)), color = "#CB4149FF",linetype = "dashed",size =1) +  # First y variable
 #   geom_ribbon(data = plot_VegC_Temp_L4, aes(x = xvar, ymin = (lower*15), ymax = (upper*15)), alpha = 0.2, fill = "#CB4149FF") +  # CI for Variable1
 #   scale_y_continuous(
 #     name = "",
 #     limits = c(-0.4, 1),  # Set your primary axis limits here
 #     
 #     sec.axis = sec_axis(~ . /15, name = ""
 #     )  # Adjust as necessary
 #   ) +  
 #   xlim(-5,5)+# Adjust scale as needed
 #   theme_minimal()+
 #   theme(
 #     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
 #     axis.title.x = element_blank(),  # Removes the x-axis label
 #     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
 #   )+
 #   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 # # +
 # #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
 # 
 # p234
 # dev.off()
 # 
 # 
 # pdf("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig21\\fig23_5.pdf",width = 6,height = 2.2)
 # 
 # p235 <- ggplot() +
 #   geom_line(data = plot_AABI_Temp_L5, aes(x = xvar, y = fitted), color = "#F78311FF",size =1) +
 #   geom_ribbon(data = plot_AABI_Temp_L5, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#F78311FF") +  # CI for Variable1
 #   geom_line(data = plot_VegC_Temp_L5, aes(x = xvar, y = (fitted*5)), color = "#F78311FF",linetype = "dashed",size =1) +  # First y variable
 #   geom_ribbon(data = plot_VegC_Temp_L5, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#F78311FF") +  # CI for Variable1
 #   scale_y_continuous(
 #     name = "",
 #     limits = c(-1.3, 1),  # Set your primary axis limits here
 #     sec.axis = sec_axis(~ . /5, name = ""
 #     )  # Adjust as necessary
 #   ) +  
 #   xlim(-5,5)+# Adjust scale as needed
 #   theme_minimal()+
 #   theme(
 #     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
 #     axis.title.x = element_blank(),  # Removes the x-axis label
 #     axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
 #   )+
 #   theme(axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 # # +
 # #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
 # 
 # p215
 # dev.off()
 # 
 # 
 # 
 # pdf("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig21\\fig23_6.pdf",width = 6,height = 2.2)
 # 
 # p236 <- ggplot() +
 #   geom_line(data = plot_AABI_Temp_L6, aes(x = xvar, y = fitted), color = "#FCAD12FF",size =1) +
 #   geom_ribbon(data = plot_AABI_Temp_L6, aes(x = xvar, ymin = lower, ymax = upper), alpha = 0.2, fill = "#FCAD12FF") +  # CI for Variable1
 #   geom_line(data = plot_VegC_Temp_L6, aes(x = xvar, y = (fitted*5)), color = "#FCAD12FF",linetype = "dashed",size =1) +  # First y variable
 #   geom_ribbon(data = plot_VegC_Temp_L6, aes(x = xvar, ymin = (lower*5), ymax = (upper*5)), alpha = 0.2, fill = "#FCAD12FF") +  # CI for Variable1
 #   scale_y_continuous(
 #     name = "",
 #     limits = c(-1, 1),  # Set your primary axis limits here
 #     sec.axis = sec_axis(~ . /10, name = ""
 #     )  # Adjust as necessary
 #   ) +  
 #   xlim(-5,5)+# Adjust scale as needed
 #   theme_minimal()+
 #   theme(
 #     panel.background = element_rect(fill = "white", colour = "white"),  # Sets the panel background to white
 #     axis.title.x = element_blank(),  # Removes the x-axis label
 #     #axis.text.x = element_blank()  # Removes the x-axis tick labels (numbers)
 #   )+
 #   theme(axis.text = element_text(size =16),axis.text.y = element_text(size =12),axis.title.y=element_text(size=12))
 # # +
 # #   theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
 # 
 # p236
 # dev.off()
 
 
 pdf(".\\Clim_response\\data\\Fig21.pdf",width = 5.5,height = 14)
 
 p21 = p211+ p212 + p213 + p214+ p215 + p216 + plot_layout(ncol = 1)

 p21
 
 dev.off()
  
 pdf(".\\Clim_response\\data\\Fig22.pdf",width = 5.5,height = 14)

 p22 = p221+ p222 + p223 + p224+ p225 + p226 + plot_layout(ncol = 1)
 p22
 
 dev.off()
          

  
  
  
  
  

  
  
  