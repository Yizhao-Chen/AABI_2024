NPP_T = read.csv('.\\Clim_response\\data\\vgroup_NPP_T1.csv')
VegC_T = read.csv('.\\Clim_response\\data\\vgroup_VegC_T1.csv')
AABI_T = read.csv('.\\Clim_response\\data\\vgroup_growth_T1.csv')
TRENDY_GPP_T = read.csv(".\\Clim_response\\data\\vgroup_TRENDY_GPP_T.csv")
FLUXCOM_GPP_T = read.csv(".\\Clim_response\\data\\vgroup_FLUXCOM_GPP_T.csv")
RS_GPP_T = read.csv(".\\Clim_response\\data\\vgroup_RS_GPP_T.csv")
NPP_P = read.csv('.\\Clim_response\\data\\vgroup_NPP_P1.csv')
VegC_P = read.csv('.\\Clim_response\\data\\vgroup_VegC_P1.csv')
AABI_P = read.csv('.\\Clim_response\\data\\vgroup_growth_P1.csv')
TRENDY_GPP_P = read.csv(".\\Clim_response\\data\\vgroup_TRENDY_GPP_P.csv")
FLUXCOM_GPP_P = read.csv(".\\Clim_response\\data\\vgroup_FLUXCOM_GPP_P.csv")
RS_GPP_P = read.csv(".\\Clim_response\\data\\vgroup_RS_GPP_P.csv")

TRENDY_GPP_T$WenDU = NPP_T$WenDU
FLUXCOM_GPP_T$WenDU = NPP_T$WenDU
RS_GPP_T$WenDU = NPP_T$WenDU
TRENDY_GPP_P$JiangShui = NPP_P$JiangShui
FLUXCOM_GPP_P$JiangShui = NPP_P$JiangShui
RS_GPP_P$JiangShui = NPP_P$JiangShui

#calculate derivitives for AABI responses
# d_AABI_T = AABI_T$Normalization_AABI - lag(AABI_T$Normalization_AABI)
# d_AABI_T = d_AABI_T[2:826]
# d_AABI_T_threshold = max(d_AABI_T) * 0.5  #0.001809957 216  452
# # 0.25   0.0009049786  106 521
# 
# d_AABI_T <- AABI_T %>%
#   mutate(dy = c(NA, diff(Normalization_AABI)),
#          dx = c(NA, diff(WenDU)),
#          derivative = dy / dx) %>%
#   na.omit()
# 
# 
# d_AABI_P = AABI_P$Normalization_AABI - lag(AABI_P$Normalization_AABI)
# d_AABI_P = d_AABI_P[2:826]
# d_AABI_P_threshold = max(d_AABI_P) * 0.5  #0.00264788 414 586 #0.25 0.00132394  386 645
# 
# 
# d_AABI_P <- AABI_P %>%
#   mutate(dy = c(NA, diff(Normalization_AABI)),
#          dx = c(NA, diff(JiangShui)),
#          derivative = dy / dx) %>%
#   na.omit()


TRENDY_GPP_T$d_TRENDY_GPP_T = TRENDY_GPP_T$Normalization_GPP - lag(TRENDY_GPP_T$Normalization_GPP)
FLUXCOM_GPP_T$d_FLUXCOM_GPP_T = FLUXCOM_GPP_T$Normalization_GPP - lag(FLUXCOM_GPP_T$Normalization_GPP)
RS_GPP_T$d_RS_GPP_T = RS_GPP_T$Normalization_GPP - lag(RS_GPP_T$Normalization_GPP)
AABI_T$d_AABI_T = AABI_T$Normalization_growth - lag(AABI_T$Normalization_growth)
NPP_T$d_NPP_T = NPP_T$Normalization_NPP - lag(NPP_T$Normalization_NPP)
VegC_T$d_VegC_T = VegC_T$Normalization_VegC - lag(VegC_T$Normalization_VegC)

TRENDY_GPP_P$d_TRENDY_GPP_P = TRENDY_GPP_P$Normalization_GPP - lag(TRENDY_GPP_P$Normalization_GPP)
FLUXCOM_GPP_P$d_FLUXCOM_GPP_P = FLUXCOM_GPP_P$Normalization_GPP - lag(FLUXCOM_GPP_P$Normalization_GPP)
RS_GPP_P$d_RS_GPP_P = RS_GPP_P$Normalization_GPP - lag(RS_GPP_P$Normalization_GPP)
AABI_P$d_AABI_P = AABI_P$Normalization_growth - lag(AABI_P$Normalization_growth)
NPP_P$d_NPP_P = NPP_P$Normalization_NPP - lag(NPP_P$Normalization_NPP)
VegC_P$d_VegC_P = VegC_P$Normalization_VegC - lag(VegC_P$Normalization_VegC)

p111 <- ggplot() + 
  #geom_rect(aes(xmin = 6.78, xmax = 14.2, ymin = 0, ymax = 1),
      #      fill = "#F57f17", alpha = 0.2) +
  geom_line(data = TRENDY_GPP_T, aes(x = as.numeric(WenDU), y = Normalization_GPP, color = "d",linetype = "dd"), alpha = 0.6, size = 0.8) +
  geom_line(data = FLUXCOM_GPP_T, aes(x = as.numeric(WenDU), y = Normalization_GPP, color = "e",linetype = "ee"), alpha = 0.6, size = 0.8) +
  geom_line(data = RS_GPP_T, aes(x = as.numeric(WenDU), y = Normalization_GPP, color = "f",linetype = "ff"), alpha = 0.6, size = 0.8) +
  #geom_line(data = VegC_T, aes(x = as.numeric(WenDU), y = Normalization_VegC, color = "c"), alpha = 0.9, size = 0.8) +
  geom_line(data = NPP_T, aes(x = as.numeric(WenDU), y = Normalization_NPP, color = "b"), alpha = 0.9, size = 0.8) +
  geom_line(data = AABI_T, aes(x = as.numeric(WenDU), y = Normalization_growth, color = "a"), alpha = 0.9, size = 0.8) +
  scale_x_continuous(limits = c(-0.5,27),breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(x = expression("T"["grow"] ~ "/" ~ degree * "C"), y = "Normalised index") +
  scale_colour_manual(name = "", values = c("a" = "#F57f17", "b" = "#7AD151FF", 
                                            #"c" = "#481567FF",
                                            "d" = "#B24745FF","e" = "#481567FF","f" = "#00A1D5FF"),labels = c(expression("AABI"["per_tree"]), expression("NPP"["TRENDY"]), 
                                                                                                              #expression("dVegC"["TRENDY"]),
                                                                                                              expression("GPP"["TRENDY"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"]))) +
  scale_linetype_manual(values = c(rep("longdash", 3)),guide = "none")+
  theme_bw() +
  #theme(legend.position = c(0.8,0.25),legend.text = element_text(size = 14, face = "bold"),
      #  legend.title = element_text(face = "bold"))+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 26), axis.text.x = element_text(size = 26), axis.text.y = element_text(size = 26),
        axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26)) +
  theme(panel.border = element_rect(fill = NA, color = "black", size = 2, linetype = "solid"))
#########################


p222 <- ggplot() + 
  #geom_rect(aes(xmin = 450.47, xmax = 613.1, ymin = 0, ymax = 1),
   #         fill = "#F57f17", alpha = 0.2) +
  geom_line(data = TRENDY_GPP_P, aes(x = as.numeric(JiangShui), y = Normalization_GPP, color = "d",linetype = "dd"), alpha = 0.6, size = 0.8) +
  geom_line(data = FLUXCOM_GPP_P, aes(x = as.numeric(JiangShui), y = Normalization_GPP, color = "e",linetype = "ee"), alpha = 0.6, size = 0.8) +
  geom_line(data = RS_GPP_P, aes(x = as.numeric(JiangShui), y = Normalization_GPP, color = "f",linetype = "ff"), alpha = 0.6, size = 0.8) +
  #geom_line(data = VegC_P, aes(x = as.numeric(JiangShui), y = Normalization_VegC, color = "c"), alpha = 0.9, size = 0.8) +
  geom_line(data = NPP_P, aes(x = as.numeric(JiangShui), y = Normalization_NPP, color = "b"), alpha = 0.9, size = 0.8) +
  geom_line(data = AABI_P, aes(x = as.numeric(JiangShui), y = Normalization_growth, color = "a"), alpha = 0.9, size = 0.8) +
  scale_x_continuous(limits = c(25,900),breaks = c(50,200,400,600,800))+
  scale_y_continuous(limits = c(0,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(x= expression("P"["grow"]~"/"~"mm"),y="Normalised index")+
  scale_colour_manual(name = "", values = c("a" = "#F57f17", "b" = "#7AD151FF", 
                                            #"c" = "#481567FF",
                                            "d" = "#B24745FF","e" = "#481567FF","f" = "#00A1D5FF"),labels = c(expression("AABI"["per_tree"]), expression("NPP"["TRENDY"]), 
                                                                                                              #expression("dVegC"["TRENDY"]),
                                                                                                              expression("GPP"["TRENDY"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"]))) +
  scale_linetype_manual(values = c("dd" = "longdash","ee" = "longdash", "ff" = "longdash"),guide = "none")+
  theme_bw() +
  # theme(legend.position = c(0.8,0.25),legend.text = element_text(size = 14, face = "bold"),
  #       legend.title = element_text(face = "bold"))+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =26),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))

ggsave(".\\Clim_response\\data\\Fig3a_subplot.pdf", p111, device = "pdf",width =6,height = 6)
ggsave(".\\Clim_response\\data\\Fig3b_subplot.pdf", p222, device = "pdf",width =6,height = 6)

#derivitive plots
# p111_d <- ggplot() + 
#   # geom_rect(aes(xmin = 6.78, xmax = 14.2, ymin = 0, ymax = 1),
#   #           fill = "#F57f17", alpha = 0.2) +
#   geom_line(data = TRENDY_GPP_T, aes(x = as.numeric(WenDU), y = d_TRENDY_GPP_T, color = "d",linetype = "dd"), alpha = 0.6, size = 0.8) +
#   geom_line(data = FLUXCOM_GPP_T, aes(x = as.numeric(WenDU), y = d_FLUXCOM_GPP_T, color = "e",linetype = "ee"), alpha = 0.6, size = 0.8) +
#   geom_line(data = RS_GPP_T, aes(x = as.numeric(WenDU), y = d_RS_GPP_T, color = "f",linetype = "ff"), alpha = 0.6, size = 0.8) +
#   geom_line(data = VegC_T, aes(x = as.numeric(WenDU), y = d_VegC_T, color = "c"), alpha = 0.9, size = 0.8) +
#   geom_line(data = NPP_T, aes(x = as.numeric(WenDU), y = d_NPP_T, color = "b"), alpha = 0.9, size = 0.8) +
#   geom_line(data = AABI_T, aes(x = as.numeric(WenDU), y = d_AABI_T, color = "a"), alpha = 0.9, size = 0.8) +
#   scale_x_continuous(limits = c(-0.5,26),breaks = c(0, 5, 10, 15, 20, 25)) +
#   #scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
#   labs(x = expression("T"["grow"] ~ "/" ~ degree * "C"), y = "d(Normalised index)") +
#   scale_colour_manual(name = "", values = c("a" = "#F57f17", "b" = "#7AD151FF", "c" = "#481567FF","d" = "#B24745FF","e" = "#79AF97FF","f" = "#00A1D5FF"),
#                       labels = c(expression("AABI"["per_tree"]), expression("NPP"["TRENDY"]), expression("dVegC"["TRENDY"]),expression("GPP"["TRENDY"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"]))) +
#   scale_linetype_manual(values = c(rep("longdash", 3)),guide = "none")+
#   theme_bw() +
#   theme(legend.position = "none")+
#   theme(axis.text = element_text(size = 26), axis.text.x = element_text(size = 26), axis.text.y = element_text(size = 20),
#         axis.title.x = element_text(size = 26), axis.title.y = element_text(size = 26)) +
#   theme(panel.border = element_rect(fill = NA, color = "black", size = 2, linetype = "solid"))
# #########################
# 
# 
# p222_d <- ggplot() + 
#   geom_line(data = TRENDY_GPP_P, aes(x = as.numeric(JiangShui), y = d_TRENDY_GPP_P, color = "d",linetype = "dd"), alpha = 0.6, size = 0.8) +
#   geom_line(data = FLUXCOM_GPP_P, aes(x = as.numeric(JiangShui), y = d_FLUXCOM_GPP_P, color = "e",linetype = "ee"), alpha = 0.6, size = 0.8) +
#   geom_line(data = RS_GPP_P, aes(x = as.numeric(JiangShui), y = d_RS_GPP_P, color = "f",linetype = "ff"), alpha = 0.6, size = 0.8) +
#   geom_line(data = VegC_P, aes(x = as.numeric(JiangShui), y = d_VegC_P, color = "c"), alpha = 0.9, size = 0.8) +
#   geom_line(data = NPP_P, aes(x = as.numeric(JiangShui), y = d_NPP_P, color = "b"), alpha = 0.9, size = 0.8) +
#   geom_line(data = AABI_P, aes(x = as.numeric(JiangShui), y = d_AABI_P, color = "a"), alpha = 0.9, size = 0.8) +
#   scale_x_continuous(limits = c(25,820),breaks = c(50,200,400,600,800))+
#   #scale_y_continuous(limits = c(0,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
#   labs(x= expression("P"["grow"]~"/"~"mm"),y="d(Normalised index)")+
#   scale_colour_manual(name = "", values = c("a" = "#F57f17", "b" = "#7AD151FF", "c" = "#481567FF","d" = "#B24745FF","e" = "#79AF97FF","f" = "#00A1D5FF"),labels = c(expression("AABI"["per_tree"]), expression("NPP"["TRENDY"]), expression("dVegC"["TRENDY"]),expression("GPP"["TRENDY"]),expression("GPP"["FLUXCOM"]),expression("GPP"["RS"]))) +
#   scale_linetype_manual(values = c("dd" = "longdash","ee" = "longdash", "ff" = "longdash"),guide = "none")+
#   theme_bw() +
#   theme(legend.position = "none")+
#   theme(axis.text = element_text(size =26),axis.text.x = element_text(size =26),axis.text.y = element_text(size =20),axis.title.x=element_text(size=26),axis.title.y=element_text(size=26))+
#   theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))
# 
# ggsave("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig3\\fig3a_subplot1_d.pdf", p111_d, device = "pdf",width =6.5,height = 6)
# ggsave("D:\\Live_cases_2023\\Growth_GPP_revision\\AABI_new_data_code\\Clim_response\\Fig3\\fig3b_subplot1_d.pdf", p222_d, device = "pdf",width =6.5,height = 6)
