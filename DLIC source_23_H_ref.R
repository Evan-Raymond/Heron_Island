

library(plyr)
library(tibble)
library(clipr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(MVN)
library(agricolae)
library(multcompView)
library(RColorBrewer)
library(lemon)
library(MASS)
library(dplyr)
library(stringr)
library(factoextra)
library(FactoMineR)
library(corrr)
library(ggcorrplot)
library(corrplot)
library(cluster)
library(ggrepel)
library(concaveman)
library(ggforce)
library(fpc)
library(ggtext)

options(ggrepel.max.overlaps = Inf)

ggtext::element_markdown()
filter<-dplyr::filter
select<-dplyr::select

install.packages("ggtext")

detach(package:plyr)

Test<-rbind(B1_HT, B2_HT)

# Stupidly single digits need ([a|b]), double digits (aa|bb)

B2_417_HT_34_2<-list.files(pattern = ("\\(18|19).CSV")) %>% 
  ldply(read_csv2, skip = 1) %>% 

  select("Y(II)", "Y(NPQ)", "Y(NO)", "No.") %>%
  na.omit() %>%
  mutate(Type = "B2",Type_2="H_L", Light = "400_417", AL_PAR = "417", Temp="34") %>% 
  rename(TP = No.,
         YII = "Y(II)",
         YNPQ = "Y(NPQ)",
         YNO = "Y(NO)")  %>% 
   
  mutate(across(.cols=1:4, .fns=as.numeric))



Type_2="L_L", Light = "80_147", AL_PAR = "147",
Type_2="H_H", Light = "200_698", AL_PAR = "698",
Type_2="H_L", Light = "400_417", AL_PAR = "417",

DLIC_22_23_All_Complete # Most recent DLIC_22_23 (04/10/23)

Ab_HT_adj<-rbind(Ab_147_HT_1, Ab_147_HT_2, Ab_147_HT_3, Ab_147_HT_4, Ab_417_HT_1, Ab_417_HT_2, Ab_417_HT_3, Ab_417_HT_4) 
Ab_LT_adj<-rbind(Ab_147_LT_1, Ab_147_LT_2, Ab_147_LT_3, Ab_147_LT_4, Ab_417_LT_1, Ab_417_LT_2, Ab_417_LT_3, Ab_417_LT_4)
A1_HT_adj<-rbind(A1_147_HT_1, A1_147_HT_2, A1_147_HT_3, A1_147_HT_4, A1_417_HT_1, A1_417_HT_2, A1_417_HT_3, A1_417_HT_4)
A1_LT_adj<-rbind(A1_147_LT_1, A1_147_LT_2, A1_147_LT_3, A1_147_LT_4, A1_417_LT_1, A1_417_LT_2, A1_417_LT_3, A1_417_LT_4)


C1_HT<-rbind(C1_H_417_HT_0305, C1_L_147_HT_0305, C1_L_147_HT_1005, C1_H_400_HT_1005)
Ab_HT<-rbind(Ab_H_417_HT_0305, Ab_L_147_HT_0305, Ab_L_147_HT_0805, Ab_L_147_HT_1005)
C2_HT<-rbind(C2_H_417_HT_0305, C2_L_147_HT_0305, C2_H_400_HT_1005, C2_L_147_HT_1005)
Ax_HT<-rbind(Ax_L_147_HT_0805, Ax_H_400_HT_1105, Ax_L_147_HT_1105, Ax_H_400_HT_0805)
A3_HT<-rbind(A3_H_400_HT_0805, A3_L_147_HT_0905, A3_H_400_HT_1105, A3_L_147_HT_1105)
A4_HT<-rbind(A4_L_147_HT_0905, A4_H_400_HT_0905, A4_H_400_HT_1205, A4_L_147_HT_1505)
A13_HT<-rbind(A13_H_400_HT_0905, A13_L_147_HT_0905, A13_H_400_HT_1205, A13_L_147_HT_1205)
B1_HT<-rbind(B1_H_400_HT_1005, B1_L_147_HT_1505, B1_H_400_HT_1505, B1_L_147_HT_0905)
B2_HT<-rbind(B2_H_400_HT_1005, B2_L_147_HT_1005, B2_H_400_HT_1505, B2_L_147_HT_1505)

HT_All_23<-rbind(C1_HT, Ab_HT, C2_HT, Ax_HT, A3_HT, A4_HT, A13_HT, B1_HT, B2_HT)

C2_HT_2<-rbind(C2_147_HT_30_1, C2_147_HT_30_2)
C1_HT_2<-rbind(C1_147_HT_30_1, C1_147_HT_30_2)
Ab_HT_2<-rbind(Ab_147_HT_30_1)
A13_HT_2<-rbind(A13_147_HT_30_1, A13_147_HT_30_2)
B1_HT_2<-rbind(B1_147_HT_30_1)
B2_HT_2<-rbind(B2_147_HT_30_1, B2_147_HT_30_2)
A4_HT_2<-rbind(A4_147_HT_30_1, A4_147_HT_30_2)
Ax_HT_2<-rbind(Ax_147_HT_30_1)
A3_HT_2<-rbind(A3_147_HT_30_1)

C1_HT_3<-rbind(C1_417_HT_34_1)
C2_HT_3<-rbind(C2_417_HT_34_1, C2_417_HT_34_2)
Ab_HT_3<-rbind(Ab_417_HT_34_2)
Ax_HT_3<-rbind(Ax_417_HT_34_2, Ax_417_HT_34_1)
A3_HT_3<-rbind(A3_417_HT_34_1, A3_417_HT_34_2)
A4_HT_3<-rbind(A4_417_HT_34_1)
A13_HT_3<-rbind(A13_417_HT_34_1, A13_417_HT_34_2)
B1_HT_3<-rbind(B1_417_HT_34_1)
B2_HT_3<-rbind(B2_417_HT_34_1, B2_417_HT_34_2)

DLIC_HT_2<-rbind(C2_HT_2, C1_HT_2, Ab_HT_2, A13_HT_2, B1_HT_2, B2_HT_2, A4_HT_2,Ax_HT_2, A3_HT_2) %>% 
  mutate(Year = "23_2")
DLIC_HT_3<-rbind(C1_HT_3, C2_HT_3, Ab_HT_3, Ax_HT_3, A3_HT_3, A4_HT_3, A13_HT_3, B1_HT_3, B2_HT_3) %>% 
  mutate(Year = "23_3")

Ax_LT<-rbind(Ax_L_LT_3001, Ax_L_LT_3101, Ax_H_200_LT_1702, Ax_H_200_LT_1702_2, Ax_H_400_LT_2703, Ax_H_400_LT_2903, Ax_L_291_LT_0304,
             Ax_L_291_LT_0604)
C2_LT<-rbind(C2_L_LT_3001, C2_L_LT_3101, C2_H_200_LT_1702, C2_H_400_LT_2703, C2_H_400_LT_2903, C2_L_291_LT_0304)
C1_LT<-rbind(C1_L_LT_3001, C1_L_LT_0102, C1_H_200_LT_1602, C1_H_400_LT_2703, C1_H_400_LT_2903, C1_H_400_LT_2903_2, C1_L_291_LT_0304, 
             C1_L_291_LT_0404, C1_L_291_LT_0404_2)
B2_LT<-rbind(B2_L_LT_3001, B2_L_LT_0102, B2_H_200_LT_2202, B2_H_200_LT_2202_2, B2_H_400_LT_2803, B2_H_400_LT_3003, B2_L_291_LT_0404,
             B2_L_291_LT_0704)
A13_LT<-rbind(A13_L_LT_3101, A13_L_LT_0102, A13_H_200_LT_2202, A13_H_400_LT_2803, A13_H_400_LT_3003, A13_L_291_LT_0404, A13_L_291_LT_0604)
A4_LT<-rbind(A4_L_LT_3101, A4_L_LT_0102, A4_H_200_LT_2102, A4_H_400_LT_2803, A4_H_400_LT_3003, A4_L_291_LT_0404, A4_L_291_LT_0604)
A3_LT<-rbind(A3_L_LT_3101, A3_L_LT_0202, A3_H_200_LT_2102, A3_H_400_LT_2703, A3_H_400_LT_2703_2, A3_H_400_LT_3003, A3_L_291_LT_0304,
             A3_L_291_LT_0604)
B1_LT<-rbind(B1_L_LT_3101_1, B1_L_LT_3101_2, B1_L_LT_0102, B1_H_200_LT_2202, B1_H_400_LT_2803, B1_H_400_LT_3003, B1_L_291_LT_0404, 
             B1_L_291_LT_0704)
Ab_LT<-rbind(Ab_L_LT_2702, Ab_L_LT_2702_2, Ab_H_LT_2702, Ab_H_400_LT_2703, Ab_H_400_LT_2903, Ab_L_291_LT_0304)


A1_Ab_adj_<-rbind(Ab_HT_adj, Ab_LT_adj, A1_HT_adj, A1_LT_adj)

A1_Ab_adj<-A1_Ab_adj_ %>% 
  mutate(Type_4 = case_when(Type == "Ab" ~ "A1", TRUE ~ Type) ) %>% 
  group_by(Sample, Light, Temp, TP, Type_4, Type_2, AL_PAR) %>% 
  summarise(YII_ = mean(YII), YNPQ_ = mean(YNPQ), YNO_ = mean(YNO)) %>% 
  mutate( Type = "Ab_2") %>% 
  rename(YII = YII_, YNPQ = YNPQ_, YNO = YNO_) %>%
  ungroup() %>% 
  select( -Type_4)


A1_Ab_adj_2<-rbind(A1_Ab_adj_, A1_Ab_adj)

A1_Ab_adj_3<-A1_Ab_adj_2 %>% 
  filter(Type != "Ab") %>% 
  mutate(Type_4 = case_when(Type == "Ab_2" ~ "A1", TRUE ~ Type) ) %>%
  group_by(Sample, Light, Temp, TP, Type_4, Type_2, AL_PAR) %>% 
  summarise(YII_ = mean(YII), YNPQ_ = mean(YNPQ), YNO_ = mean(YNO)) %>% 
  mutate(Type = "A1_2") %>% 
  rename(YII = YII_, YNPQ = YNPQ_, YNO = YNO_) %>%
  ungroup() %>% 
  select (-Type_4)

A1_Ab_adj_4<-rbind(A1_Ab_adj_2, A1_Ab_adj_3)
A1_Ab_adj_4<-A1_Ab_adj_4 %>% 
  mutate(Year = case_when(grepl("Ab", Type) ~ "23",
                          grepl("A1", Type) ~ "22")) %>% 
  select(-Sample)



LT_All_23<-rbind(Ax_LT, C2_LT, C1_LT, B2_LT, A13_LT, A4_LT, A3_LT, B1_LT, Ab_LT)
LT_All_23_80_400<-LT_All_23 %>% 
  filter(Light %in% c("80_147", "400_417")) %>% 
  mutate(Year = "23")

HT_All_23_80_400<-HT_All %>% 
  filter(Light %in% c("80_147", "400_417")) %>% 
  mutate(Year = "23")

DLIC_22_All<-DLIC_22_All %>% 
  mutate(Year = "22")

DLIC_22_All_NoA1<-DLIC_22_All %>% 
  filter(!Type == "A1")


DLIC_23_All_Abx<-rbind(HT_All_23_80_400, LT_All_23_80_400, A1_Ab_adj_4, DLIC_HT_2, DLIC_HT_3)
str(DLIC_23_All)

DLIC_23_All<-DLIC_23_All %>% 
  filter(TP<13) %>%
  mutate(Sample = rep(1:210, each = 12))

DLIC_22_All<-DLIC_22_All %>% 
  filter(TP<13) %>%
  mutate(Sample = rep(1:147, each = 12))


DLIC_23_All<-DLIC_23_All_Abx %>% 
  filter(!Type %in% c("A1", "Ab")) %>% 
  mutate(Type = case_when(Type == "Ab_2" ~ "Ab",
                          Type == "A1_2" ~ "A1",
                           TRUE ~ Type))

DLIC_22_23_All<-DLIC_22_23_All %>% 
  mutate(Type_4 = case_when(Type == "Ab" ~ "A1_23",
                            Type == "Ab_2" ~ "A1_23_b",
                            Type == "A1" ~ "A1_22",
                            Type == "A1_2" ~ "A1_22_b",
                            Type == "C1" & Year == "22" ~ "C1_a",
                            Type == "C1" & Year %in% c("23", "23_2", "23_3") ~ "C1_b",
                            Type == "B2" & Year == "22" ~ "B2_a",
                            Type == "B2" & Year %in% c("23", "23_2", "23_3") ~ "B2_b",
                            Type == "A4" & Year == "22" ~ "A4_a",
                            Type == "A4" & Year %in% c("23", "23_2", "23_3") ~ "A4_b",
                            TRUE ~ as.character(Type)))

DLIC_22_All<-DLIC_22_All %>% 
  filter(Type_4 != "A1_22")

DLIC_22_23_All<-rbind(DLIC_22_All, DLIC_23_All)
str(DLIC_22_23_All)

DLIC_22_23_All_Complete<-DLIC_22_23_All %>%  
  filter(Light %in% c("80_147", "400_417"),
         Temp %in% c("26", "32"))





DLIC_22_23_All_Complete$Light<-factor(DLIC_22_23_All_Complete$Light, levels =c('80_147', '400_417'))

DLIC_23_All_mean <- DLIC_23_All %>% 
  pivot_longer(cols = c(YNPQ, YII, YNO), names_to="Parameter", values_to="Value") %>% 
  group_by(TP, Parameter, Temp,Type, Light ) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n())) %>% 
  ungroup() %>% 
  mutate_at("TP", as.numeric) %>%
  mutate(Parameter = factor(Parameter, levels=c("YII","YNPQ", "YNO")),
         Light = factor(Light, levels=c("80_147","80_291", "200_698", "400_417"))) %>% 
  filter(TP < 17) 


 DLIC_23_All_mean_80_400<-DLIC_23_All_mean %>% 
  filter(Light != c("200_698", "80_291")) 
  
   


DLIC_23_All_mean_hline<-DLIC_23_All_mean %>% 
  filter(Parameter == "YNO", TP == 1)

DLIC_Ax_mean<-DLIC_23_All_mean %>% 
  filter(Type == "Ax")
DLIC_Ax_mean_hline<-DLIC_23_All_mean_hline %>% 
  filter(Type == "Ax")

DLIC_Ax_mean_26<-DLIC_Ax_mean %>% 
  filter(Temp  == "26" & Light == "80_147")

DLIC_Ax_mean_26_hline<-DLIC_Ax_mean_26 %>% 
  filter(Parameter == "YNO", TP == 1)


ggplot(DLIC_23_All_mean, aes(x=TP, y=Mean)) +
  geom_line(aes(group=Parameter,),size=1 )+
  scale_fill_manual(values=c("#BD0026","#006D2C","#2171B5"),labels=c("Y(II)","Y(NPQ)" , "Y(NO)"))+
  geom_point(aes(shape=Parameter, fill=Parameter,), size=3)+
  theme_classic()+
  ylim(0,1)+
  scale_shape_manual(values=c(21,22,24),labels=c("Y(II)","Y(NPQ)" , "Y(NO)"))+
  geom_errorbar(aes(x=TP, ymax=Mean+SE, ymin=Mean-SE), colour="black", width=0.3)+
  labs(x= "Time (min)", y="Parameter Value")+
  facet_rep_grid(cols=vars(Light, Temp), rows=vars(Type), scales ="free")+
  theme(strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0),
        legend.justification = c("left"))+
  geom_vline(xintercept=c(2,12), linetype="dashed") +
  geom_hline(aes(yintercept=Mean), DLIC_23_All_mean_80_400_hline)


ggplot(DLIC_23_All_mean, aes(x=TP, y=Mean, fill= Parameter))+
  geom_col(aes( fill = Parameter ), colour = "black", position = "fill")+
  geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean),DLIC_23_All_mean_hline )+
  theme_classic()+
  facet_rep_grid(cols=vars(Light, Temp), rows=vars(Type), scales ="free")+
  theme(strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0),
        legend.justification = c("left"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))

# Including YNO-Fo in DLIC data

DLIC_22_23_All_YNO_D_2<-DLIC_22_23_All %>% 
  select(YNO, TP, Type_4, Light, Temp, Year, Type, Sample) %>%
  filter(TP<13) %>% 
  pivot_wider(names_from = TP, values_from = YNO, names_prefix = "YNO") %>%
  select(YNO1, Type_4, Light, Temp, Sample, Year, Type) %>% 
  ungroup()

DLIC_22_23_Complete_1<-merge(DLIC_22_23_All,DLIC_22_23_All_YNO_D_2, by = c("Type_4", "Light", "Temp", "Year", "Sample")) %>%
   
  mutate(YNO_Fo = (YNO1 - YNO)) %>% 
  select(-YNO1) %>% 
  unique()

DLIC_22_23_Complete<-DLIC_22_23_Complete_1 %>% 
  filter(Light %in% c("80_147","400_417"), 
         Temp %in% c("26", "32")) %>% 
  select(-Type.y, -Type.x)


str(DLIC_22_23_All_)

DLIC_22_23_All            # Most up to date raw DLIC of all phylotypes, no YNO-d, all temps 17/10/23
DLIC_22_23_Complete       # Most up to date raw DLIC of all phylotypes, including YNO-d (26 and 32 deg only) 10/10/23
DLIC_22_23_All_Complete   # Most up to date raw DLIC of all phylotypes, no YNO-d (26 and 32 deg only) 10/10/23
DLIC_22_23_Complete_mean  # Most up to date mean DLIC of all phylotypes, no YNO-d (26 and 32 deg only) 10/10/23 
DLIC_22_23_Complete_mean_YNOd # Most up to date mean DLIC of all phylotypes, including YNO-d (26 and 32 deg only) 14/11/23

DLIC_22_23_Complete<-DLIC_22_23_Complete %>% 
  mutate(Type_5 = case_when(Type_4 == "A1_23" ~ "S. microadraticum_23",
                            Type_4 == "A1_22" ~ "S. microadraticum_22",
                            Type_4 == "A2" ~ "Symbiodinium sp. A2",
                            Type_4 == "A4_a" ~ "Symbiodinium sp. A4",
                            Type_4 == "Ax" ~ "S. natans",
                            Type_4 == "A3" ~ "S. tridacnidorum",
                            Type_4 == "A4_b" ~ "S. linuchae",
                            Type_4 == "A13" ~ "S. necroappetens",
                            Type_4 == "B1" ~ "B. aenigmatum",
                            Type_4 == "Bf" ~ "B. minutum (f)",
                            Type_4 == "Bm" ~ "B. minutum (m)",
                            Type_4 == "B2_a" ~ "B. psygmophilum (a)",
                            Type_4 == "B2_b" ~ "B. psygmophilum (b)",
                            Type_4 == "B3" ~ "B. psygmophilum (c)",
                            Type_4 == "C1_a" ~ "C. goreaui (a)",
                            Type_4 == "C1_b" ~ "C. goreaui (b)",
                            Type_4 == "C2" ~ "C. infistulum",
                            Type_4 == "D1" ~ "D. trenchii"))



DLIC_22_23_Complete_mean<-DLIC_22_23_All_Complete %>% 
  filter(Light %in% c("80_147", "400_417"), !Temp %in% c("30", "34")) %>% 
  pivot_longer(cols = c(YNPQ, YII, YNO), names_to="Parameter", values_to="Value") %>% 
  group_by(TP, Parameter, Temp,Type_5, Light) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n())) %>% 
  ungroup() %>% 
  mutate_at("TP", as.numeric) %>%
  mutate(Parameter = factor(Parameter, levels=c("YII","YNPQ", "YNO"))) %>% 
  filter(TP < 17)

DLIC_22_23_Complete_mean_hline<-DLIC_22_23_Complete_mean %>% 
  filter(Parameter == "YNO", TP == 1)
  
ggplot(DLIC_22_23_Complete_mean, aes(x=TP, y=Mean, fill= Parameter))+
  geom_col(aes( fill = Parameter ), colour = "black", position = "fill")+
  geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean),DLIC_22_23_Complete_mean_hline )+
  theme_classic()+
  facet_rep_grid(cols=vars( Light_Temp), rows=vars(Type_5), scales ="free")+
  theme(strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0),
        legend.justification = c("left"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))

# Plots of parameter means over all phylotypes 
str(DLIC_22_23_Complete_mean_Treatment)
ggplot(DLIC_22_23_Complete_mean_Treatment_1, aes(x=as.factor(TP), y=Mean_1, fill= Parameter))+
  geom_col(aes( fill = Parameter ), colour = "black", position = "fill")+
  geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean_1),DLIC_22_23_Complete_mean_Treatment_hline_1 )+
  theme_classic()+
  facet_rep_grid(cols=vars(Light_Temp))+
  labs(x= "Time Point (min)", 
       y = "Parameter Value (a.u)", element_text(size = 20)) +
  theme(strip.background = element_blank(),
        axis.text.y=element_text(size=11), axis.title.y = element_text(size=12,face = "bold"),
        legend.justification = c("left"),
        strip.text.x =element_text(size =(12), face = "bold" ),
        axis.text.x=element_text(size=11), axis.title.x = element_text(size=12,face = "bold"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_discrete(breaks = c("1","2","3", "7","12", "16"), labels = c("Fo","AL","1", "5", "10", "18")) 
scales ="free"



DLIC_22_23_All_Complete<-DLIC_22_23_All_Complete %>% 
  unite(Light_Temp, "Light", "Temp", remove = FALSE)



DLIC_22_23_All_Complete$Light_Temp<-factor(DLIC_22_23_All_Complete$Light_Temp,
                                                      levels = c("80_147_26", "80_147_32", "400_417_26","400_417_32"),
                                                      labels = c("LL  26\u00b0C",
                                                                 "LL  32\u00b0C",
                                                                 "HL  26\u00b0C",
                                                                 "HL  32\u00b0C")) 
  
str(DLIC_22_23_Complete)
DLIC_22_23_Complete$Light<-factor(DLIC_22_23_Complete$Light,levels=c("80_147","400_417" ))
DLIC_22_23_Complete$Temp<-factor(DLIC_22_23_Complete$Temp,levels=c("26","32" ))

DLIC_22_23_Complete_Grouped<-DLIC_22_23_Complete %>% 
  group_by(Type_5)


# DLIC data non-parametric so use Kruskal-Wallis and Dunn's tests instead 

DLIC_22_23_Complete_1<-DLIC_22_23_Complete %>% 
  unite(Light_Temp, "Light", "Temp", remove = FALSE)


DLIC_22_23_Dunn_YNPQ_1<-DLIC_22_23_Complete_1 %>% 
  group_by(Type_5) %>% 
  dunn_test(YNPQ~ Light_Temp, p.adjust.method = "holm") %>%  
  filter(!c(group1 == "400_417_26" & group2 == "80_147_32"),
         !c(group1 == "400_417_32" & group2 == "80_147_26"),
         p.adj.signif == "ns")

DLIC_22_23_Dunn_YNPQ_Light<-DLIC_22_23_Complete_1 %>% 
  group_by(Type_5, Temp) %>% 
  dunn_test(YNPQ~ Light, p.adjust.method = "holm")  %>% 
  filter(p.adj.signif == "ns")

DLIC_22_23_Dunn_YNPQ_Temp<-DLIC_22_23_Complete_1 %>% 
  group_by(Type_5, Light) %>% 
  dunn_test(YNPQ~ Temp, p.adjust.method = "holm", detailed = TRUE)   
  filter(p.adj.signif == "ns")



DLIC_22_23_YNPQ_Tukey<-DLIC_22_23_Complete %>% 
  group_by(Type_5) %>% 
  tukey_hsd(YNPQ~Temp+Light)  
  filter(p.adj.signif == "ns") %>% 
  mutate(Read = "YNPQ")
  
DLIC_22_23_YNPQ_Tukey<-DLIC_22_23_Complete %>% 
  group_by(Type_5, Light) %>% 
  tukey_hsd(YNPQ~Temp, p.adjust.method = "holm")  
  filter(p.adj.signif == "ns") %>% 
  mutate(Read = "YNPQ")


DLIC_22_23_All_an_YII_1<-DLIC_22_23_Complete %>% 
  group_by(Type_5) %>% 
  tukey_hsd(YII~Temp*Light) %>% 
  filter(p.adj.signif == "ns") %>% 
  mutate(Read = "YII")


DLIC_22_23_All_an_YNO_1<-DLIC_22_23_Complete %>% 
  group_by(Type_5) %>% 
  tukey_hsd(YNO~Temp*Light) %>% 
  filter(p.adj.signif == "ns") %>% 
  mutate(Read = "YNO")


DLIC_22_23_All_an_YNOD_1<-DLIC_22_23_Complete %>% 
  group_by(Type_5) %>% 
  tukey_hsd(YNO_Fo~Temp + Light) %>% 
  filter(p.adj.signif == "ns") %>% 
  mutate(Read = "YNO_D")

DLIC_22_23_All_an_YNPQ_Tukey<-DLIC_22_23_Complete %>% 
  group_by(Type_5) %>% 
  tukey_hsd(YNPQ~Temp +Light)

# Checking for normality with shapiro-wilk

DLIC_22_23_Norm_Check<-DLIC_22_23_Complete %>%
  filter(TP > 2) %>% 
  group_by(Light, Temp, TP, Type_5) %>% 
  shapiro_test(YII, YNPQ, YNO)
  
DLIC_22_23_Norm_Check_1<-DLIC_22_23_Norm_Check %>% 
  filter(p<0.05)

DLIC_22_23_Complete_Grouped<-DLIC_22_23_Complete %>% 
group_by(Temp, Light, Type_5) %>%
  summarise(count = n(),
    mean = mean(YNPQ),
    sd = sd(YNPQ)) %>% 
  mutate(Read = "YNPQ")

pairwise.t.test(DLIC_22_23_Complete$YNPQ, DLIC_22_23_Complete$Temp,
                p.adjust.method = "BH")

# Parameters against temperature

DLIC_22_23_All_an_YNPQ_T<-compare_means(YNPQ~ Temp + Light, DLIC_22_23_Complete ,method = 'anova', group.by = c( 'Type_4')) %>% 
    filter(p.signif != 'ns')

DLIC_22_23_All_an_YNPQ_T<-compare_means(YNPQ~Light + Temp, DLIC_22_23_Complete ,method = 'anova', group.by = c( 'Type_5')) 
  # filter(p.signif == 'ns')
  
DLIC_22_23_All_an_YII_T<-compare_means(YII~Temp, DLIC_22_23_Complete ,method = 't.test', group.by = c( 'Type_4', 'Light')) %>% 
    filter(p.signif == 'ns')

DLIC_22_23_All_an_YNO_T<-compare_means(YNO~Temp, DLIC_22_23_Complete ,method = 't.test', group.by = c( 'Type_4', 'Light')) %>% 
  filter(p.signif != 'ns')

DLIC_22_23_All_an_YNOD_T<-compare_means(YNO_Fo~Temp, DLIC_22_23_Complete ,method = 't.test', group.by = c( 'Type_4', 'Light')) %>% 
  filter(p.signif != 'ns')

# Parameters against Light

DLIC_22_23_All_an_YNPQ_L<-compare_means(YNPQ~Light, DLIC_22_23_Complete ,method = 't.test', group.by = c( 'Type_4', 'Temp')) %>% 
  filter(p.signif == 'ns')

DLIC_22_23_All_an_YII_L<-compare_means(YII~Light, DLIC_22_23_Complete ,method = 't.test', group.by = c( 'Type_4', 'Temp')) %>% 
  filter(p.signif != 'ns')

DLIC_22_23_All_an_YNO_L<-compare_means(YNO~Light, DLIC_22_23_Complete ,method = 't.test', group.by = c( 'Type_4', 'Temp')) %>% 
  filter(p.signif != 'ns')

DLIC_22_23_All_an_YNOD_L<-compare_means(YNO_Fo~Light, DLIC_22_23_Complete ,method = 't.test', group.by = c( 'Type_4', 'Temp')) %>% 
  filter(p.signif != 'ns')

# Above t.tests include TPs 1 and 2 which probably shouldn't be included in YNOd because of transient PSII closure

DLIC_22_23_Complete_noTP2<-DLIC_22_23_Complete %>% 
  filter(TP>2)

DLIC_22_23_All_an_YNOD_noTP2_T<-compare_means(YNO_Fo~ Temp, DLIC_22_23_Complete_noTP2 ,method = 't.test', group.by = c( 'Type_5', "Light"),
                                              p.adjust.method = "holm") %>% 
  mutate(p.adjust.sig = case_when(p.adj >= 0.05 ~ "ns", p.adj < 0.05 & p.adj >= 0.005 ~ "*", p.adj < 0.005 & p.adj >= 0.0005 ~ "**",
                                  p.adj <= 0.0005 & p.adj >= 0.00005~ "***", 
                                   p.adj <= 0.00005 ~ "****")) 
  filter(p.adjust.sig != 'ns')

DLIC_22_23_All_an_YNOD_noTP2_L<-compare_means(YNO_Fo~Light, DLIC_22_23_Complete_noTP2 ,method = 't.test', group.by = c( 'Type_5', 'Temp')) %>% 
  mutate(p.adjust.sig = case_when(p.adj >= 0.05 ~ "ns", p.adj < 0.05 & p.adj >= 0.005 ~ "*", p.adj < 0.005 & p.adj >= 0.0005 ~ "**",
                                  p.adj <= 0.0005 & p.adj >= 0.00005~ "***", 
                                  p.adj <= 0.00005 ~ "****")) 
  filter(p.adjust.sig != 'ns')

DLIC_22_23_Complete_noTP2_1<-DLIC_22_23_Complete_noTP2 %>% 
  group_by(TP, Type_5, Light, Temp) %>% 
  summarize(Mean = mean(YNO_Fo)) %>% 
  filter(Mean > 0)

DLIC_YNOd_noTP2_23<-DLIC_22_23_Complete_noTP2 %>% 
  group_by(Type_4, Light, Temp) %>% 
  summarize(YNOd = mean(YNO_Fo), SE = (sd(YNO_Fo)/2)) %>% 
  mutate(Light_2 = case_when(Light == "400_417" ~ "HL",
                             TRUE ~ "LL")) %>% 
  unite(Light_Temp, c(Light_2, Temp)) %>% 
  ungroup() %>% 
  select(-Light) %>% 
  pivot_wider(names_from = c(Light_Temp), values_from = c(YNOd,SE)) %>% 
  mutate(HL_Temp_Diff = YNOd_HL_32 - YNOd_HL_26,
         LL_Temp_Diff = YNOd_LL_32 - YNOd_LL_26, 
         LT_Light_Diff = YNOd_HL_26 - YNOd_LL_26,
         HT_Light_Diff = YNOd_HL_32 - YNOd_LL_32) 
  filter(LT_Light_Diff >0, 
         HT_Light_Diff >0,
         LL_Temp_Diff >0,
         HL_Temp_Diff>0)

# building df for comparison of actual parameter values between treatments

DLIC_22_23_Compare<-DLIC_22_23_Complete %>% 
  mutate(Light_2 = case_when(Light == "80_147" ~ "L",
                             TRUE ~ "H")) %>% 
  select(-Year, -Sample, -Light, -AL_PAR, -Type_2) %>% 
  group_by(Type_4, Light_2, Temp) %>%
  mutate(Sample_2 = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(Sample_2) %>% 
  mutate(Sample_3 = row_number()) %>% 
  ungroup() %>% 
  select(-Sample_2) %>%
  pivot_wider(names_from = c("Light_2","Temp"), values_from = c(YII, YNPQ, YNO, YNO_Fo)) %>% 
  na.omit() %>% 
  group_by(Type_4) %>% 
  summarise(across(c("YII_H_26":"YNO_Fo_L_32"), ~mean(.x))) %>% 
  mutate(YII_L_26_d = YII_H_26 - YII_L_26,
         YII_L_32_d = YII_H_32 - YII_L_32,
         YII_T_HL_d = YII_H_32 - YII_H_26,
         YII_T_LL_d = YII_L_32 - YII_L_26, 
         YNPQ_L_26_d = YNPQ_H_26 - YNPQ_L_26,
         YNPQ_L_32_d = YNPQ_H_32 - YNPQ_L_32,
         YNPQ_T_HL_d = YNPQ_H_32 - YNPQ_H_26,
         YNPQ_T_LL_d = YNPQ_L_32 - YNPQ_L_26, 
         YNO_L_26_d = YNO_H_26 - YNO_L_26,
         YNO_L_32_d = YNO_H_32 - YNO_L_32,
         YNO_T_HL_d = YNO_H_32 - YNO_H_26,
         YNO_T_LL_d = YNO_L_32 - YNO_L_26) %>% 
  select(Type_4,YII_L_26_d, YII_L_32_d, YII_T_HL_d, YII_T_LL_d, YNPQ_L_26_d, YNPQ_L_32_d, YNPQ_T_HL_d, YNPQ_T_LL_d, 
         YNO_L_26_d, YNO_L_32_d, YNO_T_HL_d, YNO_T_LL_d,)
  



# Rebuilding DLIC for PCA transformation without extra variables (YNO-D, Min/Max etc.) 

DLIC_23_PCA_Clean<-DLIC_23_Complete %>%
  select(-YNO_Fo) %>% 
  pivot_longer(cols = c(YNPQ, YII, YNO), names_to="Parameter", values_to="Value") %>% 
  group_by(TP, Parameter, Temp,Type_4, Light ) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  filter( TP<13, TP>2) %>%
  pivot_wider(names_from = c(TP), values_from = Mean) %>% 
  pivot_wider(names_from = Parameter, values_from = c("3":"12" )) %>% 
  filter(Light != "80_291", Light != "200_698") %>%
  mutate(Light = case_when(Light =="80_147" ~ "L",
                           TRUE ~ "H")) %>% 
  
  unite(Light_Temp, c( Light,Temp ), remove  = TRUE) %>% 
  pivot_wider(names_from = Light_Temp, values_from = c("3_YII":"12_YNPQ")) %>%
  remove_rownames() %>% 
  column_to_rownames(var="Type_4") 

# Rebuilding DLIC for PCA transformation with extra variables

DLIC_23_PCA_<-DLIC_23_Complete %>%
  pivot_longer(cols = c(YNPQ, YII, YNO, YNO_Fo), names_to="Parameter", values_to="Value") %>% 
  group_by(TP, Parameter, Temp,Type_4, Light ) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  filter( TP<13, TP>2) %>%
  pivot_wider(names_from = c(TP), values_from = Mean) %>% 
  rowwise() %>%
  mutate(Max = max(across(where(is.numeric))),
         Min = min(across(where(is.numeric)))) %>% 
  pivot_wider(names_from = Parameter, values_from = c("3":"12", "Max", "Min", )) %>% 
  filter(Light != "80_291", Light != "200_698") %>%
  mutate(YII_YNO_2 = Max_YII/Max_YNO, YII_YNPQ_2 = Max_YII/Max_YNPQ, YNPQ_YNO_3 = Max_YNPQ/Max_YNO, 
         Light = case_when(Light =="80_147" ~ "L",
                           TRUE ~ "H")) %>% 
  
  unite(Light_Temp, c( Light,Temp ), remove  = TRUE) %>% 
  pivot_wider(names_from = Light_Temp, values_from = c("3_YII":"YNPQ_YNO_3")) %>% 
  remove_rownames %>% 
  column_to_rownames(var="Type_4") 

# Rebuilding 23 DLIC for PCA transformation without averaging

str(DLIC_23_PCA_NoAv)

DLIC_23_PCA_NoAv<-DLIC_23_Complete_1 %>%
  ungroup() %>% 
  filter(!Temp %in% c("30","34")) %>% 
  
  select(-YNO_Fo) %>%
  unique() %>% 
  pivot_longer(cols = c(YNPQ, YII, YNO), names_to="Parameter", values_to="Value") %>%
  unique() %>% 
  filter( TP<13, TP>2) %>%
  pivot_wider(names_from = c(TP), values_from = "Value") %>%
  pivot_wider(names_from = c(Parameter), values_from = c("3":"12" )) %>%
  mutate(Light = case_when(Light =="80_147" ~ "L",
                           TRUE ~ "H")) %>% 
  unite(Light_Temp, c( Light,Temp ), remove  = TRUE) %>%
  group_by(Type_4, Year, Light_Temp) %>%
  mutate(Sample_2 = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(Sample_2) %>% 
  mutate(Sample_3 = row_number()) %>% 
  ungroup() %>% 
  select(-Sample,-Sample_2, -Year, -Type_2, -AL_PAR) %>%
  pivot_wider(names_from = Light_Temp, values_from = c("3_YNPQ":"12_YNO")) %>% 
  mutate(Sample = row_number(), 
         Sample_2 = row_number()) %>%
  select(-Sample_3) %>% 
  remove_rownames %>% 
  column_to_rownames(var="Sample") %>% 
  select( -Sample_2,-Type)

# Data frame with the sample (row) numbers to match phylotype in above PCA transformation

DLIC_Sample_Type<-data.frame(DLIC_23_PCA_NoAv$Type_4, DLIC_23_PCA_NoAv$Sample_2)
colnames(DLIC_Sample_Type)<-c("Type_4", "Sample")

# Rebuilding 22_23 DLIC for PCA transformation without averaging 

str(DLIC_22_23_PCA_NoAv)

DLIC_22_23_PCA_NoAv<-DLIC_22_23_All_Complete %>%
  filter(TP>2) %>% 
  ungroup() %>% 
  unique() %>% 
  filter(!Temp %in% c("30", "34")) %>% 
  pivot_longer(cols = c(YNPQ, YII, YNO), names_to="Parameter", values_to="Value") %>%
  unique() %>% 
  pivot_wider(names_from = c(TP), values_from = "Value") %>%
  pivot_wider(names_from = c(Parameter), values_from = c("3":"12" )) %>%
  mutate(Light = case_when(Light =="80_147" ~ "L",
                           TRUE ~ "H")) %>% 
  unite(Light_Temp, c( Light,Temp ), remove  = TRUE) %>%
  group_by(Type_4, Year, Light_Temp) %>%
  mutate(Sample_2 = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(Sample_2) %>% 
  mutate(Sample_3 = row_number()) %>% 
  ungroup() %>% 
  select(-Sample,-Sample_2, -Year, -Type_2, -AL_PAR) %>%
  pivot_wider(names_from = Light_Temp, values_from = c("3_YNPQ":"12_YNO")) %>% 
  mutate(Sample = row_number(), 
         Sample_2 = row_number()) %>%
  select(-Sample_3) %>%
  na.omit() %>% 
  remove_rownames %>% 
  column_to_rownames(var="Sample") %>% 
  select( -Sample_2,-Type)


# Rebuilding 22_23 DLIC for PCA transformation with YNO-d and without averaging 

str(DLIC_22_23_PCA_NoAv)
colnames(DLIC_22_23_PCA_YNOd_NoAv)
new_order<-c("Type_4","Light", "Temp","Sample","Type","Type_2","AL_PAR","Year","Parameter","3","4","5","6","7","8","9", "10","11","12")     
DLIC_22_23_PCA_YNOd_NoAv_2<-DLIC_22_23_PCA_YNOd_NoAv[,new_order]    # Have to re-order the columns because they get all mixed up       

DLIC_22_23_PCA_YNOd_NoAv<-DLIC_22_23_Complete %>%
  ungroup() %>% 
  unique() %>% 
  filter(!Temp %in% c("30", "34")) %>% 
  group_by(Type_4, Temp, Light, Sample) %>%
  filter(TP>2) %>% 
  pivot_longer(cols = c(YNPQ, YII, YNO, YNO_Fo), names_to="Parameter", values_to="Value") %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(TP), values_from = "Value") %>% 
  pivot_wider(names_from = c(Parameter), values_from = c("3":"12" )) %>%
  mutate(Light = case_when(Light =="80_147" ~ "L",
                           TRUE ~ "H")) %>% 
  unite(Light_Temp, c( Light,Temp ), remove  = TRUE) %>%
  group_by(Type_4, Year, Light_Temp) %>%
  mutate(Sample_2 = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(Sample_2) %>% 
  mutate(Sample_3 = row_number()) %>% 
  ungroup() %>% 
  select(-Sample,-Sample_2, -Year, -Type_2, -AL_PAR) %>%
  pivot_wider(names_from = Light_Temp, values_from = c("3_YNPQ":"12_YNO_Fo")) %>% 
  mutate(Sample = row_number(), 
         Sample_2 = row_number()) %>%
  select(-Sample_3) %>%
  na.omit() %>% DLIC_22_23_Complete
  remove_rownames %>% 
  column_to_rownames(var="Sample") %>% 
  select(-Type.x, -Type.y, -Sample_2)



# Running PCA on un-averaged dfs from above
str(DLIC_23_PCA_NoAv)
DLIC_23_PCA_NoAv_pca<-PCA(DLIC_23_PCA_NoAv,scale = TRUE)


 
cur_group_id()
ID_2 <- rep(1:4, times = 19520)
rep(1:4, each = 2, len = 4)  
rep(1:4, each = 2, times = 3)

# relocating columns into groups of 12. 
DLIC_22_23_MFA  
DLIC_22_23_MFA_1<-DLIC_22_23_All_MFA_2 %>%
   
  relocate(ends_with("_YNO_Fo_H_32")) %>%
  relocate(ends_with("_YNO_Fo_L_32")) %>%
  relocate(ends_with("_YNO_Fo_H_26")) %>%
  relocate(ends_with("_YNO_Fo_L_26")) %>%
  relocate(ends_with("_YNO_H_32")) %>%
  relocate(ends_with("_YNO_L_32")) %>%
  relocate(ends_with("_YNO_H_26")) %>%
  relocate(ends_with("_YNO_L_26")) %>%
  relocate(ends_with("_YNPQ_H_32")) %>%
  relocate(ends_with("_YNPQ_L_32")) %>%
  relocate(ends_with("_YNPQ_H_26")) %>%
  relocate(ends_with("_YNPQ_L_26")) %>%
  relocate(ends_with("_YII_H_32")) %>%
  relocate(ends_with("_YII_L_32")) %>%
  relocate(ends_with("_YII_H_26")) %>%
  relocate(ends_with("_YII_L_26")) %>% 
  relocate(starts_with("YNPQ_YNO_H_32"), .after = last_col()) %>%
  relocate(starts_with("YNPQ_YNO_L_32"), .after = last_col()) %>%
  relocate(starts_with("YNPQ_YNO_H_26"), .after = last_col()) %>%
  relocate(starts_with("YNPQ_YNO_L_26"), .after = last_col()) %>%
  relocate(starts_with("YII_YNPQ_H_32"), .after = last_col()) %>%
  relocate(starts_with("YII_YNPQ_L_32"), .after = last_col()) %>%
  relocate(starts_with("YII_YNPQ_H_26"), .after = last_col()) %>%
  relocate(starts_with("YII_YNPQ_L_26"), .after = last_col()) %>%
  relocate(starts_with("YII_YNO_H_32"), .after = last_col()) %>%
  relocate(starts_with("YII_YNO_L_32"), .after = last_col()) %>%
  relocate(starts_with("YII_YNO_H_26"), .after = last_col()) %>%
  relocate(starts_with("YII_YNO_L_26"), .after = last_col()) 

  
DLIC_22_23_MFA_2[2:103]<-as.data.frame(scale(DLIC_22_23_MFA_2[2:103]))
  
  

select(-c("3_YNO_Fo_26":"Type_4")) 
str(DLIC_22_23_MFA_2) 
  
DLIC_22_23_mfa<- MFA(DLIC_22_23_MFA_2, 
                     group = c(rep(12,16),rep(2, 6)),
                     type = c(rep("s",22)),
                      
                     name.group = c( "YII_L_26", "YII_H_26","YII_L_32", "YII_H_32", "YNPQ_L_26", "YNPQ_H_26","YNPQ_L_32", "YNPQ_H_32", 
                                     "YNO_L_26", "YNO_H_26","YNO_L_32", "YNO_H_32","YNO_Fo_L_26", "YNO_Fo_H_26", "YNO_Fo_L_32", "YNO_Fo_H_32",
                                     "YNPQ:YNO_32","YNPQ:YNO_26", "YII:YNPQ_32", "YII:YNPQ_26", "YII:YNO_32", "YII:YNO_26")) 

# Plot groups 
fviz_mfa_axes(DLIC_22_23_mfa, repel = TRUE)

DLIC_22_23_mfa$separate.analyses                   
get_mfa_var(DLIC_22_23_mfa$cos2)

print(DLIC_22_23_mfa)
DLIC_22_23_mfa_eig_val <- get_eigenvalue(DLIC_22_23_mfa)
fviz_eig(DLIC_22_23_mfa)   
group <- get_mfa_var(DLIC_22_23_mfa, "group")
group$cos2

# Plot groups of variables
fviz_mfa_var(DLIC_22_23_mfa, "group")

# Extract results for quantitative variables
quanti.var <- get_mfa_var(DLIC_22_23_mfa, "quanti.var")

quanti.var$contrib
quanti.var$coord

fviz_mfa_var(DLIC_22_23_mfa, "quanti", palette = "rainbow", 
             axes=c(1,2),
             col.var.sup = "violet", repel = TRUE, geom = c("point"),
             shape.var = 16)

habillage = "Light",

fviz_contrib(DLIC_22_23_mfa, choice = "group", axes = c(1), top = 20,
             palette = "rainbow")


fviz_mfa_quali_biplot(DLIC_22_23_mfa, axes = c(1, 2),
  geom = c("point", "text"),
  repel = repel,
  title = "Biplot of individuals and qualitative variables - MFA",
  ...
)



# Below doesn't work without categorical variables
fviz_mfa_ind(DLIC_22_23_mfa, 
             habillage = c("Temp","Light"), # color by groups 
             palette = ("rainbow"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE) 


plotellipses(DLIC_22_23_mfa, keepvar = "all", axes = c(1, 2), means=TRUE, level = 0.95, 
             magnify = 2, cex = 1, pch = 20, pch.means=15, type = "g", 
             keepnames = TRUE,  
             label="all", 
             graph.type = "ggplot")

# Using MFA build for PCA to check for resolution loss
DLIC_22_23_PCA_1<-PCA(DLIC_22_23_PCA)
DLIC_22_23_PCA_cor<-cor(DLIC_22_23_cats)
DLIC_22_23_PCA_prc<-princomp(DLIC_22_23_PCA_cor)

autoplot(DLIC_22_23_PCA_prc, data= DLIC_22_23_coord_1, colour = c("Temp", "Light"))
DLIC_plot<-autoplot(DLIC_22_23_PCA_prc)

DLIC_22_23_coord_prc <- as.data.frame(get_pca_var(DLIC_22_23_PCA_prc)$coord)  
DLIC_22_23_coord_pca <- as.data.frame(DLIC_22_23_PCA_1$var$coord[,1:2])

DLIC_22_23_coord_pca_1 <- tibble::rownames_to_column(DLIC_22_23_coord_pca, "Variable") %>% 
  mutate("Temp" = case_when(grepl("26", Variable) ~ "26",
                            grepl("32", Variable) ~ "32"),
         "Light" = case_when(grepl("L", Variable) ~ "L",
                             grepl("H", Variable) ~"H",
                             grepl("80", Variable) ~ "L",
                             grepl("400", Variable) ~"H"),
         "Parameter" = case_when(grepl("YII_L", Variable) ~ "YII",
                                 grepl("YII_H", Variable) ~ "YII",
                                 grepl("YNPQ_L", Variable) ~ "YNPQ",
                                 grepl("YNPQ_H", Variable) ~ "YNPQ",
                                 grepl("YNO_L", Variable) ~ "YNO",
                                 grepl("YNO_H", Variable) ~ "YNO",
                                 grepl("YII_80", Variable) ~ "YII",
                                 grepl("YII_400", Variable) ~ "YII",
                                 grepl("YNPQ_80", Variable) ~ "YNPQ",
                                 grepl("YNPQ_400", Variable) ~ "YNPQ",
                                 grepl("YNO_80", Variable) ~ "YNO",
                                 grepl("YNO_400", Variable) ~ "YNO",
                                 grepl("YII_YNO_2", Variable) ~ "YII:YNO",
                                 grepl("YII_YNPQ_2", Variable) ~ "YII:YNPQ",
                                 grepl("YNO_3", Variable) ~ "YNPQ:YNO", 
                                 grepl("YNO_Fo", Variable) ~ "YNO:Fo")) %>% 
  unite(Parameter_Temp,c(Parameter, Temp), remove = FALSE) %>%
  unite(Light_Temp, c( Light,Temp ),remove  = FALSE) %>% 
  unite(Type, c( Parameter,Light_Temp), remove  = FALSE) %>% 
  mutate(P_T_2 = case_when(grepl("YII_L_26", Type) ~ "YII_L_26", 
                           grepl("YII_H_26", Type) ~ "YII_H_26",
                           grepl("YII_L_32", Type) ~ "YII_L_32",
                           grepl("YII_H_32", Type) ~ "YII_H_32",
                           grepl("_YNO_L_26", Variable) ~ Type,
                           grepl("_YNO_H_26", Variable) ~ Type,
                           grepl("_YNO_L_32", Variable) ~ Type,
                           grepl("_YNO_H_32", Variable) ~ Type,
                           TRUE ~ Parameter_Temp)) %>% 
  select(Variable, Light, Temp, Dim.1, Dim.2, Light_Temp, Parameter, Type, P_T_2) %>% 
  mutate(pam_cluster = pam_DIC$clustering)
         
DLIC_22_23_coord_pca_2<-DLIC_22_23_coord_pca_1 %>%
  convert_as_factor(pam_cluster) %>% 
  mutate(Group = case_when(pam_cluster == "1" ~ "YII 26",
                           pam_cluster == "2" ~ "YII LL 32",
                           pam_cluster == "3" ~ "YII HL 32", 
                           pam_cluster == "4" ~ "YNO LL 26",
                           pam_cluster == "5" ~ "YNO HL 32",
                           pam_cluster == "6" ~ "YNO-Fo 32",
                           pam_cluster == "7" ~ "YNPQ 32",
                           pam_cluster == "8" ~ "YNO LL 32", 
                           pam_cluster == "9" ~ "YNPQ 26"))
                           pam_cluster == "10" ~ "YNO-Fo 32 2"


str(DLIC_22_23_coord_pca_2)

t1  
t2
                                     
# Plotting DLIC variables in clusters

ggplot(DLIC_22_23_coord_pca_2, aes(x=Dim.1, y=Dim.2))+
  geom_point(aes(color = Group),size = 2, alpha = 0.5 ) +
  ggtitle("Correlation between Clustered DLIC Variables")+
  
  coord_fixed(ratio = 1)+
  geom_circle(aes(x0 = 0, y0 = 0, r = 1))+
  theme_bw() + theme(aspect.ratio = 1) + theme(panel.grid = element_blank())+
  geom_hline(yintercept = 0 )+
  geom_vline(xintercept = 0)+
  geom_mark_hull(aes(fill = Group, label = Group),label.fill="green", label.fontsize = 10, con.cap = 0, concavity = 5, expand = unit(2.5, "mm")) 
  
  


# Following clustering method from youtube
# Run prcomp on numerical data set
DLIC_22_23_PRC<-prcomp(DLIC_22_23_All_MFA_2, scale = TRUE) 

# Extract eigenvalues for later
DLIC_22_23_PRC_eig_val<-get_eigenvalue(DLIC_22_23_PRC)
  
str(DLIC_22_23_PRC)
DLIC_22_23_PRC$x

# Bind PCs 1 & 2 to the original (unPCAed) df
DLIC_22_23_prc_pca<-cbind(DLIC_22_23_All_MFA_2, DLIC_22_23_PRC$x[,1:2])

# Visualise scree plot
fviz_eig(DLIC_22_23_PRC)

fviz_pca_biplot(DLIC_22_23_PRC, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")

plot(DLIC_22_23_PRC$x[,1], DLIC_22_23_PRC$x[,2])

# Create PCs as columns
DLIC_22_23_PRC_data<-data.frame(DLIC_22_23_PRC$x)

# Assign first and second PCs as plot x and y
DLIC_22_23_PRC_data$plotx<-DLIC_22_23_PRC_data[,1]
DLIC_22_23_PRC_data$ploty<-DLIC_22_23_PRC_data[,2]



# Using coordinates to compare between YNO, YNPQ and YII at 26 deg

DLIC_22_23_coord_pca_2_Check_26<-DLIC_22_23_coord_pca_2 %>% 
  filter(Parameter %in% c("YII", "YNO", "YNPQ"),
         Temp == "26") %>%
  group_by(Parameter, Light) %>% 
  mutate(Mean_1 = mean(Dim.1),
         Mean_2 = mean(Dim.2), 
         Mean_3 = mean(Mean_1 + Mean_2)) %>% 
  pivot_wider(names_from = "Parameter", values_from = c("Mean_1", "Mean_2")) %>% 
  select(Light, Mean_1_YII, Mean_1_YNO, Mean_1_YNPQ, Mean_2_YII, Mean_2_YNO, Mean_2_YNPQ)
  
  
  
  unite(P_Mean_1, "Parameter", "Mean_1", remove = FALSE) %>% 
  unite(P_Mean_2, "Parameter", "Mean_2", remove = FALSE)

# Using coordinates to compare between YNO, YNPQ and YII at 32 deg

DLIC_22_23_coord_pca_2_Check_32<-DLIC_22_23_coord_pca_2 %>% 
  filter(Parameter %in% c("YII", "YNO", "YNPQ"),
         Temp == "32") %>%
  group_by(Parameter) %>% 
  mutate(Mean_1 = mean(Dim.1),
         Mean_2 = mean(Dim.2), 
         Mean_3 = mean(Mean_1 + Mean_2))


# Checking correlation remainder between three variables
  
DLIC_22_23_All_PCA_1_Dim_1<-data.frame(DLIC_22_23_All_PCA_1_Dim$Dim.1) %>% 
  rename(Dim.1_Corr = quanti.correlation, 
         Dim.1_P = quanti.p.value) %>% 
  rownames_to_column((var = "Variable"))

DLIC_22_23_All_PCA_1_Dim_2<-data.frame(DLIC_22_23_All_PCA_1_Dim$Dim.2) %>% 
  rename(Dim.2_Corr = quanti.correlation, 
         Dim.2_P = quanti.p.value) %>% 
  rownames_to_column((var = "Variable"))

DLIC_22_23_All_PCA_1_Dims<-merge(DLIC_22_23_All_PCA_1_Dim_1, DLIC_22_23_All_PCA_1_Dim_2, all=TRUE)

DLIC_22_23_All_PCA_1_Dims_1 <- DLIC_22_23_All_PCA_1_Dims %>% 
  mutate("Temp" = case_when(grepl("26", Variable) ~ "26",
                            grepl("32", Variable) ~ "32"),
         "Light" = case_when(grepl("L", Variable) ~ "L",
                             grepl("H", Variable) ~"H",
                             grepl("80", Variable) ~ "L",
                             grepl("400", Variable) ~"H"),
         "Parameter" = case_when(grepl("YII_L", Variable) ~ "YII",
                                 grepl("YII_H", Variable) ~ "YII",
                                 grepl("YNPQ_L", Variable) ~ "YNPQ",
                                 grepl("YNPQ_H", Variable) ~ "YNPQ",
                                 grepl("YNO_L", Variable) ~ "YNO",
                                 grepl("YNO_H", Variable) ~ "YNO",
                                 grepl("YII_80", Variable) ~ "YII",
                                 grepl("YII_400", Variable) ~ "YII",
                                 grepl("YNPQ_80", Variable) ~ "YNPQ",
                                 grepl("YNPQ_400", Variable) ~ "YNPQ",
                                 grepl("YNO_80", Variable) ~ "YNO",
                                 grepl("YNO_400", Variable) ~ "YNO",
                                 grepl("YII_YNO_2", Variable) ~ "YII:YNO",
                                 grepl("YII_YNPQ_2", Variable) ~ "YII:YNPQ",
                                 grepl("YNO_3", Variable) ~ "YNPQ:YNO", 
                                 grepl("YNO_Fo", Variable) ~ "YNO:Fo")) %>% 
  unite(Parameter_Temp,c(Parameter, Temp), remove = FALSE) %>%
  unite(Light_Temp, c( Light,Temp ),remove  = FALSE) %>% 
  unite(Type, c( Parameter,Light_Temp), remove  = FALSE) %>% 
  mutate(P_T_2 = case_when(grepl("YII_L_26", Type) ~ "YII_L_26", 
                           grepl("YII_H_26", Type) ~ "YII_H_26",
                           grepl("YII_L_32", Type) ~ "YII_L_32",
                           grepl("YII_H_32", Type) ~ "YII_H_32",
                           grepl("_YNO_L_26", Variable) ~ Type,
                           grepl("_YNO_H_26", Variable) ~ Type,
                           grepl("_YNO_L_32", Variable) ~ Type,
                           grepl("_YNO_H_32", Variable) ~ Type,
                           TRUE ~ Parameter_Temp))  
  

DLIC_22_23_All_PCA_1_Dims_1_26<-DLIC_22_23_All_PCA_1_Dims_1 %>% 
  filter(Parameter %in% c("YII", "YNPQ", "YNO"),
         Temp == "26") %>% 
  group_by(Parameter) %>% 
  mutate(Sum_1 = mean(Dim.1_Corr, na.rm=TRUE),
         Sum_2 = mean(Dim.2_Corr, na.rm=TRUE), 
         Sum_3 = mean(Sum_1 + Sum_2))
DLIC_22_23_cats

DLIC_22_23_Complete_mean_YNOd_check<-DLIC_22_23_Complete_mean_YNOd %>% 
  filter(TP == 10) %>% 
  group_by(Light, Temp, Parameter) %>% 
  summarize(Mean = mean(Mean))

DLIC_22_23_Complete_mean_Treatment<-DLIC_22_23_Complete_mean %>% 
  group_by(Temp, Light, TP, Parameter) %>% 
  summarise(Mean_1 = mean(Mean), SD = sd(Mean), SE= SD/2) %>% 
  ungroup()

DLIC_23_Complete_mean_hline<-DLIC_23_Complete_mean %>% 
  filter(TP == "1", Parameter == "YNO")

DLIC_22_23_Complete_noTP2_g<-DLIC_22_23_Complete_noTP2 %>% 
  group_by(Type_5, Light, Temp)

DLIC_22_23_Complete_noTP2_lm<-lm(YNO_Fo~ Light + Temp, DLIC_22_23_Complete_noTP2_g)

anova(DLIC_22_23_Complete_noTP2_lm)
summary(DLIC_22_23_Complete_noTP2_lm)

DLIC_22_23_Complete_noTP2_g$Light<-factor(DLIC_22_23_Complete_noTP2_g$Light, c("80_147", "400_417"))


DLIC_22_Complete_Labelled<-DLIC_22_23_All_Complete %>% 
  filter(Year == "22")

DLIC_23_Complete_Labelled<-DLIC_22_23_All_Complete %>% 
  filter(Year == "23")

DLIC_23_Complete_mean<-DLIC_23_Complete_Labelled %>% 
  filter(Light %in% c("80_147", "400_417"), !Temp %in% c("30", "34")) %>% 
  pivot_longer(cols = c(YNPQ, YII, YNO), names_to="Parameter", values_to="Value") %>% 
  group_by(TP, Parameter,Type_5, Light_Temp) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n())) %>% 
  ungroup() %>% 
  mutate_at("TP", as.numeric) %>%
  mutate(Parameter = factor(Parameter, levels=c("YII","YNPQ", "YNO"))) %>% 
  filter(TP < 17)


ggplot(DLIC_23_Complete_mean, aes(x=TP, y=Mean, fill= Parameter))+
  geom_col(aes( fill = Parameter ), colour = "black", position = "fill")+
  geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean),DLIC_23_Complete_mean_hline )+
  theme_classic()+
  facet_rep_grid(cols=vars( Light_Temp), rows=vars(Type_5), scales ="free")+
  theme(strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0),
        legend.justification = c("left"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))





