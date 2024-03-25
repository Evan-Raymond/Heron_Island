
library(ggplot2)
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
library(lubridate)
options(ggrepel.max.overlaps = Inf)

filter<-dplyr::filter
select<-dplyr::select

detach(package:dplyr)

install.packages("lubridate")


# Bringing DLIC data in from "\WinControl\Heron Island\DLIC_CSV"
 # Mutating 1st YNO and YNPQ values to Fo/Fm and 0 respectively. May need to rethink that with respect to actual YNPQ values resulting from diel cycle.
  # Could be that 1st YNPQ is Fv/Fm - YII? Then YNO would be the remainder. Will only be possible with data where morning Fv/Fm is available 

DLIC_HI_31_01_d<- list.files(pattern = ("\\_31_01_d.csv")) %>% 
  ldply(read_csv2, skip=0, col_select = c(ms = 'Time (rel/ms)', everything())) %>% 
   rename(eF = "1:F",
         eFm = "1:Fm'",
         PAR = "1:PAR",
         Temp = "1:Temp",
         YII = "1:Y (II)",
         YNO = "1:Y (NO)",
         YNPQ = "1:Y (NPQ)",
         ETR = "1:ETR",
         Fo = "1:Fo",
         Fm = "1:Fm",
         FvFm = "1:Fv/Fm") %>% 
   mutate(YNO = case_when(YNO == "-" ~ Fo/Fm,
                          TRUE ~ as.numeric(YNO)),
          YNPQ = case_when(YNPQ < 0 ~ 0,
                           YNPQ == "-" ~ 0,
                           TRUE ~ as.numeric(YNPQ)),
          # Sample = rep(1:16, length.out = n()),
          X = case_when(Type == "SICS" ~ eFm),
          Sample_1 = rep(1:40, each = 16, length.out = n())) %>% 
  group_by(Sample_1) %>% 
    mutate(Treatment = first(X)) %>% 
    mutate(across(c(eF, eFm, YII, FvFm), .fns=as.numeric)) %>%
  ungroup() %>% 
  select(-Sample_1, -X) %>% 
  na.omit()
   # group_by(Sample_1) %>%                              


str(DLIC_HI_29_01_a)
str(DLIC_HI_29_01_b)

DLIC_HI_30_01<-DLIC_HI_30_01 %>%
  ungroup() %>% 
  select(-Sample_1)
 
DLIC_29_01<-rbind(DLIC_HI_29_01_a, DLIC_HI_29_01_b)  
DLIC_30_01<-rbind(DLIC_HI_30_01)
DLIC_31_01<-rbind(DLIC_HI_31_01_a, DLIC_HI_31_01_b, DLIC_HI_31_01_c, DLIC_HI_31_01_d)
DLIC_01_02<-rbind(DLIC_HI_01_02_a, DLIC_HI_01_02_b)
DLIC_02_02<-rbind(DLIC_HI_02_02)
DLIC_03_02<-rbind(DLIC_HI_03_02_a, DLIC_HI_03_02_b, DLIC_HI_03_02_c)
DLIC_04_02<-rbind(DLIC_HI_04_02)
DLIC_05_02<-rbind(DLIC_HI_05_02)

HI_DLIC_All<-rbind(DLIC_29_01, DLIC_30_01, DLIC_31_01, DLIC_01_02, DLIC_02_02, DLIC_03_02, DLIC_04_02, DLIC_05_02)

HI_DLIC_All<-HI_DLIC_All %>% 
  mutate(Treatment = case_when(Treatment == "IVT_1_45min_DA_1439" ~ "IVT_1_1439_45min_DA",
                               TRUE ~ Treatment)) 


HI_DLIC_All_1<-HI_DLIC_All %>%   
  separate_wider_delim(Treatment, "_", names = c("Treatment_1", "Sample", "Start", "Notes"), too_few = "align_start", too_many = "merge") %>% 
  mutate(Notes = case_when(Notes == "45min_DA" ~ "45 min DA",TRUE ~ Notes)) %>%
  replace_na(list(Notes = "-")) %>% 
  separate_wider_position(Treatment_1, c(Species = 1, Position = 1, Treatment = 1)) %>% 
  mutate(Species = case_when(Species == "O" ~ "A. aspera", Species == "I" ~ "A. muricata"),
         Position = case_when(Position == "D" ~ "Dorsal", Position == "V" ~ "Ventral"),
         Treatment = case_when(Treatment == "T" ~ "HL", Treatment == "C" ~ "ML")) %>% 
  select(-ms, -No.) %>% 
  mutate_at(c("Temp", "Start", "ETR"), .funs=as.numeric) %>%
  mutate_at(c("Position", "Treatment", "Date"), .funs=as.factor) %>% 
  mutate(Temp = case_when(Temp > 100 ~ Temp / 10, TRUE ~ Temp),
         Type_1 = case_when(Species == "A. aspera" ~ "A", Species == "A. muricata" ~ "M"),
         Sun = case_when(Start %in% c(0758:1114) ~ "Morning",
                         Start %in% c(1115:1354) ~ "Midday",
                         Start %in% c(1355:1709) ~ "Afternoon"),
         AmPm = case_when(Start %in% c(758:1230) ~ "AM",
                          Start %in% c(1231:1709) ~ "PM"),
         Date_1 = case_when(Date %in% c("2024-01-29","2024-01-30","2024-01-31") ~ "1-3",
                            Date %in% c('2024-02-01',"2024-02-02")  ~ "4-5",
                            Date %in% c("2024-02-04","2024-02-05","2024-02-03") ~ "6-8"),
         Date_2 = case_when(Date %in% c("2024-01-29","2024-01-30","2024-01-31",'2024-02-01') ~ "1-4",
                            Date %in% c("2024-02-02","2024-02-03","2024-02-04","2024-02-05") ~ "5-8")) %>% 
  group_by(Species,Position, Treatment, Sample, Start) %>% 
  mutate(SP = row_number()) %>% 
  ungroup()


   

str(HI_DLIC_All_1)

HI_DLIC_All_2<-HI_DLIC_All_1 %>% 
  group_by(Date_2) %>% 
  summarise(N=n())
HI_DLIC_All_2

HI_DLIC_All_3<-HI_DLIC_All_1 %>% 
  filter(Type != "FO")


HI_DLIC_All_y_mean_D1<-HI_DLIC_All_1 %>% 
  filter(Notes == "-") %>% 
  pivot_longer(cols = starts_with("Y"), names_to = "Parameter", values_to = "Value") %>% 
  group_by(Species, Position, Treatment,  SP, Parameter, Date_1, Sun) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n()),
            PAR = mean(PAR)) %>% 
  ungroup() %>% 
  mutate(Parameter = factor(Parameter, levels=c("YII","YNPQ", "YNO", "PAR")),
         Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon")),
         Treatment = factor(Treatment, levels = c("ML", "HL")),
         Position = factor(Position, levels = c("Ventral", "Dorsal")))
  
  # na.omit()

  
HI_DLIC_All_mean_hline_D1<- HI_DLIC_All_y_mean_D1 %>% 
  filter(Parameter == "YNO", SP == 1) 
  


HI_DLIC_All_etr_mean_D1<-HI_DLIC_All_1 %>% 
  filter(Notes == "-") %>% 
  pivot_longer(cols = ETR, names_to = "Parameter", values_to = "Value") %>% 
  group_by(Species, Position, Treatment,  SP, Parameter, Date_1, Sun) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n())) %>% 
  ungroup() %>% 
  mutate(
         Treatment = factor(Treatment, levels = c("ML", "HL")),
         Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon")),
         Position = factor(Position, levels = c("Ventral", "Dorsal"))) %>% 
  filter(SP != 1) 
  # na.omit()

coef<-1000

Plot_DLIC_D
Plot_DLIC_D1
Plot_DLIC_D2
 # _D = Date (1,2,3,4,5,6,7,8)
 # _D1 = Date_1 (1-3, 4-5, 6-8)
 # _D2 = Date_2 (1-4, 5-8)
 


# Plot_DLIC_D<-
ggplot()+
  geom_col(aes(x=SP, y=Mean, fill = Parameter),HI_DLIC_All_y_mean_D , position = "fill")+
  geom_line(aes(x=SP, y=Mean/coef),HI_DLIC_All_etr_mean_D)+
  geom_point(aes(x=SP, y=Mean/coef,fill = Parameter),HI_DLIC_All_etr_mean_D)+
 
  # geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean),HI_DLIC_All_mean_hline_D)+
  geom_errorbar(aes(x=SP, ymax=(Mean/coef+SE/coef), ymin=(Mean/coef-SE/coef)),HI_DLIC_All_etr_mean_D, colour="black", width=0.3)+
  theme_classic()+
  scale_y_continuous(expand = c(0, 0),sec.axis = sec_axis(~.*coef, name="ETR"))+
  facet_rep_grid(cols=vars(Treatment,Date,Sun), rows=vars( Species,Position), scales ="free")+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0), 
        axis.title.y.right = element_text(vjust=-42),
        legend.justification = c("left"))+
  scale_x_continuous(expand = c(0, 0))+
  coord_cartesian(ylim = c(0, 1))

# Plot_DLIC_D1<-
ggplot()+
  geom_col(aes(x=SP, y=Mean, fill = Parameter),HI_DLIC_All_y_mean_D1 , position = "fill")+
  geom_line(aes(x=SP, y=Mean/coef),HI_DLIC_All_etr_mean_D1)+
  geom_point(aes(x=SP, y=Mean/coef,fill = Parameter),HI_DLIC_All_etr_mean_D1)+
  geom_line(aes(x=SP, y=PAR/coef),HI_DLIC_All_y_mean_D1)+
  geom_point(aes(x=SP, y=PAR/coef),HI_DLIC_All_y_mean_D1, shape = 2, show.legend = TRUE)+
  # geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean),HI_DLIC_All_mean_hline_D1)+
  geom_errorbar(aes(x=SP, ymax=(Mean/coef+SE/coef), ymin=(Mean/coef-SE/coef)),HI_DLIC_All_etr_mean_D1, colour="black", width=0.3)+
  theme_classic()+
  scale_y_continuous(expand = c(0, 0),sec.axis = sec_axis(~.*coef, name="ETR"))+
  facet_rep_grid(cols=vars(Treatment,Date_1,Sun), rows=vars( Species,Position), scales ="free")+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0), 
        axis.title.y.right = element_text(vjust=-42),
        legend.justification = c("left"))+
  scale_x_continuous(expand = c(0, 0))+
  coord_cartesian(ylim = c(0, 1))

colour = "black"
, limits = c(0.25, 1)

HI_DLIC_All_Kruskal_FvFm<-compare_means(FvFm ~ Species,HI_DLIC_All_1, method = "kruskal.test", group.by = c("Sun", "Treatment", "Position"))


HI_DLIC_All_1 %>% 
  group_by("Sun", "Treatment", "Position") %>% 
  tukey_hsd(FvFm ~ Species)

HI_DLIC_Dunn_FvFm_1<-HI_DLIC_All_1 %>% 
  group_by(Treatment, Sun, Position) %>% 
  dunn_test(FvFm ~ Species, p.adjust.method = "holm")  
  filter(!p.adj.signif == "ns")

DLIC_22_23_Dunn_YNPQ_Light<-DLIC_22_23_Complete_1 %>% 
  group_by(Type_5, Temp) %>% 
  dunn_test(YNPQ~ Light, p.adjust.method = "holm")  %>% 
  filter(p.adj.signif == "ns")

DLIC_22_23_Dunn_YNPQ_Temp<-DLIC_22_23_Complete_1 %>% 
  group_by(Type_5, Light) %>% 
  dunn_test(YNPQ~ Temp, p.adjust.method = "holm", detailed = TRUE)   
filter(p.adj.signif == "ns")

# Building HI_DLIC_All_y_mean_1 to merge with FvFmFo df to compare Fv / diel effects


HI_DLIC_All_y_mean_1<-HI_DLIC_All_1 %>% 
  filter(Notes %in% c("-"), Type == "FO") %>%  
  select(Date, YII, Fo, Fm, YNO, YNPQ, Species, Position, Treatment, Sample, AmPm, Sun)

FvFmFo_HI_mean<-rbind(Fv_HI_mean_n5_1, FmFo_HI_mean_n5_1) %>% 
  select(-SE_Fv, -Notes, -Type_1, -SE_Value_Fv) %>% 
  unique() %>% 
  pivot_wider(names_from = Parameter_Fv, names_prefix = "DA_", values_from = Value_Fv)

  
HI_DLIC_FvFmFo_mean<-merge(FvFmFo_HI_mean, HI_DLIC_All_y_mean_1) 

coef_Fo<-2.070037
coef_Fm<-1.900761

# Found a common ratio by averaging the the difference in Fm and Fo when Fv/Fm -0.05 <> 0.05

HI_DLIC_FmFo_mean_compare<-HI_DLIC_FvFmFo_mean %>% 
  mutate(E_Fo =  Fo - DA_Fo*coef_Fo, E_Fm =  Fm - DA_Fm*coef_Fm, E_YII =  YII - DA_FvFm, 
         Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon"))) %>% 
  group_by(Species, Date, Position, Treatment, Sun) %>% 
  summarise(Change_in_Fo = mean(E_Fo), Change_in_Fm = mean(E_Fm), Change_in_YII = mean(E_YII), 
            Fo = mean(Fo), Fm = mean(Fm), mean_YII = mean(YII),DA_Fo = mean(DA_Fo), DA_Fm = mean(DA_Fm), DA_FvFm = mean(DA_FvFm)) %>% 
  pivot_longer(cols = starts_with("Change_in_"), names_to = "Effective_Parameter", values_to = "Value")
  mutate(Ratio_Fo = Fo/DA_Fo, Ratio_Fm = Fm/DA_Fm) %>% 
  filter(Effective_Parameter == "Change_in_YII" ) %>% 
  filter(Value >= -0.05 & Value <= 0.05) %>% 
  ungroup() %>% 
  summarise(Mean_Ratio_Fo = mean(Ratio_Fo), Mean_Ratio_Fm = mean(Ratio_Fm))

str(HI_DLIC_FmFo_mean_compare)  
  
coef_3<-2000 


ggplot()+
  geom_col(aes(x = Date, y = Value, colour = Effective_Parameter, group = Effective_Parameter, fill = Effective_Parameter),
           HI_DLIC_FmFo_mean_compare,position = "dodge")+
  geom_line(aes(x = Date, y = Value*coef_3, group = Effective_Parameter),
            HI_DLIC_Fv_mean_compare,color = "black",show.legend = TRUE)+
  theme_classic()+
  geom_hline(yintercept = 0) + 
  scale_y_continuous(expand = c(0, 0),sec.axis = sec_axis(~./coef_3, name="Fv/Fm"))+
  facet_rep_grid(cols = vars(Treatment,Sun), rows = vars( Species,Position))+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0), 
        axis.title.y.right = element_text(vjust=-42),
        legend.justification = c(0.5,0.5), legend.position = c(1.22, 0.5)) 


HI_DLIC_Fv_mean_compare<-HI_DLIC_FvFmFo_mean %>% 
  mutate(E_YII =  YII - DA_FvFm ,
         Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon"))) %>% 
  group_by(Species, Date, Position, Treatment, Sun) %>% 
  summarise(Change_in_YII = mean(E_YII)) %>% 
  pivot_longer(cols = starts_with("Change_in_"), names_to = "Effective_Parameter", values_to = "Value") 
  


ggplot()+
  # geom_col(aes(x = Date, y = Value, colour = Effective_Parameter, group = Effective_Parameter, fill = Effective_Parameter),
  #          HI_DLIC_FmFo_mean_compare,position = "dodge")+
  geom_line(aes(x = Date, y = Value*coef_3, colour = Sun),
           HI_DLIC_Fv_mean_compare,show.legend = TRUE)+
  theme_classic()+
  facet_rep_grid(cols = vars(Treatment), rows = vars(Position, Species))+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0), 
        axis.title.y.right = element_text(vjust=-42),
        legend.justification = c("left"))





str(HI_DLIC_Fv_mean_compare)


