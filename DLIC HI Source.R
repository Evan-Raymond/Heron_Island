
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
         Sun = case_when(Start %in% c(758:1110) ~ "Morning",
                         Start %in% c(1111:1400) ~ "Midday",
                         Start %in% c(1401:1709) ~ "Afternoon"),
         AmPm = case_when(Start %in% c(758:1230) ~ "AM",
                          Start %in% c(1231:1709) ~ "PM"),
         Date_1 = case_when(Date %in% c("2024-01-30","2024-01-31") ~ "1",
                            Date %in% c('2024-02-01',"2024-02-02","2024-02-03")  ~ "2",
                            Date %in% c("2024-02-04","2024-02-05") ~ "3")) %>% 
  group_by(Species,Position, Treatment, Sample, Start) %>% 
  mutate(SP = row_number()) %>% 
  ungroup()


   

str(HI_DLIC_All_1)

HI_DLIC_All_2<-HI_DLIC_All_1 %>% 
  group_by(Date) %>% 
  summarise(N=n())





HI_DLIC_All_y_mean<-HI_DLIC_All_1 %>% 
  filter(Notes == "-") %>% 
  pivot_longer(cols = starts_with("Y"), names_to = "Parameter", values_to = "Value") %>% 
  group_by(Species, Position, Treatment,  SP, Parameter, AmPm, Date_1) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n())) %>% 
  ungroup() %>% 
  mutate(Parameter = factor(Parameter, levels=c("YII","YNPQ", "YNO")),
         # Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon")),
         Treatment = factor(Treatment, levels = c("ML", "HL")),
         Position = factor(Position, levels = c("Ventral", "Dorsal"))) %>% 
  na.omit()

  
HI_DLIC_All_mean_hline<- HI_DLIC_All_y_mean %>% 
  filter(Parameter == "YNO", SP == 1) %>% 
  na.omit()


HI_DLIC_All_etr_mean<-HI_DLIC_All_1 %>% 
  filter(Notes == "-") %>% 
  pivot_longer(cols = ETR, names_to = "Parameter", values_to = "Value") %>% 
  group_by(Species, Position, Treatment,  SP, Parameter, AmPm, Date_1) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n())) %>% 
  ungroup() %>% 
  mutate(
         Treatment = factor(Treatment, levels = c("ML", "HL")),
         # Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon")),
         Position = factor(Position, levels = c("Ventral", "Dorsal"))) %>% 
  filter(SP != 1) %>% 
  na.omit()

coef<-700


ggplot()+
  geom_col(aes(x=SP, y=Mean, fill = Parameter ),HI_DLIC_All_y_mean , position = "fill")+
  geom_line(aes(x=SP, y=Mean/coef),HI_DLIC_All_etr_mean)+
  geom_point(aes(x=SP, y=Mean/coef,fill = Parameter),HI_DLIC_All_etr_mean)+
  # geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean),HI_DLIC_All_mean_hline)+
  # geom_errorbar(aes(x=SP, ymax=(Mean/coef+SE/coef), ymin=(Mean/coef-SE/coef)),HI_DLIC_All_etr_mean, colour="black", width=0.3)+
  theme_classic()+
  scale_y_continuous(expand = c(0, 0),sec.axis = sec_axis(~.*coef, name="ETR"))+
  facet_rep_grid(cols=vars(Date_1,Treatment,AmPm), rows=vars( Species,Position), scales ="free")+
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
  filter(Notes == "-") %>% 
  pivot_longer(cols = starts_with("Y"), names_to = "Parameter", values_to = "Value_1") %>%
  group_by(Species, Position, Treatment,Parameter,  SP, Date) %>% 
  summarise(Value=mean(Value_1, na.rm=TRUE), SE_Value=sd(Value_1)/sqrt(n())) %>%
  ungroup() %>% 
  mutate(Parameter = factor(Parameter, levels=c("YII","YNPQ", "YNO")),
         # Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon")),
         Treatment = factor(Treatment, levels = c("ML", "HL")),
         Position = factor(Position, levels = c("Ventral", "Dorsal"))) %>% 
  na.omit()

FmFo_HI_mean<- Fv_HI_All_1 %>% 
  group_by(Species, Position, Date, Treatment, AmPm) %>% 
  summarise(Fm_m = mean(Fm), FmSE = sd(Fm)/sqrt(n()),
            Fo_m = mean(Fo), FoSE = sd(Fo)/sqrt(n())) %>% 
  rename(Fm = "Fm_m", Fo = "Fo_m") %>% 
  # group_by(Species, Position, Date, Treatment)
  pivot_longer(cols = c(Fm, Fo), names_to = "Parameter", values_to = "Value") %>% 
  # filter(AmPm != "AM") %>%
  mutate(Treatment = factor(Treatment, levels=c("ML","HL")))

FmFo_HI_mean_n5_1<-FmFo_HI_mean_n5 %>% 
  pivot_longer(c(FmSE,FoSE), names_to = "SE", values_to = "SE_Value")





