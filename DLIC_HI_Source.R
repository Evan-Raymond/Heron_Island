
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
options(ggrepel.max.overlaps = Inf)

filter<-dplyr::filter
select<-dplyr::select

detach(package:dplyr)

sample_n<-

# Stupidly single digits need ([a|b]), double digits (aa|bb)

A1_417_LT_4<-list.files(pattern = ("\\(9).CSV")) %>% 
  ldply(read_csv2, skip=1) %>% 
  
  select("Y(II)", "Y(NPQ)", "Y(NO)", "No.") %>%
  mutate(Type = "A1", Type_2="H_L", Light = "400_417", AL_PAR = "417", Temp = "26", Sample = 4) %>% 
  rename(TP = No.,
         YII = "Y(II)",
         YNPQ = "Y(NPQ)",
         YNO = "Y(NO)")  %>% 
  na.omit() %>% 
  mutate(across(.cols=1:4, .fns=as.numeric))


# Bringing DLIC data in from "\WinControl\Heron Island\DLIC_CSV"
 # Mutating 1st YNO and YNPQ values to Fo/Fm and 0 respectively. May need to rethink that with respect to actual YNPQ values resulting from diel cycle.
  # Could be that 1st YNPQ is Fv/Fm - YII? Then YNO would be the remainder. Will only be possible with data where morning Fv/Fm is available 

DLIC_HI_29_01_a<- list.files(pattern = ("\\_29_01_a.csv")) %>% 
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
  select(-Sample_1, -X, -Datetime) %>% 
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
  mutate(Notes = case_when(Notes == "45min_DA" ~ "45 min DA", TRUE ~ Notes)) %>% 
  separate_wider_position(Treatment_1, c(Species = 1, Position = 1, Treatment = 1)) %>% 
  mutate(Species = case_when(Species == "O" ~ "A. aspera", Species == "I" ~ "A. muricata"),
         Position = case_when(Position == "D" ~ "Dorsal", Position == "V" ~ "Ventral"),
         Treatment = case_when(Treatment == "T" ~ "HL", Treatment == "C" ~ "ML")) %>% 
  select(-ms, -No.) %>% 
  mutate_at(c("Temp", "Start", "ETR"), .funs=as.numeric) %>%
  mutate_at(c("Position", "Treatment"), .funs=as.factor) %>% 
  mutate(Temp = case_when(Temp > 100 ~ Temp / 10, TRUE ~ Temp),
         Type_1 = case_when(Species == "A. aspera" ~ "A", Species == "A. muricata" ~ "M"),
         Sun = case_when(Start %in% c(758:1110) ~ "Morning",
                         Start %in% c(1111:1400) ~ "Midday",
                         Start %in% c(1401:1700) ~ "Afternoon"))
  

HI_DLIC_All_1 %>% 
  group_by(Sun) %>% 
  summarise(N = n())

  
str(HI_DLIC_All_1)


HI_DLIC_All_anova_FvFm<-compare_means(FvFm ~ Species,HI_DLIC_All_1, method = "kruskal.test", group.by = c("Sun", "Treatment", "Position"))


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





