
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
                         Start %in% c(1401:1709) ~ "Afternoon")) %>% 
  group_by(Species,Position, Treatment, Sample, Start) %>% 
  mutate(SP = row_number()) %>% 
  ungroup()
  # mutate(nYNPQ = (YNPQ*100/PAR), nYNO = (YNO*100/PAR), nYII = (YII*100/PAR)) 

str(HI_DLIC_All_1)

HI_DLIC_All_2<-HI_DLIC_All_1 %>% 
  group_by(Species,Position, Treatment, Sample, Start) %>% 
  summarise(N=n())





HI_DLIC_All_mean<-HI_DLIC_All_1 %>% 
  pivot_longer(cols = c(ETR, starts_with("Y")), names_to = "Parameter", values_to = "Value") %>% 
  # pivot_longer(cols = starts_with("n"), names_to = "nParameter", values_to = "n_Value") %>%
  group_by(Species, Position, Treatment, Sun, SP, Parameter) %>% 
  summarise(Mean=mean(Value, na.rm=TRUE), SD=sd(Value, na.rm=TRUE), SE=SD/sqrt(n()),
            # nMean=mean(n_Value, na.rm=TRUE), nSD=sd(n_Value, na.rm=TRUE), nSE=nSD/sqrt(n()),
            ETR_Mean = mean(ETR)) %>% 
  ungroup() %>% 
  mutate(Parameter = factor(Parameter, levels=c("YII","YNPQ", "YNO")),
         # nParameter = factor(nParameter, levels=c("nYII","nYNPQ", "nYNO")),
         Sun = factor(Sun, levels = c("Morning", "Midday", "Afternoon")),
         Treatment = factor(Treatment, levels = c("ML", "HL")),
         Position = factor(Position, levels = c("Ventral", "Dorsal")))
  

HI_DLIC_All_mean_hline<- HI_DLIC_All_mean %>% 
  filter(Parameter == "YNO", SP == 1)





ggplot(HI_DLIC_All_mean, aes(x=SP, y=Mean, fill= Parameter))+
  geom_col(aes( fill = Parameter ), colour = "black", position = "fill")+
  # geom_vline(xintercept=c(2,12), linetype="dashed")+
  geom_hline(aes(yintercept = Mean),HI_DLIC_All_mean_hline )+
  theme_classic()+
  facet_rep_grid(cols=vars(Sun, Position), rows=vars(Species, Treatment), scales ="free")+
  theme(strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0),
        legend.justification = c("left"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  coord_cartesian(ylim = c(0.25, NA))


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
