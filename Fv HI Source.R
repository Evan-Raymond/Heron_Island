

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
library(hms)
options(ggrepel.max.overlaps = Inf)

filter<-dplyr::filter
select<-dplyr::select

detach(package:dplyr)

install.packages(hms)


FvAm_HI_06_02<- list.files(pattern = ("AM_06_02.csv")) %>% 
  ldply(read_csv2, skip=1, col_select = c(ms = 'Time (rel/ms)', everything())) %>% 
  rename(PAR = "1:PAR",
    FvFm = "1:Y (II)",
    Fo = "1:F",
    Fm = "1:Fm'") %>% filter(Type != "D" | ms != "0") %>% 
  # filter(Type != "C" | ms != "1248148") %>%
  mutate(ID = ab) %>% 
  group_by(ID) %>% 
  mutate(Treatment = first(ms)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(Sample = c(cd)) %>% 
  select(-ms, -Type, -No., -PAR, -ID) %>% 
  mutate_at(c("FvFm", "Fo", "Fm"), .funs=as.numeric)
  # mutate_at(c("Position", "Treatment", "Date"), .funs=as.factor) %>% 
  

a<-rep(1:4, each = 6, length.out = 22)
b<-rep(5:7, each = 6, length.out = 18)
c_2<-rep(6:8, each =6)
ab<-c(a,b)

c<-rep(1:5, length.out = 18 )
d<-rep(1:5, length.out = 15)
d_2<-rep(1:5, length.out = 15)
cd<-c(c,d)


str(ab)
str(Fv_HI_31_01_a)
 rep(1:5, length.out = n())

Fv_31_01_a<-rbind(Fv_HI_31_01_a) # Too early
FvPm_31_01<-rbind(FvPm_HI_31_01_b, FvPm_HI_31_01_c)
Fv_01_02<-rbind(FvAm_HI_01_02, FvPm_HI_01_02)
Fv_02_02<-rbind(FvPm_HI_02_02, FvAm_HI_02_02)
Fv_03_02<-rbind(FvAm_HI_03_02, FvPm_HI_03_02)
Fv_04_02<-rbind(FvPm_HI_04_02, FvAm_HI_04_02)
Fv_05_02<-rbind(FvAm_HI_05_02, FvPm_HI_05_02)
Fv_06_02<-rbind(FvAm_HI_06_02)


Fv_HI_All<-rbind(FvPm_31_01, Fv_01_02, Fv_02_02, Fv_03_02, Fv_04_02, Fv_05_02, Fv_06_02)

str(Fv_HI_All_1)

Fv_HI_All_1<-Fv_HI_All %>% 
  separate_wider_delim(Treatment, "_", names = c("Treatment_1","Notes"), too_few = "align_start", too_many = "merge") %>% 
  replace_na(list(Notes = "-")) %>% 
  separate_wider_position(Treatment_1, c(Species = 1, Position = 1, Treatment = 1)) %>% 
  mutate(Species = case_when(Species == "O" ~ "A. aspera", Species == "I" ~ "A. muricata"),
         Position = case_when(Position == "D" ~ "Dorsal", Position == "V" ~ "Ventral"),
         Treatment = case_when(Treatment == "T" ~ "HL", Treatment == "C" ~ "ML"),
         Type_1 = case_when(Species == "A. aspera" ~ "A", Species == "A. muricata" ~ "M"),
         AmPm = case_when(Time  < as_hms("10:00:00") ~ "AM",
                          TRUE ~ "PM")) 

FmFo_HI_mean<- Fv_HI_All_1 %>% 
  group_by(Species, Position, Date, Treatment) %>% 
  summarise(Fm_m = mean(Fm), FmSE = sd(Fm)/sqrt(n()),
            Fo_m = mean(Fo), FoSE = sd(Fo)/sqrt(n())) %>% 
  rename(Fm = "Fm_m", Fo = "Fo_m") %>% 
  pivot_longer(cols = c(Fm, Fo), names_to = "Parameter", values_to = "Value") %>% 
  # filter(AmPm != "AM") %>% 
  mutate(Treatment = factor(Treatment, levels=c("ML","HL")))

FmFo_HI_mean_n5<- Fv_HI_All_1 %>% 
  group_by(Species, Position, Date, Treatment) %>% 
  summarise(Fm_m = sum(Fm/5), FmSE = sd(Fm)/sqrt(5),
            Fo_m = sum(Fo/5), FoSE = sd(Fo)/sqrt(5)) %>% 
  rename(Fm = "Fm_m", Fo = "Fo_m") %>% 
  pivot_longer(cols = c(Fm, Fo), names_to = "Parameter", values_to = "Value") %>% 
  # filter(AmPm != "AM") %>% 
  mutate(Treatment = factor(Treatment, levels=c("ML","HL")))
  
Fv_HI_mean<- Fv_HI_All_1 %>% 
  group_by(Species, Position, Date, Treatment) %>% 
  summarise(FvFm_m = mean(FvFm), FvSE = sd(FvFm)/sqrt(n())) %>%
  rename(FvFm = "FvFm_m") %>% 
  pivot_longer(cols = c(FvFm), names_to = "Parameter", values_to = "Value") %>% 
  # filter(AmPm != "AM") %>% 
  mutate(Treatment = factor(Treatment, levels=c("ML","HL")))

Fv_HI_mean_n5<- Fv_HI_All_1 %>% 
  group_by(Species, Position, Date, Treatment) %>% 
  summarise(FvFm_m = sum(FvFm)/5, FvSE = sd(FvFm)/sqrt(5)) %>%
  rename(FvFm = "FvFm_m") %>% 
  pivot_longer(cols = c(FvFm), names_to = "Parameter", values_to = "Value") %>% 
  # filter(AmPm != "AM") %>% 
  mutate(Treatment = factor(Treatment, levels=c("ML","HL")))
              
coef_1<- 1500              
  

ggplot()+
  geom_col(aes(x = Date, y = Value, fill = Parameter), FmFo_HI_mean_n5,position = "dodge") +
  # scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values=c("#2B8CBE","#5AAE61"))+
  geom_line(aes(x = Date, y=Value*coef_1),Fv_HI_mean_n5) +
  geom_point(aes(x= Date, y=Value*coef_1,),Fv_HI_mean_n5 )+
  geom_errorbar(aes(x=Date, ymax= (Value+FvSE)*coef_1, ymin=(Value-FvSE)*coef_1),Fv_HI_mean_n, width = 0.25 )+
  scale_y_continuous(name = "Fluorescence (au)", expand = c(0, 0),sec.axis = sec_axis(~./coef_1, name="Fv/Fm"))+
  # scale_x_continuous(expand = c(NA,NA))+
  facet_rep_grid(cols=vars(Treatment,Position), rows=vars( Species), scales ="fixed")+
  theme_classic()+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0), 
        # axis.title.y.left = element_text()
        axis.title.y.right = element_text(vjust=-22),
        legend.justification = c("left"))

ggplot()+
  geom_col(aes(x = Date, y = Value, fill = Parameter), FmFo_HI_mean,position = "dodge") +
  # scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values=c("#2B8CBE","#5AAE61"))+
  geom_line(aes(x = Date, y=Value*coef_1),Fv_HI_mean) +
  geom_point(aes(x= Date, y=Value*coef_1,),Fv_HI_mean )+
  geom_errorbar(aes(x=Date, ymax= (Value+FvSE)*coef_1, ymin=(Value-FvSE)*coef_1),Fv_HI_mean, width = 0.25 )+
  scale_y_continuous(name = "Fluorescence (au)", expand = c(0, 0),sec.axis = sec_axis(~./coef_1, name="Fv/Fm"))+
  # scale_x_continuous(expand = c(NA,NA))+
  facet_rep_grid(cols=vars(Treatment,Position), rows=vars( Species), scales ="fixed")+
  theme_classic()+
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y.right= element_text(angle=0), 
        # axis.title.y.left = element_text()
        axis.title.y.right = element_text(vjust=-22),
        legend.justification = c("left"))


display.brewer.pal(n = 8, name = 'PRGn') 
brewer.pal(n = 8, name = "PRGn")


fill = Parameter














