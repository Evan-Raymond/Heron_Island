

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







