
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

detach(package:plyr)



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

DLIC_HI_29_01_a<-DLIC_HI_29_01_a %>% 
  mutate(
    Treatment = case_when(Time..rel.ms. < 1613647 ~ "ODT_2_1023",
                               Time..rel.ms. > 1613646 ~ 	"ODT_3_1053")) %>% 
  










