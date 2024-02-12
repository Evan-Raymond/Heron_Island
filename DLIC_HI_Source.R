
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

DLIC_HI_30_01 <- list.files(pattern = ("\\_30_01.csv")) %>% 
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
          Sample = rep(1:16, times = 28),
          X = case_when(Sample == 1 ~ eFm),
          Sample_1 = rep(1:28, each = 16)) %>% 
  group_by(Sample_1) %>% 
    mutate(Treatment = first(X)) %>% 
    mutate(across(c(eF, eFm, YII, FvFm), .fns=as.numeric)) %>%
  select(-Sample, -Sample_1, -X) %>% 
  na.omit()
   # group_by(Sample_1) %>%                              


str(DLIC_HI_29_01_b)


 
DLIC_29_01<-rbind(DLIC_HI_29_01_a, DLIC_HI_29_01_b)  
DLIC_30_01<-rbind(DLIC_HI_30_01)










