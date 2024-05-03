
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

detach(package:lubridate)

install.packages(hms)


RLC_Test<- list.files(pattern = ("\\All_.")) %>% 
  ldply(read_csv2, skip=1, col_select = c(ms = 'Time (rel/ms)', everything())) %>% 
  rename(eF = "1:F",
         eFm = "1:Fm'",
         PAR = "1:PAR",
         YII = "1:Y (II)",
         ETR = "1:ETR") %>% 
  filter(Type != "D")
  
  
  
  
  
  
  
  
  
  
  



















