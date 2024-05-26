
install.packages("devtools")

library("devtools")  
install_github("cbielow/PTXQC", build_vignettes=TRUE, dependencies=TRUE)  

library(PTXQC)  
PTXQC::createReport("C:\Users\Core i7 PC\OneDrive - Victoria University of Wellington - STAFF\Desktop\PhD\Mass Spec\combined_303_403__26_05_24\txt")  