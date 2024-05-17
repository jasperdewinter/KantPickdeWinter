################ Nowcasting GDP using machine learning methods #################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     May 16, 2024
#
################################################################################
#
######################### Preliminaries ########################################
rm(list=ls())
ROOT <- rprojroot::find_rstudio_root_file()  
Sys.setenv(TZ ="UTC")

# Models         
setwd(paste0(ROOT,"/Set-ups_public/"))                                                 
models    <- c("RW", "AR", "MIDAS-F", "LASSO", "EN", "RS", "RP", "RF")          
modelName <- models[3]                                                          # Adjust to run a specific model
source(paste0(modelName, ".r"))                                                                                          

# Reproduce RMSFEs paper
setwd(paste0(ROOT,"/Set-ups_public/"))                                                 
source("RMSFE.r")

# Reproduce Diebold-Mariano tests paper
setwd(paste0(ROOT,"/Set-ups_public/"))                                                 
source("DM_TEST.r")

library(parallel)
detectCores()