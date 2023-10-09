############################ Nowcasting GDP using machine learning methods #####################################
#
#                               Dennis Kant, Andreas Pick and Jasper de Winter
#                                           09/10/2023
#
###############################################################################################################

rm(list=ls())
ROOT <- rprojroot::find_rstudio_root_file()  # !!!! Adjust main path
Sys.setenv(TZ ="UTC")

# Models         
setwd(paste0(ROOT,"/Set-ups/"))                                                 # Adjust Estimation.r to change settings
models    <- c("RW", "AR", "MIDAS-F", "LASSO", "EN", "RS", "RP", "RF")          
modelName <- models[4]                                                          # Adjust to run a specific model
source(paste0(modelName, ".r"))                                                                                          

# Calculate RMSFEs
setwd(paste0(ROOT,"/Set-ups/"))                                                 
source("RMSFE.r")

# Diebold-Mariano tests
setwd(paste0(ROOT,"/Set-ups/"))                                                 
source("DM_TEST.r")