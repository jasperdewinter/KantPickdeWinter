################## Average and write RF forecasts###############################
#                                                                              #
#                              Jasper de Winter                                #
#                                   4/15/2023                                  #
#                                                                              #
################################################################################

## Preliminaries ###############################################################
rm(list=ls())
library(readxl)
library(openxlsx)
library(data.table)

# Define library
ROOT <- rprojroot::find_rstudio_root_file()  # !!!! Adjust main path

setwd(paste0(ROOT,"/Results/fcst/Raw RF/0.6"))
load("RF_0.6.Rdata")
FcstSave_0.6 <-FcstSave
# setwd(paste0(ROOT,"/Results/fcst/Raw RF/"))
# fwrite(FcstSave_0.6,"RW_0.6.csv")

setwd(paste0(ROOT,"/Results/fcst/Raw RF/0.7"))
load("RF_0.7.Rdata")
FcstSave_0.7 <-FcstSave
# setwd(paste0(ROOT,"/Results/fcst/Raw RF/"))
# fwrite(FcstSave_0.7,"RW_0.7.csv")

setwd(paste0(ROOT,"/Results/fcst/Raw RF/0.8"))
load("RF_0.8.Rdata")
FcstSave_0.8 <-FcstSave
# setwd(paste0(ROOT,"/Results/fcst/Raw RF/"))
# fwrite(FcstSave_0.8,"RW_0.8.csv")

setwd(paste0(ROOT,"/Results/fcst/Raw RF/0.9"))
load("RF_0.9.Rdata")
FcstSave_0.9 <-FcstSave
# setwd(paste0(ROOT,"/Results/fcst/Raw RF/"))
# fwrite(FcstSave_0.9,"RW_0.9.csv")

# Average Matrices
X <- list(FcstSave_0.6, FcstSave_0.7, FcstSave_0.8, FcstSave_0.9)
Y <- Reduce("+", X) / length(X)

# Write matrix
setwd(paste0(ROOT,"/Results/fcst/Raw RF/"))
fwrite(Y,"RW averaged.csv")