################################################# Estimation #################################################
#
#                                       Dennis Kant and Jasper de Winter
#                                                 04/02/2023
#
###############################################################################################################
#
#
setwd(paste0(ROOT,"/Set-ups/Support code"))

# set Java memory
options(java.parameters = "-Xmx28g")
library(rJava)
.jinit(classpath="myClasses.jar", parameters="-Xmx28g")
num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9 
paste("You have ", round(num_gigs_ram_available, 2), "GB memory available.", sep = "")

# Install and load packages
# install.packages("midasr")                                                    # MIDAS 
library(midasr)
# install.packages('MLmetrics')                                                 # RMSE 
library(MLmetrics) 
# install.packages("astsa")                                                     # Applied Statistical Time Series Analysis package
library(astsa)
# install.packages("R.matlab")                                                  # Read matlab files
library(R.matlab)
# install.packages("naturalsort")                                               # Natural sorting for file names
library(naturalsort)
# install.packages("data.table")                                                # Shift vectors
library(data.table)
# install.packages("xlsx")                                                      # Write XLSX-file
library("xlsx")
# install.packages("glmnet")                                                    # LASSO 
library(glmnet)
# install.packages("openxlsx")                                                  # Openxlsx library
library(openxlsx)
# install.packages("tidyverse")                                                 # Tidyverse
library(tidyverse)
# install.packages("tikzDevice")                                                # Draw Tikz
library(tikzDevice)                                                             
# install.packages("glmnetUtils")                                               # EN regression
library(glmnetUtils)
# install.packages("randomForest")                                              # Random Forest 
library(randomForest)
# install.packages("iml")                                                       # Random Forest contributions
library(iml)
# install.packages("pracma")                                                    # Ceiling function (used in MIDAS-F)
library(pracma)

# Load functions
source('column_id_nr.r')
source('datestr2vec.r')
source('fcst_goal.r')
source('formula_fun.r')
source('m2q.r')
source('pad.r')
source('Process_data.r')                                                        # Reads and transforms data
source('quarter_diff.r')
source('realign.r')
source('RS_formula_fun.r')
source('split_sample.r')
source('str2vec_quarter_MIDASF.r')                                              # Transforms file names into date vectors
source('str2vec_quarter.r')

if (modelName == "MIDAS-F") {
  setwd(paste0(ROOT,"/Data/MIDAS-F"))
  } else {
  setwd(paste0(ROOT,"/Data/Regular"))                                                                     
  }

# Load data 
myFiles    <- list.files(pattern="NL .*mat")                                    # Sort in natural order
myFiles    <- naturalsort(myFiles) 

# MIDAS-F.R specification
LowFreq    <- 4                                                                 # Quarterly data (4 times a year)
HighFreq   <- 12                                                                # Monthly data (12 times a year)
FreqRatio  <- HighFreq/LowFreq
saveFlag   <- 1                                                                 # save to file
HorizonSet <- c(-1,0,1,2)                                                       # Horizon for MIDAS models (in quarters)
LagOrder   <- 2                                                                 # Contemporaneous lag is already included!
# MIDAS with Almon lag specification
# num_factors<- 1                                                                 # Number of factors in MIDAS, vary between [1,2,3,4,5,6]
typeFlag   <- 0                                                                 # 0: unrestricted MIDAS, 1: MIDAS with exponential Almon lags
num_param  <- 2                                                                 # number of parameters in restricted form (exponential Almon lags)

# EN.R specification
ENtype     <- 0                                                                 # 0: lambda_min; 1: lambda_1se

# RS.R and RP.R specification
fcst_av_nr <- 1000
k_set      <- 2:5

# RF specification
omega      <- 0.9                                                               # vary between [0.6, 0.7, 0.8, 0.9]

# Pre-allocate storage
if (modelName == "MIDAS-F") {
  fcst_vec         <- unique(str2vec_quarter_MIDASF(myFiles,2))
} else {
  fcst_vec         <- unique(str2vec_quarter(myFiles,2))
  }
fcst_vec_id        <- apply(fcst_vec, 1, function(x) paste(x[1],x[2]) )
FcstSave           <- matrix(NA, nrow = dim(fcst_vec)[1], ncol = (12+3))        # Count the first quarter as well and add 3 for h = -1 and h = 2 at the beginning/end of the forecast spectrum. Add three columns to store dates and true Y values
FcstSave[,1:2]     <- fcst_vec
colnames(FcstSave) <- c('Year', 'Quarter', 'Data', 'Backcast(3)', 'Backcast(2)',
                        'Backcast(1)','Nowcast(3)', 'Nowcast(2)','Nowcast(1)',
                        'Forecast 1Q(3)', 'Forecast 1Q(2)','Forecast 1Q(1)',
                        'Forecast 2Q(3)', 'Forecast 2Q(2)','Forecast 2Q(1)')

# Add reference value of low-frequency variable
Y_ref              <- readMat(myFiles[length(myFiles)])$Yc.final
Y_ref              <- Y_ref[!is.nan(Y_ref)]
FcstSave[1:111,3]  <- Y_ref[22:132]
