################################################# Estimation #################################################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     May 10, 2024
#
################################################################################
#
######################### Preliminaries ########################################
setwd(paste0(ROOT,"/Set-ups_public/Support code"))

oldw <- getOption("warn")
options(warn = -1)

# set Java memory
options(java.parameters = "-Xmx28g")
library(rJava)
.jinit(classpath="myClasses.jar", parameters="-Xmx28g")
num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9 
paste("You have ", round(num_gigs_ram_available, 2), "GB memory available.", sep = "")

# Install required packages
list.of.packages <- c('midasr',                                                 # MIDAS 
                      'MLmetrics',                                              # RMSE 
                      'astsa',                                                  # Applied Statistical Time Series Analysis package
                      'R.matlab',                                               # Read matlab files
                      'naturalsort',                                            # Natural sorting for file names
                      'data.table',                                             # Shift vectors
                      'xlsx',                                                   # Write XLSX-file
                      'glmnet',                                                 # LASSO
                      'openxlsx',                                               # Excel-file utility
                      'tidyverse',                                              # Tidyverse
                      'tikzDevice',                                             # Draw Tikz
                      'glmnetUtils',                                            # EN regression
                      'randomForest',                                           # Random Forest
                      'iml',                                                    # Random Forest contributions
                      'pracma',                                                 # Ceiling function
                      'readxl',                                                 # Read Excel-files
                      'forecast',                                               # Forecasts etc.
                      'openxlsx'                                                # Alternative Excel-file utility
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(midasr)
library(MLmetrics) 
library(astsa)
library(R.matlab)
library(naturalsort)
library(data.table)
library(xlsx)
library(glmnet)
library(openxlsx)
library(tidyverse)
library(tikzDevice)                                                             
library(glmnetUtils)
library(randomForest)
library(iml)
library(pracma)
library(readxl)
library(forecast)
library(openxlsx)


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
  setwd(paste0(ROOT,"/Data_public/MIDAS-F"))
  } else {
  setwd(paste0(ROOT,"/Data_public/Regular"))                                                                     
  }

# Load data 
myFiles    <- list.files(pattern="NL .*mat")                                    # Sort in natural order
myFiles    <- naturalsort(myFiles) 

# Number of variables
NumVars     = 56

# Start, End and Periods
EvalStart   = 4                                                                 # 1992Q1
EvalEnd     = 111                                                               # 2018Q4  

# MIDAS-F.R specification
LowFreq    <- 4                                                                 # Quarterly data (4 times a year)
HighFreq   <- 12                                                                # Monthly data (12 times a year)
FreqRatio  <- HighFreq/LowFreq
saveFlag   <- 1                                                                 # Save to file
HorizonSet <- c(-1,0,1,2)                                                       # Horizon for MIDAS models (in quarters)
PeriodsHorizon = length(myFiles) * length(HorizonSet)                           # Periods * Horizons
LagOrder   <- 2                                                                 # Contemporaneous lag is already included!
# MIDAS with Almon lag specification
# num_factors<- 1                                                               # Number of factors in MIDAS, vary between [1,2,3,4,5,6]
typeFlag   <- 0                                                                 # 0: unrestricted MIDAS, 1: MIDAS with exponential Almon lags
num_param  <- 2                                                                 # Number of parameters in restricted form (exponential Almon lags)

# EN.R specification
ENtype     <- 0                                                                 # 0: lambda_min; 1: lambda_1se

# RS.R and RP.R specification
# k_set      <- 2:5                                                             # WARNING: k_set used in paper
k_set      <- 2                                                                 # WARNING: k_set for non-public data/demonstration purposes
fcst_av_nr <- 1000                                                              # Number of sumsamples

# RF specification
# omega      <- 0.6:0.9                                                           # WARNING: omgea setting in paper
omega      <- 0.9                                                               # WARNING: k_set for non-public data/demonstration purposes

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

options(warn = oldw)