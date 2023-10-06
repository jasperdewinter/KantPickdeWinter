###################### MIDASSO Nowcasting ############################
#
#                         Dennis Kant
#
#                         20/05/2019
#
######################################################################

######################### Preliminaries ##############################
rm(list=ls())                                                                                      # Clear working environment
# Set Working Directory
setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDASSO")
dir <- getwd()

# Install and load packages
# install.packages('midasr')                                                                       # MIDAS
library(midasr)
# install.packages("astsa")                                                                        # Applied Statistical Time Series Analysis package
library(astsa)
# install.packages('MLmetrics')                                                                    # RMSE package
library(MLmetrics) 
# install.packages("R.matlab")                                                                     # Read matlab files
library(R.matlab)
# install.packages("naturalsort")                                                                  # Natural sorting for file names
library(naturalsort)
# install.packages("data.table")                                                                   # Shift vectors
library(data.table)
# install.packages("xlsx")                                                                         # Write XLSX-file
library("xlsx")
# install.packages("glmnet")                                                                       # LASSO package
library(glmnet)

# Load packages

# Load data 
myFiles   <- list.files(pattern="NL .*mat")
myFiles   <- naturalsort(myFiles)                                                                  # Sort in natural order

# Specify frequency parameters for MIDAS regression
LowFreq    <- 4                                                                                    # Quarterly data (4 times a year)
HighFreq   <- 12                                                                                   # Monthly data (12 times a year)
FreqRatio  <- HighFreq/LowFreq
LagOrder   <- 2                                                                                     # Contemporaneous lag is already included!
HorizonSet <- c(-1,0,1,2)                                                                          # Horizon for MIDAS models (in quarters)
saveFlag   <- 1     
num_factors<- 4

# Pre-allocate storage
fcst_vec        <- unique(str2vec_quarter(myFiles,2))
fcst_vec_id     <- apply(fcst_vec, 1, function(x) paste(x[1],x[2]) )
FcstSave        <- matrix(NA, nrow = dim(fcst_vec)[1], ncol = (12+3))                              # Count the first quarter as well and add 3 for h = -1 and h = 2 at the beginning/end of the forecast spectrum. Add three columns to store dates and true Y values
FcstSave[,1:2]  <- fcst_vec
colnames(FcstSave) <- c('Year', 'Quarter', 'Data', 'Backcast(3)', 'Backcast(2)',
                        'Backcast(1)','Nowcast(3)', 'Nowcast(2)','Nowcast(1)',
                        'Forecast 1Q(3)', 'Forecast 1Q(2)','Forecast 1Q(1)',
                        'Forecast 2Q(3)', 'Forecast 2Q(2)','Forecast 2Q(1)')

# Add reference value of low-frequency variable
Y_ref             <- readMat(myFiles[length(myFiles)])$Yc.final
Y_ref_date        <- readMat(myFiles[length(myFiles)])$DateX.final
Y_ref             <- Y_ref[!is.nan(Y_ref)]
FcstSave[1:111,3] <- Y_ref[22:132]
FcstSave_RMSE     <- FcstSave

#############################################################################
start_time <- Sys.time()

# for (i in 1:length(myFiles)) {
#   # Load relevant data for iteration
#   data             <- readMat(myFiles[i])
#   Y_q              <- data$yf[!is.nan(data$yf)]
#   F_m              <- data$SmoothedFactors
#   F_m              <- F_m[,1:num_factors]                                                                    # Number of factors included!
#   Y_date           <- data$Date.f
#   F_date           <- data$DateD.adj
#   
#   for (h in 1:length(HorizonSet)) {
#     # Iterate over different forecast horizons
#     Horizon <- HorizonSet[h]
#     print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
#     Y_q_shifted      <- shift(Y_q, n = Horizon, type = "shift")      
#     
#     glmnet()
#     
#     
#   }
  
}