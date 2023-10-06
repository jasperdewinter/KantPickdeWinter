###################### MIDAS-F Nowcasting ############################
#
#                         Dennis Kant
#
#                         15/05/2019
#
######################################################################

######################### Preliminaries ##############################
rm(list=ls())                                                               # Clear working environment
# Set Working Directory
setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDAS")
dir <- getwd()

# Install and load packages
# install.packages('midasr')                                                # MIDAS
library(midasr)
# install.packages("astsa")                                                 # Applied Statistical Time Series Analysis package
library(astsa)
# install.packages('MLmetrics')                                             # RMSE package
library(MLmetrics)
# install.packages("R.matlab")                                              # Read matlab files
library(R.matlab)
# install.packages("naturalsort")                                           # Natural sorting for file names
library(naturalsort)
# install.packages("data.table")                                            # Shift vectors
library(data.table)
# install.packages("xlsx")                                                  # Write XLSX-file
library("xlsx")

# Load functions
# source('find_quarter_month.r')                                              # Find quarter 
# source('read_Y.r')                                                          # Provide current available Y
# source('quarter_diff.r')                                                    # Difference in quarters
source('str2vec_quarter.r')                                                 # Transforms file names into date vectors
# source('m2q.r')                                                             # Provides quarter corresponding to month
# source('column_id_nr.r')
# source('datestr2vec.r')

# Load data 
myFiles   <- list.files(pattern="NL .*mat")
myFiles   <- naturalsort(myFiles)                                           # Sort in natural order

# Specify frequency parameters for MIDAS regression
LowFreq    <- 4                                                             # Quarterly data (4 times a year)
HighFreq   <- 12                                                            # Monthly data (12 times a year)
FreqRatio  <- HighFreq/LowFreq
LagOrder   <- 2                                                             # Contemporaneous lag is already included!
HorizonSet <- c(-1,0,1,2)                                                   # Horizon for MIDAS models (in quarters)
saveFlag   <- 1     

# Pre-allocate storage
fcst_vec        <- unique(str2vec_quarter(myFiles,2))
fcst_vec_id     <- apply(fcst_vec, 1, function(x) paste(x[1],x[2]) )
FcstSave        <- matrix(NA, nrow = dim(fcst_vec)[1], ncol = (12+3))       # Count the first quarter as well and add 3 for h = -1 and h = 2 at the beginning/end of the forecast spectrum. Add three columns to store dates and true Y values
FcstSave[,1:2]  <- fcst_vec
FcstSave_RMSE   <- matrix(NA, dim(FcstSave)[1], dim(FcstSave)[2])