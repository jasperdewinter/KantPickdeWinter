###################### MIDAS-F Nowcasting ############################
#
#                         Dennis Kant
#
#                         24/06/2019
#
######################################################################

######################### Preliminaries ##############################
rm(list=ls())                                                                                      # Clear working environment
# Set Working Directory
setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDAS")
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
# install.packages("pracma")
library(pracma)

# Load functions
source('formula_fun.r')
source('split_sample_skip.r')
source('quarter_diff.r')                                                                           # Difference in quarters
source('str2vec_quarter_MIDASF.r')                                                                        # Transforms file names into date vectors
source('m2q.r')                                                                                    # Provides quarter corresponding to month
source('column_id_nr.r')
source('datestr2vec.r')
source('fcst_goal.r')
source('readConfig.r')

# Load data 
setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//Factor extraction new methods//Skip-sampled PCA")
myFiles   <- list.files(pattern="NL .*mat")
myFiles   <- naturalsort(myFiles)                                                                  # Sort in natural order

# Specify frequency parameters for MIDAS regression
LowFreq    <- 4                                                                                    # Quarterly data (4 times a year)
HighFreq   <- 12                                                                                   # Monthly data (12 times a year)
FreqRatio  <- HighFreq/LowFreq
HorizonSet <- c(-1,0,1,2)                                                                          # Horizon for MIDAS models (in quarters)
saveFlag   <- 1     
num_param  <- 2                                                                                    # Specify number of parameters in restricted form
num_factors<- 1 
modelName  <- "Skip-sampled PCA Factor model 1 lag 1991M8"
LagOrder   <- 5
typeFlag   <- 0


# Pre-allocate storage
fcst_vec        <- unique(str2vec_quarter_MIDASF(myFiles,2))
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
FcstSave[1:111,3] <- Y_ref[22:132]                                                                # ADJUST

#############################################################################
start_time <- Sys.time()

  for (i in 2:(length(myFiles)/3)) {
    # Load relevant data for iteration
    data             <- readMat(myFiles[i])
    F_m2             <- readMat(myFiles[(length(myFiles)/3)+i])$SmoothedFactors[,1]
    F_m3             <- readMat(myFiles[(2*length(myFiles)/3)+i])$SmoothedFactors[,1]
    Y_q              <- data$yf[!is.nan(data$yf)]
    F_m1             <- data$SmoothedFactors[,1]
    config_nr        <- readConfig(myFiles[i]) 
    num_factors      <- 1
    Y_date           <- data$Date.f
    F_date           <- data$DateD.adj
    
    for (h in 1:length(HorizonSet)) {
      # Iterate over different forecast horizons
      Horizon <- HorizonSet[h]
      print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
      Y_q_shifted      <- shift(Y_q, n = Horizon, type = "shift")                                    # Transform data according to forecast horizon
      
      # Prepare split into estimation and forecast sample
      current_quarter  <- m2q(F_date[dim(F_date)[1],dim(F_date)[2]])
      current_year     <- F_date[dim(F_date)[1],1]
      current_moment   <- c(current_year, current_quarter)                                           # Current date expressed in quarters
      fcst_moment      <- fcst_goal(Horizon, current_moment)                                         # Find quarter for which to forecast in this iteration
      if (Horizon %in% c(1,2)) {                                                                     # If/else to properly read NA's at beginning/end of sample
        Y_available_last <- Y_date[(length(Y_q_shifted))*FreqRatio,]
        Y_available_last <- c(Y_available_last[1],m2q(Y_available_last[2]))
        Y_available_nr   <- length(Y_q_shifted)
        fcst_diff        <- quarter_diff(Y_available_last, fcst_moment) 
      } else {
        Y_available_last <- Y_date[(length(Y_q_shifted)-sum(is.na(Y_q_shifted)))*FreqRatio,]
        Y_available_last <- c(Y_available_last[1],m2q(Y_available_last[2]))
        Y_available_nr   <- length(Y_q_shifted) - sum(is.na(Y_q_shifted))
        fcst_diff        <- quarter_diff(Y_available_last, fcst_moment) 
      }
      
      if (fcst_diff == 0) {
        Y_q_shifted[Y_available_nr] <- NA
        Y_available_last <- Y_date[(length(Y_q_shifted)-1)*FreqRatio,]
        Y_available_last <- c(Y_available_last[1],m2q(Y_available_last[2]))
        Y_available_nr   <- length(Y_q_shifted)
        fcst_diff        <- quarter_diff(Y_available_last, fcst_moment)
      }
      
      # Split into estimation and forecast sample
      F_m4 <- shift(F_m1, n = -1)[-(length(F_m1))]
      F_m5 <- shift(F_m2, n = -1)[-(length(F_m2))]
      F_m6 <- shift(F_m3, n = -1)[-(length(F_m3))]
      
      min_vector <- list(F_m1,F_m2,F_m3,F_m4,F_m5,F_m6)
      min_length <- length(min_vector[[which.min(as.vector(c(length(F_m1),length(F_m2),length(F_m3),length(F_m4),length(F_m5),length(F_m6))))]])
      F_m <- cbind(F_m1[1:min_length], F_m2[1:min_length],F_m3[1:min_length],F_m4[1:min_length],F_m5[1:min_length],F_m6[1:min_length])
      fcst_sample <- F_m[(dim(F_m)[1]-fcst_diff + 1):dim(F_m)[1],]
      est_sample  <- F_m[1:(dim(F_m)[1]-fcst_diff),]

        Y_est_start     <- Y_available_nr - (length(est_sample)/6) + 1                              # 1 factor case
        if (Y_est_start < 1) {
              Y_est_start   <- 1
              est_sample    <- est_sample[(length(est_sample) -
                                             (length(Y_est_start:Y_available_nr)*FreqRatio) + 1):
                                            length(est_sample)]
        } 
        
        Y_q_shifted             <- Y_q_shifted[Y_est_start:Y_available_nr]
        
      # Transform data into time series form for MIDAS regression set-up
      start_date_year     <- Y_date[1,1]                                                                  
      start_date_quarter  <- m2q(Y_date[1,2])
      Y_q_shifted                 <- ts(Y_q_shifted, start = c(start_date_year, start_date_quarter),               # Act as if data starts at start_date (although it has been shifted to create the horizon-specific model)
                                        frequency = LowFreq)
      
      
      # ##################### MIDAS Regression ##############################
      y <- Y_q_shifted
      x <- data.frame(est_sample)
      skip_fit <- lm (y ~ X1 + X2 + X3 + X4 + X5 + X6, data = x)
      
      if (is.null(dim(fcst_sample))[1]) {
        fcst_sample <- matrix(fcst_sample, nrow = 1, ncol = 3)
      } 
      colnames(fcst_sample) = c("X1", "X2", "X3", "X4","X5","X6")
      
      ##################### MIDAS Forecasting ###########################
      Y_q_fcst    <- forecast(skip_fit, newdata = as.data.frame(fcst_sample), method = "static")
      
      
      # Forecast low-frequency variable
      result_fcst <- Y_q_fcst$mean
      
      ##################### Post-estimation operations ##################
      fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
      row_id <- match(fcst_quarter_id, fcst_vec_id)
      currentMonth <- F_date[dim(F_date)[1],2]
      col_id <- column_id_nr(currentMonth, Horizon)
      FcstSave[row_id,col_id+3] <- Y_q_fcst$mean[fcst_diff]
      
    }  # Horizon loop
    
    
  } # myFiles loop
  

end_time <- Sys.time()
end_time - start_time

################### Horizon performance ############################
  FcstSave_RMSE <- rep(NA, 12)
  a <- 1
  for (b in 4:(dim(FcstSave)[2]-1)) {
    FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
    a<- a + 1
  }
  FcstSave_RMSE[12] <- RMSE(FcstSave[(5:(dim(FcstSave)[1]-6)),15],FcstSave[(5:(dim(FcstSave)[1]-6)),3])
FcstSave_RMSE

##################### Save results ###############################
if (saveFlag == 1) {
  fileName_results <- paste("fcst results", modelName, ".xlsx")
  fileName_RMSE <- paste("fcst RMSE", modelName, ".xlsx")
  setwd("C://Users//Dennis//Documents//Study//Thesis//Forecast results//Factor extraction methods//Skip-sample")
  write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
}

###################### Check if forecasts get (way) better over time #############
# library(xlsx)
# data <- read.xlsx("fcst results RMSE U1F.xlsx", 1)
# results <- matrix(NA, length(1:11), 108)
# ev_end <- dim(data)[1] - 6
# for (p in 1:11) {
#   a <- (5+p):dim(data)[2]
#   b <- a[seq(1, length(a), 15)]
#   results[p,] <- apply(data[(4:ev_end),b], 1, function(x) mean(x[!is.na(x)]))
# }

# } # configurations loop

########################## Autocorrelogram #########################
# Check all factors -> different autocorrelograms
# acfpl <- acf(F_m1, plot=FALSE)
# acfpl$lag <- acfpl$lag * 12
# plot(acfpl, xlab="Lag (months)")
# 
# acf(Y_q)

#### TO DO ###
# 1a) dynamic structure on Y_ref
# 1) Measure of (rel. .....) Schumacher
# 3) Varying lags
# 4) Check autocorrelogram to see what lags to include

