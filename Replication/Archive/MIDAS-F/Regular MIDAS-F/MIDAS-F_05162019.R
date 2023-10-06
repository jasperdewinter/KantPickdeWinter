###################### MIDAS-F Nowcasting ############################
#
#                         Dennis Kant
#
#                         16/05/2019
#
######################################################################

######################### Preliminaries ##############################
rm(list=ls())                                                                                      # Clear working environment
# Set Working Directory
setwd(paste0(path,"/Set-ups/Support code"))
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

# Load functions
source('Y_availability.r')                                                                         # Provide current available Y
source('quarter_diff.r')                                                                           # Difference in quarters
source('str2vec_quarter.r')                                                                        # Transforms file names into date vectors
source('m2q.r')                                                                                    # Provides quarter corresponding to month
source('column_id_nr.r')
source('datestr2vec.r')
source('fcst_goal.r')

setwd(paste0(path,"/Data"))
# Load data 
myFiles   <- list.files(pattern="NL .*mat")
myFiles   <- naturalsort(myFiles)                                                                  # Sort in natural order

# Specify frequency parameters for MIDAS regression
LowFreq    <- 4                                                                                    # Quarterly data (4 times a year)
HighFreq   <- 12                                                                                   # Monthly data (12 times a year)
FreqRatio  <- HighFreq/LowFreq
LagOrder   <- 2                                                                                    # Contemporaneous lag is already included!
HorizonSet <- c(-1,0,1,2)                                                                          # Horizon for MIDAS models (in quarters)
saveFlag   <- 1     

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
Y_ref             <- readMat(myFiles[length(myFiles)])$Yd
Y_ref_date        <- sapply(unlist(readMat(myFiles[length(myFiles)])$DateT.q), function(x) datestr2vec(x)) 
Y_ref             <- Y_ref[!is.nan(Y_ref)]
FcstSave[1:111,3] <- Y_ref[22:132]
FcstSave_RMSE     <- FcstSave

#############################################################################
start_time <- Sys.time()

for (i in 1:length(myFiles)) {
  # Load relevant data for iteration
  data             <- readMat(myFiles[i])
  Y_q              <- data$Yf.q
  F_m              <- data$Fj
  Y_date           <- sapply(data$DateT.q, function(x){x})
  Y_date           <- sapply(unlist(Y_date), function(x) datestr2vec(x))
  F_date           <- sapply(data$DateT, function(x){x})
  F_date           <- sapply(unlist(F_date), function(x) datestr2vec(x))
    
  for (h in 1:length(HorizonSet)) {
    # Iterate over different forecast horizons
    Horizon <- HorizonSet[h]
    print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
    Y_q_shifted      <- shift(Y_q, n = Horizon, type = "shift")                                    # Transform data according to forecast horizon
    
    # Prepare split into estimation and forecast sample
    current_quarter  <- m2q(F_date[2,dim(F_date)[2]])
    current_year     <- F_date[1,dim(F_date)[2]]
    current_moment   <- c(current_year, current_quarter)                                           # Current date expressed in quarters
    fcst_moment      <- fcst_goal(Horizon, current_moment)                                         # Find quarter for which to forecast in this iteration
    Y_available_last <- Y_availability(Y_date, Y_q_shifted,Horizon)[1:2]
    Y_available_nr   <- Y_availability(Y_date, Y_q_shifted, Horizon)[3]
    fcst_diff        <- quarter_diff(Y_available_last, fcst_moment)                                # Difference in quarters
    
    if (fcst_diff == 0) {
    Y_q_shifted[Y_available_nr] <- NA
    Y_available_last <- Y_availability(Y_date, Y_q_shifted, Horizon)[1:2]
    Y_available_nr   <- Y_availability(Y_date, Y_q_shifted, Horizon)[3]
    fcst_diff        <- quarter_diff(Y_available_last, fcst_moment)
    }
    
      # Split into estimation and forecast sample
      fcst_sample     <- F_m[(dim(F_m)[1]-(fcst_diff*FreqRatio)+1):
                               dim(F_m)[1],]
      est_sample      <- F_m[1:(dim(F_m)[1]-(fcst_diff*FreqRatio)),]
      
      # Realign estimation sample such that estimation can take place
      required_data   <- dim(est_sample)[1] - (dim(est_sample)[1] %% FreqRatio)
      est_sample      <- est_sample[((dim(est_sample)[1] - required_data + 1): 
                                       dim(est_sample)[1]),]
      
      Y_est_start     <- Y_available_nr - (length(est_sample[,1])/FreqRatio) + 1
      if (Y_est_start < 1) {
        Y_est_start   <- 1
        est_sample    <- est_sample[(length(est_sample[,1]) - 
                                    (length(Y_est_start:Y_available_nr)*FreqRatio) + 1):
                                   length(est_sample[,1]),]
      }
      Y_q_shifted             <- Y_q_shifted[Y_est_start:Y_available_nr]
      
      # Transform data into time series form
      start_date_year     <- Y_date[1,1]
      start_date_quarter  <- m2q(Y_date[2,1])
      Y_q_shifted                 <- ts(Y_q_shifted, start = c(start_date_year, start_date_quarter),               # Act as if data starts at start_date (although it has been shifted to create the horizon-specific model)         
                                frequency = LowFreq)   
      for (h in 1:ncol(F_m)) {                                                  
        nam <- paste("F_m", h, sep = "")
        assign(nam, ts(est_sample[,h], start = (c(start_date_year, start_date_quarter) + c(0, 2)), # Change addition to start_date to realign X! 
                       frequency = HighFreq)) 
      }
      
      ##################### MIDAS Regression ##############################
      formula = y ~ fmls(a,LagOrder,FreqRatio) +
        fmls(b, LagOrder, FreqRatio) +
        fmls(c, LagOrder, FreqRatio) +
        fmls(d, LagOrder, FreqRatio)
      y <- Y_q_shifted
      data_midas <- list(y=Y_q_shifted,a=F_m1,b=F_m2,c=F_m3,d=F_m4)
      midas_fit <- midas_r( formula, data_midas,                                                   # Starting values need to be of order 1 + LagOrder (as contamporaneous lag is automatically included)
                            start=list(a=c(1,1,1),
                                       b=c(1,1,1),c=c(1,1,1), d=c(1,1,1)) )
      
      ##################### MIDAS Forecasting ############################
      data_fcst <- list(a=fcst_sample[,1],
                        b=fcst_sample[,2],
                        c=fcst_sample[,3],
                        d=fcst_sample[,4])
      
      Y_q_fcst    <- forecast(midas_fit, data_fcst, method = "static")                             # Forecast low-frequency variable
      plot(Y_q_fcst)
      result_fcst <- Y_q_fcst$mean
      
      ##################### Post-estimation operations ##################
      fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
      row_id <- match(fcst_quarter_id, fcst_vec_id)
      currentMonth <- F_date[2,dim(F_date)[2]]
      col_id <- column_id_nr(currentMonth, Horizon)
      FcstSave[row_id,col_id+3] <- Y_q_fcst$mean[fcst_diff]
    
  
      
  }
  
}

end_time <- Sys.time()
end_time - start_time


##################### Calculate RMSE ###############################
for (a in 1:dim(FcstSave)[1]) {
  for (b in 4:dim(FcstSave)[2]) {
    FcstSave_RMSE[a,b] <- RMSE(FcstSave[a,3],FcstSave[a,b])
  }
}

################### Horizon performance ############################
horizon_performance <- apply(FcstSave_RMSE[,4:15], 2, function(x) mean(x[!is.na(x)]))
x <- 1:12
y <- horizon_performance
lo <- loess(y~x)
plot(x,y, main="MIDAS performance",
     xlab="Horizon", ylab="Average RMSE")
lines(predict(lo), col='red', lwd=2)

##################### Save results ###############################
if (saveFlag == 1) {
  setwd(paste0(path,"/Results"))
    write.xlsx(FcstSave, file="fcst results.xlsx", sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(FcstSave_RMSE, file ="fcst results RMSE.xlsx", sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
}


#### TO DO ###
# 1a) dynamic structure on Y_ref
# 1) Measure of (rel. .....) Schumacher
# 2) New factors
# 3) Varying lags
# 