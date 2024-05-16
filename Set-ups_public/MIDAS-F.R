################################### MIDAS-F ####################################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     May 13, 2024
#
################################################################################

######################### Estimation ###########################################
source("ESTIMATION.R")

Fcsts <- vector(mode = "list", 126)                                             # Store forecasts for all 126 factor configurations in list

start_time <- Sys.time()
for (i in 1:length(myFiles)) {                                                  

  data             <- readMat(myFiles[i])
  
  config_nr        <- ceil(i / 342)                                             # gives you 1 up until 126 configurations
  col_factors      <- 1                                                         # number of columns containing factors
  nr_factors       <- 1                                                         # number of static factors
  Y_q              <- data$yf[!is.nan(data$yf)]
  F_m              <- data$SmoothedFactors[,1:col_factors]                      # F_m contains # columns that contain (lagged) factors
  Y_date           <- data$Date.f
  F_date           <- data$DateD.adj
  
  for (h in 1:length(HorizonSet)) {
    
    # h = 1
    # Iterate over different forecast horizons
    Horizon            <- HorizonSet[h]
    print(paste('File: ', myFiles[i], 'and forecast horizon: ', Horizon))
    Y_q_shifted        <- shift(Y_q, n = Horizon, type = "shift")               # Transform data according to forecast horizon
    
    # Prepare split into estimation and forecast sample
    current_quarter    <- m2q(F_date[dim(F_date)[1], dim(F_date)[2]])
    current_year       <- F_date[dim(F_date)[1], 1]
    current_moment     <- c(current_year, current_quarter)                      # Current date expressed in quarters
    fcst_moment        <- fcst_goal(Horizon, current_moment)                    # Find quarter for which to forecast in this iteration
    
    if (Horizon %in% c(1, 2)) {
      # If/else to properly read NA's at beginning/end of sample
      Y_available_last <- Y_date[(length(Y_q_shifted)) * FreqRatio, ]
      Y_available_last <- c(Y_available_last[1], m2q(Y_available_last[2]))
      Y_available_nr   <- length(Y_q_shifted)
      fcst_diff        <- quarter_diff(Y_available_last, fcst_moment)
    } else {
      Y_available_last <- Y_date[(length(Y_q_shifted) - sum(is.na(Y_q_shifted))) * FreqRatio, ]
      Y_available_last <- c(Y_available_last[1], m2q(Y_available_last[2]))
      Y_available_nr   <- length(Y_q_shifted) - sum(is.na(Y_q_shifted))
      fcst_diff        <- quarter_diff(Y_available_last, fcst_moment)
    }
    
    if (fcst_diff == 0) {
      Y_q_shifted[Y_available_nr] <- NA
      Y_available_last <- Y_date[(length(Y_q_shifted) - 1) * FreqRatio, ]
      Y_available_last <- c(Y_available_last[1], m2q(Y_available_last[2]))
      Y_available_nr   <- length(Y_q_shifted)
      fcst_diff        <- quarter_diff(Y_available_last, fcst_moment)
    }
    
    # Split into estimation and forecast sample 
    est_sample         <- as.data.frame(split_sample(F_m, col_factors)[[1]])                         # select factors est. sample from $smoothed factors
    fcst_sample        <- as.data.frame(split_sample(F_m, col_factors)[[2]])
    
    Y_est_start      <- Y_available_nr - (length(est_sample[, 1]) / FreqRatio) + 1                   # > 1 factor
    if (Y_est_start  < 1) {
      Y_est_start    <- 1
      est_sample     <- est_sample[(length(est_sample[, 1]) - (length(Y_est_start:Y_available_nr) * FreqRatio) + 1):length(est_sample[, 1]), ]
    }
    Y_q_shifted      <- Y_q_shifted[Y_est_start:Y_available_nr]
    
    # Transform data into time series form for MIDAS regression set-up
    start_date_year    <- Y_date[1, 1]
    start_date_quarter <- m2q(Y_date[1, 2])
    Y_q_shifted        <- ts(Y_q_shifted,
                             start = c(start_date_year, start_date_quarter),
                             # Act as if data starts at start_date (although it has been shifted to create the horizon-specific model)
                             frequency = LowFreq)
    
    nam <- paste("F_m", nr_factors, sep = "")
    assign(nam, ts(est_sample,
                   start = (c(start_date_year,                              # Change addition to start_date to realign X!
                              start_date_quarter) + c(0, 2)),               
                   frequency = HighFreq))
    
    # ##################### MIDAS Regression ##############################
    
    list_vector_matrix <- 
      if (nr_factors == 1) {
        if (col_factors == 1) {
          # if just one factor ts than take simple list  
          data_midas <- list(Y_q_shifted, F_m1)
        } else {
          # ...else convert the matrix of factor ts to individual lists
          data_midas <- c(list(Y_q_shifted), lapply(1:ncol(F_m1), function(i) F_m1[,i]))
        }
      } else if (nr_factors == 2) {
        data_midas <- c(list(Y_q_shifted), lapply(1:ncol(F_m2), function(i) F_m2[,i]))
      } else if (nr_factors == 3) {
        data_midas <- c(list(Y_q_shifted), lapply(1:ncol(F_m3), function(i) F_m3[,i]))
      } else if (nr_factors == 4) {
        data_midas <- c(list(Y_q_shifted), lapply(1:ncol(F_m4), function(i) F_m4[,i]))
      } else if (nr_factors == 5) {
        data_midas <- c(list(Y_q_shifted), lapply(1:ncol(F_m5), function(i) F_m5[,i]))
      } else  {
        data_midas <- c(list(Y_q_shifted), lapply(1:ncol(F_m6), function(i) F_m6[,i]))
      }
    
    ######################### Unrestricted MIDAS ##########################
    formula           <- formula_fun(LagOrder, FreqRatio, col_factors, 0)[[1]]
    factor_names      <- formula_fun(LagOrder, FreqRatio, col_factors, 0)[[2]]
    names(data_midas) <- c('y', factor_names)
    y                 <- Y_q_shifted
    
    if (typeFlag == 0) {
      start_values           <- matrix(1, (LagOrder + 1), col_factors)
      colnames(start_values) <- factor_names
      midas_fit              <- midas_r(formula,                                # unrestricted MIDAS (setup was estimating the restricted MIDAS)
                                        data_midas,
                                        start_values)                           # Starting values need to be of order 1 + LagOrder (as contemporaneous lag is automatically included)
    }
    # summary(midas_fit)
    
    ##################### MIDAS Forecasting ############################
    
    data_fcst <- c(lapply(1:ncol(fcst_sample), function(i) fcst_sample[,i]))
    
    # re-assign letters to variables for use in MIDAS-F
    lettersL        <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS))) 
    names(data_fcst) <- lettersL[1:size(data_fcst)[2]]
    
    Y_q_fcst    <- forecast(midas_fit, data_fcst, method = "static")            # Forecast low-frequency variable
    # plot(Y_q_fcst)                                                            # Plot forecast
    result_fcst <- Y_q_fcst$mean                                                # Result forecast
    
    ##################### Post-estimation operations ##################
    fcst_quarter_id <- paste(fcst_moment[1], fcst_moment[2])
    # rows
    row_id          <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth    <- F_date[dim(F_date)[1], 2]
    # columns
    col_id          <- column_id_nr(currentMonth, Horizon)
    FcstSave[row_id, col_id + 3] <- Y_q_fcst$mean[fcst_diff]
    
  }  # Horizon loop
  
  Fcsts[[config_nr]] <- FcstSave
  
} # myFiles loop

end_time <- Sys.time()
end_time - start_time

# Save file before writing outcomes to Excel

##################### Save results ###############################
if (saveFlag == 1) {
  setwd(paste0(ROOT,"/Results_public/fcst"))
  # save Rdatafile with outcomes
  save.image(paste0(modelName,".Rdata")) 
  # Write outcome to Excel-file (sheet 1, Sheet 2 .... Sheet 126)
  write.xlsx(setNames(as.list(lapply(Fcsts, data.frame)), 
                      names(Fcsts)), 
             file=paste("fcst results", modelName,".xlsx"))
}