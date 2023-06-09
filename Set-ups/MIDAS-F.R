################################################### MIDAS-F ###################################################
#
#                                   Dennis Kant, Andreas Pick and Jasper de Winter
#                                                 3/14/2023
#
###############################################################################################################

######################### Preliminaries ##############################
# Estimation
source("Estimation.r")

Fcsts <- vector(mode = "list", 126)                                             # Store forecasts for all 126 factor configurations in list

start_time <- Sys.time()

# for (i in 1:length(myFiles)) {                                                  # length(myFiles) = 43,092 = 126 specs of models [1-6 factors (6), 1-6 lags (21)] * 342 months (every month new skip sampled data set)
# for (i in 1:7182) {
# for (i in 7183:14364) {
# for (i in 14365:21546) {
# for (i in 21547:28728) {
# for (i in 28729:35910) {
for (i in 35911:43092) {

  # ============================================================================
  # for each i you calculate four forecast, i.e: 1 backcast, 1 nowcast, 1 1q 
  # forecast and 1q forecast and you write this to the matrix FcstSave. Each
  # round, you save the new information to Fcsts, overwriting anything that is
  # in this matrix. NL 1 has 342 forecast, so save the matrix every 342 rounds
  # you control this with config_nr (see l.37 & l.335). Magic numbers: 342, 126, 
  # 6 and 21 (see \Support Code\MIDAS confid & magic numbers.xlsx)
  #
  # at the end you will have 126 matrices of forecast, 1 for each modelspec.
  # save this to one Excel-file
  # ============================================================================

  # Load relevant data for iteration
  data             <- readMat(myFiles[i])
  
  Y_q              <- data$yf[!is.nan(data$yf)]
  F_m              <- data$SmoothedFactors
  Y_date           <- data$Date.f
  F_date           <- data$DateD.adj
  config_nr        <- ceil(i / 342)                                             # Hard-coded (!) you have 342 periods in the 

  for (h in 1:length(HorizonSet)) {
    
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
    est_sample         <- split_sample(F_m, num_factors)[[1]]
    fcst_sample        <- split_sample(F_m, num_factors)[[2]]
    
    if (num_factors == 1) {
      Y_est_start      <- Y_available_nr - (length(est_sample) / FreqRatio) + 1                              # 1 factor case
      if (Y_est_start  < 1) {
        Y_est_start    <- 1
        est_sample     <- est_sample[(length(est_sample) -
                                        (length(Y_est_start:Y_available_nr) * FreqRatio) + 1):length(est_sample)]
      }
      Y_q_shifted      <- Y_q_shifted[Y_est_start:Y_available_nr]
    } else {
      Y_est_start      <- Y_available_nr - (length(est_sample[, 1]) / FreqRatio) + 1                         # > 1 factor
      if (Y_est_start  < 1) {
        Y_est_start    <- 1
        est_sample     <- est_sample[(length(est_sample[, 1]) -
                                        (length(Y_est_start:Y_available_nr) * FreqRatio) + 1):length(est_sample[, 1]), ]
      }
      Y_q_shifted      <- Y_q_shifted[Y_est_start:Y_available_nr]
    }
    
    # Transform data into time series form for MIDAS regression set-up
    start_date_year    <- Y_date[1, 1]
    start_date_quarter <- m2q(Y_date[1, 2])
    Y_q_shifted        <- ts(Y_q_shifted,
                             start = c(start_date_year, start_date_quarter),
                             # Act as if data starts at start_date (although it has been shifted to create the horizon-specific model)
                             frequency = LowFreq)
    
    if (num_factors == 1) {
      assign('F_m1', ts(
        est_sample,
        start = (c(start_date_year, 
                   start_date_quarter) + c(0, 2)),
        frequency = HighFreq))                                       # Change addition to start_date to realign X!
    } else{
      for (h in 1:ncol(F_m)) {
        # > 1 factor
        nam <- paste("F_m", h, sep = "")
        assign(nam, ts(est_sample[, h],
                       start = (c(start_date_year,                              # Change addition to start_date to realign X!
                                  start_date_quarter) + c(0, 2)),               
                       frequency = HighFreq))
      }
    }
    
    # ##################### MIDAS Regression ##############################
    if (num_factors == 1) {
      data_midas <- list(Y_q_shifted, F_m1)
    } else if (num_factors == 2) {
      data_midas <- list(Y_q_shifted, F_m1, F_m2)
    } else if (num_factors == 3) {
      data_midas <- list(Y_q_shifted, F_m1, F_m2, F_m3)
    } else if (num_factors == 4) {
      data_midas <- list(Y_q_shifted, F_m1, F_m2, F_m3, F_m4)
    } else if (num_factors == 5) {
      data_midas <- list(Y_q_shifted, F_m1, F_m2, F_m3, F_m4, F_m5)
    } else  {
      data_midas <- list(Y_q_shifted, F_m1, F_m2, F_m3, F_m4, F_m5, F_m6)
    }
    
    ######################### Unrestricted MIDAS ##########################
    formula           <- formula_fun(LagOrder, FreqRatio, num_factors, 0)[[1]]
    factor_names      <- formula_fun(LagOrder, FreqRatio, num_factors, 0)[[2]]
    names(data_midas) <- c('y', factor_names)
    y                 <- Y_q_shifted
    
    if (typeFlag == 0) {
      start_values           <- matrix(1, (LagOrder + 1), num_factors)
      colnames(start_values) <- factor_names
      midas_fit              <- midas_r(formula, 
                                        data_midas, 
                                        start_values)                           # Starting values need to be of order 1 + LagOrder (as contemporaneous lag is automatically included)
    }
    
    ######################### MIDAS Exponential Almon lag  ##########################
    if (typeFlag == 1) {
      formula     <- formula_fun(LagOrder, FreqRatio, num_factors, 1)[[1]]

      if (num_factors == 1) {
        a         <- F_m1
        midas_fit <- midas_r(formula,
                             start = list(a = rep(0.5, num_param)))

      } else if (num_factors == 2) {
        a         <- F_m1
        b         <- F_m2
        midas_fit <- midas_r(y ~ fmls(a,
                                      LagOrder,
                                      FreqRatio,
                                      nealmon) +
                               fmls(b,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon),
                             start = list(a = rep(0.5, num_param),
                                          b = rep(0.5, num_param)))
      } else if (num_factors == 3) {
        a         <- F_m1
        b         <- F_m2
        c         <- F_m3
        midas_fit <- midas_r(y ~ fmls(a,
                                      LagOrder,
                                      FreqRatio,
                                      nealmon) +
                               fmls(b,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(c,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon),
                             start = list(a = rep(0.5, num_param),
                                          b = rep(0.5, num_param),
                                          c = rep(0.5, num_param)))
      } else if (num_factors == 4) {
        a         <- F_m1
        b         <- F_m2
        c         <- F_m3
        d         <- F_m4
        midas_fit <- midas_r(y ~ fmls(a,
                                      LagOrder,
                                      FreqRatio,
                                      nealmon) +
                               fmls(b,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(c,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(d,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon),
                             start = list(a = rep(0.5, num_param),
                                          b = rep(0.5, num_param),
                                          c = rep(0.5, num_param),
                                          d = rep(0.5, num_param)))
      } else if (num_factors == 5) {
        e         <- F_m5
        a         <- F_m1
        b         <- F_m2
        c         <- F_m3
        d         <- F_m4
        midas_fit <- midas_r(y ~ fmls(a,
                                      LagOrder,
                                      FreqRatio,
                                      nealmon) +
                               fmls(b,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(c,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(d,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(e,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon),
                             start = list(a = rep(0.5, num_param),
                                          b = rep(0.5, num_param),
                                          c = rep(0.5, num_param),
                                          d = rep(0.5, num_param),
                                          e = rep(0.5, num_param)))
      } else {
        a         <- F_m1
        b         <- F_m2
        c         <- F_m3
        d         <- F_m4
        e         <- F_m5
        f         <- F_m6
        midas_fit <- midas_r(y ~ fmls(a,
                                      LagOrder,
                                      FreqRatio,
                                      nealmon) +
                               fmls(b,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(c,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(d,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(e,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon) +
                               fmls(f,
                                    LagOrder,
                                    FreqRatio,
                                    nealmon),
                             start = list(a = rep(0.5, num_param),
                                          b = rep(0.5, num_param),
                                          c = rep(0.5, num_param),
                                          d = rep(0.5, num_param),
                                          e = rep(0.5, num_param),
                                          f = rep(0.5, num_param)))
      }

    }
    
    ##################### MIDAS Forecasting ############################
    if (num_factors == 1) {
      data_fcst <- list(a = fcst_sample)
    } else if (num_factors == 2) {
      data_fcst <- list(a = fcst_sample[, 1],
                        b = fcst_sample[, 2])
    } else if (num_factors == 3) {
      data_fcst <- list(a = fcst_sample[, 1],
                        b = fcst_sample[, 2],
                        c = fcst_sample[, 3])
    } else if (num_factors == 4) {
      data_fcst <- list(a = fcst_sample[, 1],
                        b = fcst_sample[, 2],
                        # 4 factor setup 
                        c = fcst_sample[, 3],
                        d = fcst_sample[, 4])
    } else if (num_factors == 5) {
      data_fcst <- list(a = fcst_sample[, 1],
                        b = fcst_sample[, 2],
                        # 5 factor setup
                        c = fcst_sample[, 3],
                        d = fcst_sample[, 4],
                        e = fcst_sample[, 5])
    } else {
      data_fcst <- list(a = fcst_sample[, 1],
                        b = fcst_sample[, 2],
                        # 6 factor setup
                        c = fcst_sample[, 3],
                        d = fcst_sample[, 4],
                        e = fcst_sample[, 5],
                        f = fcst_sample[, 6])
    }
    
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
  setwd(paste0(ROOT,"/Results/"))
  # save Rdatafile with outcomes
  save.image(paste0(modelName,"part_",i,".Rdata")) 
  # Write outcome to Excel-file (sheet 1, Sheet 2 .... Sheet 126)
  write.xlsx(setNames(as.list(lapply(Fcsts, data.frame)), 
                      names(Fcsts)), 
                      file=paste("fcst results", modelName,"part_",i,".xlsx"))
}
# EXPLANATION MyFiles and Matlab files
########### i=1: NL 1 Period 1991-7
# Datef           [75 x 2]  :: [1986 1] - [1992 3]
# DateD.adj       [65 x 2 ] :: [1986 3] - [1991 7]
# DateX.final     [408 x 2] :: [1987 1] - [2019 12]
#
# SmoothedFactors [73 x 3]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
#
# Yc.final        [408 x 1] :: Y on a monthly basis compete sample per [1987 1] - [2019 12]]
# yf              [75 x 1]  :: Y on a monthly basis over sample period [1986 1] - [1992 3]
########### 
# If you run all files with NL 1 [1 factor, 3 skip sampled lags] you have a full forecasting matrix that is filled in this loop
# If you run all files with NL 2 [2 factor, 4 skip sampled lags] you have a full forecasting matrix that is filled in this loop
# ..
# idem but:
# NL  2 Period 1991-7: SmoothedFactors [73 x 4]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL  3 Period 1991-7: SmoothedFactors [73 x 5]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL  4 Period 1991-7: SmoothedFactors [73 x 6]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL  5 Period 1991-7: SmoothedFactors [73 x 7]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL  6 Period 1991-7: SmoothedFactors [73 x 8]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# ...
# NL  7 Period 1991-7: SmoothedFactors [73 x 4]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL  8 Period 1991-7: SmoothedFactors [73 x 5]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL  9 Period 1991-7: SmoothedFactors [73 x 6]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 10 Period 1991-7: SmoothedFactors [73 x 7]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 11 Period 1991-7: SmoothedFactors [73 x 8]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# ...
# NL 12 Period 1991-7: SmoothedFactors [73 x 5]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 13 Period 1991-7: SmoothedFactors [73 x 6]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 14 Period 1991-7: SmoothedFactors [73 x 7]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 15 Period 1991-7: SmoothedFactors [73 x 8]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# ...
# NL 16 Period 1991-7: SmoothedFactors [73 x 6]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 17 Period 1991-7: SmoothedFactors [73 x 7]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 18 Period 1991-7: SmoothedFactors [73 x 8]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# ...
# NL 19 Period 1991-7: SmoothedFactors [73 x 7]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# NL 20 Period 1991-7: SmoothedFactors [73 x 8]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# ...
# NL 21 Period 1991-7: SmoothedFactors [73 x 8]  :: Skip sampled smoothed factors of DFM with one lag (and the contemporaneous lag)
# ...
# ---------------------------------------------------------------------------------------------------------------------------------

# ...
#######################################################