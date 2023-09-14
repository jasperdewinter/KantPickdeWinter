################################################### MIDAS-F ###################################################
#
#                                   Dennis Kant, Andreas Pick and Jasper de Winter
#                                                 09/12/2023
#
# ###############################################################################################################

######################### Preliminaries ##############################
# Estimation
source("Estimation.r")

Fcsts <- vector(mode = "list", 126)                                             # Store forecasts for all 126 factor configurations in list

specs <- MIDAS_config_magic_numbers <- readxl::read_excel("G:/EBO/ECMO/de Winter/Werk/Onderzoek/PROJECT 11.  KANT PICK DE WINTER/Replication/Results/other/MIDAS config & magic numbers.xlsx", 
                                         sheet = "export")                      # Import specs, including number of columns that contain factors

start_time <- Sys.time()

# for (i in 1:length(myFiles)) {                                                # length(myFiles) = 43,092 = 126 specs of models [1-6 factors (6), 1-6 lags (21)] * 342 months (every month new skip sampled data set)
# for (i in 1:7182) {
# for (i in 7183:14364) {
# for (i in 14365:21546) {
# for (i in 21547:28728) {
# for (i in 28729:35910) {
# for (i in 35911:43092) {
  
  # ============================================================================
  # for each i you calculate four forecast, i.e: 1 backcast, 1 nowcast, 1 1q 
  # forecast and 1q forecast and you write this to the matrix FcstSave. Each
  # round, you save the new information to Fcsts, overwriting anything that is
  # in this matrix. NL 1 has 342 forecast, so save the matrix every 342 rounds
  # you control this with config_nr. Magic numbers: 342, 126, 
  # 6 and 21 (see \Support Code\MIDAS confid & magic numbers.xlsx)
  #
  # at the end you will have 126 matrices of forecast, 1 for each modelspec.
  # save this to one Excel-file
  # ============================================================================

  # VARIANT A: only take first factor on board, across all 126 specs; corrected version of what is in paper now  ####
  # WARNING: On Github you can only test i =1, i = 343 and i = 685
  # ================================================================================================================ #
  # i = 1            # spec 1: 1 factor, 1 lag - LagOrder 0/ LagOrder 1/ LagOrder 2/ LagOrder 3 converges (standard setting)
  # i = 343          # spec 2: 2 factors, 1 lag - LagOrder 0 converges
  i = 685          # spec 3: 3 factors, 1 lag - LagOrder 0 converges
  # i = 1027         # spec 4: 4 factors, 1 lag  - LagOrder 0 converges
  # i = 1369         # spec 5: 5 factors, 1 lag- LagOrder 0 converges
  # i = 1711         # spec 6: 6 factors, 1 lag


  data             <- readMat(myFiles[i])
  
  config_nr        <- ceil(i / 342)                                             # gives you 1 up until 126 configurations
  col_factors      <- 6                                                         # number of columns containing factors
  nr_factors       <- 6                                                         # number of static factors
  LagOrder         <- 1
  Y_q              <- data$yf[!is.nan(data$yf)]
  F_m              <- data$SmoothedFactors[,1:col_factors]                      # F_m contains # columns that contain (lagged) factors
  Y_date           <- data$Date.f
  F_date           <- data$DateD.adj
  
  # VARIANT B: take on board all factors and lags of factors in MIDAS, differs per each of the 126 specs ####
  # ========================================================================================================#
  # data             <- readMat(myFiles[i])
  # 
  # config_nr        <- ceil(i / 342)                                             # gives you 1 up until 126 configurations
  # col_factors      <- specs$col.factors[config_nr]                              # number of columns containing factors
  # nr_factors       <- specs$r[config_nr]                                        # number of static factors
  # Y_q              <- data$yf[!is.nan(data$yf)]
  # F_m              <- data$SmoothedFactors[,1:col_factors]                      # F_m contains # columns that contain (lagged) factors
  # Y_date           <- data$Date.f
  # F_date           <- data$DateD.adj

  # for (h in 1:length(HorizonSet)) {
    
    h = 1
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
    summary(midas_fit)
    
    ######################### Restricted MIDAS ##########################
      # if (typeFlag == 0) {
      #   start_values           <- matrix(1, (LagOrder + 1), col_factors)
      #   colnames(start_values) <- factor_names
      #   midas_fit              <- midas_r(formula,                              # restricted MIDAS (was in original setup oF Dennis, but Overleaf says we are estimating unrestriicted MIDAS)
      #                                     data_midas,
      #                                     start_values)                         # Starting values need to be of order 1 + LagOrder (as contemporaneous lag is automatically included)
      # }
      # summary(midas_fit)   

    # ######################### MIDAS Exponential Almon lag  ##########################
    # if (typeFlag == 1) {
    #   formula     <- formula_fun(LagOrder, FreqRatio, nr_factors, 1)[[1]]
    # 
    #   if (nr_factors == 1) {
    #     a         <- F_m1
    #     midas_fit <- midas_r(formula,
    #                          start = list(a = rep(0.5, num_param)))
    # 
    #   } else if (nr_factors == 2) {
    #     a         <- F_m1
    #     b         <- F_m2
    #     midas_fit <- midas_r(y ~ fmls(a,
    #                                   LagOrder,
    #                                   FreqRatio,
    #                                   nealmon) +
    #                            fmls(b,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon),
    #                          start = list(a = rep(0.5, num_param),
    #                                       b = rep(0.5, num_param)))
    #   } else if (nr_factors == 3) {
    #     a         <- F_m1
    #     b         <- F_m2
    #     c         <- F_m3
    #     midas_fit <- midas_r(y ~ fmls(a,
    #                                   LagOrder,
    #                                   FreqRatio,
    #                                   nealmon) +
    #                            fmls(b,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(c,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon),
    #                          start = list(a = rep(0.5, num_param),
    #                                       b = rep(0.5, num_param),
    #                                       c = rep(0.5, num_param)))
    #   } else if (nr_factors == 4) {
    #     a         <- F_m1
    #     b         <- F_m2
    #     c         <- F_m3
    #     d         <- F_m4
    #     midas_fit <- midas_r(y ~ fmls(a,
    #                                   LagOrder,
    #                                   FreqRatio,
    #                                   nealmon) +
    #                            fmls(b,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(c,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(d,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon),
    #                          start = list(a = rep(0.5, num_param),
    #                                       b = rep(0.5, num_param),
    #                                       c = rep(0.5, num_param),
    #                                       d = rep(0.5, num_param)))
    #   } else if (nr_factors == 5) {
    #     e         <- F_m5
    #     a         <- F_m1
    #     b         <- F_m2
    #     c         <- F_m3
    #     d         <- F_m4
    #     midas_fit <- midas_r(y ~ fmls(a,
    #                                   LagOrder,
    #                                   FreqRatio,
    #                                   nealmon) +
    #                            fmls(b,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(c,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(d,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(e,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon),
    #                          start = list(a = rep(0.5, num_param),
    #                                       b = rep(0.5, num_param),
    #                                       c = rep(0.5, num_param),
    #                                       d = rep(0.5, num_param),
    #                                       e = rep(0.5, num_param)))
    #   } else {
    #     a         <- F_m1
    #     b         <- F_m2
    #     c         <- F_m3
    #     d         <- F_m4
    #     e         <- F_m5
    #     f         <- F_m6
    #     midas_fit <- midas_r(y ~ fmls(a,
    #                                   LagOrder,
    #                                   FreqRatio,
    #                                   nealmon) +
    #                            fmls(b,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(c,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(d,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(e,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon) +
    #                            fmls(f,
    #                                 LagOrder,
    #                                 FreqRatio,
    #                                 nealmon),
    #                          start = list(a = rep(0.5, num_param),
    #                                       b = rep(0.5, num_param),
    #                                       c = rep(0.5, num_param),
    #                                       d = rep(0.5, num_param),
    #                                       e = rep(0.5, num_param),
    #                                       f = rep(0.5, num_param)))
    #   }
    # 
    # }
    
    
    
    ##################### MIDAS Forecasting ############################
    
    data_fcst <- c(lapply(1:ncol(fcst_sample), function(i) fcst_sample[,i]))
    
    # re-assign letters to variables for use in MIDAS-F
    lettersL        <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS))) # 702 Uppercase letters
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
    
#  }  # Horizon loop
  
  Fcsts[[config_nr]] <- FcstSave
  
#} # myFiles loop

end_time <- Sys.time()
end_time - start_time

# Save file before writing outcomes to Excel

##################### Save results ###############################
if (saveFlag == 1) {
  setwd(paste0(ROOT,"/Results/"))
  # save Rdatafile with outcomes
  save.image(paste0(modelName,".Rdata")) 
  # Write outcome to Excel-file (sheet 1, Sheet 2 .... Sheet 126)
  write.xlsx(setNames(as.list(lapply(Fcsts, data.frame)), 
                      names(Fcsts)), 
                      file=paste("fcst results", modelName,".xlsx"))
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