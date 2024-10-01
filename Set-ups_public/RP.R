######################## Random Projections ####################################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     October 1, 2024
#
################################################################################
#
######################### Estimation ###########################################
source("ESTIMATION.R")

save_results             <- matrix(NA, length(k_set), 12)
Fcsts                    <- vector(mode = "list", length(k_set))
save_cumulative_results  <- vector(mode = "list", length(k_set))
save_coeff               <- vector(mode = "list", 3*length(k_set)*fcst_av_nr)   # Pre-allocate coefficient list for last estimation sample
seeds                    <- c(1:fcst_av_nr)                                     # Fixed seeds

for (p in 1:length(k_set)) {
  Fcsts[[p]] <- FcstSave
  save_cumulative_results[[p]] <- matrix(NA, (length(4:(dim(FcstSave)[1]-6))), 12)
}
names(Fcsts) <- k_set


################################################################################
k_loop <- 1                                                                     

start_time <- Sys.time()

for (k in k_set) {
  
  for (i in 1:length(myFiles)) {
    # Load relevant data for iteration
    data             <- readMat(myFiles[i])
    Y_q              <- data$Yd[!is.nan(data$Yd)]
    X                <- data$Xd                                                                    
    Date             <- data$DateD
    
    for (h in 1:length(HorizonSet)) {
      # Iterate over different forecast horizons
      Horizon <- HorizonSet[h]
      print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon, 'and k is ', k))
      Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
      num_y        <- length(Y_q_shifted)
      
      fcst_av_storage <- rep(NA,fcst_av_nr)
      
      # Transform X such that skip-sampling can take place (i.e. make a multiple
      # of FreqRatio
      X_fill_nr       <- FreqRatio - (dim(X)[1] %% FreqRatio)
      X_fill          <- matrix(NaN, X_fill_nr, dim(X)[2])
      X_sel_complete  <- rbind(X, X_fill)                                       # All X-variables
      
      # Transform X in skip-sampled form
      data_X <- matrix(NA, (dim(X_sel_complete)[1]/FreqRatio), (FreqRatio * dim(X_sel_complete)[2]) )
      for (g in 1:dim(X)[2]) {
        col_id <- g - 1
        data_X[,((1+col_id*FreqRatio):(3+col_id*FreqRatio))] <- fmls(X_sel_complete[,g],LagOrder,FreqRatio)
      }
      
      realign_X <- realign(data_X)
      data_X_h  <- realign_X[[1]][-(1:realign_X[[2]]),]
      
      # Prepare split into estimation and forecast sample
      current_quarter  <- m2q(Date[dim(Date)[1],dim(Date)[2]])
      current_year     <- Date[dim(Date)[1],1]
      current_moment   <- c(current_year, current_quarter)                      # Current date expressed in quarters
      fcst_moment      <- fcst_goal(Horizon, current_moment)
      
      if (Horizon %in% c(1,2)) {                                                # If/else to properly read NA's at beginning/end of sample
        Y_available_last <- Date[num_y*FreqRatio,]
        Y_available_last <- c(Y_available_last[1],m2q(Y_available_last[2]))
        Y_available_nr   <- length(Y_q_shifted)
        fcst_diff        <- quarter_diff(Y_available_last, fcst_moment) 
      } else {
        Y_available_last <- Date[((num_y-sum(is.na(Y_q_shifted)))*FreqRatio),]
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
      fcst_sample     <- data_X_h[ (dim(data_X_h)[1]-fcst_diff+1) :
                                     dim(data_X_h)[1], ]
      est_sample      <- data_X_h[1:(dim(data_X_h)[1]-fcst_diff),]
      x_sample        <- rbind(est_sample, fcst_sample)
      
      ######################### Random Projection  #############################
      y_est    <- Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr]
      y_fcst   <- rep(NA, (dim(x_sample)[1]-length(y_est)))
      y_sample <- c(y_est,y_fcst)
      
      data_RP  <- data.frame(y_sample, x_sample)
      
      formula_RP <- RS_formula_fun(vector(mode="numeric",k),1) 
      
      for (fcst_loop in 1:fcst_av_nr) { #fcst_av_nr                             # Randomly select sub-samples
        
        set.seed(seeds[fcst_loop])                                              # WARNING: set fixed seed for each fcst_loop for exact replication 
        # sample weight matrix from standard normal distribution
        R <- matrix(rnorm(k*FreqRatio*dim(est_sample)[2], 
                          mean = 0, sd = 1), 
                    nrow = dim(x_sample)[2], 
                    ncol = k*FreqRatio)
        
        RP <- lm(formula = formula_RP, data = data.frame(y_sample, (as.matrix(x_sample) %*% R)))
        
        if (i == length(myFiles)) {                                             # Save last iteration coefficients
          save_coeff[[(k_loop-1)*fcst_av_nr*3+(fcst_loop-1)*3+1]] <- RP$coefficients # Every 1st element: coefficients
          save_coeff[[(k_loop-1)*fcst_av_nr*3+(fcst_loop-1)*3+2]] <- R          # Every 1st element: coefficients
          save_coeff[[(k_loop-1)*fcst_av_nr*3+(fcst_loop-1)*3+3]] <- k          # Every 3rd element: number of predictors
        }
        
        ######################## MIDASSO prediction ###########################
        Y_q_fcst <- predict(RP, newdata = data.frame(as.matrix(data_RP[((length(y_est)+1):length(y_sample)),][-1]) %*% R ))
        fcst_av_storage[fcst_loop] <- Y_q_fcst[fcst_diff]
        
      } # fcst average loop
      
      Y_q_fcst <- mean(fcst_av_storage)
      
      ##################### Post-estimation operations #########################
      fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
      row_id <- match(fcst_quarter_id, fcst_vec_id)
      currentMonth <- Date[dim(Date)[1],2]
      col_id <- column_id_nr(currentMonth, Horizon)
      FcstSave[row_id,col_id+3] <- Y_q_fcst
      
      Fcsts[[k_loop]][row_id,col_id+3] <- Y_q_fcst
      
    }
  }
  
  end_time <- Sys.time()
  end_time - start_time
  
  ################### Horizon performance ######################################
  FcstSave_RMSE <- rep(NA, 12)
  a<-1
  for (b in 4:dim(FcstSave)[2]) {
    FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
    a <- a + 1
  }
  save_results[k_loop,] <- as.vector(round(FcstSave_RMSE,2))
  
  ############### Calculate optimal k recursively ##############################
  # Calculate RMSFE up to certain time
  cumulative_RMSE         <- matrix(NA, (length(4:(dim(FcstSave)[1]-6))), 12)
  for (z in 4:(dim(FcstSave)[1]-6)) {
    for (b in 4:dim(FcstSave)[2]) {
      cumulative_RMSE[(z-3),(b-3)]  <- RMSE(FcstSave[(4:z),b],FcstSave[(4:(z)),3])
    }
  }
  
  save_cumulative_results[[k_loop]] <- cumulative_RMSE
  
  k_loop <- k_loop + 1
} # k loop

##################### Save results #############################################
if (saveFlag == 1) {
  setwd(paste0(ROOT,"/Results_public/"))
  save.image(paste0(modelName,".RData")) 
  
  for (k in k_set) {
    fileName_results <- paste("fcst/fcst results ", modelName,"k=",k,".xlsx")
    xlsx::write.xlsx(Fcsts[k-1], file =fileName_results, sheetName="Fcst Results", col.names=TRUE, row.names=TRUE, append=FALSE)
  }
}
#
######################################