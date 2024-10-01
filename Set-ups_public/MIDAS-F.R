################################### MIDAS-F ####################################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     October 1, 2024
#
################################################################################
#
######################### Preliminaries ##############################
# Estimation
source("Estimation.r")  

# Store cross-validation properties across loop
cv_storage <- vector(mode = "list", (length(myFiles) * length(HorizonSet)))                            # Store cross-validation data
coefficients_storage <- matrix(NA, (length(myFiles) * length(HorizonSet)), 253)                        # Store coefficients
X_con <- vector(mode="list", 1368)                                                                     # Store (realigned) 'data vintages'
nr_forecasts <- matrix(NA, ncol=6)                                                                     # Store forecasts
nr_factors <- 6                                                                                        # set max. nr of factors  
nr_lags <- 6                                                                                           # set max. nr of lags for observation matrix (input for PCA)
FcstSave_list       <- list(FcstSave,FcstSave,FcstSave,FcstSave,FcstSave,FcstSave)                     # create list to save fcst results for each nr_factors specification

start_time <- Sys.time()

for (i in 1:length(myFiles)) {                                                                         # Loop through data vintages
 
  # Load relevant data for iteration
  data   <- readMat(myFiles[i])
  Y_q    <- Process_data_MIDASF(data)[[1]]
  Date   <- Process_data_MIDASF(data)[[2]]
  input_lag_matrix   <- data$Xd

  for (j in 1:nr_lags) {                                                                               # add lags to observation matrix
    if (j == 1) {
      lag_matrix <- rbind(NA, head(input_lag_matrix, -1))
      lag_matrix_output <- cbind(input_lag_matrix, lag_matrix)
    } else {
      lag_matrix <- rbind(NA, head(lag_matrix, -1))
      lag_matrix_output <- cbind(lag_matrix_output, lag_matrix)
          }
  }
  
  data_X2 <- prcomp(na.omit(lag_matrix_output))$x                                                       # PCA extraction

  for (h in 1:length(HorizonSet)) {                                                                     # Iterate over different forecast horizons
   
    for (nr_factors_loop in 1:nr_factors) { 

      data$Xd <- data_X2[,1:nr_factors_loop]                                                            # Only include nr. of factors              
      data_X <- Process_data_MIDASF(data)[[3]]                                                          # Skip-sampling 
   
      Horizon <- HorizonSet[h]
      print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
      Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
      num_y        <- length(Y_q_shifted)
      data_X_h     <- data_X

      # Prepare split into estimation and forecast sample
      current_quarter  <- m2q(Date[dim(Date)[1],dim(Date)[2]])
      current_year     <- Date[dim(Date)[1],1]
      current_moment   <- c(current_year, current_quarter)                                              # Current date expressed in quarters
      fcst_moment      <- fcst_goal(Horizon, current_moment)
  
      if (Horizon %in% c(1,2)) {                                                                        # If/else to properly read NAs at beginning/end of sample
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
  
      ######################### MIDASSO regression #############################################################
  
      MIDASSO_fit_cv <- cv.glmnet(est_sample, Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], alpha = 1) # Cross-validation
      coefficients_storage_test <- as.vector(coef(MIDASSO_fit_cv, s = MIDASSO_fit_cv$lambda.min))
      
      ######################## MIDASSO prediction ##############################################################
  
      if (is.null(dim(fcst_sample)[1])) {
        Y_q_fcst <- predict(MIDASSO_fit_cv, newx = rbind(fcst_sample,fcst_sample),s=MIDASSO_fit_cv$lambda.min )  # s: lambda.min or lambda.1se (for 1 stand. dev. distance)
      } else {
        Y_q_fcst <- predict(MIDASSO_fit_cv, newx = fcst_sample,s=MIDASSO_fit_cv$lambda.min )
      }
    
    nr_forecasts[nr_factors_loop] <- Y_q_fcst[fcst_diff]
    
    fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
    row_id <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth <- Date[dim(Date)[1],2]
    col_id <- column_id_nr(currentMonth, Horizon)
    FcstSave_list[[nr_factors_loop]][row_id,col_id+3] <- nr_forecasts[nr_factors_loop] 
    
    }

    ##################### Post-estimation operations #########################################################
    fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
    row_id <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth <- Date[dim(Date)[1],2]
    col_id <- column_id_nr(currentMonth, Horizon)
    FcstSave[row_id,col_id+3] <- mean(nr_forecasts[!is.na(nr_forecasts)])                                                 

  }
}

end_time <- Sys.time()
end_time - start_time

################### Horizon performance ######################################################################
FcstSave_RMSE <- rep(NA, 12)
a<-1
for (b in 4:dim(FcstSave)[2]) {
  FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
  a <- a + 1
}
as.vector(round(FcstSave_RMSE,2))[-1]

##################### Save results ###########################################################################
if (saveFlag == 1) {

  setwd(paste0(ROOT,"/Results_public/"))
  save.image(paste0(modelName,".RData")) 
  
  fileName_results <- paste0("fcst/fcst results ", modelName, ".xlsx")
  fileName_RMSE    <- paste0("rmsfe/fcst RMSE ", modelName, ".xlsx")
  fileName_vintage <- paste0("other/fcst vintage ", modelName, ".xlsx")

  xlsx::write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
}

##################### Save fcst and performance per nr_factor #####################

for (nr_factors_loop in 1:nr_factors) {
  
  FcstSave_RMSE <- rep(NA, 12)
  a<-1
  for (b in 4:dim(FcstSave_list[[nr_factors_loop]])[2]) {
    FcstSave_RMSE[a] <- RMSE(FcstSave_list[[nr_factors_loop]][(4:(dim(FcstSave_list[[nr_factors_loop]])[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
    a <- a + 1
  }
  as.vector(round(FcstSave_RMSE,2))[-1]
  
  if (saveFlag == 1) {
  
  setwd(paste0(ROOT,"/Results_public/"))
  save.image(paste0(modelName,".RData")) 
  
  fileName_results <- paste0("fcst/fcst results ", modelName, " ", nr_factors_loop, ".xlsx")
  fileName_RMSE    <- paste0("rmsfe/fcst RMSE ", modelName, " ", nr_factors_loop, ".xlsx")

  xlsx::write.xlsx(FcstSave_list[[nr_factors_loop]], file=fileName_results, sheetName="Fcst Results", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
}
  
}