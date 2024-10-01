######################## Autoregressive model ##################################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     October 1, 2024
#
################################################################################
#
######################### Preliminaries ##############################
# Estimation
source("ESTIMATION.R")

FcstSave_RMSE     <- FcstSave

start_time <- Sys.time()
for (i in 1:length(myFiles)) {
  # Load relevant data for iteration
  data             <- readMat(myFiles[i])
  Y_q              <- data$Yd[!is.nan(data$Yd)]
  Date             <- data$DateD
  
  for (h in 1:length(HorizonSet)) {
    # Iterate over different forecast horizons
    Horizon <- HorizonSet[h]
    print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
    Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
    num_y        <- length(Y_q_shifted)
    
    # Prepare split into estimation and forecast sample
    current_quarter  <- m2q(Date[dim(Date)[1],dim(Date)[2]])
    current_year     <- Date[dim(Date)[1],1]
    current_moment   <- c(current_year, current_quarter)                        
    fcst_moment      <- fcst_goal(Horizon, current_moment)
    
    if (Horizon %in% c(1,2)) {                                                  # If/else to properly read NA's at beginning/end of sample
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
    
    ############################ Model ######################################
    y_ts <- ts(Y_q_shifted[!is.na(Y_q_shifted)], start = 1986, frequency = 4)
    model_ar <- ar(y_ts, aic = TRUE, order.max = 4)
    
    ############################ Forecast  ##################################
    Y_q_fcst <- predict(model_ar, n.ahead = fcst_diff)$pred
    
    ##################### Post-estimation operations ##################
    fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
    row_id <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth <- Date[dim(Date)[1],2]
    col_id <- column_id_nr(currentMonth, Horizon)
    FcstSave[row_id,col_id+3] <- Y_q_fcst[fcst_diff]
    
  } # HorizonSet loop
  
} # myFiles loop

end_time <- Sys.time()
end_time - start_time

################### Horizon performance ############################
FcstSave_RMSE <- rep(NA, 12)
a<-1
  for (b in 4:dim(FcstSave)[2]) {
    FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
  a <- a + 1
    }
as.vector(round(FcstSave_RMSE,2))[-1]

##################### Save results ###############################
if (saveFlag == 1) {
  
  setwd(paste0(ROOT,"/Results_public/"))
  save.image(paste0(modelName,".RData")) 
  
  fileName_results <- paste("fcst/fcst results", modelName, ".xlsx")
  fileName_RMSE    <- paste("rmsfe/fcst RMSE", modelName, ".xlsx")
  
  xlsx::write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
}
