################################################ Random Forest #####################################################
#
#                                   Dennis Kant, Andreas Pick and Jasper de Winter
#                                                 04/02/2023
#
###############################################################################################################
#
######################### Preliminaries ##############################
# Estimation
source("Estimation.r")

shap_coeff <- matrix(NA, 1368, 252)                                                                        # Store Shapley value coefficients
shap_var   <- matrix(NA, 1368, 252)
shap_feat  <- matrix(NA, 1368, 252)

#############################################################################

start_time <- Sys.time()

for (i in 1:length(myFiles)) {

  # Load relevant data for iteration
  data   <- readMat(myFiles[i])
  Y_q    <- Process_data(data)[[1]]
  Date   <- Process_data(data)[[2]]
  data_X <- Process_data(data)[[3]]
  
  for (h in 1:length(HorizonSet)) {
    
    # Iterate over different forecast horizons
    Horizon <- HorizonSet[h]
    print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
    Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
    num_y        <- length(Y_q_shifted)
    data_X_h     <- data_X
    
    # Prepare split into estimation and forecast sample
    current_quarter  <- m2q(Date[dim(Date)[1],dim(Date)[2]])
    current_year     <- Date[dim(Date)[1],1]
    current_moment   <- c(current_year, current_quarter)                        # Current date expressed in quarters
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
    
    # Split into estimation and forecast sample
    fcst_sample     <- data_X_h[ (dim(data_X_h)[1]-fcst_diff+1) : dim(data_X_h)[1], ]
    est_sample      <- data_X_h[1:(dim(data_X_h)[1]-fcst_diff),]
    
    # Split est_sample in training and validation set to determine number of predictors at all splits
    set.seed(500)
    train_set <- sample(1:nrow(est_sample),(floor((omega)*nrow(est_sample))))
    est_data  <- data.frame(y_est = Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], est_sample)
    
    ######################### Random Forest ################
    
    oob.err  = double(dim(est_sample)[2])
    test.err = double(dim(est_sample)[2])
    
    for(mtry in 1:dim(est_sample)[2]) 
    {
      set.seed(500)
      rf = randomForest(y_est ~ . , data = est_data , subset = train_set, mtry=mtry, ntree=100) 
      oob.err[mtry] = rf$mse[100]                                               # Error of all Trees fitted :: out of bag error; cross validation
      
      pred <- predict(rf,est_data[-train_set,])                                 # Predictions on Test Set for each Tree
      test.err[mtry] = with(est_data[-train_set,], mean( (y_est - pred)^2))     # Mean Squared Test Error
      
    }
    
    mtry_opt <- which.min(oob.err)
    set.seed(500)
    RF <- randomForest(y_est ~ . , data = est_data, mtry=mtry_opt, ntree=400, importance=TRUE)
    
    ######################## MIDASSO prediction #################a
    if (is.null(dim(fcst_sample)[1])) {
      fcst_sample           <- matrix(fcst_sample, 1, length(fcst_sample))
      colnames(fcst_sample) <- colnames(est_data[,-1])
      Y_q_fcst <- predict(RF, newdata = fcst_sample, mtry = mtry_opt, ntree=400)
    } else {
      Y_q_fcst <- predict(RF, newdata = data.frame(fcst_sample), mtry = mtry_opt, ntree=400)
    }
    
    ################################ Forecast contributions ################################
    features <- as.data.frame(rbind(est_sample,fcst_sample))                                                  # 1. create a data frame with just the features
    response <- as.numeric(c(Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], Y_q_fcst))    # 2. Create a vector with the actual responses
    est_data <- data.frame(y_est = response, 
                           features)
    set.seed(500)
    RF <- randomForest(y_est ~ . , data = est_data ,mtry=mtry_opt,ntree=400, importance = TRUE)
    
    pred <- function(model, newdata)  {
      results <- as.data.frame(predict(RF, newdata,mtry = mtry_opt, ntree=400))
      return(results)
    }
    predictor.RF <- Predictor$new(
      model = RF, 
      data = features, 
      y = response, 
      predict.fun = pred)
    
    expl_fcst_nr <- dim(features)[1]
    shapley = Shapley$new(predictor.RF, x.interest = features[expl_fcst_nr,], sample.size = 100)
    shap_explain <- shapley$results
    shap_coeff[((i-1)*4 + h),] <- shap_explain[,2] 
    shap_var[((i-1)*4 + h),]   <- shap_explain[,3]
    shap_feat[((i-1)*4 + h),]  <- as.vector(unlist(features[expl_fcst_nr,]))
    Y_q_fcst <- shapley$y.hat.interest
    
    #####################################################################################
    ##################### Post-estimation operations ##################
    fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
    row_id <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth <- Date[dim(Date)[1],2]
    col_id <- column_id_nr(currentMonth, Horizon)
    FcstSave[row_id,col_id+3] <- Y_q_fcst
    
  }
}

end_time <- Sys.time()
end_time - start_time

################### Horizon performance ############################
FcstSave_RMSE <- rep(NA, 12)
a <- 1
for (b in 4:dim(FcstSave)[2]) {
  FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
  a <- a + 1
}
as.vector(round(FcstSave_RMSE,2))[-1]

##################### Save results ###############################
if (saveFlag == 1) {
  
  setwd(paste0(ROOT,"/Results/"))
  save.image(paste0(modelName,"_",omega,".RData")) 
  
  fileName_results <- paste("fcst results", modelName, ".xlsx")
  fileName_RMSE    <- paste("fcst RMSE", modelName, ".xlsx")
  fileName_coeff   <- paste("fcst coeff", modelName, ".xlsx")
  fileName_feat    <- paste("fcst feat values", modelName, ".xlsx")
  fileName_var     <- paste("fcst coeff var", modelName, ".xlsx")

  write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(shap_coeff, file =fileName_coeff, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(shap_feat, file =fileName_feat, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(shap_var, file =fileName_var, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
}

######################################## Contributions calculations ###############################

# Load file with outcomes
rm(list=ls())
ROOT <- rprojroot::find_rstudio_root_file()  # !!!! Adjust main path
Sys.setenv(TZ ="UTC")
load(paste0(ROOT,"/Results/fcst/Raw RF/0.6/RF_0.6.RData"))
library(tidyverse)
library(data.table)

######################## Contributions #############################
# setwd('C:/Users/Dennis/Documents/Study/Thesis/Analysis/Forecast contributions/RF/Overall results')
# contributions <- read.xlsx('fcst coeff Random Forest Shapley overall.xlsx', 1)[,-1]
# FcstSave <- read.xlsx('fcst results Random Forest Shapley overall.xlsx',1)
contributions <- shap_coeff
index <- vector(mode="list", 11) # Store relevant indices to retrieve contributions for a specific forecast differential

for (a in 1:length(index)) {
  element_id <- seq(a+4, 1368, 12)
  index[[a]] <- element_id
}

# Pre-allocate memory for forecast Great Moderation (GM), Financial crisis (FC) and Post Financial Crisis (PFC)
GM  <- 1:64
FC  <- 65:79
PFC <- 80:108
period <- c(GM, FC, PFC)
period_contributions <- matrix(NA, length(index), 252)

# Indices categories (5 in total)
prod_sales <- 1:63
surveys    <- 64:171
financial  <- 172:195
prices     <- 196:237
other      <- 238:251

# Horizon predictions
back_avg <- rowMeans(FcstSave[(4:111),(5:6)][period,])
now_avg  <- rowMeans(FcstSave[(4:111),(7:9)][period,])
f1_avg   <- rowMeans(FcstSave[(4:111),(10:12)][period,])
f2_avg   <- rowMeans(FcstSave[(4:111),(13:15)][period,])
dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")
