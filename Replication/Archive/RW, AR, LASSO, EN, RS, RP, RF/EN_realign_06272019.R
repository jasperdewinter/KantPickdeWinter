###################### MIDASSO Nowcasting ############################
#
#                         Dennis Kant
#
#                         31/05/2019
#
######################################################################

######################### Preliminaries ##############################
rm(list=ls())                                                                                              # Clear working environment
# Set Working Directory
setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDASSO")
dir <- getwd()

# Install and load packages
# install.packages("midasr")                                                                               # MIDAS 
library(midasr)
# install.packages('MLmetrics')                                                                            # RMSE 
library(MLmetrics) 
# install.packages("R.matlab")                                                                             # Read matlab files
library(R.matlab)
# install.packages("naturalsort")                                                                          # Natural sorting for file names
library(naturalsort)
# install.packages("data.table")                                                                           # Shift vectors
library(data.table)
# install.packages("xlsx")                                                                                 # Write XLSX-file
library("xlsx")
# install.packages("glmnet")                                                                               # LASSO 
library(glmnet)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("tikzDevice")
library(tikzDevice)
# install.packages("glmnetUtils")                                                                          # EN regression
library(glmnetUtils)

# Load functions
source('str2vec_quarter.r')
source('m2q.r')
source('pad.r')
source('fcst_goal.r')
source('quarter_diff.r')
source('column_id_nr.r')
source('realign.r')

# Load data 
myFiles   <- list.files(pattern="NL .*mat")
myFiles   <- naturalsort(myFiles)                                                                          # Sort in natural order

# Specify frequency parameters for MIDAS regression
LowFreq    <- 4                                                                                            # Quarterly data (4 times a year)
HighFreq   <- 12                                                                                           # Monthly data (12 times a year)
FreqRatio  <- HighFreq/LowFreq
LagOrder   <- 2                                                                                            # Contemporaneous lag is already included!
HorizonSet <- c(-1,0,1,2)                                                                                  # Horizon for MIDAS models (in quarters)
saveFlag   <- 1     
ENtype     <- 0                                                                                            # 0: lambda_min; 1: lambda_1se
modelName  <- "EN realign"

# Pre-allocate storage
fcst_vec        <- unique(str2vec_quarter(myFiles,2))
fcst_vec_id     <- apply(fcst_vec, 1, function(x) paste(x[1],x[2]) )
FcstSave        <- matrix(NA, nrow = dim(fcst_vec)[1], ncol = (12+3))                                      # Count the first quarter as well and add 3 for h = -1 and h = 2 at the beginning/end of the forecast spectrum. Add three columns to store dates and true Y values
FcstSave[,1:2]  <- fcst_vec
colnames(FcstSave) <- c('Year', 'Quarter', 'Data', 'Backcast(3)', 'Backcast(2)',
                        'Backcast(1)','Nowcast(3)', 'Nowcast(2)','Nowcast(1)',
                        'Forecast 1Q(3)', 'Forecast 1Q(2)','Forecast 1Q(1)',
                        'Forecast 2Q(3)', 'Forecast 2Q(2)','Forecast 2Q(1)')

# Add reference value of low-frequency variable
Y_ref             <- readMat(myFiles[length(myFiles)])$Yc.final
Y_ref             <- Y_ref[!is.nan(Y_ref)]
FcstSave[1:111,3] <- Y_ref[22:132]

# Store cross-validation properties across loop
cv_storage <- vector(mode = "list", (length(myFiles)*length(HorizonSet)))
alpha_storage <- vector(mode = "list", (length(myFiles)*length(HorizonSet)))
#############################################################################

start_time <- Sys.time()

for (i in 1:length(myFiles)) {
  # Load relevant data for iteration
  data             <- readMat(myFiles[i])
  Y_q              <- data$Yd[!is.nan(data$Yd)]
  X                <- data$Xd                                                                    
  Date             <- data$DateD
  
  # Transform X such that skip-sampling can take place (i.e. make a multiple of FreqRatio)
   X_fill_nr <- FreqRatio - (dim(X)[1] %% FreqRatio)
   X_fill    <- matrix(NaN, X_fill_nr, dim(X)[2])
   X         <- rbind(X, X_fill)

  # Transform X in skip-sampled form
  data_X <- matrix(NA, (dim(X)[1]/FreqRatio), (FreqRatio * dim(X)[2]) )
  for (h in 1:dim(X)[2]) {
    col_id <- h - 1
    data_X[,((1+col_id*FreqRatio):(3+col_id*FreqRatio))] <- fmls(X[,h],LagOrder,FreqRatio)
  }
  
  # data_X_na    <- apply(data_X, 2, function(x) sum(is.na(x)))
  # del_rows     <- max(data_X_na)
  # data_X       <- data_X[1:(dim(data_X)[1]-del_rows),]
  
  realign_X <- realign(data_X)
  data_X <- realign_X[[1]][-(1:realign_X[[2]]),]
  
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
    current_moment   <- c(current_year, current_quarter)                                                   # Current date expressed in quarters
    fcst_moment      <- fcst_goal(Horizon, current_moment)
    
    if (Horizon %in% c(1,2)) {                                                                     # If/else to properly read NA's at beginning/end of sample
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
      
    ######################### MIDASSO regression ################
    # Cross-validation
    # MIDASSO_fit_ridge_cv <- cv.glmnet(est_sample, 
    #                             Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], 
    #                             alpha = 0)
    
    EN_cv <- cva.glmnet(est_sample,Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr])

    if (ENtype == 0){
    cv.glmnet.dt <- data.table()                 
    for (k in 1:length(EN_cv$alpha)){
      glmnet.model <- EN_cv$modlist[[k]]
      min.mse <-  min(glmnet.model$cvm)
      min.lambda <- glmnet.model$lambda.min
      alpha.value <- EN_cv$alpha[k]
      new.cv.glmnet.dt <- data.table(alpha=alpha.value,min_mse=min.mse,lambda_EN=min.lambda)
      cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
    }
    best.params <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
    } else if (ENtype == 1) {
      # Extract MSE to find 1se deviation
      # Find lambda corresponding to 1se of lowest CV. Store lambda and the corresponding alpha
      cv.glmnet.dt <- data.table()                 
      for (k in 1:length(EN_cv$alpha)) {
        glmnet.model <- EN_cv$modlist[[k]]
        cvm_low_index <- which.min(glmnet.model$cvm)
        lam_min <- glmnet.model$lambda[cvm_low_index]
        cv_up  <- glmnet.model$cvup[cvm_low_index]
        lam_up <- glmnet.model$lambda.1se
        alpha.value <- EN_cv$alpha[k]
        new.cv.glmnet.dt <- data.table(alpha=alpha.value,se_mse=cv_up,lambda_EN=lam_up)
        cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
      }
      best.params <- cv.glmnet.dt[which.min(cv.glmnet.dt$se_mse)]
    } else {
      warning('Please submit a EN type (lambda_min or lambda_1se).')
    }

    alpha_storage[[(i-1)*length(HorizonSet)+h]] <- best.params$alpha
    
    EN <- glmnet(est_sample, 
           Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], 
           alpha = best.params$alpha, lambda = best.params$lambda_EN)
    
    
    # MIDASSO_fit_cv <- cv.glmnet(est_sample,
    #           Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr],
    #           alpha = alpha_type )
    # 
    # MIDASSO_fit_cv <- cv.glmnet(est_sample, 
    #                             Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr])$glmnet.fit
    # # coefficients_cv <- coef(MIDASSO_fit_cv)
    # cv_storage[[(i-1)*length(HorizonSet)+h]] <- MIDASSO_fit_cv
    
    # Regular fit for chosen lambda
    # MIDASSO_fit <- glmnet(est_sample, 
    #                       Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], 
    #                       alpha = 1, intercept = TRUE)                                                     # alpha = 1: LASSO penalty; alpha = 0: ridge penalty; alpha in (0,1): elastic net 
    # coef(MIDASSO_fit, s = 0.01)                                                                            # Cross-validate this! extract coefficients at a single value of lambda
    
    # r_squared <- function(y, yhat) {
    #   ybar <- mean(y)
    #   ## Total SS
    #   ss_tot <- sum((y - ybar)^2)
    #   ## Residual SS
    #   ss_res <- sum((y - yhat)^2)
    #   ## R^2 = 1 - ss_res/ ss_tot
    #   1 - (ss_res / ss_tot)
    # }
    # ## Function for Adjusted R^2
    # ## n sample size, p number of prameters
    # adj_r_squared <- function(r_squared, n, p) {
    #   1 - (1 - r_squared) * (n - 1) / (n - p - 1)
    # }
    # ## Obtain R^2
    # r_squared_alasso1 <- r_squared(as.vector(y_cont), as.vector(predict(alasso1, newx = x_cont, s = alasso1_cv$lambda.min)))
    # r_squared_alasso1
    
    ######################## MIDASSO prediction #################
    # Cross-validated prediction
    if (is.null(dim(fcst_sample)[1])) {
      Y_q_fcst <- predict(EN, newx = rbind(fcst_sample,fcst_sample),s=best.params$lambda.min )                # s: lambda.min or lambda.1se (for 1 stand. dev. distance)
    } else {
      Y_q_fcst <- predict(EN, newx = fcst_sample,s=best.params$lambda.min )                                           
    }
    # Regular prediction for chosen lambda
    # if (is.null(dim(fcst_sample)[1])) {
    #   Y_q_fcst <- predict(MIDASSO_fit, newx = rbind(fcst_sample,fcst_sample), s=0.1)                        # If/else because glmnet needs a matrix for newx. Provide new regressor values and values for lambda (through cross-validation)
    # } else {
    #   Y_q_fcst <- predict(MIDASSO_fit, newx = fcst_sample, s=0.1)                                           # Provide new regressor values and values for lambda (through cross-validation)
    # }

    ##################### Post-estimation operations ##################
    fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
    row_id <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth <- Date[dim(Date)[1],2]
    col_id <- column_id_nr(currentMonth, Horizon)
    FcstSave[row_id,col_id+3] <- Y_q_fcst[fcst_diff]
    
  }
}

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
  fileName_results <- paste("fcst results", modelName, ".xlsx")
  fileName_RMSE <- paste("fcst RMSE", modelName, ".xlsx")
  setwd("C://Users//Dennis//Documents//Study//Thesis//Forecast results")
  write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
}

################################ CV ##############################
# fit <- MIDASSO_fit$glmnet.fit$df
# fit2 <- log(MIDASSO_fit$glmnet.fit$lambda)
# lm(fit~fit2)
# 
# MIDASSO_fit <- cv_storage[[1336]]
# plot(MIDASSO_fit)
# 
# MIDASSO_fit_cv$glmnet.fit
# plot(MIDASSO_fit_cv$glmnet.fit, xvar = "lambda")
# 
# fit <- fit$df
# cv <- MIDASSO_fit$cvm
# sd <- MIDASSO_fit$cvsd
# lam <- log(MIDASSO_fit$lambda)
# plot(lam,sd)
# 
# data_gg <- data.frame(lam, cv, sd)
# 
# scaleFUN <- function(x) sprintf("%.2f", x)
# 
# p <- ggplot(data_gg) + geom_point(aes(y=cv, x= lam),color = "#C4961A")
# p <- p + geom_errorbar(aes(x=lam, ymin=cv-sd, ymax=cv+sd ),width=0.00005,color = "#AFAFB9")
# p + labs(title = "Cross-validation LASSO", x = "Forecast distance", y = "Average MSE") + theme(legend.position = c(0.8, 0.1))+
#   theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold")) +
#   theme(axis.text=element_text(size=16),
#         axis.title=element_text(size=20)) +
#   theme(legend.title = element_text( size = 18),
#         legend.text = element_text(size = 16)) + scale_y_continuous(labels=scaleFUN) + theme(aspect.ratio=1) +
#   scale_x_continuous(name = "Log(t)", sec.axis = sec_axis(~./(-0.045) - 25, name = "Number of variables included"),
#                      trans = 'reverse')


# ################################ Coefficients ##############################
# #scaleFUN <- function(x) sprintf("%.2f", x)
# data_coeff <- data.frame(cv = coefficients_cv[,100], x = 1:length(coefficient_cv[,100]))
# 
# p <- ggplot(data_coeff) +geom_bar(stat="identity", position="dodge",aes(y=cv, x),color = "#C4961A")  # + geom_point(aes(y=cv, x),color = "#C4961A")
# p + labs(title = "Selection LASSO", x = "Forecast distance", y = "Coefficient") + theme(legend.position = c(0.8, 0.1))+
#   theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold")) +
#   theme(axis.text=element_text(size=16),
#         axis.title=element_text(size=20)) +
#   theme(legend.title = element_text( size = 18),
#         legend.text = element_text(size = 16)) + scale_y_continuous(labels=scaleFUN) + theme(aspect.ratio=1) +
#   scale_x_continuous(name = "Variable number")
# 
