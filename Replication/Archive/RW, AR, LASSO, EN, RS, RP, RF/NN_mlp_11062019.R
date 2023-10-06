###################### Neural Network MLP #################################
#
#                         Dennis Kant
#
#                         11/06/2019
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
# install.packages("neuralnet")
library(neuralnet)
# install.packages("plyr")
library(plyr)
# install.packages("RSNNS")
library(RSNNS)
# install.packages("keras")
library(keras)
# install.packages("reticulate")
library(reticulate)


# Load packages
source('str2vec_quarter.r')
source('m2q.r')
source('pad.r')
source('fcst_goal.r')
source('quarter_diff.r')
source('column_id_nr.r')

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
modelName  <- "MLP hidden"

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

# setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//RNN")
# FcstSave <- readRDS("RNN_test.rds")
# setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDASSO")

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
  for (f in 1:dim(X)[2]) {
    col_id <- f - 1
    data_X[,((1+col_id*FreqRatio):(3+col_id*FreqRatio))] <- fmls(X[,f],LagOrder,FreqRatio)
  }
  
  data_X_na    <- apply(data_X, 2, function(x) sum(is.na(x)))
  del_rows     <- max(data_X_na)
  data_X       <- data_X[1:(dim(data_X)[1]-del_rows),]
  
  for (h in 1:length(HorizonSet)) {
    # Iterate over different forecast horizons
    Horizon <- HorizonSet[h]
    print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
    Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
    num_y        <- length(Y_q_shifted)
    data_X_h     <- data_X
    rm(model)
    
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
    
    #Split est_sample in training and validation set to determine number of predictors at all splits
    train_index <- sample(1:nrow(est_sample),(floor((2/3)*nrow(est_sample))))
    train_set <- est_sample[train_index,]
    test_set  <- est_sample[-train_index,]
    est_data <- data.frame(y_est = Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr],
                           est_sample)
    y_est = Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr]
    ######################### Neural Network training ################
    mlp.fit <- mlp(est_sample, y_est, size = c(150, 75, 38, 15))
    
    #################################### Stacked recurrent layer NN #############################
    # # NETWORK TRAINING -> Stacked recurrent layer
    # model <- keras_model_sequential() %>%
    #   layer_gru(units = 32,
    #             dropout = 0.1,
    #             recurrent_dropout = 0.5,
    #             return_sequences = TRUE,
    #             input_shape = list(NULL, dim(est_sample)[2]) ) %>%
    #         layer_gru(units = 64, activation = "relu",
    #             dropout = 0.1,
    #             recurrent_dropout = 0.5) %>%
    #   layer_dense(units = 1)
    # 
    #   
    # model %>% compile(
    #   optimizer = optimizer_rmsprop(),
    #   loss = "mae",
    #   metrics = list('mae') 
    # )
    # 
    # est_sample_rs <- array_reshape(x = est_sample, dim = list(dim(est_sample)[1], 1, 252))
    # 
    # 
    # model %>% fit(
    #   x = list(est_sample_rs),
    #   y = list(y_est),
    #   epochs = 30,
    #   batch_size = 20
    # )
    # 
    # fcst_sample_rs <- array_reshape(x = fcst_sample, dim = list(fcst_diff, 1, 252))
    # 
    # Y_q_fcst <- model %>% predict(fcst_sample_rs, steps = 1) 


    ####################################### Standard NN ##########################
    # n <- names(est_data)
    # nn_formula <- as.formula(paste("y_est ~", paste(n[!n %in% "y_est"], collapse = "+")))
    # nn <- neuralnet(nn_formula, data = est_data, hidden = c(80, 9), linear.output=TRUE)
    ######################## Neural Network prediction #################
    if (fcst_diff == 1) {
      Y_q_fcst <- predict(mlp.fit, newdata = matrix(fcst_sample, 1, length(fcst_sample)))
    } else {
      Y_q_fcst <- predict(mlp.fit, newdata = data.frame(fcst_sample))
                }
    
    # if (fcst_diff == 1) {
    #   Y_q_fcst <- predict(nn, newdata = matrix(fcst_sample, 1, length(fcst_sample)))
    # } else {
    #   Y_q_fcst <- predict(nn, newdata = data.frame(fcst_sample))
    # }
    ##################### Post-estimation operations ##################
    fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
    row_id <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth <- Date[dim(Date)[1],2]
    col_id <- column_id_nr(currentMonth, Horizon)
    FcstSave[row_id,col_id+3] <- Y_q_fcst[fcst_diff]
    
    # setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//RNN")
    # saveRDS(FcstSave, file = paste(modelName, " results.rds"))
    # setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDASSO")
    
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


#### TO DO ####
# Is de MIDAS-approahc nodig in de RF-setting?
# ExtraTrees?
# Note in thesis: RF can not predict values that were not present in training set
# Adding lagging variables to provide connection past-present (?)
# Fix max # of variables used?