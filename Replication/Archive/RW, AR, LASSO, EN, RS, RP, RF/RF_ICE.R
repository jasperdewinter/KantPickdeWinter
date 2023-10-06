###################### Random Forest #################################
#
#                         Dennis Kant
#
#                         08/06/2019
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
# install.packages("randomForest")
library(randomForest)

# Load packages
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
saveFlag   <- 0     
modelName  <- "Random Forest ICE"

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

importance_storage_deltaMSE <- vector(mode="list", (length(myFiles) * length(HorizonSet)) )
importance_storage_purity <- vector(mode="list", (length(myFiles) * length(HorizonSet)) )
importance_storage_SD <- vector(mode="list", (length(myFiles) * length(HorizonSet)) )

#############################################################################

start_time <- Sys.time()

for (i in 342:length(myFiles)) {
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
    
    #Split est_sample in training and validation set to determine number of predictors at all splits
    train_set <- sample(1:nrow(est_sample),(floor((2/3)*nrow(est_sample))))
    est_data <- data.frame(y_est = Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], 
                           est_sample)

    ######################### Random Forest ################

    oob.err=double(dim(est_sample)[2])
    test.err=double(dim(est_sample)[2])
    
    for(mtry in 1:dim(est_sample)[2]) 
    {
      rf=randomForest(y_est ~ . , data = est_data , subset = train_set ,mtry=mtry,ntree=100) 
      oob.err[mtry] = rf$mse[100] #Error of all Trees fitted
      
      pred<-predict(rf,est_data[-train_set,]) #Predictions on Test Set for each Tree
      test.err[mtry]= with(est_data[-train_set,], mean( (y_est - pred)^2)) #Mean Squared Test Error
      
      # cat(mtry," ") #printing the output to the console
      
    }

    # matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
    # legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
    # 
    mtry_opt <- which.min(oob.err)
    RF <- randomForest(y_est ~ . , data = est_data ,mtry=mtry_opt,ntree=400, importance = TRUE)

    importance_storage_deltaMSE[[(i-1)*length(HorizonSet)+h]] <- RF$importance[,1]
    importance_storage_purity[[(i-1)*length(HorizonSet)+h]] <- RF$importance[,2]
    importance_storage_SD[[(i-1)*length(HorizonSet)+h]] <- RF$importanceSD
    

    ######################## MIDASSO prediction #################a
    if (is.null(dim(fcst_sample)[1])) {
      fcst_sample           <- matrix(fcst_sample, 1, length(fcst_sample))
      colnames(fcst_sample) <- colnames(est_data[,-1])
      Y_q_fcst <- predict(RF, newdata = fcst_sample, mtry = mtry_opt, ntree=400)
    } else {
      Y_q_fcst <- predict(RF, newdata = data.frame(fcst_sample), mtry = mtry_opt, ntree=400)
          }
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
  write.xlsx(importance_storage_deltaMSE, file = "fcst importance delta MSE.xlsx", 
             sheetName = "Importance", col.names = TRUE, row.names=TRUE, append = FALSE)
  write.xlsx(importance_storage_purity, file = "fcst importance purity.xlsx", 
             sheetName = "Importance", col.names = TRUE, row.names=TRUE, append = FALSE)
  write.xlsx(importance_storage_SD, file = "fcst importance SD.xlsx", 
             sheetName = "Importance", col.names = TRUE, row.names=TRUE, append = FALSE)
}

################################ Forecast contributions ################################

# 1. create a data frame with just the features
# features <- as.data.frame(splits$valid) %>% select(-Attrition)
features <- as.data.frame(est_data[,-1])

# 2. Create a vector with the actual responses
response <- as.numeric(as.vector(est_data[,1]))

# 3. Create custom predict function that returns the predicted values as a
#    vector (probability of purchasing in our example)
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(RF, newdata ,mtry = mtry_opt, ntree=400))
  return(results)
}

# example of prediction output
pred(RF, features) %>% head()



predictor.RF <- Predictor$new(
  model = RF, 
  data = features, 
  y = response, 
  predict.fun = pred)


# RF model
rf.age1 <- Partial$new(predictor.RF, "X33", ice = TRUE, grid.size = 50)
rf.age2 <- Partial$new(predictor.RF, "X64", ice = TRUE, grid.size = 50)
rf.age3 <- Partial$new(predictor.RF, "X80", ice = TRUE, grid.size = 50)
rf.age1$center(min(features$X33))
rf.age2$center(min(features$X64))
rf.age3$center(min(features$X60))


p1 <- plot(rf.age1) + ggtitle("RF Individual Conditional Expectation") + labs(x ="Consumer confidence")
p2 <- plot(rf.age2) + ggtitle("RF Individual Conditional Expectation") + labs(x ="Share index (AEX)")
p3 <- plot(rf.age3) + ggtitle("RF Individual Conditional Expectation") + labs(x ="Unemployment")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
