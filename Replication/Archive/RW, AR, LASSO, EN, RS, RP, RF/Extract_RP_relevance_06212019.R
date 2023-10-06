#################### Extracting results RP ####################
setwd("C://Users//Dennis//Documents//Study//Thesis//Forecast results//Random subspace")
library(xlsx)
library(MLmetrics)
library(R.matlab)
library(naturalsort)
library(data.table)
library(midasr)

# Load functions
source('str2vec_quarter.r')
source('m2q.r')
source('pad.r')
source('fcst_goal.r')
source('quarter_diff.r')
source('column_id_nr.r')
source('RS_formula_fun.r')

Fcsts <- read.xlsx("fcst results RP.xlsx",1)
Fcsts <- Fcsts[,-(1:31)]
Fcsts <- Fcsts[,-(16:dim(Fcsts)[2])]
setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDASSO")
myFiles   <- list.files(pattern="NL .*mat")
myFiles   <- naturalsort(myFiles)  
Y_ref             <- readMat(myFiles[length(myFiles)])$Yc.final
Y_ref             <- Y_ref[!is.nan(Y_ref)]
Fcsts[1:111,3]    <- Y_ref[22:132]

################### Horizon performance ############################ (TEST)
FcstSave_RMSE <- rep(NA, 12)
a<-1
for (b in 4:dim(Fcsts)[2]) {
  FcstSave_RMSE[a] <- RMSE(Fcsts[(4:(dim(Fcsts)[1]-6)),b],Fcsts[(4:(dim(Fcsts)[1]-6)),3])
  a <- a + 1
}
as.vector(round(FcstSave_RMSE,2))

################## Extract info on predictor relevance ############
y_hat     <- Fcsts[4:111,-(1:4)]
X         <- readMat(myFiles[length(myFiles)])$Xd
LagOrder  <- 2
FreqRatio <- 3

lm(y_hat ~ X)


data             <- readMat(myFiles[length(myFiles)])
Y_q              <- data$Yd[!is.nan(data$Yd)]
X                <- data$Xd                                                                    
Date             <- data$DateD
i <- 330
Horizon <- 0

#for (h in 1:12) {
  # if (h %in% c(1,2,3)) {
  #   Horizon <- -1
  # } else if (h %in% c(4,5,6)) {
  #   Horizon <- 0
  # } else if (h %in% c(7,8,9)) {
  #   Horizon <- 1
  # } else {
  #   Horizon <- 2
  # }
  # Iterate over different forecast horizons
  print(paste('File: ', myFiles[i],'and forecast horizon h: ', h))
  Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
  num_y        <- length(Y_q_shifted)
  
  # Transform X such that skip-sampling can take place (i.e. make a multiple of FreqRatio
  X_fill_nr <- FreqRatio - (dim(X)[1] %% FreqRatio)
  X_fill    <- matrix(NaN, X_fill_nr, dim(X)[2])
  X_sel_complete  <- rbind(X, X_fill)
  
  # Transform X in skip-sampled form
  data_X <- matrix(NA, (dim(X_sel_complete)[1]/FreqRatio), (FreqRatio * dim(X_sel_complete)[2]) )
  for (g in 1:dim(X)[2]) {
    col_id <- g - 1
    data_X[,((1+col_id*FreqRatio):(3+col_id*FreqRatio))] <- fmls(X_sel_complete[,g],LagOrder,FreqRatio)
  }
  
  data_X_na    <- apply(data_X, 2, function(x) sum(is.na(x)))
  del_rows     <- max(data_X_na)
  data_X       <- data_X[1:(dim(data_X)[1]-del_rows),]
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
  x_sample <- rbind(est_sample, fcst_sample)
  
  x_sample <- x_sample[((dim(x_sample)[1]-dim(y_hat)[1] + 1):dim(x_sample)[1]),]
  
  lm(y_hat[,4] ~ x_sample)
  
# }