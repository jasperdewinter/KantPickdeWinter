###################### RS complete variable Nowcasting ###############
#
#                         Dennis Kant
#
#                        18/06/2019
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
# install.packages("tidyverse")
library(tidyverse)
#library(astsa)


# Load functions
source('str2vec_quarter.r')
source('m2q.r')
source('pad.r')
source('fcst_goal.r')
source('quarter_diff.r')
source('column_id_nr.r')
source('RS_formula_fun.r')
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
fcst_av_nr <- 1000
k_set      <- 2:20                                                                                            # k > 1
modelName  <- "RP 2_20 realign 1000"

# Pre-allocate storage 
fcst_vec        <- unique(str2vec_quarter(myFiles,2))
fcst_vec_id     <- apply(fcst_vec, 1, function(x) paste(x[1],x[2]) )
FcstSave        <- matrix(NA, nrow = dim(fcst_vec)[1], ncol = (12+3))                                      # Count the first quarter as well and add 3 for h = -1 and h = 2 at the beginning/end of the forecast spectrum. Add three columns to store dates and true Y values
FcstSave[,1:2]  <- fcst_vec
colnames(FcstSave) <- c('Year', 'Quarter', 'Data', 'Backcast(3)', 'Backcast(2)',
                        'Backcast(1)','Nowcast(3)', 'Nowcast(2)','Nowcast(1)',
                        'Forecast 1Q(3)', 'Forecast 1Q(2)','Forecast 1Q(1)',
                        'Forecast 2Q(3)', 'Forecast 2Q(2)','Forecast 2Q(1)')
save_results <- matrix(NA, length(k_set), 12)
Fcsts <- vector(mode = "list", length(k_set))
save_cumulative_results <- vector(mode = "list", length(k_set))
save_coeff <- vector(mode = "list", 3*length(k_set)*fcst_av_nr) # Preallocate coefficient list for last estimation sample
for (p in 1:length(k_set)) {
  Fcsts[[p]] <- FcstSave
  save_cumulative_results[[p]] <- matrix(NA, (length(4:(dim(FcstSave)[1]-6))), 12)
}

# Add reference value of low-frequency variable
Y_ref             <- readMat(myFiles[length(myFiles)])$Yc.final
Y_ref             <- Y_ref[!is.nan(Y_ref)]
FcstSave[1:111,3] <- Y_ref[22:132]

#############################################################################

start_time <- Sys.time()

k_loop <- 1
for (k in k_set[-(1:5)]) {
  
  
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
        
        # data_X_na    <- apply(data_X, 2, function(x) sum(is.na(x)))
        # del_rows     <- max(data_X_na)
        # data_X       <- data_X[1:(dim(data_X)[1]-del_rows),]
        # data_X_h     <- data_X
        # 
        realign_X <- realign(data_X)
        data_X_h <- realign_X[[1]][-(1:realign_X[[2]]),]
        
        
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
        
        ######################### Random Subset regression ################
        y_est <- Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr]
        # y_fcst <- rep(NA, fcst_diff)
        y_fcst <- rep(NA, (dim(x_sample)[1]-length(y_est)))
        y_sample <- c(y_est,y_fcst)
        
        data_RS <- data.frame(y_sample, x_sample)
        
        formula_RS <- RS_formula_fun(vector(mode="numeric",k),1) 
        
        for (fcst_loop in 1:fcst_av_nr) { #fcst_av_nr
          
        R <- matrix(rnorm(k*FreqRatio*dim(est_sample)[2], mean = 0, sd = 1), nrow = dim(x_sample)[2], ncol = k*FreqRatio)
        # Postmutliply by R? Or take it in the estimation of the equation?  
        
        # RS <- lm(formula = formula_RS, data = data_RS[1:length(y_est),])
        
        RS <- lm(formula = formula_RS, data = data.frame(y_sample, (as.matrix(x_sample) %*% R)))
        
       if (i == length(myFiles)) {                                                             # Save last iteration coefficients
          save_coeff[[(k_loop-1)*fcst_av_nr*3+(fcst_loop-1)*3+1]] <- RS$coefficients  #every 1st element: coefficients
          save_coeff[[(k_loop-1)*fcst_av_nr*3+(fcst_loop-1)*3+2]] <- R  #every 1st element: coefficients
          save_coeff[[(k_loop-1)*fcst_av_nr*3+(fcst_loop-1)*3+3]] <- k                #every 3rd element: number of predictors
        }
        
        ######################## MIDASSO prediction #################
        # Y_q_fcst <- predict(RS, newdata = data_RS[((length(y_est)+1):length(y_sample)),(2:(3*k+1))])
        
         # Y_q_fcst <- predict(RS, newdata = data.frame(as.matrix(data_RS[((length(y_est)+1):length(y_sample)),(2:(3*k+1))]) %*% R ))
        Y_q_fcst <- predict(RS, newdata = data.frame(as.matrix(data_RS[((length(y_est)+1):length(y_sample)),][-1]) %*% R ))
        
        fcst_av_storage[fcst_loop] <- Y_q_fcst[fcst_diff]
        
      } # fcst average loop
      
      Y_q_fcst <- mean(fcst_av_storage)
      
      ##################### Post-estimation operations ##################
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
  
  ################### Horizon performance ############################
  FcstSave_RMSE <- rep(NA, 12)
  a<-1
  for (b in 4:dim(FcstSave)[2]) {
    FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
    a <- a + 1
  }
  save_results[k_loop,] <- as.vector(round(FcstSave_RMSE,2))
  
  ############### Calculate optimal k recusively ######################
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

# Calculate cumulative RMSE for each data vintage at all values of the subspace (k)
save_k <- matrix(NA, dim(cumulative_RMSE)[1], 12)
for (fcst_distance in 1:12) {
  for (t in 1:dim(cumulative_RMSE)[1]) {
    save_k[t,fcst_distance] <- k_set[which.min( unlist(lapply(save_cumulative_results, function(x) x[t,fcst_distance])) )]
  }
}

# Calculate ex-post optimal coefficients 
index_opt_coeff <- grep(save_k[dim(cumulative_RMSE)[1],4], save_coeff[seq(3,3*length(k_set)*fcst_av_nr,3)])
avg_coeff_matrix <- matrix(NA, length(index_opt_coeff), dim(X)[2]*3 )
for (a in 1:length(index_opt_coeff)){
  ind_coeff   <- save_coeff[[3*index_opt_coeff[a] - 1]]
  x_coeff     <- save_coeff[[3*index_opt_coeff[a] - 2]][-1]
  for (b in 1:length(x_coeff)){
    avg_coeff_matrix[a, ind_coeff[b]] <-  x_coeff[b]
  }
} 

avg_coefficients <- apply(avg_coeff_matrix, 2, function(x) mean(x[!is.na(x)]))
plot(avg_coefficients, type = "h")

##################### Save results ###############################
if (saveFlag == 1) {
  fileName_results <- paste("fcst results ", modelName, ".xlsx")
  fileName_RMSE <- paste("fcst RMSE", modelName, ".xlsx")
  fileName_RMSE_cum <- paste("fcst RMSE cumulative", modelName, ".xlsx")
  fileName_opt_k <- paste("fcst opt k", modelName, ".xlsx")
  
  setwd("C://Users//Dennis//Documents//Study//Thesis//Forecast results")
  write.xlsx(Fcsts, file=fileName_results, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(save_results, file =fileName_RMSE, sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(save_cumulative_results, file =fileName_RMSE_cum, sheetName="Fcst Results RMSE cumulative", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(save_k, file =fileName_opt_k, sheetName="Fcst Results optimal k", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
}

setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
RP_opt <- read.xlsx("fcst opt k RP 2_20 realign 1000 .xlsx",1)

########################### Plot optimal k over time ####################
# Recursive selection
dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")
data_optimal_k <- data.frame(date = dates, k = save_k)

p <- ggplot(data_optimal_k)  + geom_line(aes(y = k.X10, x = dates),linetype = "longdash") +geom_line(aes(y = k.X11, x = dates),linetype = "dotted") +
 geom_line(aes(y = k.X12, x = dates),linetype = "solid") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ),
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    aspect.ratio=1)

p<- p + labs(title = "Forecast 2Q ahead", x = "Years", y = "Optimal k") +
  theme(plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=10),
        axis.title=element_text(size=13))
p

ggsave("RP_opt_k_2q.pdf", width = 4.8, height = 4.8)


########################## Plot RMSFE ##################################
dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")
data_RMSFE <- data.frame(k = k_set, RMSE_k = save_results[,8])

p <- ggplot(data_RMSFE) + geom_line(aes(x = k, y = RMSE_k)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ),
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    aspect.ratio=1)

p + labs(title = "RMSFE for different subspace dimensions", x = "Subspace dimension (k)", y = "RMSFE") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20))

######################### Plot optimal coefficients ###################
data_optimal_coefficients <- data.frame(coef = avg_coefficients, var_num = 1:(FreqRatio*dim(X)[2]))

p <- ggplot(data_optimal_coefficients, aes(y = coef, x= var_num)) + geom_bar(stat="identity", position="dodge",width = 0.1) +
  # p <- ggplot(data_optimal_coefficients, aes(y = coef, x= var_num)) + geom_bar(stat="identity", position="dodge") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    aspect.ratio=1) 

p + labs(title = "Average coefficients for ex-post optimal k ", x = "Variable number", y = "Coefficient") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20)) 
