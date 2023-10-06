######################################## Diebold-Mariano test ############################
rm(list=ls())
library(openxlsx)
library(forecast)

GM  <- 1:64
FC  <- 65:79
PFC <- 80:108
period <- c(GM,FC,PFC)

crisis <- c(2:7, 66:70, 80:85)
expansion <- c(12:36, 44:61, 85:101)
regular <- period[-c(crisis,expansion)]
state <- expansion

# RW
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Common benchmarks")
RW <- read.xlsx("fcst results RW .xlsx",1)[(4:111),(6:16)][period,][state,]

# AR
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Common benchmarks")
AR <- read.xlsx("fcst results AR .xlsx",1)[(4:111),(6:16)][period,][state,]

# DFM
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/DFM")
DFM <- read.xlsx('DFM_fcst_average.xlsx', 1)
DFM_a <- DFM[,-1]
DFM <- DFM_a[1:108,(2:12)][period,][state,]

# MIDAS-F
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/MIDAS-F")
MDF <- read.xlsx("fcst results MIDAS-F R 2 lags .xlsx",1)[,-1]
MDF_save <- vector(mode="list", 126)
for (i in 1:126) {
  MDF_save[[i]] <- MDF[(4:111),(5+15*(i-1)):(15+15*(i-1))]
}
MDF <- Reduce("+", MDF_save) / length(MDF_save)
MDF <- MDF[period,][state,]

# LASSO
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/LASSO, Ridge and EN")
LASSO <- read.xlsx("fcst results LASSO realign .xlsx",1)[-1][(4:111),-(1:4)]
LASSO <- LASSO[period,][state,]

# EN
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/LASSO, Ridge and EN")
EN <- read.xlsx("fcst results EN realign .xlsx",1)[-1][(4:111),-(1:4)]
EN <- EN[period,][state,]

# RS
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
RS <- read.xlsx("RS_fcst_average.xlsx",1)[(4:111),(6:16)]
RS <- RS[period,][state,]

# RP 
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
RP <- read.xlsx("RP_fcst_average.xlsx",1)[(4:111),(6:16)]
RP <- RP[period,][state,]

# RF 
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Machine Learning/RF")
RF <- readRDS('fcst results Random Forest realign 0.9 .rds')
RF <- RF[(4:(dim(RF)[1]-6)),(5:15)]
RF <- data.frame(RF)[period,][state,]

# Y_ref
Y_ref <- DFM_a[period,1][state]

models <- list(RW, AR, DFM, MDF, LASSO, EN, RS, RP, RF)

# General results table
for (j in 1:length(models)){
  models[[j]][,1] <- rowMeans(models[[j]][,(1:2)])
  models[[j]][,2] <- rowMeans(models[[j]][,(3:5)])
  models[[j]][,3] <- rowMeans(models[[j]][,(6:8)])
  models[[j]][,4] <- rowMeans(models[[j]][,((9:11))])
  models[[j]] <- models[[j]][,(1:4)]
}

error_list <- vector(mode="list", 9)
for (j in 1:length(error_list)) {
  error_list[[j]] <- matrix(NA, nrow = 108, ncol = dim(models[[1]])[2])
}

# Calculate errors
for (h in 1:length(models)) {
  for (p in 1:dim(models[[1]])[2]) {
    error_list[[h]][,p] <- abs(matrix(unlist(models[h]), nrow = 108, ncol = dim(models[[1]])[2])[,p]-Y_ref)
  }
}

# Calculate statistical significance relative to RW 
test_results   <- matrix(NA, nrow=dim(models[[1]])[2], ncol = 8)
relative_model <- 3 # index
for (hor in 1:dim(models[[1]])[2]){
  col_index <- 1
  for (mod in (1:(length(models)))[-relative_model]  ){
    if (hor %in% c(1,2)) {
      dm_horizon <- 1
    } else if (hor %in% c(3,4,5)) {
      dm_horizon <- 1
    } else if (hor %in% c(6,7,8)) {
      dm_horizon <- 1
    } else {
      dm_horizon <- 2
    }
    test_results[hor, col_index] <- dm.test(error_list[[relative_model]][,hor], error_list[[mod]][,hor],
                                         alternative = "two.sided", h = dm_horizon, power=1)$p.value
    col_index <- col_index + 1
      }
}
test_results

setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Diebold-Mariano test")
write.xlsx(test_results, "test_results_expansion.xlsx")
# Caution: relative_model is removed, so columns have been shifted!
