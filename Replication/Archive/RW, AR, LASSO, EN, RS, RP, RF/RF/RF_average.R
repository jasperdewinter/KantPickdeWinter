library(xlsx)
library(MLmetrics)
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Machine Learning/RF")

RMSE_0.9 <- readRDS('fcst results Random Forest 0.9 .rds')

FcstSave_RMSE <- rep(NA, 12)
a<-1
for (b in 4:dim(RMSE_0.9)[2]) {
  FcstSave_RMSE[a] <- RMSE(RMSE_0.9[(4:(dim(RMSE_0.9)[1]-6)),b],RMSE_0.9[(4:(dim(RMSE_0.9)[1]-6)),3])
  a <- a + 1
}

RMSE_0.9 <- FcstSave_RMSE
write.xlsx(RMSE_0.9, file = "RF ratio 0.9 RMSE.xlsx")

##################################
RMSE_0.6 <- read.xlsx("RF ratio 0.6 RMSE.xlsx",1)
RMSE_0.7 <- read.xlsx("RF ratio 0.7 RMSE.xlsx",1)
RMSE_0.8 <- read.xlsx("RF ratio 0.8 RMSE.xlsx",1)
myList <- list(RMSE_0.6[,-1], RMSE_0.7[,-1], RMSE_0.8[,-1], RMSE_0.9)

add <- function(x) Reduce("+", x)

RMSE_add <- add(myList)

RMSE_average <- RMSE_add/4

write.xlsx(RMSE_average, file = "RF_RMSE_average.xlsx")
