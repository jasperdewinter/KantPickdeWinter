rm(list=ls())
library(xlsx)
library(MLmetrics)
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Common benchmarks")

RW_results <- read.xlsx("fcst results RW .xlsx",1)[-2]
RW_results <- RW_results[-(1:73),]

FcstSave_RMSE <- rep(NA, 11)
a <- 1
for (b in 4:15) {
  FcstSave_RMSE[a] <- RMSE(RW_results[(1:(dim(RW_results)[1]-6)),b], RW_results[(1:(dim(RW_results)[1]-6)),3])
  a<- a + 1
}

write.xlsx(Fcsts_RMSE, file ="RW benchmark from 2009M1 onwards.xlsx", sheetName="Fcst Results RMSE", 
           col.names=TRUE, row.names=TRUE, append=FALSE)