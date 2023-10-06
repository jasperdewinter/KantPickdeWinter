##################################### Plot information adaption #############################
rm(list=ls())
library(xlsx)
library(openxlsx)
library(ggplot2)
library(reshape2)
# install.packages("dplyr")
library(dplyr)
plot_matrix <- matrix(NA, nrow = 11, ncol = 8)
plot_matrix[,1] <- rev(1:11) #1:11 
GM  <- 1:64
FC  <- 65:79
PFC <- 80:108
period <- GM

# DFM
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/DFM")
DFM <- read.xlsx('DFM_fcst_average.xlsx', 1)
DFM_a <- DFM[,-1]
DFM <- DFM_a[1:108,(2:12)]
plot_matrix[,2] <- colMeans(DFM[period,])

# MIDAS-F
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/MIDAS-F")
MDF <- read.xlsx("fcst results MIDAS-F R 2 lags .xlsx",1)[,-1]
MDF_save <- vector(mode="list", 126)
for (i in 1:126) {
  MDF_save[[i]] <- MDF[(4:111),(5+15*(i-1)):(15+15*(i-1))]
}
MDF <- Reduce("+", MDF_save) / length(MDF_save)
plot_matrix[,3] <- colMeans(MDF[period,])

# LASSO
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/LASSO, Ridge and EN")
LASSO <- read.xlsx("fcst results LASSO realign .xlsx",1)[-1][(4:111),-(1:4)]
plot_matrix[,4] <- colMeans(LASSO[period,])

# EN
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/LASSO, Ridge and EN")
EN <- read.xlsx("fcst results EN realign .xlsx",1)[-1][(4:111),-(1:4)]
plot_matrix[,5] <- colMeans(EN[period,])

# RS
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
RS <- read.xlsx("RS_fcst_average.xlsx",1)[(4:111),(6:16)]
plot_matrix[,6] <- colMeans(RS[period,])

# RP 
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
RP <- read.xlsx("RP_fcst_average.xlsx",1)[(4:111),(6:16)]
plot_matrix[,7] <- colMeans(RP[period,])

# RF 
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Machine Learning/RF")
RF <- readRDS('fcst results Random Forest realign 0.9 .rds')
RF <- RF[(4:(dim(RF)[1]-6)),(5:15)]
plot_matrix[,8] <- colMeans(RF[period,])

# Y_ref
Y_ref <- DFM_a[period,1]
Y_ref_m <- mean(Y_ref)

# Plot                   
colnames(plot_matrix) <- c("Horizon", "DFM", "MIDAS-F", "LASSO", "EN", "RS", "RP", "RF")
df <- melt(plot_matrix)[-(1:11),]
df$Var1 <-rev(df$Var1)
df <- data.frame(df)

p <- ggplot(df,aes(x = Var1, y = value)) + geom_point(aes(shape = factor(Var2), color = factor(Var2)), size = 3) +
 scale_shape_manual(values = c(2:9)) +
  geom_hline(yintercept = Y_ref_m, linetype= "dashed") 
p

  p <- p + theme( # remove the vertical grid lines 
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1)# +
 # scale_y_continuous(limits = c(0.45, 0.70)) #+ scale_x_reverse()
p

p <- p + labs(title = "Information incorporation", x = "Horizon", y = "Average GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
         legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) + 
  labs(fill = "Category", face = "bold") +labs(col="Model", shape="Model")

p



################### Horizon performance ############################
# RMSE
FcstSave_RMSE <- rep(NA, 11)
library(MLmetrics)
FcstSave <- RF[period,]
a<-1
for (b in 1:dim(FcstSave)[2]) {
  FcstSave_RMSE[a] <- RMSE(FcstSave[,b],Y_ref)
  a <- a + 1
}
as.vector(round(FcstSave_RMSE,2))


# MSE
a<-1
for (b in 1:dim(FcstSave)[2]) {
  FcstSave_RMSE[a] <- MSE(FcstSave[,b],Y_ref)
  a <- a + 1
}
as.vector(round(FcstSave_RMSE,2))





