##################################### Plot information adaption #############################
rm(list=ls())
library(xlsx)
library(openxlsx)
library(ggplot2)
library(reshape2)
plot_matrix <- matrix(NA, nrow = 11, ncol = 9)
plot_matrix[,1] <- rev(1:11) #1:11 

# DFM
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/DFM")
DFM <- read.xlsx('DFM_fcst_average.xlsx', 1)
DFM <- DFM[,-1]
plot_matrix[,2] <- colMeans(DFM[1:108,(2:12)])

# MIDAS-F
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/MIDAS-F")
MDF <- read.xlsx("fcst results MIDAS-F R 2 lags .xlsx",1)[,-1]
MDF_save <- vector(mode="list", 126)
for (i in 1:126) {
  MDF_save[[i]] <- MDF[(4:111),(5+15*(i-1)):(15+15*(i-1))]
}
MDF <- Reduce("+", MDF_save) / length(MDF_save)
plot_matrix[,3] <- colMeans(MDF)

# LASSO
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/LASSO, Ridge and EN")
LASSO <- read.xlsx("fcst results LASSO realign .xlsx",1)[-1][(4:111),]
plot_matrix[,4] <- colMeans(LASSO)[-(1:4)]

# EN
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/LASSO, Ridge and EN")
EN <- read.xlsx("fcst results EN realign .xlsx",1)[-1][(4:111),]
plot_matrix[,5] <- colMeans(EN)[-(1:4)]

# RS
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
RS <- read.xlsx("RS_fcst_average.xlsx",1)[(4:111),(6:16)]
plot_matrix[,6] <- colMeans(RS)

# RP 
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
RP <- read.xlsx("RP_fcst_average.xlsx",1)[(4:111),(6:16)]
plot_matrix[,7] <- colMeans(RP)

# RF 
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Machine Learning/RF")
RF <- readRDS('fcst results Random Forest realign 0.9 .rds')
plot_matrix[,8] <- colMeans(RF[(4:(dim(RF)[1]-6)),(5:15)])

# Y_ref
Y_ref <- DFM[(1:108),1]
Y_ref <- mean(Y_ref)
plot_matrix[,9] <- rep(Y_ref, 11)

# Plot
df <- melt(plot_matrix)[-(1:11),]
df <- data.frame(df)

p <- ggplot(df,aes(x = Var1, y = value)) + geom_point(aes(shape = factor(Var2), color = factor(Var2)), size = 5) +
  scale_shape_manual(values = c(2:9))
p

p <- p + theme( # remove the vertical grid lines 
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_y_continuous(limits = c(0.45, 0.70)) #+ scale_x_reverse()
p

p <- p + labs(title = "Information adaption", x = "Horizon", y = "Average GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) + 
  labs(fill = "Category", face = "bold")
p
