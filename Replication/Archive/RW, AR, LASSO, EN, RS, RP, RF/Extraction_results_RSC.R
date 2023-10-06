setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/Random subspace")
library(xlsx)
library(ggplot2)

avg_coefficients <- read.xlsx("fcst results RSP k7 avg coeff.xlsx",1)
RMSE <- readRDS("fcst RMSE RS - kopie.rds")
Results <- readRDS("fcst results RS - kopie.rds")

RMSE <- readRDS("fcst RMSE RS.rds")
Results <- readRDS("fcst results RS.rds")
opt_k <- readRDS("fcst opt k RS.rds")
cum_RMS <- readRDS("fcst cumulative RMSE RS.rds")

write.xlsx(Results, file ="fcst results RSP k7.xlsx", sheetName="Fcst Results RMSE", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

cum_RMS <- read.xlsx("fcst opt k RP .xlsx",1)
cum_RMS <- cum_RMS[,-1]

k_set <- 2:21
opt_k_matrix <- matrix(NA, 108, 12)
for (t in 1:108) {
  for (p in 1:12) {
    opt_k_matrix[t,p] <- k_set[which.min(cum_RMS[t,seq(p,22*12,12)])]
  }
}

plot(opt_k_matrix[108,], type = "l")

FcstSave_RMSE <- rep(NA, 12)
a<-1
for (b in seq(1,265,12)) {
  FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
  a <- a + 1
}
as.vector(round(FcstSave_RMSE,2))[-1]

opt_k <- opt_k_matrix


plot(opt_k[,6], type = "l")

dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")
data_optimal_k <- data.frame(date = dates, k = opt_k[,2:3])

p <- ggplot() +  geom_line(data = data.frame(opt_k[,10]), aes(y = opt_k[,10], x = dates)) +
  geom_line(data = data.frame(opt_k[,11]), aes(y = opt_k[,11], x = dates), linetype = "dotted")  +
  geom_line(data = data.frame(opt_k[,12]), aes(y = opt_k[,12], x = dates), linetype = "longdash")  +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ),
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    aspect.ratio=1)

p <- p + labs(title = "Forecast 2Q ahead", x = "Years", y = "Optimal k") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20))

p
# 
pdf("opt_k_back_RP.pdf")
# 2. Create a plot
p
# Close the pdf file
dev.off()

#######################################################################################

data_optimal_k <- data.frame(x = 1:252, coeff = avg_coeff)

p <-  ggplot(data =  data_optimal_k, aes(y = coeff, x = x)) +
 geom_bar(stat = "identity") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ),
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))

p <- p + labs(title = "Ex-post optimal k coefficients", x = "Variable number", y = "Coefficient size") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20))

p


# 
pdf("opt_k_coeff.pdf")
# 2. Create a plot
p
# Close the pdf file
dev.off()


ggsave(filename = "opt_k_coeff.pdf", plot = last_plot(), width = 10, height = 5)
#######################################################################
setwd("C://Users//Dennis//Documents//Study//Thesis//Forecast results//Random subspace")
avg_coeff <- readRDS("save_coeff RP.rds")
 save_coeff <- avg_coeff
 
 k_set

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

###################### EXTRACT COEFFICIENTS VIA MATRIX R PROPERLY? ################
data_optimal_coefficients <- data.frame(coef = avg_coeff, var_num = 1:11)

p <- ggplot(data_optimal_coefficients, aes(y = coef, x= var_num)) + geom_bar(stat="identity", position="dodge",width = 0.5) +
  # p <- ggplot(data_optimal_coefficients, aes(y = coef, x= var_num)) + geom_bar(stat="identity", position="dodge") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5) #,
    # aspect.ratio=1) 
  )

p <- p + labs(title = "Selection of variables", x = "Variable number", y = "Coefficient") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20)) 
p

# 2. Save the plot to a pdf
ggsave("RP_k3_coeff.pdf", width = 10, height = 5)

ggsave(name, width = 10, height = 5)

name <- "RP k3 coeff.pdf"
p
dev.off()



#############################################################################################
setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results/DFM/Figures")
f1 <- unlist(readMat("RMSE_avg_1_lag.mat"))
f1_1 <- f1[1]
f1 <- f1/f1_1
f3 <- unlist(readMat("RMSE_avg_3_lag.mat"))
f5 <- unlist(readMat("RMSE_avg_5_lag.mat"))
f3 <- f3/f1_1
f5 <- f5/f1_1

data_optimal_coefficients <- data.frame(f = cbind(f1,f3,f5), var_num = 1:11)

p <- ggplot(data_optimal_coefficients, aes(var_num))  +
   geom_line(aes(y=f1),linetype = "dashed",)           +
  geom_line(aes(y=f3),linetype = "dotted")            +
  geom_line(aes(y=f5))            +
theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ),
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    aspect.ratio=1)


p <- p + labs(title = "Learning curve lags", x = "Forecast distance", y = "Relative RMSFE") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20)) 
p



# 2. Save the plot to a pdf
pdf("DFM_lags.pdf")
p
dev.off()


