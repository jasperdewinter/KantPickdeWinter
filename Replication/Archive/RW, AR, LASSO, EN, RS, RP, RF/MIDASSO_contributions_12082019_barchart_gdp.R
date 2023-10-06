###################### MIDASSO Nowcasting ############################
#
#                         Dennis Kant
#
#                         26/06/2019
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
modelName  <- "LASSO weights"

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

# Store cross-validation properties across loop
cv_storage <- vector(mode = "list", (length(myFiles)*length(HorizonSet)))
coefficients_storage <- matrix(NA, (length(myFiles) * length(HorizonSet)), 253)     # Store coefficients
X_con <- vector(mode="list", 1368)                                                # Store (realigned) 'data vintages'


#############################################################################

start_time <- Sys.time()

for (i in 1:length(myFiles)) {
  # Load relevant data for iteration
  data             <- readMat(myFiles[i])
  Y_q              <- data$Yd[!is.nan(data$Yd)]
  X                <- data$Xd                                                                    
  Date             <- data$DateD
  
  # Transform X such that skip-sampling can take place (i.e. make a multiple of FreqRatio
  
   X_fill_nr <- FreqRatio - (dim(X)[1] %% FreqRatio)
   X_fill    <- matrix(NaN, X_fill_nr, dim(X)[2])
   X         <- rbind(X, X_fill)

  # Transform X in skip-sampled form
  data_X <- matrix(NA, (dim(X)[1]/FreqRatio), (FreqRatio * dim(X)[2]) )
  for (h in 1:dim(X)[2]) {
    col_id <- h - 1
    data_X[,((1+col_id*FreqRatio):(3+col_id*FreqRatio))] <- fmls(X[,h],LagOrder,FreqRatio)
  }
  
  # data_X_na    <- apply(data_X, 2, function(x) sum(is.na(x)))               # Delete lower NA rows
  # del_rows     <- max(data_X_na)
  # data_X       <- data_X[1:(dim(data_X)[1]-del_rows),]
  
  realign_X <- realign(data_X)
  data_X <- realign_X[[1]][-(1:realign_X[[2]]),]

  for (h in 1:length(HorizonSet)) {
    # Iterate over different forecast horizons
    Horizon <- HorizonSet[h]
    print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
    Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
    num_y        <- length(Y_q_shifted)
    data_X_h     <- data_X
    X_con[[ ((i-1)*4 + h ) ]] <- data_X_h[dim(data_X_h)[1],]
    
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
      
    ######################### MIDASSO regression ################
    # Cross-validation
    MIDASSO_fit_cv <- cv.glmnet(est_sample, 
              Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], alpha = 1)
    # MIDASSO_fit_cv <- cv.glmnet(est_sample,
                                # Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr])$glmnet.fit
    coefficients_storage[(4*(i-1) + h),] <- as.vector(coef(MIDASSO_fit_cv, s = MIDASSO_fit_cv$lambda.min))
    
    # cv_storage[[(i-1)*length(HorizonSet)+h]] <- cv.glmnet(est_sample, 
    #                                                                      Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr])
    # Regular fit for chosen lambda
    # MIDASSO_fit <- glmnet(est_sample,
    #                       Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr],
    #                       alpha = 1, lambda=lambda_cv, intercept = TRUE)                                                     # alpha = 1: LASSO penalty; alpha = 0: ridge penalty; alpha in (0,1): elastic net
    # test <- coef(MIDASSO_fit)
    # plot(test, type = "b") 
    
      # Cross-validate this! extract coefficients at a single value of lambda
    
    ######################## MIDASSO prediction #################
    # Cross-validated prediction
    if (is.null(dim(fcst_sample)[1])) {
      Y_q_fcst <- predict(MIDASSO_fit_cv, newx = rbind(fcst_sample,fcst_sample),s=MIDASSO_fit_cv$lambda.min )                # s: lambda.min or lambda.1se (for 1 stand. dev. distance)
    } else {
      Y_q_fcst <- predict(MIDASSO_fit_cv, newx = fcst_sample,s=MIDASSO_fit_cv$lambda.min )                                           
    }
    # Regular prediction for chosen lambda
    # if (is.null(dim(fcst_sample)[1])) {
    #   Y_q_fcst <- predict(MIDASSO_fit, newx = rbind(fcst_sample,fcst_sample), s=0.1)                        # If/else because glmnet needs a matrix for newx. Provide new regressor values and values for lambda (through cross-validation)
    # } else {
    #   Y_q_fcst <- predict(MIDASSO_fit, newx = fcst_sample, s=0.1)                                           # Provide new regressor values and values for lambda (through cross-validation)
    # }

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
  fileName_coeff <- paste("fcst coeff storage", modelName, ".xlsx")
  fileName_vintage <- paste("fcst vintage", modelName, ".xlsx")
  setwd("C://Users//Dennis//Documents//Study//Thesis//Analysis//Forecast contributions")
  write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(coefficients_storage, file = fileName_coeff, sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(X_con, file = fileName_vintage, sheetName="Fcst Results RMSE", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
}


################################### Contributions ##################################
contributions <- matrix(NA, 1368, 253)
for (x in 1:1368) {
  contributions[x,] <- coefficients_storage[x,] * as.vector(c(1,X_con[[x]]))
}

# X_matrix <- matrix(NA, 1368,252)
# for (z in 1:length(X_con)) {
#   X_matrix[z,] <- X_con[[z]]
# }


index <- vector(mode="list", 11) # Store relevant indices to retrieve contributions for a specific forecast differential

for (a in 1:length(index)) {
  element_id <- seq(a+4,1368,12)
  index[[a]] <- element_id
  }

# Average contributions over forecast horizon
# GM  <- 20:67 ########
GM  <- 1:64 ########
FC  <- 65:79
PFC <- 80:108
period <- c(GM, FC, PFC)
period_contributions <- matrix(NA, length(index), 252)

# Horizon predictions
back_avg <- rowMeans(FcstSave[(4:111),(5:6)][period,])
now_avg  <- rowMeans(FcstSave[(4:111),(7:9)][period,])
f1_avg   <- rowMeans(FcstSave[(4:111),(10:12)][period,])
f2_avg   <- rowMeans(FcstSave[(4:111),(13:15)][period,])
dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")

# avg_contributions <- matrix(NA, length(index), 252)
# period_contributions <- matrix(NA, length(index), 252)
period_contributions <- list(mode="list", 2)
perc_contr <- matrix(NA, length(period), 252)
for (b in 1:2) {

  for (j in 4:111) {
    perc_contr[(j-3),] <- abs(contributions[index[[b]],][j,-1]) / sum( abs( contributions[index[[b]],][j,-1] ) )
  }
  
  perc_contr <- perc_contr[period,]
  # row <- length(perc_contr[ !is.na(perc_contr) ]) / 252
  # perc_matrix <- matrix(perc_contr[ !is.na(perc_contr) ],nrow = row, ncol=252)
  #period_contributions[b,] <- colMeans( perc_matrix )
  period_contributions[[b]] <- perc_contr
  perc_contr <- matrix(NA, length(period), 252)
}

back_contributions <- Reduce("+", period_contributions) / length(period_contributions)
categories <- list(prod_sales,surveys,financial,prices,other)
category_contributions <- rep(NA, length(period)*5)


for (t in 1:length(period)) {
  for (cat_nr in 1:5) {
    cat <- unlist(categories[[cat_nr]])
    category_contributions[(5*(t-1) + cat_nr)] <- sum(back_contributions[t,cat])
  }
  category_contributions[((1+(t-1)*5):(t*5))] <- category_contributions[((1+(t-1)*5):(t*5))] * back_avg[t]
}

category <- c("Production & Sales", "Surveys", "Financial", "Prices", "Other")
category_rep <- rep(category, length(period))
df_back_avg <- rep(back_avg, each = 5)
df_dates    <- rep(dates[period], each = 5)
df_y <- rep(FcstSave[(4:111),3][period], each = 5)


df_plot <- data.frame(df_back_avg, df_y,df_dates, category_rep, category_contributions ) # Add all relevant ...


p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +  
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") + 
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_back_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) + 
                    scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines 
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ),
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))  

p <- p + labs(title = "LASSO backcast contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20), legend.position = c(0.85,0.15),
legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) + 
labs(fill = "Category", face = "bold")

p
# 
pdf("LASSO_contributions_back.pdf")
p
dev.off()
######################################


back_GM <- colMeans(period_contributions[(1:2),])
now_GM  <- colMeans(period_contributions[(3:5),])
f1_GM  <- colMeans(period_contributions[(6:8),])
f2_GM  <- colMeans(period_contributions[(9:11),])

back_FC <- colMeans(period_contributions[(1:2),])
now_FC  <- colMeans(period_contributions[(3:5),])
f1_FC <- colMeans(period_contributions[(6:8),])
f2_FC  <- colMeans(period_contributions[(9:11),])

back_PFC <- colMeans(period_contributions[(1:2),])
now_PFC  <- colMeans(period_contributions[(3:5),])
f1_PFC  <- colMeans(period_contributions[(6:8),])
f2_PFC  <- colMeans(period_contributions[(9:11),])


# Categories
prod_sales <- 1:63
surveys    <- 64:171
financial  <- 172:195
prices     <- 196:237
other      <- 238:252

cat_contribution <- c(sum(back_GM[prod_sales]),sum(back_GM[surveys]),
                      sum(back_GM[financial]),sum(back_GM[prices]),sum(back_GM[other]),
                      sum(back_FC[prod_sales]),sum(back_FC[surveys]),
                      sum(back_FC[financial]),sum(back_FC[prices]),sum(back_FC[other]),
                      sum(back_PFC[prod_sales]),sum(back_PFC[surveys]),
                      sum(back_PFC[financial]),sum(back_PFC[prices]),sum(back_PFC[other])) 

category <- c("P&S", "S", "F", "P", "O")
period <- c(rep("GM",5), rep("FC", 5), rep("PFC", 5))

df <- data.frame(category,cat_contribution, period)
df$period <- factor(df$period, levels = c("GM", "FC", "PFC"))

p <- ggplot(df, aes(category,cat_contribution)) +geom_bar(stat="identity", position="dodge", aes(fill = period))

p <- p + labs(title = "LASSO contributions backcast", x = "Variable", y = "Contribution") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))  #+ 
 # theme(aspect.ratio=1,legend.position = "none")

p <- p + 
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

p


# Save contributions
# setwd("C:/Users/Dennis/Documents/Study/Thesis/Analysis/Forecast contributions/LASSO")
# write.xlsx(X_matrix, file = paste0(modelName," Regressors", ".xlsx"), sheetName="Contributions",
#            col.names=TRUE, row.names=TRUE, append=FALSE)
# write.xlsx(coefficients_storage, file = paste0(modelName," Coefficients", ".xlsx"), sheetName="Contributions",
#            col.names=TRUE, row.names=TRUE, append=FALSE)
# write.xlsx(contributions, file = paste0(modelName," Contributions", ".xlsx"), sheetName="Contributions",
#            col.names=TRUE, row.names=TRUE, append=FALSE)
# write.xlsx(avg_contributions, file = paste0(modelName," Average contributions", ".xlsx"), sheetName="Contributions",
#            col.names=TRUE, row.names=TRUE, append=FALSE)
# 
# write.xlsx(back, file = paste0(modelName," horizon average back", ".xlsx"), sheetName="Contributions",
#            col.names=TRUE, row.names=TRUE, append=FALSE)


# ################################ Coefficients ##############################
scaleFUN <- function(x) sprintf("%.1f", x)
# data_coeff <- data.frame(cv = cbind(back_GM[-1], back_PFC[-1]), x = 1:252)
# data_coeff <- data.frame(cv = cbind(back_GM[-1], back_PFC[-1]))
variable <- c(rep("GM", 252), rep("PFC", 252))
con <- c(back_GM, back_PFC)
data_coeff <- data.frame(x = c(1:252 , 1:252), variable, con)

p <- ggplot(data_coeff, aes(x = x)) +geom_bar(stat="identity", position="dodge",
                                              aes(y = con, fill = factor(variable),colour = factor(variable), linetype = factor(variable))
                                              ) +
  scale_color_manual(values=c("#000000", "#B7B6B6")) +
  scale_fill_manual(values=c("#000000", "#B7B6B6"))
p

p <- p + labs(title = "LASSO contributions backcast", x = "Variable", y = "Contribution") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20)) + scale_y_continuous(labels=scaleFUN) + 
   theme(aspect.ratio=1,legend.position = "none")  +
  scale_x_continuous(name = "Variable number")

p <- p + 
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

p

# ggsave("LASSO_contributions_back.pdf", width = 5, height = 5)

# pdf("LASSO_contributions_back.pdf")
# # 2. Create a plot
# p
# dev.off()
# 


####################################
con_t <- contributions[index[[1]][4:111],]
LASSO_fcst <- 
  plot(Y_t, type = "l")
lines(con_t[,1], lty="dashed")


