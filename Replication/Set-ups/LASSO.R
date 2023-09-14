################################################### LASSO #####################################################
#
#                                   Dennis Kant, Andreas Pick and Jasper de Winter
#                                                 3/14/2023
#
###############################################################################################################
#
######################### Preliminaries #######################################################################
# Estimation
source("Estimation.r")

# Store cross-validation properties across loop
cv_storage <- vector(mode = "list", (length(myFiles)*length(HorizonSet)))                              # Store cross-validation data
coefficients_storage <- matrix(NA, (length(myFiles) * length(HorizonSet)), 253)                        # Store coefficients
X_con <- vector(mode="list", 1368)                                                                     # Store (realigned) 'data vintages'

start_time <- Sys.time()

for (i in 1:length(myFiles)) {                                                                         # Loop through data vintages

  # Load relevant data for iteration
  data   <- readMat(myFiles[i])
  Y_q    <- Process_data(data)[[1]]
  Date   <- Process_data(data)[[2]]
  data_X <- Process_data(data)[[3]]

  for (h in 1:length(HorizonSet)) {                                                                     # Iterate over different forecast horizons
    Horizon <- HorizonSet[h]
    print(paste('File: ', myFiles[i],'and forecast horizon: ', Horizon))
    Y_q_shifted  <- shift(Y_q, n = Horizon, type = "shift")
    num_y        <- length(Y_q_shifted)
    data_X_h     <- data_X
    X_con[[ ((i-1)*4 + h ) ]] <- data_X_h[dim(data_X_h)[1],]

    # Prepare split into estimation and forecast sample
    current_quarter  <- m2q(Date[dim(Date)[1],dim(Date)[2]])
    current_year     <- Date[dim(Date)[1],1]
    current_moment   <- c(current_year, current_quarter)                                                # Current date expressed in quarters
    fcst_moment      <- fcst_goal(Horizon, current_moment)

    if (Horizon %in% c(1,2)) {                                                                          # If/else to properly read NAs at beginning/end of sample
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

    ######################### MIDASSO regression #############################################################

    MIDASSO_fit_cv <- cv.glmnet(est_sample, Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], alpha = 1) # Cross-validation
    coefficients_storage[(4*(i-1) + h),] <- as.vector(coef(MIDASSO_fit_cv, s = MIDASSO_fit_cv$lambda.min))

    ######################## MIDASSO prediction ##############################################################

    if (is.null(dim(fcst_sample)[1])) {
      Y_q_fcst <- predict(MIDASSO_fit_cv, newx = rbind(fcst_sample,fcst_sample),s=MIDASSO_fit_cv$lambda.min )  # s: lambda.min or lambda.1se (for 1 stand. dev. distance)
    } else {
      Y_q_fcst <- predict(MIDASSO_fit_cv, newx = fcst_sample,s=MIDASSO_fit_cv$lambda.min )
    }

    ##################### Post-estimation operations #########################################################
    fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
    row_id <- match(fcst_quarter_id, fcst_vec_id)
    currentMonth <- Date[dim(Date)[1],2]
    col_id <- column_id_nr(currentMonth, Horizon)
    FcstSave[row_id,col_id+3] <- Y_q_fcst[fcst_diff]

  }
}

end_time <- Sys.time()
end_time - start_time

################### Horizon performance ######################################################################
FcstSave_RMSE <- rep(NA, 12)
a<-1
for (b in 4:dim(FcstSave)[2]) {
  FcstSave_RMSE[a] <- RMSE(FcstSave[(4:(dim(FcstSave)[1]-6)),b],FcstSave[(4:(dim(FcstSave)[1]-6)),3])
  a <- a + 1
}
as.vector(round(FcstSave_RMSE,2))[-1]

##################### Save results ###########################################################################
if (saveFlag == 1) {

  setwd(paste0(ROOT,"/Results/"))
  save.image(paste0(modelName,".RData")) 
  
  fileName_results <- paste("fcst results", modelName, ".xlsx")
  fileName_RMSE    <- paste("fcst RMSE", modelName, ".xlsx")
  fileName_coeff   <- paste("fcst coeff storage", modelName, ".xlsx")
  fileName_vintage <- paste("fcst vintage", modelName, ".xlsx")

  xlsx::write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(coefficients_storage, file = fileName_coeff, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(X_con, file = fileName_vintage, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
}

################################### Contributions ##################################
# Matrix of constributions
contributions <- matrix(NA, 1368, 253)
for (x in 1:1368) {
  contributions[x,] <- coefficients_storage[x,] * as.vector(c(1,X_con[[x]]))
}

# Store relevant indices to retrieve contributions for a specific forecast differential
index <- vector(mode="list", 11) 

for (a in 1:length(index)) {
  element_id <- seq(a+4,1368,12)
  index[[a]] <- element_id
  }

# Pre-allocate memory for forecast Great Moderation (GM), Financial crisis (FC) and Post Financial Crisis (PFC)
GM     <- 1:64
FC     <- 65:79
PFC    <- 80:108
period <- c(GM, FC, PFC)
period_contributions <- matrix(NA, length(index), 252)

# Indices categories
prod_sales <- 1:63
surveys    <- 64:171
financial  <- 172:195
prices     <- 196:237
other      <- 238:252

# Average backcasts over 2M, Nowcasts, 1Q and 2Q forecasts over 3 months
gdp          <- FcstSave[(4:111),3]
back_avg     <- rowMeans(FcstSave[(4:111),(5:6)][period,])
now_avg      <- rowMeans(FcstSave[(4:111),(7:9)][period,])
f1_avg       <- rowMeans(FcstSave[(4:111),(10:12)][period,])
f2_avg       <- rowMeans(FcstSave[(4:111),(13:15)][period,])
dates        <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")
FcstSave_avg <- cbind(gdp, back_avg, now_avg, f1_avg, f2_avg)
FcstSave_avg <- data.frame(dates, as.data.frame(FcstSave_avg))









############# Graph Backcast contributions #########################

# Backcast contributions per period
period_contributions <- list(mode="list", 2)
perc_contr <- matrix(NA, length(period), 252)
for (b in 1:2) {
    for (j in 4:111) {
    perc_contr[(j-3),] <- abs(contributions[index[[b]],][j,-1]) / sum( abs( contributions[index[[b]],][j,-1] ) )
  }
  perc_contr <- perc_contr[period,]
  period_contributions[[b]] <- perc_contr
  perc_contr <- matrix(NA, length(period), 252)
}

back_contributions     <- Reduce("+", period_contributions) / length(period_contributions)
categories             <- list(prod_sales, surveys, financial, prices, other)
category_contributions <- rep(NA, length(period)*5)

for (t in 1:length(period)) {
  for (cat_nr in 1:5) {
    cat <- unlist(categories[[cat_nr]])
    category_contributions[(5*(t-1) + cat_nr)] <- sum(back_contributions[t,cat])
  }
  category_contributions[((1+(t-1)*5):(t*5))] <- category_contributions[((1+(t-1)*5):(t*5))] * back_avg[t]
}

category     <- c("Production & Sales", "Surveys", "Financial", "Prices", "Other")
category_rep <- rep(category, length(period))
df_back_avg  <- rep(back_avg, each = 5)
df_dates     <- rep(dates[period], each = 5)
df_y         <- rep(FcstSave[(4:111),3][period], each = 5)

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


# back_GM <- colMeans(period_contributions[(1:2),])
# now_GM  <- colMeans(period_contributions[(3:5),])
# f1_GM   <- colMeans(period_contributions[(6:8),])
# f2_GM   <- colMeans(period_contributions[(9:11),])
# 
# back_FC <- colMeans(period_contributions[(1:2),])
# now_FC  <- colMeans(period_contributions[(3:5),])
# f1_FC   <- colMeans(period_contributions[(6:8),])
# f2_FC   <- colMeans(period_contributions[(9:11),])
# 
# back_PFC <- colMeans(period_contributions[(1:2),])
# now_PFC  <- colMeans(period_contributions[(3:5),])
# f1_PFC   <- colMeans(period_contributions[(6:8),])
# f2_PFC   <- colMeans(period_contributions[(9:11),])
# 
# cat_contribution <- c(sum(back_GM[prod_sales]),sum(back_GM[surveys]),
#                       sum(back_GM[financial]),sum(back_GM[prices]),sum(back_GM[other]),
#                       sum(back_FC[prod_sales]),sum(back_FC[surveys]),
#                       sum(back_FC[financial]),sum(back_FC[prices]),sum(back_FC[other]),
#                       sum(back_PFC[prod_sales]),sum(back_PFC[surveys]),
#                       sum(back_PFC[financial]),sum(back_PFC[prices]),sum(back_PFC[other]))
# 
# category <- c("P&S", "S", "F", "P", "O")
# period <- c(rep("GM",5), rep("FC", 5), rep("PFC", 5))
# 
# df <- data.frame(category,cat_contribution, period)
# df$period <- factor(df$period, levels = c("GM", "FC", "PFC"))
# 
# p <- ggplot(df, aes(category,cat_contribution)) +geom_bar(stat="identity", position="dodge", aes(fill = period))
# 
# p <- p + labs(title = "LASSO contributions backcast", x = "Variable", y = "Contribution") +
#   theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold")) +
#   theme(axis.text=element_text(size=16),
#         axis.title=element_text(size=20))  #+
#  # theme(aspect.ratio=1,legend.position = "none")
# 
# p <- p +
#   theme( # remove the vertical grid lines
#     panel.grid.major.x = element_blank() ,
#     # explicitly set the horizontal lines (or they will disappear too)
#     panel.grid.major.y = element_line( size=.1, color="grey" ),
#     panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
# 
# p
# 
# 
# 
# 
# 
# # ################################ Coefficients ##############################
# scaleFUN <- function(x) sprintf("%.1f", x)
# # data_coeff <- data.frame(cv = cbind(back_GM[-1], back_PFC[-1]), x = 1:252)
# # data_coeff <- data.frame(cv = cbind(back_GM[-1], back_PFC[-1]))
# variable <- c(rep("GM", 252), rep("PFC", 252))
# con <- c(back_GM, back_PFC)
# data_coeff <- data.frame(x = c(1:252 , 1:252), variable, con)
# 
# p <- ggplot(data_coeff, aes(x = x)) +geom_bar(stat="identity", position="dodge",
#                                               aes(y = con, fill = factor(variable),colour = factor(variable), linetype = factor(variable))
#                                               ) +
#   scale_color_manual(values=c("#000000", "#B7B6B6")) +
#   scale_fill_manual(values=c("#000000", "#B7B6B6"))
# p
# 
# p <- p + labs(title = "LASSO contributions backcast", x = "Variable", y = "Contribution") +
#   theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold")) +
#   theme(axis.text=element_text(size=16),
#         axis.title=element_text(size=20)) + scale_y_continuous(labels=scaleFUN) +
#    theme(aspect.ratio=1,legend.position = "none")  +
#   scale_x_continuous(name = "Variable number")
# 
# p <- p +
#   theme( # remove the vertical grid lines
#     panel.grid.major.x = element_blank() ,
#     # explicitly set the horizontal lines (or they will disappear too)
#     panel.grid.major.y = element_line( size=.1, color="grey" ),
#     panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
# 
# p
# 
# # ggsave("LASSO_contributions_back.pdf", width = 5, height = 5)
# 
# # pdf("LASSO_contributions_back.pdf")
# # # 2. Create a plot
# # p
# # dev.off()
# #
# 
# 
# ####################################
# con_t <- contributions[index[[1]][4:111],]
# LASSO_fcst <-
#   plot(Y_t, type = "l")
# lines(con_t[,1], lty="dashed")


