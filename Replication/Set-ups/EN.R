##################################################### EN ######################################################
#
#                                   Dennis Kant, Andreas Pick and Jasper de Winter
#                                                 3/14/2023
#
###############################################################################################################
#
######################### Preliminaries ##############################
# Estimation
source("Estimation.r")

cv_storage <- vector(mode = "list", (length(myFiles)*length(HorizonSet)))                                 # Store cross-validation properties across loop
alpha_storage <- vector(mode = "list", (length(myFiles)*length(HorizonSet)))                              # Store extent of l1/l2-penalty 
coefficients_storage <- matrix(NA, (length(myFiles) * length(HorizonSet)), 253)                           # Store coefficients
X_con <- vector(mode="list", 1368)                                                                        # Store (realigned) 'data vintages'

#############################################################################

start_time <- Sys.time()

for (i in 1:length(myFiles)) {
  # Load relevant data for iteration
  data   <- readMat(myFiles[i])
  Y_q    <- Process_data(data)[[1]]
  Date   <- Process_data(data)[[2]]
  data_X <- Process_data(data)[[3]]
  
  for (h in 1:length(HorizonSet)) {
    Horizon <- HorizonSet[h]                                                                               # Iterate over different forecast horizons
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
    
    if (Horizon %in% c(1,2)) {                                                                             # If/else to properly read NA's at beginning/end of sample
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
      
    ######################### EN regression ################
    EN_cv <- cva.glmnet(est_sample,Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr])

    if (ENtype == 0){
    cv.glmnet.dt <- data.table()                 
    for (k in 1:length(EN_cv$alpha)){
      glmnet.model <- EN_cv$modlist[[k]]
      min.mse <-  min(glmnet.model$cvm)
      min.lambda <- glmnet.model$lambda.min
      alpha.value <- EN_cv$alpha[k]
      new.cv.glmnet.dt <- data.table(alpha=alpha.value,min_mse=min.mse,lambda_EN=min.lambda)
      cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
    }
    best.params <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
    } else if (ENtype == 1) {
      # Extract MSE to find 1se deviation
      # Find lambda corresponding to 1se of lowest CV. Store lambda and the corresponding alpha
      cv.glmnet.dt <- data.table()                 
      for (k in 1:length(EN_cv$alpha)) {
        glmnet.model <- EN_cv$modlist[[k]]
        cvm_low_index <- which.min(glmnet.model$cvm)
        lam_min <- glmnet.model$lambda[cvm_low_index]
        cv_up  <- glmnet.model$cvup[cvm_low_index]
        lam_up <- glmnet.model$lambda.1se
        alpha.value <- EN_cv$alpha[k]
        new.cv.glmnet.dt <- data.table(alpha=alpha.value,se_mse=cv_up,lambda_EN=lam_up)
        cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
      }
      best.params <- cv.glmnet.dt[which.min(cv.glmnet.dt$se_mse)]
    } else {
      warning('Please submit a EN type (lambda_min or lambda_1se).')
    }

    alpha_storage[[(i-1)*length(HorizonSet)+h]] <- best.params$alpha
    
    EN <- glmnet(est_sample, 
           Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr], 
           alpha = best.params$alpha, lambda = best.params$lambda_EN)
    
    ######################## MIDASSO prediction #################
    if (is.null(dim(fcst_sample)[1])) {                                                                       # Cross-validated prediction
      Y_q_fcst <- predict(EN, newx = rbind(fcst_sample,fcst_sample),s=best.params$lambda.min )                # s: lambda.min or lambda.1se (for 1 stand. dev. distance)
    } else {
      Y_q_fcst <- predict(EN, newx = fcst_sample,s=best.params$lambda.min )                                           
    }
    
    coefficients_storage[(4*(i-1) + h),] <- as.vector(predict(EN, type ="coefficients"))

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
  
  setwd(paste0(ROOT,"/Results/"))
  save.image(paste0(modelName,".RData")) 
  
  fileName_results <- paste("fcst results", modelName, ".xlsx")
  fileName_RMSE    <- paste("fcst RMSE", modelName, ".xlsx")
  fileName_coeff   <- paste("fcst coeff storage", modelName, ".xlsx")
  fileName_vintage <- paste("fcst vintage", modelName, ".xlsx")

  xlsx:: write.xlsx(FcstSave, file=fileName_results, sheetName="Fcst Results", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(FcstSave_RMSE, file =fileName_RMSE, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(coefficients_storage, file = fileName_coeff, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
  xlsx::write.xlsx(X_con, file = fileName_vintage, sheetName="Fcst Results RMSE", col.names=TRUE, row.names=TRUE, append=FALSE)
}

################################### Contributions ##################################
contributions <- matrix(NA, 1368, 253)
for (x in 1:1368) {
  contributions[x,] <- coefficients_storage[x,] * as.vector(c(1,X_con[[x]]))
}
X_matrix <- matrix(NA, 1368,252)
for (z in 1:length(X_con)) {
  X_matrix[z,] <- X_con[[z]]
}


index <- vector(mode="list", 11) # Store relevant indices to retrieve contributions for a specific forecast differential

for (a in 1:length(index)) {
  element_id <- seq(a+4,1368,12)
  index[[a]] <- element_id
}

# Average contributions over forecast horizon
avg_contributions <- matrix(NA, length(index), 253)
for (b in 1:length(index)) {
  avg_contributions[b,] <- colMeans(contributions[index[[b]],])
}

# plot(avg_contributions[1,])
# which(avg_contributions[1,]!=0, arr.ind = T)   # Note: skip-sampled.

# Horizon-specific contributions
back <- colMeans(avg_contributions[(1:2),])
now  <- colMeans(avg_contributions[(3:5),])
f1   <- colMeans(avg_contributions[(6:8),])
f2   <- colMeans(avg_contributions[(9:11),])

GM  <- 20:67
FC  <- 68:82
PFC <- 83:111
period <- PFC
period_contributions <- matrix(NA, length(index), 253)
for (b in 1:length(index)) {
  sample <- index[[b]][4:111]
  period_contributions[b,] <-colMeans(contributions[sample[period][!is.na(sample[period])],])
}

back_GM <- colMeans(period_contributions[(1:2),])
now_GM  <- colMeans(period_contributions[(3:5),])
f1_GM  <- colMeans(period_contributions[(6:8),])
f2_GM  <- colMeans(period_contributions[(9:11),])

back_PFC <- colMeans(period_contributions[(1:2),])
now_PFC  <- colMeans(period_contributions[(3:5),])
f1_PFC  <- colMeans(period_contributions[(6:8),])
f2_PFC  <- colMeans(period_contributions[(9:11),])


# ################################ Coefficients ##############################
scaleFUN <- function(x) sprintf("%.3f", x)
# data_coeff <- data.frame(cv = cbind(back_GM[-1], back_PFC[-1]), x = 1:252)
# data_coeff <- data.frame(cv = cbind(back_GM[-1], back_PFC[-1]))
variable <- c(rep("GM", 252), rep("PFC", 252))
con <- c(f2_GM[-1], f2_PFC[-1])
data_coeff <- data.frame(x = c(1:252 , 1:252), variable, con)

p <- ggplot(data_coeff, aes(x = x)) +geom_bar(stat="identity", position="dodge",
                                              aes(y = con, fill = factor(variable),colour = factor(variable), linetype = factor(variable))
) +
  scale_color_manual(values=c("#000000", "#B7B6B6")) +
  scale_fill_manual(values=c("#000000", "#B7B6B6"))
p

p <- p + labs(title = "EN contributions 2Q ahead", x = "Variable", y = "Contribution") +
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

pdf("EN_contributions_2q.pdf")
# 2. Create a plot
p
dev.off()





