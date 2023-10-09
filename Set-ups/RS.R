##################################################### RS #####################################################
#
#                                   Dennis Kant, Andreas Pick and Jasper de Winter
#                                                 3/14/2023
#
# 13.03.2023: introduced fixed seeds for Random Subsampling
# 13.03.2023: created loop and write a worksheet for each k
#
###############################################################################################################
#
######################### Preliminaries ##############################
# Estimation
source("Estimation.r")

save_results        <- matrix(NA, length(k_set), 12)
Fcsts               <- vector(mode = "list", length(k_set))
contributions_avg   <- matrix(NA, 1368, 253)
seeds               <- c(1:fcst_av_nr)                                          # Fixed seeds

for (p in 1:length(k_set)) {
  Fcsts[[p]] <- FcstSave
}
names(Fcsts) <- k_set

#############################################################################
k_loop <- 1                                                                     # Incr. counter within loop

start_time <- Sys.time()

for (k in k_set) {

  save_coeff          <- matrix(NA, fcst_av_nr, (k*3+1))                        
  X_con               <- matrix(NA, fcst_av_nr, (k*3))                          
  X_value             <- matrix(NA, fcst_av_nr, (k*3))                          
  
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
      
      for (fcst_loop in 1:fcst_av_nr) { #fcst_av_nr                             # randomly select 1,000 sub-samples
        # fcst_loop = 1                                                         # TEMP: for testing    
        # take a sample of k X's from the X-matrix
        set.seed(seeds[fcst_loop])                                              # WARNING: set fixed seed for each fcst_loop for exact replication 
        select_X <- c(sort(sample(1:dim(X)[2], k, replace=F)))                    
        X_selected <- X[,select_X]
        
        # Transform X such that skip-sampling can take place (i.e. make a multiple of FreqRatio
        X_fill_nr <- FreqRatio - (dim(X_selected)[1] %% FreqRatio)
        X_fill    <- matrix(NaN, X_fill_nr, dim(X_selected)[2])
        X_sel_complete  <- rbind(X_selected, X_fill)
        
        # Transform X in skip-sampled form
        data_X <- matrix(NA, (dim(X_sel_complete)[1]/FreqRatio), (FreqRatio * dim(X_sel_complete)[2]) )
        for (g in 1:dim(X_selected)[2]) {
          col_id <- g - 1
          data_X[,((1+col_id*FreqRatio):(3+col_id*FreqRatio))] <- fmls(X_sel_complete[,g],LagOrder,FreqRatio)
        }
        
        realign_X <- realign(data_X)
        data_X_h <- realign_X[[1]][-(1:realign_X[[2]]),]
        
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
        fcst_sample <- data_X_h[ (dim(data_X_h)[1]-fcst_diff+1) : dim(data_X_h)[1], ]
        est_sample  <- data_X_h[1:(dim(data_X_h)[1]-fcst_diff),]
        x_sample    <- rbind(est_sample, fcst_sample)
        
        ######################### Random Subset regression ################
        y_est                  <- Y_q_shifted[(Y_available_nr-dim(est_sample)[1]+1):Y_available_nr]
        # y_fcst <- rep(NA, fcst_diff)
        y_fcst                 <- rep(NA, (dim(x_sample)[1]-length(y_est)))
        y_sample               <- c(y_est,y_fcst)
        
        data_RS                <- data.frame(y_sample, x_sample)
        
        formula_RS             <- RS_formula_fun(c(select_X *3, select_X * 3 - 1, select_X * 3 -2),1) 
        RS                     <- lm(formula = formula_RS, data = data_RS[1:length(y_est),]) 
        
        save_coeff[fcst_loop,] <- RS$coefficients                                                              
        X_con[fcst_loop,]      <- c(select_X *3, select_X * 3 - 1, select_X * 3 -2)                            
        X_value[fcst_loop,]    <- as.numeric(data_RS[(length(y_sample)),(2:(3*k+1))])
        
        ######################## Prediction #################
        Y_q_fcst <- predict(RS, newdata = data_RS[((length(y_est)+1):length(y_sample)),(2:(3*k+1))])
        
        fcst_av_storage[fcst_loop] <- Y_q_fcst[fcst_diff]
        
      } # fcst average loop
      
      contributions <- matrix(NA, fcst_av_nr, 253)
      for (x in 1:fcst_av_nr) {
        contributions[x,c(1,X_con[x,])] <- save_coeff[x,] * as.vector(c(1,X_value[x,]))
      }
      contributions_avg[(i-1)*4 + h,] <- colMeans(contributions, na.rm = TRUE)
      
      Y_q_fcst <- mean(fcst_av_storage)
      
      ##################### Post-estimation operations ##################
      fcst_quarter_id <- paste(fcst_moment[1],fcst_moment[2])
      row_id          <- match(fcst_quarter_id, fcst_vec_id)
      currentMonth    <- Date[dim(Date)[1],2]
      col_id          <- column_id_nr(currentMonth, Horizon)
      FcstSave[row_id,col_id+3] <- Y_q_fcst
      
      Fcsts[[k_loop]][row_id,col_id+3] <- Y_q_fcst
      
    }
  }
  
  end_time <- Sys.time()
  end_time - start_time
  
  k_loop <- k_loop + 1
} # k loop

##################### Save results ###############################
if (saveFlag == 1) {
  setwd(paste0(ROOT,"/Results/"))
  save.image(paste0(modelName,".RData")) 
  
  fileName_results <- paste("fcst results ", modelName,".xlsx")
  fileName_opt_k  <- paste("Variable contributions", modelName,".xlsx")
  
  write.xlsx(Fcsts, file=fileName_results, colNames = TRUE, rowNames = TRUE, append = FALSE, overwrite = T)
  # write.xlsx(contributions_avg, file =fileName_opt_k, sheetName="Variable contributions",
  #            col.names=TRUE, row.names=TRUE, append=FALSE)
}


# ################################### Contribution Graphs ##################################
#

# Load file with outcomes
rm(list=ls())
ROOT <- rprojroot::find_rstudio_root_file()  # !!!! Adjust main path
Sys.setenv(TZ ="UTC")
load(paste0(ROOT,"/Results/fcst/Raw RS, RP. LASSO, EN/RS. k=2,3.RData"))
library(tidyverse)
library(data.table)

######################### Contributions #############################
contributions_avg <- contributions_avg[,-dim(contributions_avg)[2]]
index <- vector(mode="list", 11) # Store relevant indices to retrieve contributions for a specific forecast differential

for (a in 1:length(index)) {
  element_id <- seq(a+4, 1368, 12)
  index[[a]] <- element_id
}

# Pre-allocate memory for forecast Great Moderation (GM), Financial crisis (FC) and Post Financial Crisis (PFC)
GM  <- 1:64 
FC  <- 65:79
PFC <- 80:108
period <- c(GM, FC, PFC)
period_contributions <- matrix(NA, length(index), 252)

# Indices categories (5 in total)
prod_sales <- 1:63
surveys    <- 64:171
financial  <- 172:195
prices     <- 196:237
other      <- 238:251

# Horizon predictions
back_avg <- rowMeans(FcstSave[(4:111),(5:6)][period,])
now_avg  <- rowMeans(FcstSave[(4:111),(7:9)][period,])
f1_avg   <- rowMeans(FcstSave[(4:111),(10:12)][period,])
f2_avg   <- rowMeans(FcstSave[(4:111),(13:15)][period,])
dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")

# Graphs backcast, nowcast, 1 quarter and 2 quarter ahead forecasts ####
# ==================================================================== #

## Backcast quarterly average ####

# Backcast contributions per period

period_contributions <- list(mode="list", 2)
perc_contr <- matrix(NA, length(period), 251)
for (b in 1:2) {
  for (j in 4:111) {
    perc_contr[(j-3),] <- abs(contributions_avg[index[[b]],][j,-1]) / sum( abs( contributions_avg[index[[b]],][j,-1] ) )
  }
  
  perc_contr <- perc_contr[period,]
  period_contributions[[b]] <- perc_contr
  perc_contr <- matrix(NA, length(period), 251)
}

now_contributions <- Reduce("+", period_contributions) / length(period_contributions)
categories <- list(prod_sales, surveys, financial,prices, other)
category_contributions <- rep(NA, length(period)*5)


for (t in 1:length(period)) {
  for (cat_nr in 1:5) {
    cat <- unlist(categories[[cat_nr]])
    category_contributions[(5*(t-1) + cat_nr)] <- sum(now_contributions[t,cat])
  }
  category_contributions[((1+(t-1)*5):(t*5))] <- category_contributions[((1+(t-1)*5):(t*5))] * now_avg[t]
}

category     <- c("Production & Sales", "Surveys", "Financial", "Prices", "Other")
category_rep <- rep(category, length(period))
df_now_avg   <- rep(now_avg, each = 5)
df_dates     <- rep(dates[period], each = 5)


df_plot <- data.frame(df_now_avg, df_dates, category_rep, category_contributions ) # Add all relevant ...


p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_now_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "RS backcast contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/RS_contributions_back.pdf"))
p
dev.off()


## Nowcast quarterly average ####

# Nowcast contributions per period

period_contributions <- list(mode="list", 3)
perc_contr <- matrix(NA, length(period), 251)
for (b in 3:5) {
  
  for (j in 4:111) {
    perc_contr[(j-3),] <- abs(contributions_avg[index[[b]],][j,-1]) / sum( abs( contributions_avg[index[[b]],][j,-1] ) )
  }
  
  perc_contr <- perc_contr[period,]
  # row <- length(perc_contr[ !is.na(perc_contr) ]) / 252
  # perc_matrix <- matrix(perc_contr[ !is.na(perc_contr) ],nrow = row, ncol=252)
  #period_contributions[b,] <- colMeans( perc_matrix )
  period_contributions[[b-2]] <- perc_contr
  perc_contr <- matrix(NA, length(period), 251)
}

now_contributions <- Reduce("+", period_contributions) / length(period_contributions)
categories <- list(prod_sales,surveys,financial,prices,other)
category_contributions <- rep(NA, length(period)*5)


for (t in 1:length(period)) {
  for (cat_nr in 1:5) {
    cat <- unlist(categories[[cat_nr]])
    category_contributions[(5*(t-1) + cat_nr)] <- sum(now_contributions[t,cat])
  }
  category_contributions[((1+(t-1)*5):(t*5))] <- category_contributions[((1+(t-1)*5):(t*5))] * now_avg[t]
}

category <- c("Production & Sales", "Surveys", "Financial", "Prices", "Other")
category_rep <- rep(category, length(period))
df_now_avg <- rep(now_avg, each = 5)
df_dates    <- rep(dates[period], each = 5)


df_plot <- data.frame(df_now_avg, df_dates, category_rep, category_contributions ) # Add all relevant ...


p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_now_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "RS nowcast contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/RS_contributions_now.pdf"))
p
dev.off()

## 1 quarter ahead quarterly average ####

# 1 quarter ahead contributions per period

period_contributions <- list(mode="list", 3)
perc_contr <- matrix(NA, length(period), 251)
for (b in 6:8) {
  
  for (j in 4:111) {
    perc_contr[(j-3),] <- abs(contributions_avg[index[[b]],][j,-1]) / sum( abs( contributions_avg[index[[b]],][j,-1] ) )
  }
  
  perc_contr <- perc_contr[period,]
  # row <- length(perc_contr[ !is.na(perc_contr) ]) / 252
  # perc_matrix <- matrix(perc_contr[ !is.na(perc_contr) ],nrow = row, ncol=252)
  #period_contributions[b,] <- colMeans( perc_matrix )
  period_contributions[[b-5]] <- perc_contr
  perc_contr <- matrix(NA, length(period), 251)
}

f1_contributions <- Reduce("+", period_contributions) / length(period_contributions)
categories <- list(prod_sales,surveys,financial,prices,other)
category_contributions <- rep(NA, length(period)*5)


for (t in 1:length(period)) {
  for (cat_nr in 1:5) {
    cat <- unlist(categories[[cat_nr]])
    category_contributions[(5*(t-1) + cat_nr)] <- sum(f1_contributions[t,cat])
  }
  category_contributions[((1+(t-1)*5):(t*5))] <- category_contributions[((1+(t-1)*5):(t*5))] * f1_avg[t]
}

category <- c("Production & Sales", "Surveys", "Financial", "Prices", "Other")
category_rep <- rep(category, length(period))
df_f1_avg <- rep(f1_avg, each = 5)
df_dates    <- rep(dates[period], each = 5)


df_plot <- data.frame(df_f1_avg, df_dates, category_rep, category_contributions ) # Add all relevant ...


p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_f1_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "RS 1Q ahead contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/RS_contributions_1q.pdf"))
p
dev.off()

## 2 quarter ahead quarterly average ####

# 2 quarter ahead contributions per period

period_contributions <- list(mode="list", 3)
perc_contr <- matrix(NA, length(period), 251)
for (b in 9:11) {

  for (j in 4:111) {
    perc_contr[(j-3),] <- abs(contributions_avg[index[[b]],][j,-1]) / sum( abs( contributions_avg[index[[b]],][j,-1] ) )
  }

  perc_contr <- perc_contr[period,]
  # row <- length(perc_contr[ !is.na(perc_contr) ]) / 252
  # perc_matrix <- matrix(perc_contr[ !is.na(perc_contr) ],nrow = row, ncol=252)
  #period_contributions[b,] <- colMeans( perc_matrix )
  period_contributions[[b-8]] <- perc_contr
  perc_contr <- matrix(NA, length(period), 251)
}

f2_contributions <- Reduce("+", period_contributions) / length(period_contributions)
categories <- list(prod_sales,surveys,financial,prices,other)
category_contributions <- rep(NA, length(period)*5)


for (t in 1:length(period)) {
  for (cat_nr in 1:5) {
    cat <- unlist(categories[[cat_nr]])
    category_contributions[(5*(t-1) + cat_nr)] <- sum(f2_contributions[t,cat])
  }
  category_contributions[((1+(t-1)*5):(t*5))] <- category_contributions[((1+(t-1)*5):(t*5))] * f2_avg[t]
}

category <- c("Production & Sales", "Surveys", "Financial", "Prices", "Other")
category_rep <- rep(category, length(period))
df_f2_avg <- rep(f2_avg, each = 5)
df_dates    <- rep(dates[period], each = 5)


df_plot <- data.frame(df_f2_avg, df_dates, category_rep, category_contributions ) # Add all relevant ...


p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_f2_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "RS 2Q ahead contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/RS_contributions_2q.pdf"))
p
dev.off()
######################################