###################### Data examination #################################
#
#                         Dennis Kant
#
#                         19/06/2019
#
######################################################################

######################### Preliminaries ##############################
rm(list=ls())                                                                                              # Clear working environment
# Set Working Directory
setwd("C://Users//Dennis//Documents//Study//Thesis//DFROG//Dennis - Copy (2)//MIDASSO")
dir <- getwd()

# Install and load packages
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
library(ggplot2)
# install.packages("grid")
library(grid)
# install.packages("tikzDevice")
library(tikzDevice)
library(scales)

# Load packages
source('str2vec_quarter.r')
source('m2q.r')
source('pad.r')
source('fcst_goal.r')
source('quarter_diff.r')
source('column_id_nr.r')

# Load data 
myFiles   <- list.files(pattern="NL .*mat")
myFiles   <- naturalsort(myFiles)                                                                          # Sort in natural order


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
Y_ref             <- Y_ref[25:132]

data             <- readMat(myFiles[length(myFiles)])
Y_q              <- data$Yd[!is.nan(data$Yd)]
X                <- data$Xd                                                                    
Date             <- data$DateD

###################################### Support splits #############################
prod_sales <- 1:21
surveys <- 22:57
financial <- 58:65
prices <- 66:79
other <- 80:84

X_prod_sales <- X[,prod_sales]
X_surveys <- X[,surveys]
X_financial <- X[,financial]
X_prices <- X[,prices]
X_other <- X[,other]

prod_sales_mean <- rowMeans(X_prod_sales, na.rm=TRUE)
surveys_mean <- rowMeans(X_surveys, na.rm=TRUE)
financial_mean <- rowMeans(X_financial, na.rm=TRUE)
prices_mean <- rowMeans(X_prices, na.rm=TRUE)
other_mean <- rowMeans(X_other, na.rm=TRUE)


plot(prod_sales_mean,type="l")
plot(surveys_mean,type="l")
plot(financial_mean,type="l")
plot(prices_mean,type="l")
plot(other_mean,type="l")

plot(X[,1],type="l")
lines(1:408, X[,7],type="l")



############## Split data in various time fragments #################
# PGM_ind_m <- rbind(Date[73,], Date[120,])                                                 # Indices for various time fragments in months
# GM_ind_m  <- rbind(Date[121,], Date[264,])
# FC_ind_m  <- rbind(Date[265,], Date[309,])
# PFC_ind_m <- rbind(Date[310,], Date[396,])
# 
# PGM_ind_q <- rbind(fcst_vec[4,], fcst_vec[19,])                                           # Indices for various time fragments in quarters
# GM_ind_q  <- rbind(fcst_vec[20,], fcst_vec[67,])
# FC_ind_q  <- rbind(fcst_vec[68,], fcst_vec[82,])
# PFC_ind_q <- rbind(fcst_vec[83,], fcst_vec[111,])

Y_growth_shape  <- vector(mode="numeric", length(Y_ref))
# Y_growth_shape[which(Y_ref < 0)] <- 1
# Y_growth_shape[which(Y_ref <= 1 & Y_ref > 0)] <- 2
# Y_growth_shape[which(Y_ref > 1)] <- 3

crisis <- c(2:7, 66:70, 80:85)
expansion <- c(12:36, 44:61, 85:101)
Y_growth_shape[crisis] <- 1
Y_growth_shape[expansion] <- 2
Y_growth_shape[-c(crisis,expansion)] <- 3 


############# Y examination over 4 periods ####################

dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")
data_gg <- data.frame(dates = dates, growth = Y_growth_shape, y_ref = Y_ref)

p <- ggplot(data_gg, aes(x=dates, y = y_ref, aes(xmin = as.Date("1991-12-01", "%Y-%m-%d"), 
                                                 xmax = as.Date("2019-03-01",  "%Y-%m-%d")))) +geom_point(aes(shape = factor(growth), y= y_ref, x = dates), size = 3) + geom_line(aes(y= y_ref, x = dates))  +
  geom_vline(xintercept= data_gg$dates[c(64,79)], colour="grey60",linetype = "longdash") 

p <- p + 
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    legend.title = element_blank(), legend.text=element_text(size=9), legend.position = c(0.92,0.14),
    plot.margin = unit(c(1, 1, 26, 1), unit ="pt" ), axis.title.x = element_blank(),
    legend.background = element_rect(color = "black", 
                                     fill = "white", size = 0.5, linetype = "solid") ) +
  scale_x_date(limits = c(as.Date("1991-12-01"), as.Date("2019-03-01")), breaks = pretty_breaks(10) )


p <- p + labs(title = "Real GDP growth", y = "Percentage growth (%)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  scale_shape_manual(labels = c("Crisis", "Expansion", "Regular growth"), 
                     values = c(17, 18, 19))

p 

p <- p  + annotate(geom = "text", x = as.Date("1999-12-01", "%Y-%m-%d"), 
                   y = -4.6, label = "GM", size = 6.5) +
  annotate(geom = "text", x = as.Date("2009-12-01", "%Y-%m-%d"), 
           y = -4.6, label = "FC", size = 6.5)  +
  annotate(geom = "text", x = as.Date("2015-03-01", "%Y-%m-%d"), 
           y = -4.6, label = "PFC", size = 6.5) +
  coord_cartesian(ylim = c(-4, 2), expand = FALSE, clip = "off") 

p



# # 2. Save the plot to a pdf
ggsave("Y_ref_new.pdf", width = 10, height = 5)
  p
  dev.off()

############################ Recession bars ####################################
# 
  
#   
#   recessions_eu.df = read.table(textConnection(
#     "Peak, Trough
#     1992-06-01, 1993-09-01
#     2008-06-01, 2009-06-01
#     2011-12-01, 2013-03-01"), sep=',',
#     colClasses=c('Date', 'Date'), header=TRUE)
# 
#   expansions_eu.df = read.table(textConnection(
#     "Peak, Trough
#     1994-12-01, 2000-04-01
#     2003-03-01, 2007-03-01
#     2013-03-01, 2017-03-01"), sep=',',
#     colClasses=c('Date', 'Date'), header=TRUE)
#   
#   # Source: CEPR business cycle dating (https://cepr.org/content/euro-area-business-cycle-dating-committee)
# q <-  ggplot() +
#         geom_line(aes(x=dates, y=Y_ref)) +geom_point(aes(y= Y_ref, x = dates), size = 3, shape = 18) +
#   theme(panel.grid.major.x = element_blank(),  panel.grid.major.y = element_line( size=.1, color="grey" ),
#         panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
# q
# q <- q + geom_rect(data=recessions_eu.df, aes(xmin=Peak, xmax=Trough,
#                                              ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.2)
q
# 

################################# Performance in economic periods #########################
  setwd("C:/Users/Dennis/Documents/Study/Thesis/Forecast results")
  
  sheetNames <- excel_sheets("Model comparison overview realign forecasts.xlsx")
  fcsts <- vector(mode="list", 9)
  
  for (length in 1:9) {
    fcsts[[length]] <- read.xlsx(file = "Model comparison overview realign forecasts.xlsx",length)[(4:111),-1]
  }
  
  
  
  # set <- c(4:67, 83:dim(fcsts[[1]])[1]-6)
  set <- 1:108
  set <- set[-c(crisis,expansion)]
  set <- crisis
  store <- matrix(NA, 9,11)
  
  for (p in 1:9) {
    FcstSave_RMSE <- rep(NA, 11)
    a<-1
    for (b in 5:dim(fcsts[[1]])[2]) {
      FcstSave_RMSE[a] <- RMSE(fcsts[[p]][set,b],fcsts[[p]][set,3])
      a <- a + 1
    }
    store[p,] <- FcstSave_RMSE
  }

  write.xlsx(store, file = 'RMSE crisis.xlsx', sheetName="RMSE",
             col.names=TRUE, row.names=TRUE, append=FALSE)
  