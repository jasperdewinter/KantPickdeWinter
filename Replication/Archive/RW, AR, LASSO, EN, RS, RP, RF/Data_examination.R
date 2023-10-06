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
Y_growth_shape[which(Y_ref < 0)] <- 1
Y_growth_shape[which(Y_ref <= 1 & Y_ref > 0)] <- 2
Y_growth_shape[which(Y_ref > 1)] <- 3


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
    legend.title = element_blank(), legend.text=element_text(size=9), legend.position = c(0.87,0.14),
    plot.margin = unit(c(1, 1, 26, 1), unit ="pt" ), axis.title.x = element_blank(),
    legend.background = element_rect(color = "black", 
                                     fill = "white", size = 0.5, linetype = "solid") ) +
scale_x_date(limits = c(as.Date("1991-12-01"), as.Date("2019-03-01")), breaks = pretty_breaks(10) )
  

p <- p + labs(title = "Real GDP growth", y = "Percentage growth (%)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  scale_shape_manual(labels = c("Growth < 0%", "Growth in between 0% and 1%", "Growth > 1%"), 
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


# 2. Save the plot to a pdf
ggsave("Y_ref.pdf", width = 10, height = 5)
  p
  dev.off()



# ################################ Coefficients ##############################
# #scaleFUN <- function(x) sprintf("%.2f", x)
# data_coeff <- data.frame(cv = coefficients_cv[,100], x = 1:length(coefficient_cv[,100]))
# 
# p <- ggplot(data_coeff) +geom_bar(stat="identity", position="dodge",aes(y=cv, x),color = "#C4961A")  # + geom_point(aes(y=cv, x),color = "#C4961A")
# p + labs(title = "Selection LASSO", x = "Forecast distance", y = "Coefficient") + theme(legend.position = c(0.8, 0.1))+
#   theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold")) +
#   theme(axis.text=element_text(size=16),
#         axis.title=element_text(size=20)) +
#   theme(legend.title = element_text( size = 18),
#         legend.text = element_text(size = 16)) + scale_y_continuous(labels=scaleFUN) + theme(aspect.ratio=1) +
#   scale_x_continuous(name = "Variable number")
# 





