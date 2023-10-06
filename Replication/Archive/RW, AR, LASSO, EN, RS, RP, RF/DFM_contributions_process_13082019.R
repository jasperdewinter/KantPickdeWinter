library(xlsx)
library(reshape2)
library(ggplot2)
setwd("C:/Users/Dennis/Documents/Study/Thesis/DFROG/Dennis - Copy (3)/Output/mrt19")
contributions_matrix <- as.matrix(read.xlsx("Weights_overview.xls",1))

# # Average contributions over forecast horizon
# # GM  <- 20:67 ########
# GM  <- 1:64 ########
# FC  <- 65:79
# PFC <- 80:108
# period <- c(GM, FC, PFC)
# period_contributions <- matrix(NA, length(index), 252)

# Horizon predictions
back_avg <- as.numeric(contributions_matrix[(2:109),8])
now_avg  <- as.numeric(contributions_matrix[(114:221),8])
f1_avg   <- as.numeric(contributions_matrix[(226:333),8])
f2_avg   <- as.numeric(contributions_matrix[(338:445),8])
category_contributions <- rep(NA, 540)  # Change according to forecast horizon spec
for (time in 1:108) {
  for (cat in 1:5)
  # category_contributions[(5*(time-1) + cat)] <- as.numeric(contributions_matrix[(time+1),(cat+2)])
    # category_contributions[(5*(time-1) + cat)] <- as.numeric(contributions_matrix[(time+113),(cat+2)])
    # category_contributions[(5*(time-1) + cat)] <- as.numeric(contributions_matrix[(time+225),(cat+2)])
    category_contributions[(5*(time-1) + cat)] <- as.numeric(contributions_matrix[(time+337),(cat+2)])
}

# category_contributions <- unlist(lapply(category_contributions, function(x) as.numeric(x)))

dates <- seq(as.Date("1992-03-01"), as.Date("2018-12-01"), "quarters")


category <- c("Production & Sales", "Surveys", "Financial", "Prices", "Other")
category_rep <- rep(category, 108)
df_back_avg <- rep(f2_avg, each = 5)
df_dates    <- rep(dates, each = 5)

df_plot <- data.frame(df_back_avg,df_dates, category_rep, category_contributions ) # Add all relevant ...
###################### df_back_avg does not work! Category_contributions does not seem to work either... ######

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
  scale_x_date(limits = as.Date(c(dates[1],dates[108-1])))  

p <- p + labs(title = "DFM 2Q ahead contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20), #legend.position = c(0.85,0.15),
legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) + 
labs(fill = "Category", face = "bold")

p
# 
pdf("DFM_contributions_2q.pdf")
p
dev.off()
######################################
