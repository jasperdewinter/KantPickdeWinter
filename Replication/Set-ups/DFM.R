# ################################################### DFM ################################################### #
#
#                                              Jasper de Winter
#                                                 9/29/2023
#
#
# ########################################################################################################### # 


# ################################### Contribution Graphs ##################################
#

# Load file with outcomes
rm(list=ls())
ROOT <- rprojroot::find_rstudio_root_file()  # !!!! Adjust main path
Sys.setenv(TZ ="UTC")
library(readxl)
library(tidyverse)
library(data.table)

# Create files of average weights & contributions #####
# ------------------------------------------------------

# WARNING: if you run all models code below equals file 1STDIF.DFM WEIGHTS_NL_A loop_ALLSPEC_MEAN.xlsx
# make list of all Weight files
file.list <- list.files(path = paste0(ROOT,"/DFROG/Output/mrt_19/DFROGAllSpecs/"),
                        # pattern = paste0(".*", "WEIGHT", ".*\\.xlsx$"),
                        pattern = paste0("1STDIF.DFM WEIGHTS_NL_A loop_r", ".*\\.xlsx$"),
                        full.names = TRUE)

# create empty list l.df
l.df.Cnt<- list()
l.df.Wgt<- list()

# Read all C_G worksheets for all worksheets in a list (l.df)
for(i in seq_along(file.list)){
  
  # Print the name of the file
  print(paste("Processing file:", i))
  
  df.Cnt <- read_excel(file.list[i], sheet = "C_G", col_names = FALSE)
  l.df.Cnt[[i]] <- df.Cnt
  
  df.Wgt <- read_excel(file.list[i], sheet = "W_G", col_names = FALSE)
  l.df.Wgt[[i]] <- df.Wgt
  
}

# Average all weights files and write to file
Contributions <- Reduce("+", l.df.Cnt) / length(l.df.Cnt)
fwrite(Contributions, paste0(ROOT,"/DFROG/Output/mrt_19/DFROGAllSpecs/average_contributions.csv"))

Weights <- Reduce("+", l.df.Wgt) / length(l.df.Wgt)
fwrite(Weights, paste0(ROOT,"/DFROG/Output/mrt_19/DFROGAllSpecs/average_weights.csv"))



# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"
# WARNING: copy paste into file sheet C_G in "BEW. 1STDIF.DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"



# Set working directory
setwd(paste0(ROOT,"/DFROG/Output/mrt_19/DFROGAllSpecs/")) 

FileName = "BEW. DFM WEIGHTS_NL_A loop_AVERAGE.xlsx"

## Backcast quarterly average ####

# Backcast contributions per period

df_plot <- readxl::read_excel(FileName,
                      sheet = "df_back_avg", 
                      range = "H1:K541")
df_plot$df_dates <- as.Date(df_plot$df_dates)

l_dates <- readxl::read_excel(FileName,
                     sheet = "dates", 
                     range = "A1:B109")
period <- as.integer(l_dates$period)
dates  <- as.character(l_dates$dates)
rm(l_dates)

p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_cast_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "DFM backcast contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/DFM_contributions_back.pdf"))
p
dev.off()
rm(df_plot, period, dates)


## Nowcast quarterly average ####

# Nowcast contributions per period

df_plot <- readxl::read_excel(FileName,
                      sheet = "df_now_avg", 
                      range = "H1:K541")
df_plot$df_dates <- as.Date(df_plot$df_dates)

l_dates <- readxl::read_excel(FileName,
                      sheet = "dates", 
                      range = "A1:B109")
period <- as.integer(l_dates$period)
dates  <- as.character(l_dates$dates)
rm(l_dates)

p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_cast_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "DFM nowcast contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/DFM_contributions_now.pdf"))
p
dev.off()
rm(df_plot, period, dates)


## 1 quarter ahead quarterly average ####

# 1 quarter ahead contributions per period

df_plot <- readxl::read_excel(FileName,
                      sheet = "df_f1_avg", 
                      range = "H1:K541")
df_plot$df_dates <- as.Date(df_plot$df_dates)

l_dates <- readxl::read_excel(FileName,
                      sheet = "dates", 
                      range = "A1:B109")
period <- as.integer(l_dates$period)
dates  <- as.character(l_dates$dates)
rm(l_dates)

p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_cast_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "DFM 1Q ahead contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/DFM_contributions_1q.pdf"))
p
dev.off()
rm(df_plot, period, dates)


## 2 quarter ahead quarterly average ####

# 2 quarter ahead contributions per period

df_plot <- readxl::read_excel(FileName,
                      sheet = "df_f2_avg", 
                      range = "H1:K541")
df_plot$df_dates <- as.Date(df_plot$df_dates)

l_dates <- readxl::read_excel(FileName,
                      sheet = "dates", 
                      range = "A1:B109")
period <- as.integer(l_dates$period)
dates  <- as.character(l_dates$dates)
rm(l_dates)

p <- ggplot(df_plot, aes(x = df_dates, y = category_contributions)) +
  # geom_bar(aes(x = df_dates, fill = category_rep), colour= "black", size = .00001, stat="identity") +
  geom_bar(aes(x = df_dates, fill = category_rep), stat="identity", width=105) +
  geom_line(aes(x= df_dates, df_cast_avg), size = 0.6) + # + geom_line(aes(x=df_dates, df_y), linetype = "dashed")
  scale_fill_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666")) +
  scale_colour_manual(values=c("#9999CC", "brown", "#66CC99","orange","#CC6666"))

p <- p + theme( # remove the vertical grid lines
  panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
  panel.grid.major.y = element_line( size=.1, color="grey" ),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), aspect.ratio=1) +
  scale_x_date(limits = as.Date(c(dates[1],dates[length(period)-1])))

p <- p + labs(title = "DFM 2Q ahead contribution", x = "Years", y = "GDP growth (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26,face="bold"), axis.text=element_text(size=16),
        axis.title=element_text(size=20),# legend.position = c(0.85,0.15),
        legend.position = "bottom", legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid") ) +
  labs(fill = "Category", face = "bold")

p
#
pdf(paste0(ROOT,"/Results/graphs/DFM_contributions_2q.pdf"))
p
dev.off()
rm(df_plot, period, dates)