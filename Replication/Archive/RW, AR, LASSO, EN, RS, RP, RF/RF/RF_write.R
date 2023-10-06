library(data.table)

setwd("G:/EBO/ECMO/de Winter/Werk/Onderzoek/PROJECT 11.  KANT PICK DE WINTER/Replication/Archive/RW, AR, LASSO, EN, RS, RP, RF/RF/")

RF06 <- readRDS("fcst results Random Forest 0.6 .rds")
RF07 <- readRDS("fcst results Random Forest 0.7 .rds")
RF08 <- readRDS("fcst results Random Forest 0.8 .rds")
RF09 <- readRDS("fcst results Random Forest 0.9 .rds")

fwrite(RF06,"RF 0.6.csv")
fwrite(RF07,"RF 0.7.csv")
fwrite(RF08,"RF 0.8.csv")
fwrite(RF09,"RF 0.9.csv")

