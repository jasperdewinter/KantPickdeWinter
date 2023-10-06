rm(list=ls())
library(showtext)
library(hexSticker)
library(ggplot2)
library(lattice)

s <- sticker(~plot(cars, cex=.8, cex.axis=.5, mgp=c(0,.6,0), xlab="", ylab=""),
             package="ML Inflation", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
             filename="INFL.png")
s
gc()