################################ Calculate RMSFEs ##############################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     May 10, 2024
#
#     Reproduce RMSFEs in paper
#
################################################################################
#
source("ESTIMATION.R")
setwd(paste0(ROOT,"/Results/fcst/"))
    

## Read outcomes into R ########################################################
Y      <- readxl::read_excel("fcst results PM.xlsx",     sheet = "Fcst Results")[(EvalStart:EvalEnd),(4)]
PM     <- readxl::read_excel("fcst results PM.xlsx",     sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
AR     <- readxl::read_excel("fcst results AR .xlsx",    sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
DFM    <- readxl::read_excel("fcst results DFM.xlsx",    sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
MDF    <- readxl::read_excel("fcst results MIDAS-F.xlsx",sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
LASSO  <- readxl::read_excel("fcst results LASSO.xlsx",  sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
EN     <- readxl::read_excel("fcst results EN.xlsx",     sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
RS     <- readxl::read_excel("fcst results RS.xlsx",     sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
RP     <- readxl::read_excel("fcst results RP.xlsx",     sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]
RF     <- readxl::read_excel("fcst results RF.xlsx",     sheet = "Fcst Results")[(EvalStart:EvalEnd),(6:16)]


## Define Periods ##############################################################
TOT  <- 1:108

GM   <- 1:65                       # Great Moderation
FC   <- 66:77                      # Financial Crisis
PFC  <- 78:108                     # Post Financial Crisis

# OECD indicator
# https://fred.stlouisfed.org/series/NDLREC
# https://www.oecd.org/sdd/leading-indicators/oecdcompositeleadingindicatorsreferenceturningpointsandcomponentseries.htm

# CONTRACTION VERSUS EXPANSION, OECD INDICATOR
# CRIS = RECESSION, EXP= EXPANSIONARY PERIOD, MOD= MODERATE GROWTH
CRIS <- c(1:8, 66:70, 78:86)       # Recession
EXP  <- c(9:35, 54:65, 87:104)     # Expansionary Period
MOD  <- TOT[-c(CRIS,EXP)]          # Moderate Growth

## Calculate monthly RMSFEs ####################################################
models   <- c("PM","AR","DFM","MDF","LASSO","EN","RS","RP","RF")
horizon  <- c("B2", "B1", "N3", "N2", "N1","1Q3","1Q2","1Q1","2Q3","2Q2","2Q1")

# pre-allocate memory
RMSFE           <- data.frame(matrix(nrow = (7*length(models)), ncol = length(horizon)))
rownames(RMSFE) <- c(models, paste0("GM_",models), paste0("FC_",models), paste0("PFC_",models), paste0("CRIS_",models), paste0("MOD_",models), paste0("EXP_",models))
colnames(RMSFE) <- horizon

df.FIG2            <- data.frame(matrix(nrow = length(models), ncol = length(horizon)))
rownames(df.FIG2) <- c(models)
colnames(df.FIG2) <- horizon

# FIGURE 2
Y_ref   <- data.frame(replicate(11,Y))
df.FIG2[1,] <- sqrt(colMeans((PM - Y_ref)^2))
df.FIG2[2,] <- sqrt(colMeans((AR - Y_ref)^2))
df.FIG2[3,] <- sqrt(colMeans((DFM - Y_ref)^2))
df.FIG2[4,] <- sqrt(colMeans((MDF - Y_ref)^2))
df.FIG2[5,] <- sqrt(colMeans((LASSO - Y_ref)^2))
df.FIG2[6,] <- sqrt(colMeans((EN - Y_ref)^2))
df.FIG2[7,] <- sqrt(colMeans((RS - Y_ref)^2))
df.FIG2[8,] <- sqrt(colMeans((RP - Y_ref)^2))
df.FIG2[9,] <- sqrt(colMeans((RF - Y_ref)^2))

# TOTAL SAMPLE
Y_ref   <- data.frame(replicate(11,Y))
RMSFE[1,] <- sqrt(colMeans((PM - Y_ref)^2))
RMSFE[2,] <- sqrt(colMeans((AR - Y_ref)^2))/RMSFE[1,]
RMSFE[3,] <- sqrt(colMeans((DFM - Y_ref)^2))/RMSFE[1,]
RMSFE[4,] <- sqrt(colMeans((MDF - Y_ref)^2))/RMSFE[1,]
RMSFE[5,] <- sqrt(colMeans((LASSO - Y_ref)^2))/RMSFE[1,]
RMSFE[6,] <- sqrt(colMeans((EN - Y_ref)^2))/RMSFE[1,]
RMSFE[7,] <- sqrt(colMeans((RS - Y_ref)^2))/RMSFE[1,]
RMSFE[8,] <- sqrt(colMeans((RP - Y_ref)^2))/RMSFE[1,]
RMSFE[9,] <- sqrt(colMeans((RF - Y_ref)^2))/RMSFE[1,]
# GREAT MODERATION
Y_ref   <- data.frame(replicate(11,Y[GM,]))
RMSFE[10,] <- sqrt(colMeans((PM[GM,] - Y_ref)^2)) 
RMSFE[11,] <- sqrt(colMeans((AR[GM,] - Y_ref)^2))/RMSFE[10,]
RMSFE[12,] <- sqrt(colMeans((DFM[GM,] - Y_ref)^2))/RMSFE[10,]
RMSFE[13,] <- sqrt(colMeans((MDF[GM,] - Y_ref)^2))/RMSFE[10,]
RMSFE[14,] <- sqrt(colMeans((LASSO[GM,] - Y_ref)^2))/RMSFE[10,]
RMSFE[15,] <- sqrt(colMeans((EN[GM,] - Y_ref)^2))/RMSFE[10,]
RMSFE[16,] <- sqrt(colMeans((RS[GM,] - Y_ref)^2))/RMSFE[10,]
RMSFE[17,] <- sqrt(colMeans((RP[GM,] - Y_ref)^2))/RMSFE[10,]
RMSFE[18,] <- sqrt(colMeans((RF[GM,] - Y_ref)^2))/RMSFE[10,]
# FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[FC,]))
RMSFE[19,] <- sqrt(colMeans((PM[FC,] - Y_ref)^2)) 
RMSFE[20,] <- sqrt(colMeans((AR[FC,] - Y_ref)^2))/RMSFE[19,]
RMSFE[21,] <- sqrt(colMeans((DFM[FC,] - Y_ref)^2))/RMSFE[19,]
RMSFE[22,] <- sqrt(colMeans((MDF[FC,] - Y_ref)^2))/RMSFE[19,]
RMSFE[23,] <- sqrt(colMeans((LASSO[FC,] - Y_ref)^2))/RMSFE[19,]
RMSFE[24,] <- sqrt(colMeans((EN[FC,] - Y_ref)^2))/RMSFE[19,]
RMSFE[25,] <- sqrt(colMeans((RS[FC,] - Y_ref)^2))/RMSFE[19,]
RMSFE[26,] <- sqrt(colMeans((RP[FC,] - Y_ref)^2))/RMSFE[19,]
RMSFE[27,] <- sqrt(colMeans((RF[FC,] - Y_ref)^2))/RMSFE[19,]
# POST FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[PFC,]))
RMSFE[28,] <- sqrt(colMeans((PM[PFC,] - Y_ref)^2)) 
RMSFE[29,] <- sqrt(colMeans((AR[PFC,] - Y_ref)^2))/RMSFE[28,]
RMSFE[30,] <- sqrt(colMeans((DFM[PFC,] - Y_ref)^2))/RMSFE[28,]
RMSFE[31,] <- sqrt(colMeans((MDF[PFC,] - Y_ref)^2))/RMSFE[28,]
RMSFE[32,] <- sqrt(colMeans((LASSO[PFC,] - Y_ref)^2))/RMSFE[28,]
RMSFE[33,] <- sqrt(colMeans((EN[PFC,] - Y_ref)^2))/RMSFE[28,]
RMSFE[34,] <- sqrt(colMeans((RS[PFC,] - Y_ref)^2))/RMSFE[28,]
RMSFE[35,] <- sqrt(colMeans((RP[PFC,] - Y_ref)^2))/RMSFE[28,]
RMSFE[36,] <- sqrt(colMeans((RF[PFC,] - Y_ref)^2))/RMSFE[28,]
# CRISIS
Y_ref   <- data.frame(replicate(11,Y[CRIS,]))
RMSFE[37,] <- sqrt(colMeans((PM[CRIS,] - Y_ref)^2)) 
RMSFE[38,] <- sqrt(colMeans((AR[CRIS,] - Y_ref)^2))/RMSFE[37,]
RMSFE[39,] <- sqrt(colMeans((DFM[CRIS,] - Y_ref)^2))/RMSFE[37,]
RMSFE[40,] <- sqrt(colMeans((MDF[CRIS,] - Y_ref)^2))/RMSFE[37,]
RMSFE[41,] <- sqrt(colMeans((LASSO[CRIS,] - Y_ref)^2))/RMSFE[37,]
RMSFE[42,] <- sqrt(colMeans((EN[CRIS,] - Y_ref)^2))/RMSFE[37,]
RMSFE[43,] <- sqrt(colMeans((RS[CRIS,] - Y_ref)^2))/RMSFE[37,]
RMSFE[44,] <- sqrt(colMeans((RP[CRIS,] - Y_ref)^2))/RMSFE[37,]
RMSFE[45,] <- sqrt(colMeans((RF[CRIS,] - Y_ref)^2))/RMSFE[37,]
# MODERATE
Y_ref   <- data.frame(replicate(11,Y[MOD,]))
RMSFE[46,] <- sqrt(colMeans((PM[MOD,] - Y_ref)^2)) 
RMSFE[47,] <- sqrt(colMeans((AR[MOD,] - Y_ref)^2))/RMSFE[46,]
RMSFE[48,] <- sqrt(colMeans((DFM[MOD,] - Y_ref)^2))/RMSFE[46,]
RMSFE[49,] <- sqrt(colMeans((MDF[MOD,] - Y_ref)^2))/RMSFE[46,]
RMSFE[50,] <- sqrt(colMeans((LASSO[MOD,] - Y_ref)^2))/RMSFE[46,]
RMSFE[51,] <- sqrt(colMeans((EN[MOD,] - Y_ref)^2))/RMSFE[46,]
RMSFE[52,] <- sqrt(colMeans((RS[MOD,] - Y_ref)^2))/RMSFE[46,]
RMSFE[53,] <- sqrt(colMeans((RP[MOD,] - Y_ref)^2))/RMSFE[46,]
RMSFE[54,] <- sqrt(colMeans((RF[MOD,] - Y_ref)^2))/RMSFE[46,]
# EXPANSION
Y_ref   <- data.frame(replicate(11,Y[EXP,]))
RMSFE[55,] <- sqrt(colMeans((PM[EXP,] - Y_ref)^2)) 
RMSFE[56,] <- sqrt(colMeans((AR[EXP,] - Y_ref)^2))/RMSFE[55,]
RMSFE[57,] <- sqrt(colMeans((DFM[EXP,] - Y_ref)^2))/RMSFE[55,]
RMSFE[58,] <- sqrt(colMeans((MDF[EXP,] - Y_ref)^2))/RMSFE[55,]
RMSFE[59,] <- sqrt(colMeans((LASSO[EXP,] - Y_ref)^2))/RMSFE[55,]
RMSFE[60,] <- sqrt(colMeans((EN[EXP,] - Y_ref)^2))/RMSFE[55,]
RMSFE[61,] <- sqrt(colMeans((RS[EXP,] - Y_ref)^2))/RMSFE[55,]
RMSFE[62,] <- sqrt(colMeans((RP[EXP,] - Y_ref)^2))/RMSFE[55,]
RMSFE[63,] <- sqrt(colMeans((RF[EXP,] - Y_ref)^2))/RMSFE[55,]

# put in right format for tables in paper
RMSFE     <- t(RMSFE)
RMSFE     <- RMSFE[rev(rownames(RMSFE)),]
RMSFE     <- round(RMSFE, digits = 2)
df.RMSFE  <- data.frame(rbind(RMSFE[,  1:9], 
                              RMSFE[,10:18],
                              RMSFE[,19:27],
                              RMSFE[,28:36],
                              RMSFE[,37:45],
                              RMSFE[,46:54],
                              RMSFE[,55:63])) 
horizon <-rev(horizon)
rownames(df.RMSFE) <- c(horizon, paste0("GM_",horizon), paste0("FC_",horizon), paste0("PFC_",horizon), paste0("CRIS_",horizon), paste0("MOD_",horizon), paste0("EXP_",horizon))

## Calcuate Quarterly RMSFEs ###################################################

models   <- c("PM","AR","DFM","MDF","LASSO","EN","RS","RP","RF")
modelsQ   <- c(models, paste0("GM_",models), paste0("FC_",models), paste0("PFC_",models), paste0("CRIS_",models), paste0("MOD_",models), paste0("EXP_",models))
horizonQ  <- c("2Q", "1Q", "N","B")

RMSFEQ    <- matrix(nrow = 4 , ncol = length(modelsQ))
colnames(RMSFEQ) = modelsQ
rownames(RMSFEQ) = horizonQ

# average monthly RMSFEs to quarterly RMSFEs
RMSFEQ [1, ] <- colMeans(RMSFE[1:3,])    # 2Q forecasts
RMSFEQ [2, ] <- colMeans(RMSFE[4:6,])    # 1Q forecasts
RMSFEQ [3, ] <- colMeans(RMSFE[7:9,])    # Nowcast
RMSFEQ [4, ] <- colMeans(RMSFE[10:11,])  # Backcast

df.RMSFEQ  <- data.frame(rbind(RMSFEQ[,1:9], 
                               RMSFEQ[,10:18],
                               RMSFEQ[,19:27],
                               RMSFEQ[,28:36],
                               RMSFEQ[,37:45],
                               RMSFEQ[,46:54],
                               RMSFEQ[,55:63])) 

df.RMSFEQ  <- round(df.RMSFEQ,2)
row.names(df.RMSFEQ) = c(horizonQ, paste0("GM_",horizonQ), paste0("FC_",horizonQ), paste0("PFC_",horizonQ), paste0("CRIS_",horizonQ), paste0("MOD_",horizonQ), paste0("EXP_",horizonQ))

## write to Excel ##############################################################
setwd(paste0(ROOT,"/Results_public/rmsfe/"))
openxlsx::write.xlsx(df.FIG2, "FIG2.xlsx", colNames = TRUE, rowNames = TRUE, overwrite = TRUE)
openxlsx::write.xlsx(df.RMSFE, "RMSFE_M.xlsx", colNames = TRUE, rowNames = TRUE, overwrite = TRUE)
openxlsx::write.xlsx(df.RMSFEQ, "RMSFE_Q.xlsx", colNames = TRUE, rowNames = TRUE, overwrite = TRUE)