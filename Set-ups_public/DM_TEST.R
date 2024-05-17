################################ Diebold Mariano test##########################
#
#     Dennis Kant, Andreas Pick and Jasper de Winter
#     May 16, 2024
#
#     Reproduce DM-test in paper
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

## Define output folder ########################################################
setwd(paste0(ROOT,"/Results_public/dm/"))

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

## DM-test against Prevailing Mean #############################################
models   <- c("AR","DFM","MDF","LASSO","EN","RS","RP","RF")
horizon  <- c("B2", "B1", "N3", "N2", "N1","1Q3","1Q2","1Q1","2Q3","2Q2","2Q1")

# Pre-allocate total
DM_tot   <- matrix(nrow = length(models), ncol = length(horizon))
DM_gm    <- matrix(nrow = length(models), ncol = length(horizon))
DM_fc    <- matrix(nrow = length(models), ncol = length(horizon))
DM_pfc   <- matrix(nrow = length(models), ncol = length(horizon))
DM_cris  <- matrix(nrow = length(models), ncol = length(horizon))
DM_mod   <- matrix(nrow = length(models), ncol = length(horizon))
DM_exp   <- matrix(nrow = length(models), ncol = length(horizon))

# TOTAL SAMPLE
Y_ref   <- data.frame(replicate(11,Y))
e1.tot  <- PM - Y_ref
e2.tot  <- list((AR - Y_ref), 
                (DFM - Y_ref),
                (MDF - Y_ref),
                (LASSO - Y_ref),
                (EN - Y_ref),
                (RS - Y_ref),
                (RP - Y_ref),
                (RF - Y_ref))
# GREAT MODERATION
Y_ref   <- data.frame(replicate(11,Y[GM,]))
e1.gm   <- PM[GM,] - Y_ref
e2.gm   <- list((AR[GM,] - Y_ref), 
                (DFM[GM,] - Y_ref),
                (MDF[GM,] - Y_ref),
                (LASSO[GM,] - Y_ref),
                (EN[GM,] - Y_ref),
                (RS[GM,] - Y_ref),
                (RP[GM,] - Y_ref),
                (RF[GM,] - Y_ref))
# FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[FC,]))
e1.fc   <- PM[FC,] - Y_ref
e2.fc   <- list((AR[FC,] - Y_ref), 
                (DFM[FC,] - Y_ref),
                (MDF[FC,] - Y_ref),
                (LASSO[FC,] - Y_ref),
                (EN[FC,] - Y_ref),
                (RS[FC,] - Y_ref),
                (RP[FC,] - Y_ref),
                (RF[FC,] - Y_ref))
# POST FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[PFC,]))
e1.pfc   <- PM[PFC,] - Y_ref
e2.pfc   <- list((AR[PFC,] - Y_ref), 
                 (DFM[PFC,] - Y_ref),
                 (MDF[PFC,] - Y_ref),
                 (LASSO[PFC,] - Y_ref),
                 (EN[PFC,] - Y_ref),
                 (RS[PFC,] - Y_ref),
                 (RP[PFC,] - Y_ref),
                 (RF[PFC,] - Y_ref))
# CRISIS
Y_ref   <- data.frame(replicate(11,Y[CRIS,]))
e1.cris   <- PM[CRIS,] - Y_ref
e2.cris   <- list((AR[CRIS,] - Y_ref), 
                  (DFM[CRIS,] - Y_ref),
                  (MDF[CRIS,] - Y_ref),
                  (LASSO[CRIS,] - Y_ref),
                  (EN[CRIS,] - Y_ref),
                  (RS[CRIS,] - Y_ref),
                  (RP[CRIS,] - Y_ref),
                  (RF[CRIS,] - Y_ref))

# MODERATE
Y_ref   <- data.frame(replicate(11,Y[MOD,]))
e1.mod   <- PM[MOD,] - Y_ref
e2.mod   <- list((AR[MOD,] - Y_ref), 
                 (DFM[MOD,] - Y_ref),
                 (MDF[MOD,] - Y_ref),
                 (LASSO[MOD,] - Y_ref),
                 (EN[MOD,] - Y_ref),
                 (RS[MOD,] - Y_ref),
                 (RP[MOD,] - Y_ref),
                 (RF[MOD,] - Y_ref))
# EXPANSION
Y_ref   <- data.frame(replicate(11,Y[EXP,]))
e1.exp   <- PM[EXP,] - Y_ref
e2.exp   <- list((AR[EXP,] - Y_ref), 
                 (DFM[EXP,] - Y_ref),
                 (MDF[EXP,] - Y_ref),
                 (LASSO[EXP,] - Y_ref),
                 (EN[EXP,] - Y_ref),
                 (RS[EXP,] - Y_ref),
                 (RP[EXP,] - Y_ref),
                 (RF[EXP,] - Y_ref))

# Execute DM-test
for (mod in 1:length(models)){
  for (hor in 1:dim(e1.tot)[2]){
    # aternative="greater", the alternative hypothesis is that method 2 (e2) is more accurate than method 1 (e1).
    DM_tot[mod, hor] <- dm.test(unlist(e1.tot[hor]), unlist(e2.tot[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_gm[mod, hor] <- dm.test(unlist(e1.gm[hor]), unlist(e2.gm[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_fc[mod, hor] <- dm.test(unlist(e1.fc[hor]), unlist(e2.fc[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_pfc[mod, hor] <- dm.test(unlist(e1.pfc[hor]), unlist(e2.pfc[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_cris[mod, hor] <- dm.test(unlist(e1.cris[hor]), unlist(e2.cris[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_mod[mod, hor] <- dm.test(unlist(e1.mod[hor]), unlist(e2.mod[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_exp[mod, hor] <- dm.test(unlist(e1.exp[hor]), unlist(e2.exp[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value    
  }    
}

DM <- rbind(DM_tot, DM_gm, DM_fc, DM_pfc, DM_cris, DM_mod, DM_exp)
colnames(DM)  <- horizon

DM <- DM[,rev(colnames(DM))]
DM <- data.frame(round(DM,3))
rownames(DM) <- c(models, paste0("GM_",models), paste0("FC_",models), paste0("PFC_",models), paste0("CRIS_",models), paste0("MOD_",models), paste0("EXP_",models))

temp = t(DM)
df.DM  <- data.frame(rbind(temp[,1:8], 
                           temp[,9:16],
                           temp[,17:24],
                           temp[,25:32],
                           temp[,33:40],
                           temp[,41:48],
                           temp[,49:56])) 
df.DM <-cbind(matrix(".",1),df.DM)
names(df.DM)[1] <- "PM"
rownames(df.DM) <- c(horizon, paste0("GM_",horizon), paste0("FC_",horizon), paste0("PFC_",horizon), paste0("CRIS_",horizon), paste0("MOD_",horizon), paste0("EXP_",horizon))
openxlsx::write.xlsx(df.DM, "DM_M_greater_PM.xlsx", colNames = TRUE, rowNames = TRUE, overwrite = TRUE)





## DM-test against Dynamic Factor MOdel ########################################

# error dynamic factor model
models   <- c("PM","AR","DFM","MDF","LASSO","EN","RS","RP","RF")
horizon  <- c("B2", "B1", "N3", "N2", "N1","1Q3","1Q2","1Q1","2Q3","2Q2","2Q1")

# Pre-allocate total
DM_tot   <- matrix(nrow = length(models), ncol = length(horizon))
DM_gm    <- matrix(nrow = length(models), ncol = length(horizon))
DM_fc    <- matrix(nrow = length(models), ncol = length(horizon))
DM_pfc   <- matrix(nrow = length(models), ncol = length(horizon))
DM_cris  <- matrix(nrow = length(models), ncol = length(horizon))
DM_mod   <- matrix(nrow = length(models), ncol = length(horizon))
DM_exp   <- matrix(nrow = length(models), ncol = length(horizon))

# TOTAL SAMPLE
Y_ref   <- data.frame(replicate(11,Y))
e1.tot  <- DFM - Y_ref
e2.tot  <- list((PM - Y_ref), 
                (AR - Y_ref), 
                (AR - Y_ref),
                (MDF - Y_ref),
                (LASSO - Y_ref),
                (EN - Y_ref),
                (RS - Y_ref),
                (RP - Y_ref),
                (RF - Y_ref))
# GREAT MODERATION
Y_ref   <- data.frame(replicate(11,Y[GM,]))
e1.gm   <- DFM[GM,] - Y_ref
e2.gm   <- list((PM[GM,] - Y_ref), 
                (AR[GM,] - Y_ref), 
                (AR[GM,] - Y_ref),
                (MDF[GM,] - Y_ref),
                (LASSO[GM,] - Y_ref),
                (EN[GM,] - Y_ref),
                (RS[GM,] - Y_ref),
                (RP[GM,] - Y_ref),
                (RF[GM,] - Y_ref))
# FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[FC,]))
e1.fc   <- DFM[FC,] - Y_ref
e2.fc   <- list((PM[FC,] - Y_ref), 
                (AR[FC,] - Y_ref), 
                (AR[FC,] - Y_ref),
                (MDF[FC,] - Y_ref),
                (LASSO[FC,] - Y_ref),
                (EN[FC,] - Y_ref),
                (RS[FC,] - Y_ref),
                (RP[FC,] - Y_ref),
                (RF[FC,] - Y_ref))
# POST FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[PFC,]))
e1.pfc   <- DFM[PFC,] - Y_ref
e2.pfc   <- list((PM[PFC,] - Y_ref),
                 (AR[PFC,] - Y_ref), 
                 (AR[PFC,] - Y_ref),
                 (MDF[PFC,] - Y_ref),
                 (LASSO[PFC,] - Y_ref),
                 (EN[PFC,] - Y_ref),
                 (RS[PFC,] - Y_ref),
                 (RP[PFC,] - Y_ref),
                 (RF[PFC,] - Y_ref))
# CRISIS
Y_ref   <- data.frame(replicate(11,Y[CRIS,]))
e1.cris   <- DFM[CRIS,] - Y_ref
e2.cris   <- list((PM[CRIS,] - Y_ref), 
                  (AR[CRIS,] - Y_ref), 
                  (AR[CRIS,] - Y_ref),
                  (MDF[CRIS,] - Y_ref),
                  (LASSO[CRIS,] - Y_ref),
                  (EN[CRIS,] - Y_ref),
                  (RS[CRIS,] - Y_ref),
                  (RP[CRIS,] - Y_ref),
                  (RF[CRIS,] - Y_ref))

# MODERATE
Y_ref   <- data.frame(replicate(11,Y[MOD,]))
e1.mod   <- DFM[MOD,] - Y_ref
e2.mod   <- list((PM[MOD,] - Y_ref), 
                 (AR[MOD,] - Y_ref), 
                 (AR[MOD,] - Y_ref),
                 (MDF[MOD,] - Y_ref),
                 (LASSO[MOD,] - Y_ref),
                 (EN[MOD,] - Y_ref),
                 (RS[MOD,] - Y_ref),
                 (RP[MOD,] - Y_ref),
                 (RF[MOD,] - Y_ref))
# EXPANSION
Y_ref   <- data.frame(replicate(11,Y[EXP,]))
e1.exp   <- DFM[EXP,] - Y_ref
e2.exp   <- list((PM[EXP,] - Y_ref), 
                 (AR[EXP,] - Y_ref), 
                 (AR[EXP,] - Y_ref),
                 (MDF[EXP,] - Y_ref),
                 (LASSO[EXP,] - Y_ref),
                 (EN[EXP,] - Y_ref),
                 (RS[EXP,] - Y_ref),
                 (RP[EXP,] - Y_ref),
                 (RF[EXP,] - Y_ref))

# Execute DM-test
for (mod in 1:length(models)){
  for (hor in 1:dim(e1.tot)[2]){
    # aternative="greater", the alternative hypothesis is that method 2 (e2) is more accurate than method 1 (e1).
    DM_tot[mod, hor] <- dm.test(unlist(e1.tot[hor]), unlist(e2.tot[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_gm[mod, hor] <- dm.test(unlist(e1.gm[hor]), unlist(e2.gm[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_fc[mod, hor] <- dm.test(unlist(e1.fc[hor]), unlist(e2.fc[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_pfc[mod, hor] <- dm.test(unlist(e1.pfc[hor]), unlist(e2.pfc[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_cris[mod, hor] <- dm.test(unlist(e1.cris[hor]), unlist(e2.cris[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_mod[mod, hor] <- dm.test(unlist(e1.mod[hor]), unlist(e2.mod[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_exp[mod, hor] <- dm.test(unlist(e1.exp[hor]), unlist(e2.exp[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value    
  }    
}

DM <- rbind(DM_tot, DM_gm, DM_fc, DM_pfc, DM_cris, DM_mod, DM_exp)
colnames(DM)  <- horizon

DM <- DM[,rev(colnames(DM))]
DM <- data.frame(round(DM,3))
rownames(DM) <- c(models, paste0("GM_",models), paste0("FC_",models), paste0("PFC_",models), paste0("CRIS_",models), paste0("MOD_",models), paste0("EXP_",models))

temp = t(DM)
df.DM  <- data.frame(rbind(temp[,  1:9], 
                           temp[,10:18],
                           temp[,19:27],
                           temp[,28:36],
                           temp[,37:45],
                           temp[,46:54],
                           temp[,55:63])) 
df.DM[,3] = "."
rownames(df.DM) <- c(horizon, paste0("GM_",horizon), paste0("FC_",horizon), paste0("PFC_",horizon), paste0("CRIS_",horizon), paste0("MOD_",horizon), paste0("EXP_",horizon))
openxlsx::write.xlsx(df.DM, "DM_M_greater_DFM.xlsx", colNames = TRUE, rowNames = TRUE, overwrite = TRUE)



## DM-test against Random Forest ###############################################

# error dynamic factor model
models   <- c("PM","AR","DFM","MDF","LASSO","EN","RS","RP","RF")
horizon  <- c("B2", "B1", "N3", "N2", "N1","1Q3","1Q2","1Q1","2Q3","2Q2","2Q1")

# Pre-allocate total
DM_tot   <- matrix(nrow = length(models), ncol = length(horizon))
DM_gm    <- matrix(nrow = length(models), ncol = length(horizon))
DM_fc    <- matrix(nrow = length(models), ncol = length(horizon))
DM_pfc   <- matrix(nrow = length(models), ncol = length(horizon))
DM_cris  <- matrix(nrow = length(models), ncol = length(horizon))
DM_mod   <- matrix(nrow = length(models), ncol = length(horizon))
DM_exp   <- matrix(nrow = length(models), ncol = length(horizon))

# TOTAL SAMPLE
Y_ref   <- data.frame(replicate(11,Y))
e1.tot  <- RF - Y_ref
e2.tot  <- list((PM - Y_ref), 
                (AR - Y_ref), 
                (DFM - Y_ref),
                (MDF - Y_ref),
                (LASSO - Y_ref),
                (EN - Y_ref),
                (RS - Y_ref),
                (RP - Y_ref),
                (AR - Y_ref))
# GREAT MODERATION
Y_ref   <- data.frame(replicate(11,Y[GM,]))
e1.gm   <- RF[GM,] - Y_ref
e2.gm   <- list((PM[GM,] - Y_ref), 
                (AR[GM,] - Y_ref), 
                (DFM[GM,] - Y_ref),
                (MDF[GM,] - Y_ref),
                (LASSO[GM,] - Y_ref),
                (EN[GM,] - Y_ref),
                (RS[GM,] - Y_ref),
                (RP[GM,] - Y_ref),
                (AR[GM,] - Y_ref))
# FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[FC,]))
e1.fc   <- RF[FC,] - Y_ref
e2.fc   <- list((PM[FC,] - Y_ref), 
                (AR[FC,] - Y_ref), 
                (DFM[FC,] - Y_ref),
                (MDF[FC,] - Y_ref),
                (LASSO[FC,] - Y_ref),
                (EN[FC,] - Y_ref),
                (RS[FC,] - Y_ref),
                (RP[FC,] - Y_ref),
                (AR[FC,] - Y_ref))
# POST FINANCIAL CRISIS
Y_ref   <- data.frame(replicate(11,Y[PFC,]))
e1.pfc   <- RF[PFC,] - Y_ref
e2.pfc   <- list((PM[PFC,] - Y_ref),
                 (AR[PFC,] - Y_ref), 
                 (DFM[PFC,] - Y_ref),
                 (MDF[PFC,] - Y_ref),
                 (LASSO[PFC,] - Y_ref),
                 (EN[PFC,] - Y_ref),
                 (RS[PFC,] - Y_ref),
                 (RP[PFC,] - Y_ref),
                 (AR[PFC,] - Y_ref))
# CRISIS
Y_ref   <- data.frame(replicate(11,Y[CRIS,]))
e1.cris   <- RF[CRIS,] - Y_ref
e2.cris   <- list((PM[CRIS,] - Y_ref), 
                  (AR[CRIS,] - Y_ref), 
                  (DFM[CRIS,] - Y_ref),
                  (MDF[CRIS,] - Y_ref),
                  (LASSO[CRIS,] - Y_ref),
                  (EN[CRIS,] - Y_ref),
                  (RS[CRIS,] - Y_ref),
                  (RP[CRIS,] - Y_ref),
                  (AR[CRIS,] - Y_ref))

# MODERATE
Y_ref   <- data.frame(replicate(11,Y[MOD,]))
e1.mod   <- RF[MOD,] - Y_ref
e2.mod   <- list((PM[MOD,] - Y_ref), 
                 (AR[MOD,] - Y_ref), 
                 (DFM[MOD,] - Y_ref),
                 (MDF[MOD,] - Y_ref),
                 (LASSO[MOD,] - Y_ref),
                 (EN[MOD,] - Y_ref),
                 (RS[MOD,] - Y_ref),
                 (RP[MOD,] - Y_ref),
                 (AR[MOD,] - Y_ref))
# EXPANSION
Y_ref   <- data.frame(replicate(11,Y[EXP,]))
e1.exp   <- RF[EXP,] - Y_ref
e2.exp   <- list((PM[EXP,] - Y_ref), 
                 (AR[EXP,] - Y_ref), 
                 (DFM[EXP,] - Y_ref),
                 (MDF[EXP,] - Y_ref),
                 (LASSO[EXP,] - Y_ref),
                 (EN[EXP,] - Y_ref),
                 (RS[EXP,] - Y_ref),
                 (RP[EXP,] - Y_ref),
                 (AR[EXP,] - Y_ref))

# Execute DM-test
for (mod in 1:length(models)){
  for (hor in 1:dim(e1.tot)[2]){
    # aternative="greater", the alternative hypothesis is that method 2 (e2) is more accurate than method 1 (e1).
    DM_tot[mod, hor] <- dm.test(unlist(e1.tot[hor]), unlist(e2.tot[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_gm[mod, hor] <- dm.test(unlist(e1.gm[hor]), unlist(e2.gm[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_fc[mod, hor] <- dm.test(unlist(e1.fc[hor]), unlist(e2.fc[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_pfc[mod, hor] <- dm.test(unlist(e1.pfc[hor]), unlist(e2.pfc[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_cris[mod, hor] <- dm.test(unlist(e1.cris[hor]), unlist(e2.cris[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_mod[mod, hor] <- dm.test(unlist(e1.mod[hor]), unlist(e2.mod[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value
    DM_exp[mod, hor] <- dm.test(unlist(e1.exp[hor]), unlist(e2.exp[[mod]][hor]), alternative = "greater", h = 1, power = 2)$p.value    
  }    
}

DM <- rbind(DM_tot, DM_gm, DM_fc, DM_pfc, DM_cris, DM_mod, DM_exp)
colnames(DM)  <- horizon

DM <- DM[,rev(colnames(DM))]
DM <- data.frame(round(DM,3))
rownames(DM) <- c(models, paste0("GM_",models), paste0("FC_",models), paste0("PFC_",models), paste0("CRIS_",models), paste0("MOD_",models), paste0("EXP_",models))

temp = t(DM)
df.DM  <- data.frame(rbind(temp[,  1:9], 
                           temp[,10:18],
                           temp[,19:27],
                           temp[,28:36],
                           temp[,37:45],
                           temp[,46:54],
                           temp[,55:63])) 
df.DM[,9] = "."
rownames(df.DM) <- c(horizon, paste0("GM_",horizon), paste0("FC_",horizon), paste0("PFC_",horizon), paste0("CRIS_",horizon), paste0("MOD_",horizon), paste0("EXP_",horizon))
openxlsx::write.xlsx(df.DM, "DM_M_greater_RF.xlsx", colNames = TRUE, rowNames = TRUE, overwrite = TRUE)