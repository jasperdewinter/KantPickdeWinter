Process_data_MIDASF <- function(data){
  
  ################################## Description ###############################
  # Processes data for the MIDAS-F model
  #
  # INPUTS            data               4-dimensional list object including dates, indicator information, 
  #                                      pseudo-available GDP information and all available GDP information to date
  #
  # OUTPUTS           Y_q                GDP realizations
  #                   Date               Dates
  #                   data_X             Skip-sampled transformed data
  #
  ##############################################################################
  
  Y_q              <- data$Yd[!is.nan(data$Yd)]
  X                <- data$Xd                                                                    
  Date             <- data$DateD
  
  # Transform X such that skip-sampling can take place (i.e. make a multiple of FreqRatio)
  # TO DO - eliminate NAs and then make sure length is multiple of FreqRatio - TO DO
  
  if (is.null(dim(X)[1])) {
    X <- X[complete.cases(X)]
  } else {
    X <- X[complete.cases(X),]
      }
  
  if (is.null(dim(X)[1])) {
    X_elim_nr <- length(X)%% FreqRatio 
  
    if (X_elim_nr == 0) {             
    } else {
          # X         <- c(X, rep(NA, (FreqRatio - X_fill_nr)) ) 
            X         <- X[-(1:X_elim_nr)] 
      }
    
    } else {
      X_elim_nr <- dim(X)[1]  %% FreqRatio
        
        if (X_elim_nr == 0) {
          
      } else  {
        
      #  X_fill    <- matrix(NaN, X_fill_nr, dim(X)[2])
      #  X         <- rbind(X, X_fill)
         X         <- X[-(1:X_elim_nr),] 
         
      } 
      
    }
  
  # Transform X in skip-sampled form
  if (is.null(dim(X)[1])) {
    data_X <- matrix(NA, (length(X)/FreqRatio), (FreqRatio) )
    data_X <- fmls(X,LagOrder,FreqRatio)
      } else {
  
  data_X <- matrix(NA, (dim(X)[1]/FreqRatio), (FreqRatio * dim(X)[2]) )
  for (h in 1:dim(X)[2]) {
    col_id <- h - 1
    data_X[,((1+col_id*FreqRatio):(3+col_id*FreqRatio))] <- fmls(X[,h],LagOrder,FreqRatio)
  }
 # realign_X <- realign(data_X)
 # data_X <- realign_X[[1]][-(1:realign_X[[2]]),]
  }
  
  return(list(Y_q, Date, data_X))
  
}
