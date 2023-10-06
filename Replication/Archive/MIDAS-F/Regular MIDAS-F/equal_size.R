equal_size <- function(F_date, Y_date,FreqRatio, F_m) {
  
  ################################## Description ###############################
  # Pads NA values at the top and bottom of the factors to align the high-freq
  # factors with the low-freq dependent variable.
  #
  # INPUTS            F_date             (nobs_f x 1)
  #                   Y_date             (nobs_y x 1)
  #                   FreqRatio           scalar
  #                   F_m                (nobs_f x 4)
  #
  # OUTPUTS           F_m_new            (nobs_y x 4)
  #                   NA_rows_bottom      scalar
  #
  ##############################################################################
  
  start <- rep(NA, 2)                                                                  # Preallocate values
  start[1] <- substr(F_date[1],1,2)
  start[2] <- substr(Y_date[1],1,2)
  NA_rows_front_nr <- strtoi(start[1]) - strtoi(start[2])
  NA_rows_front <- matrix(data=NA,nrow=NA_rows_front_nr,                               # Align top 
                          ncol=dim(F_m)[2])
  NA_rows_bottom <- matrix(data=NA,nrow=(length(Y_q)*FreqRatio - length(F_date) 
                                         - dim(NA_rows_front)[1]), ncol=dim(F_m)[2])   # Align bottom
  
  F_m_new <- rbind(NA_rows_front, F_m, NA_rows_bottom)
  output <- list(F_m_new, NA_rows_front_nr)
}