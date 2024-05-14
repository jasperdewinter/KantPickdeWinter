Y_availability <- function(Y_date, Y_q, Horizon) {
  
  ################################## Description ###############################
  # Provides a vector with the most recent available low-frequency variable.
  #
  # INPUTS            Y_date             (nobs x 2)
  #                   shiftQuarters       scalar
  #                   Y_binary           (nobs x 1)
  #
  # OUTPUTS           Y_available        (2 x 1)
  #
  ##############################################################################
  
  shiftQuarters       <- length(Y_q) - sum(is.na(Y_q))
  Y_binary            <- is.na(Y_q)
  
  first_Y             <- match('FALSE', Y_binary)                                        # Find first non-NAN value
  firstMonth          <- Y_date[2,(first_Y-Horizon)]
  firstYear           <- Y_date[1,(first_Y-Horizon)]
  firstQuarter        <- m2q(firstMonth)
  
  for (a in 1:(shiftQuarters-1)) {
    firstQuarter <- firstQuarter + 1 
    if (firstQuarter == 5) {
      firstYear    <- firstYear + 1
      firstQuarter <- 1
    }
  }
  
  return(c(firstYear,firstQuarter, shiftQuarters))
  
}