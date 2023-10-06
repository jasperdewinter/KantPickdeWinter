read_Y <- function(Y_date, shiftQuarters) {
  
  ################################## Description ###############################
  # Provides a vector with the most recent available low-frequency variable.
  #
  # INPUTS            Y_date             (nobs x 1)
  #                   shiftQuarters       scalar
  #
  # OUTPUTS           Y_available        (2 x 1)
  #
  ##############################################################################
  
  shiftQuarters <- shiftQuarters - 1
  
  firstMonth   <- Y_date[2,1]
  firstYear    <- Y_date[1,1]
  firstQuarter <- m2q(firstMonth)
  
  shiftYears     <- floor(shiftQuarters/4)
  shiftQuarters  <- shiftQuarters %% 4
  
  Y_years        <- firstYear + shiftYears
  Y_quarters     <- firstQuarter + shiftQuarters
  
  if (Y_quarters <= 4) {
    Y_available <- c(Y_years, Y_quarters)
  } else {
    Y_available <- c((Y_years + 1), (Y_quarters - 4))
  }
  
}