find_quarter_month <- function(F_date, Horizon, Type) {
  
  ################################## Description ###############################
  # Provides the current quarter given the data.
  #
  # INPUTS            F_date             (nobs_f x 1)
  #                   (monthly data)
  #                   Horizon             scalar
  #                   Type                scalar
  #
  # OUTPUTS           fcstQuarter        (2 x 1)
  #                   currentDate        (2 x 1)
  #
  ##############################################################################
  
  currentMonth   <- F_date[2,dim(F_date)[2]]
  currentYear    <- F_date[1,dim(F_date)[2]]
  
  if (Type == 1) {
  
      if (Horizon >= 4 | Horizon <= -4) {
        warning('Please submit a horizon of (-)1, (-)2 or (-)3 quarters.')
      } else {
          
          currentQuarter <- m2q(currentMonth)
          
          fcstQuarter_raw <- currentQuarter + Horizon
          
          quartersNextYear <- fcstQuarter_raw %% 4
          
          if (fcstQuarter_raw == 0) {                                                 # Find date for which a forecast is required given a certain horizon
            fcstYear <- currentYear - 1
            fcstQuarter <- c(fcstYear, 1)
          } else if (fcstQuarter_raw %in% c(1,2,3,4)) {
            fcstQuarter <- c(currentYear, fcstQuarter_raw)
          } else if (fcstQuarter_raw == 5) {
            fcstYear <- currentYear + 1
            fcstQuarter <- c(fcstYear, 1)
          } else {
            fcstYear <- currentYear + 1
            fcstQuarter <- c(fcstYear, quartersNextYear)
          }
          return(fcstQuarter)
          
          # if (fcstQuarter_raw <= 4) {                                                 # Original (14/05)
          #   fcstQuarter <- c(currentYear, fcstQuarter_raw)
          # } else if (fcstQuarter_raw == 5) {
          #   fcstYear <- currentYear + 1
          #   fcstQuarter <- c(fcstYear, 1)
          # } else {
          #   fcstYear <- currentYear + 1
          #   fcstQuarter <- c(fcstYear, quartersNextYear)
          # }
          # return(fcstQuarter)
      }
    
  } else if (Type == 2) {
        currentDate <- c(currentYear, currentMonth)
        return(currentDate)
  } else {
        warning('Please request either the forecast quarter (1) or the current quarter (2).')
  }
  
}