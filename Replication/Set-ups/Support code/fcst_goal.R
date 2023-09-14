fcst_goal <- function(Horizon, currentDate){
  
  ################################## Description ###############################
  # Provides a vector with the quarter for which to forecast.
  #
  # INPUTS            Horizon             scalar
  #                   currentDate        (2x1) (YYYY QQ)
  #
  # OUTPUTS           date_vector        (2x1) (YYYY QQ)
  #
  ##############################################################################
  
  # Load data
  Year    <- currentDate[1]
  Quarter <- currentDate[2]
  
  # Calculate time point for which to forecast
  Horizon_abs  <- abs(Horizon)
  Horizon_sign <- sign(Horizon)

  for (i in 1:Horizon_abs) {
    Quarter <- Quarter + Horizon_sign
    
    if (Quarter == 0 & Horizon_sign == -1) {
    Quarter <- 4
    Year <- Year - 1
    }
    if (Quarter == 5 & Horizon_sign == 1) {
      Quarter <- 1
      Year <- Year + 1
    }

  }
  
  date_vector  <- c(Year, Quarter)
  return(date_vector)
  
  
}