m2q <- function(month) {
  
  ################################## Description ###############################
  # Transforms month to corresponding quarter.
  #
  # INPUTS            month             scalar
  #
  # OUTPUTS           quarter           scalar
  #
  ##############################################################################
  
  if (month %in% c(1,2,3) ) {                                          
    quarter <- 1
  } else if (month %in% c(4,5,6)) {
    quarter <- 2
  } else if (month %in% c(7,8,9)) {
    quarter <- 3
  } else {
    quarter <- 4
  }
  
  return(quarter)
}