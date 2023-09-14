column_id_nr <- function(currentMonth, Horizon) {
  
  ################################## Description ###############################
  # Provides storage column id.
  #
  # INPUTS            currentMonth             scalar
  #                   Horizon                  scalar
  #
  # OUTPUTS           column_id                scalar
  #
  ##############################################################################
  
  colNr <- NA
  
  subCol <- currentMonth %% 3 
  if (subCol == 0) {
    subCol <- 1
  } else if (subCol == 1) {
    subCol <- 3
  } else {
    subCol <- 2
  }
  
  if (Horizon == -1){
    colNr <- subCol
    return(colNr)
  } else if (Horizon == 0) {
    colNr <- subCol + 3
    return(colNr)
  } else if (Horizon == 1) {
    colNr <- subCol + 6
    return(colNr)
  } else if (Horizon == 2) {
    colNr <- subCol + 9
    return(colNr)
  } else {
    warning('Please check the input; Horizon can only run in the range of (-1, 0, 1, 2).')
  }
  
}