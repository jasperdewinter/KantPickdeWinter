datestr2vec <- function(date_string) {
  
  ################################## Description ###############################
  # Transforms date string to vector.
  #
  # INPUTS            date_string                  string (MM/DD/YYYY)
  #
  # OUTPUTS           date_vector                  (2x1)-vector (YYYY MM)
  #
  ##############################################################################
  
  if (nchar(date_string) == 10) {
    year  <- as.numeric(substr(date_string, 7,10))
    month <- as.numeric(substr(date_string, 1, 2))
    date_vector <- c(year, month)
  } else {
    warning('Please review the data input, as configurations seem to have changed.')
  }
  
  return(date_vector)
}