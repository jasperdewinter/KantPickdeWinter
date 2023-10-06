readConfig <- function(fileName){
  
  ################################## Description ###############################
  # Read configuration from file name.
  #
  # INPUTS            fileName                  string
  #
  # OUTPUTS           config_nr                 scalar
  #  
  ##############################################################################
  
  config_string <- substr(fileName, 4,6)
  config_nr     <- as.numeric(gsub("([0-9]+).*$", "\\1", config_string))
  
  return(config_nr)
}