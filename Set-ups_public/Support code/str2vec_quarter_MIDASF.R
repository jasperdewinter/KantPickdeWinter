str2vec_quarter_MIDASF <- function(myFiles, type){ 

################################## Description ###############################
# Transforms file name to date vector.
#
# INPUTS            myFiles                  (nobs x 1)
#                   type                     (1 or 2)
#
# OUTPUTS           date_vector_complete     (nobs x 2)
#
##############################################################################

    date_vector_complete <- matrix(NA, length(myFiles),2)
  
  for (i in 1:length(myFiles)) {
    date_string <- myFiles[i]
    
    date_string_ym <- substr(date_string, nchar(date_string) - 10,nchar(date_string) - 4)
    date_string_ym <- trimws(date_string_ym, which = c("both", "left", "right"))
    
    year <- as.numeric(substr(date_string_ym, 1,4))
    if (nchar(date_string_ym) == 6) {
      month <- as.numeric(substr(date_string_ym, 6,6))
    } else if (nchar(date_string_ym) == 7) {
      month <- as.numeric(substr(date_string_ym, 6,7))
    } else {
      warning('Please review the data input, as configurations seem to have changed.')
    }
    
        # Transform to quarter
       quarter <- m2q(month)
       date_vector <- c(year, quarter)
       
       date_vector_complete[i,] <- date_vector
       
  }
  
  if (type == 1) {                                                         # Yields matrix with all quarterly dates
    return(date_vector_complete)
  } else if (type == 2) {                                                  # Yields matrix with all quarterly dates extended by horizon h = -1 and horizon h = 2 in the beginning/end of sample.
    # Add quarter for horizon h = -1
    firstMonth <- date_vector_complete[1,2]
    firstYear  <- date_vector_complete[1,1]
    if (firstMonth == 1) {
      newMonth <- 4
      newYear  <- firstYear - 1
    } else {
      newMonth <- firstMonth - 1
      newYear  <- firstYear
    }
    newDate <- c(newYear, newMonth)
    date_vector_complete <- rbind(newDate, date_vector_complete)
    
    # Add two quarters for horizon h = 2
    lastMonth <- date_vector_complete[length(myFiles),2]
    lastYear  <- date_vector_complete[length(myFiles),1]
    
    newDate <- matrix(NA, 2,2)
    Month <-lastMonth
    Year <- lastYear
    for (h in 1:2) {
      if (Month == 4) {
        Month <- 1
        Year  <- Year + 1
      } else {
        Month <- Month + 1
        Year  <- Year
      }
      newDate[h,] <- c(Year, Month)
    }
    
    date_vector_complete <- rbind(date_vector_complete,newDate)
    
    return(date_vector_complete)
    
  } else {
    warning('Please submit the correct type; 1 (yields a matrix of quarterly dates) or 2 (yields a matrix of quarterly dates for full forecast horizon.')
  }
   
}