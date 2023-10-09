quarter_diff <- function(Date_1, Date_2) {
  
  ################################## Description ###############################
  # Provides difference between two dates expressed in quarters.
  #
  # INPUTS            Date 1 (format YYYY MM)             (2 x 1)
  #                   Date 2 (format YYYY MM)             (2 x 1)
  #                     with Date 1 earlier than Date 2
  #
  # OUTPUTS           quarter_diff                         scalar
  #
  ##############################################################################
  
  diff_quarter <- Date_2[2] - Date_1[2]
  diff_year    <- Date_2[1] - Date_1[1]
  
  quarter_diff <- abs(diff_quarter + (diff_year * 4))
  
}
