split_sample <- function(F_m, col_factors){
  
  ################################## Description ###############################
  # Provides a vector with the quarter for which to forecast.
  #
  # INPUTS            F_m                (nobs x col_factors + 2 columns of yc 
  #                                       and y (see Jansen en de Winter))
  #
  # OUTPUTS           est_sample         (nobs_est x num_factors)
  #                   fcst_sample        (nobs_fcst x num_factors)
  #
  ##############################################################################

  if (col_factors == 1) {
    fcst_sample     <- F_m[ (length(F_m)-(fcst_diff*FreqRatio)+1) :
                              length(F_m)]
    est_sample      <- F_m[1:(length(F_m)-(fcst_diff*FreqRatio))]
    
    # Realign estimation sample such that estimation can take place
    required_data   <- length(est_sample) - (length(est_sample) %% FreqRatio)
    est_sample      <- est_sample[((length(est_sample) - required_data + 1):
                                     length(est_sample))]
  } else{  
    fcst_sample     <- F_m[ (dim(F_m)[1]-(fcst_diff*FreqRatio)+1) :
                              dim(F_m)[1], ]
    est_sample      <- F_m[1:((dim(F_m)[1])-(fcst_diff*FreqRatio)),]
  
    # Realign estimation sample such that estimation can take place
    required_data   <- dim(est_sample)[1] - (dim(est_sample)[1] %% FreqRatio)
    est_sample      <- est_sample[((dim(est_sample)[1] - required_data + 1):
                                     dim(est_sample)[1]),]
  }
  
  # if (num_factors == 1) {
  #   fcst_sample     <- F_m[ ((length(F_m)/num_factors)-(fcst_diff*FreqRatio)+1) :
  #                             (length(F_m)/num_factors) ]
  #   est_sample      <- F_m[1:((length(F_m)/num_factors)-(fcst_diff*FreqRatio))]
  #   
  #   # Realign estimation sample such that estimation can take place
  #   required_data   <- (length(est_sample)/num_factors) - ((length(est_sample)/num_factors) %% FreqRatio)
  #   est_sample      <- est_sample[(((length(est_sample)/num_factors) - required_data + 1):
  #                                    (length(est_sample)/num_factors))]
  # } else {
  #   fcst_sample     <- F_m[ ((length(F_m)/num_factors)-(fcst_diff*FreqRatio)+1) :
  #                             (length(F_m)/num_factors), ]
  #   est_sample      <- F_m[1:((length(F_m)/num_factors)-(fcst_diff*FreqRatio)),]
  #   
  #   # Realign estimation sample such that estimation can take place
  #   required_data   <- (length(est_sample)/num_factors) - ((length(est_sample)/num_factors) %% FreqRatio)
  #   est_sample      <- est_sample[(((length(est_sample)/num_factors) - required_data + 1):
  #                                    (length(est_sample)/num_factors)),]
  # }
  
  output <- list(est_sample, fcst_sample)
  return(output)
  
  
}