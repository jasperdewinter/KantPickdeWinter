split_sample <- function(F_m, num_factors){
  
  ################################## Description ###############################
  # Provides a vector with the quarter for which to forecast.
  #
  # INPUTS            F_m                (nobs x num_factors)
  #
  # OUTPUTS           est_sample         (nobs_est x num_factors)
  #                   fcst_sample        (nobs_fcst x num_factors)
  #
  ##############################################################################
  
  if (num_factors == 1) {
  fcst_sample     <- F_m[ ((length(F_m)/num_factors)-(fcst_diff*FreqRatio)+1) :
                            (length(F_m)/num_factors) ]
  est_sample      <- F_m[1:((length(F_m)/num_factors)-(fcst_diff*FreqRatio))]
  
  # Realign estimation sample such that estimation can take place
  required_data   <- (length(est_sample)/num_factors) - ((length(est_sample)/num_factors) %% FreqRatio)
  est_sample      <- est_sample[(((length(est_sample)/num_factors) - required_data + 1):
                                   (length(est_sample)/num_factors))]
  } else {
  fcst_sample     <- F_m[ ((length(F_m)/num_factors)-(fcst_diff*FreqRatio)+1) :
                            (length(F_m)/num_factors), ]
  est_sample      <- F_m[1:((length(F_m)/num_factors)-(fcst_diff*FreqRatio)),]
  
  # Realign estimation sample such that estimation can take place
  required_data   <- (length(est_sample)/num_factors) - ((length(est_sample)/num_factors) %% FreqRatio)
  est_sample      <- est_sample[(((length(est_sample)/num_factors) - required_data + 1):
                                   (length(est_sample)/num_factors)),]
  }
  
  output <- list(est_sample, fcst_sample)
  return(output)
  
  
}