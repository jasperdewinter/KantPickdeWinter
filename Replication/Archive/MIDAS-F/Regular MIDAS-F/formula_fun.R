formula_fun <- function(LagOrder,FreqRatio, num_factors, type) {
  
  ################################## Description ###############################
  # Provides formula based on number of factors included in analysis.
  #
  # INPUTS            LagOrder             scalar
  #                   FreqRatio            scalar
  #                   num_factors          scalar
  #                   type                 0 or 1 (0: MIDAS-U; 1: MIDAS-R)
  #                                        (MIDAS-R: nealmon is employed)
  #
  # OUTPUTS           formula              formula object
  #
  ##############################################################################
  
  formula_storage <- lapply(1:num_factors, function(i) NA)
  names           <- lapply(1:num_factors, function(i) NA)
  
  if (type == 0) {
  for (b in 1:num_factors) {
    rhs <- letters[b]
    names[b] <- rhs
    formula_storage[b] <- paste('fmls(', rhs, ',LagOrder, FreqRatio)', collapse='')
    formula_storage[b] <- gsub(' ','', formula_storage[b])
  }
  } else {
    for (b in 1:num_factors) {
      rhs <- letters[b]
      names[b] <- rhs
      formula_storage[b] <- paste('fmls(', rhs, ',LagOrder, FreqRatio, nealmon)', collapse='')
      formula_storage[b] <- gsub(' ','', formula_storage[b])
    }
  }

  formula <- as.formula(paste("y~", paste(formula_storage, collapse="+")))

  output <- list(formula, names)
  return(output)

}
