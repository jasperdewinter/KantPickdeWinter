formula_fun <- function(LagOrder,FreqRatio, col_factors, type) {
  
  ################################## Description ###############################
  # Provides formula based on number of factors included in analysis.
  #
  # INPUTS            LagOrder             scalar
  #                   FreqRatio            scalar
  #                   col_factors          scalar
  #                   type                 0 or 1 (0: MIDAS-U; 1: MIDAS-R)
  #                                        (MIDAS-R: nealmon is employed)
  #
  # OUTPUTS           formula              formula object
  #
  ##############################################################################
  
  formula_storage <- lapply(1:col_factors, function(i) NA)
  names           <- lapply(1:col_factors, function(i) NA)
  
  lettersL        <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS))) # 702 Uppercase letters
  
  if (type == 0) {
  for (b in 1:col_factors) {
    rhs <- lettersL[b]
    names[b] <- rhs
    formula_storage[b] <- paste('fmls(', rhs, ',LagOrder, FreqRatio)', collapse='')
    formula_storage[b] <- gsub(' ','', formula_storage[b])
  }
  } else {
    for (b in 1:col_factors) {
      rhs <- lettersL[b]
      names[b] <- rhs
      formula_storage[b] <- paste('fmls(', rhs, ',LagOrder, FreqRatio, nealmon)', collapse='')
      formula_storage[b] <- gsub(' ','', formula_storage[b])
    }
  }

  formula <- as.formula(paste("y~", paste(formula_storage, collapse="+")))

  output <- list(formula, names)
  return(output)

}
