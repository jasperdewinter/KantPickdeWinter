RS_formula_fun <- function(select_X, intercept) {
  
  ################################## Description ###############################
  # Provides formula based on number of factors included in analysis.
  #
  # INPUTS            select_X             scalar
  #                   intercept            binary (0: no intercept; 1: intercept)
  #
  # OUTPUTS           formula              formula object
  #                                        (! Does not include an intercept !)
  #
  ##############################################################################
  
  formula_storage <- lapply(1:length(select_X), function(i) NA)
  rhs <- 1
  
  for (b in 1:length(select_X)) {
    formula_storage[b] <- paste('X', rhs)
    formula_storage[b] <- gsub(' ','', formula_storage[b])
    rhs <- rhs + 1
  }
  
  if (intercept == 0) {
  formula_adj <- paste("y_sample ~", paste(formula_storage, collapse="+"), '+0') # WITHOUT INTERCEPT
  formula <- as.formula(formula_adj)
} else {
  formula_adj <- paste("y_sample ~", paste(formula_storage, collapse="+"))  # WITH INTERCEPT
  formula <- as.formula(formula_adj)
}

  return(formula)

}
