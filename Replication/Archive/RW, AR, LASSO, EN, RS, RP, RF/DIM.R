DIM <- function(x) {
  if(is.null(dim(x))) {
    c(length(x),1)
  } else {
    dim(x)
  }
  
}