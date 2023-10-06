obj_dim <- function(items) {
  
  dims<-vector('list',length(items))
  names(dims)<-items
  for(thing in seq(1,length(items))) {
    if (is.null(dim(get(items[thing])))) {
      
      dims[[thing]]<-length(get(items[thing]))
    } else{
      #load with dim()
      dims[[thing]]<-dim(get(items[thing]))
    }
  }
  return(dims)
}