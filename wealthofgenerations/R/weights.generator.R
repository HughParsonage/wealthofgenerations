#' weights generator

weights.generator <- function(ages.to.be.weighted,scale){
  wts <- rep(1,101)
  wts[ages.to.be.weighted+1] <- scale
  return(wts)
}
