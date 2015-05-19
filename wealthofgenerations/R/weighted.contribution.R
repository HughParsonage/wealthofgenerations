#' weighted contribution
#'
#' @param vec
#' @param wts

weighted.contribution <- function(vec,wts){
  out <- vec*wts/sum(wts)
  if(sum(wts)==0) out <- rep(0,length(vec))
  return(out)
}
