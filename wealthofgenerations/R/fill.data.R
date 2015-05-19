#' fill.data
#'
#'
#'

fill.data <- function(age.totals,age.labels){
  age_split <- strsplit(as.character(age.labels),"to")
  min.val <- min(unlist(lapply(age_split,function(x) min(as.numeric(x)))))
  max.val <- max(unlist(lapply(age_split,function(x) max(as.numeric(x)))))

  if(min.val!=0){
    new.bottom <- 0
    names(new.bottom) <- paste("0to",min.val-1,sep="")
    age.totals <- c(new.bottom,age.totals)
    age.labels <- c(names(new.bottom),age.labels)
  } else if (max.val!=100) {
    new.top <- 0
    names(new.top) <- paste(max.val+1,"to100",sep="")
    age.totals <- c(age.totals,new.top)
    age.labels <- c(age.labels,names(new.top))
  }

  out <- list(age.totals.new=age.totals,age.labels.new=age.labels)
  return(out)
}
