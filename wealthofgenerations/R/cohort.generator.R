

cohort.generator <- function(start.year,number.of.cohorts,cohort.size){
  # what is the last year we need to account for?
  end.year <- start.year+number.of.cohorts*cohort.size-1
  out <- data.frame(years=start.year:end.year)

  # get some group labels
  labels <- paste(seq(start.year,end.year-cohort.size+1,cohort.size),"to",seq(start.year+cohort.size-1,end.year,cohort.size),sep="")

  # update our final table
  out$labels <- rep(labels,each=cohort.size)
  return(out)
}
