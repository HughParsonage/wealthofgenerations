# Run analysis over FISXXc File
analyse_CURFS <- function(df,vec.code,k=30,precision=0.5,kids=TRUE,weekly=TRUE,tidy=TRUE){
  # allocate hh spending to individuals
  iterated.average <- iterate.ave(vec=vec.code,precision=precision,kids=kids,mat=df)

  # get mean by age
  ave.by.age <- tapply(iterated.average$exp_by_age.ext,iterated.average$age_label,mean)

  # make NAs zero (e.g. if we have excluded kids)
  ave.by.age <- ifelse(is.na(ave.by.age),0,ave.by.age)

  # expand the age-group means into by-single-year-of-age estimates, using a spline
  splined <- expand.MEANS(age.totals=ave.by.age,age.labels=names(ave.by.age),k=k,weekly=weekly,tidy=tidy)
  return(splined)
}
