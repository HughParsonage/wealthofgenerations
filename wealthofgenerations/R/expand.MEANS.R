#' expand.MEANS
#'
#' @param age.totals the data variable of interest. It is the MEAN SPEND for each age-group
#' @param age.labels age.group categories
#' @param k the degrees of freedom in the generalized additive model
#' @param year a placeholder
#' @param weekly (logical)
#' @param wts the weights for the generalzied additive model
#' @param tidy (logical)


# Expanded means takes AVERAGE spend per person in the age-group, rather than a total
expand.MEANS <- function(age.totals,age.labels,k=30,year="",weekly=TRUE,wts=NULL,tidy=TRUE,...){

  # this guy takes data in age-bucket format, and splits into by-single-year-of-age
  # it's purpose is to smooth out estimated means from, e.g. FISXXC files
  # the main tool we use here is a GAM
  # age.totals is the data variable of interest. It's the MEAN SPEND for each age-group.
  # age.labels has the age group categories
  # k is the degrees of freedom in the var
  # wts are best set using the weight.generator function (defined below)

  # output is the distribution of y across ages 0:100, which we define as ageall
  ageall <- 0:100

  # 1) get age buckets
  # first, do they cover the whole age range? If not, make the non-covered area == 0
  age.totals <- fill.data(age.totals,age.labels)$age.totals.new
  age.labels <- fill.data(age.totals,age.labels)$age.labels.new

  # 2) now fill out buckets, so there's one for each element in the age range
  age.bucket <- bucket.generator(age.labels) # see below; this basically just creates a vector of age_buckets across ageall

  # 3) fill out to full dataset [as an input for GAM]
  single.EXPANDED <- age.totals[match(age.bucket,age.labels)]

  # if data is weekly, expand to annual
  if(weekly==TRUE) single.EXPANDED <- single.EXPANDED*(365/7)

  # 4) run GAM (normally, this would be a loop)
  # run model and predict
  if(is.null(wts)) mod <- gam(single.EXPANDED~s(ageall,k=k))
  if(!is.null(wts)) mod <- gam(single.EXPANDED~s(ageall,k=k),weights=wts)
  single.MODELLED <- predict(mod)

  # 5) Tidy data
  # set any errant negative modelling values to zero
  single.MODELLED <- ifelse(single.MODELLED<0,0,single.MODELLED)

  # make whatever categories were originally 0, return to exactly zero
  single.MODELLED[single.EXPANDED==0] <- 0

  # 6) Plot to compare to original
  plot(ageall,single.EXPANDED,pch=16,col="grey",main=paste("Single Year Estimates\n",year,"\n original [dots] vs modelled [line]"),ylim=c(0,max(single.MODELLED)))
  legend("topright",c(paste("k=",k)),bty="n")
  lines(ageall,single.MODELLED,col="blue",lwd=2)

  # 7) Divide through, to get data as percentage spend; this is for budget-splitting elements
  if(tidy==TRUE) single.MODELLED <- single.MODELLED/sum(single.MODELLED)

  # 8) Return key variables
  names(single.MODELLED) <- ageall
  return(single.MODELLED)
}
