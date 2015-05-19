#' expand.SINGLE
#'
#' @param age.totals the data variable of interest, a total for each age bucket
#' @param age.labels the age group categories
#' @param k the degrees of freedom in the var
#' @param wts the weights for the generalized additive model
#' @param already.split (logical) is the data already in single year-of-age format


expand.SINGLE <- function(age.totals,age.labels,k=30,year="",wts=NULL,tidy=TRUE,already.split=FALSE,...){

  # this guy takes data in age-bucket format, and splits into by-single-year-of-age
  # it's purpose is to take TOTAL-spend by age-group data (e.g. historical data on Pension Spend by age group)
  # and turn it into by-single-year-of-age estimates
  # the main tool we use here is a GAM
  # age.totals is the data variable of interest. It is a TOTAL for each age bucket
  # the function then divides this through to get single-year-of-age estimates
  # age.labels has the age group categories
  # k is the degrees of freedom in the var
  # wts are best set using the weight.generator function (defined below)
  # already.split is an option to handle the situation where the data is already in single-year-of-age format

  # output is the distribution of y across ages 0:100, which we define as ageall
  ageall <- 0:100

  # 1) get age buckets
  # first, do they cover the whole age range? If not, make the non-covered area == 0
  age.totals <- fill.data(age.totals,age.labels)$age.totals.new
  age.labels <- fill.data(age.totals,age.labels)$age.labels.new

  # now fill out buckets, so there's one for each element in the age range
  age.bucket <- bucket.generator(age.labels) # see below; this basically just creates a vector of age_buckets across ageall

  # 2) get data for individual-ages
  if(already.split==TRUE) single.divided.data <- age.totals
  if(already.split==FALSE) single.divided.data <- age.totals/table(age.bucket)[match(age.labels,names(table(age.bucket)))]
  single.divided.data <- as.array(single.divided.data)

  # 3) fill out to full dataset [as an input for GAM]
  single.EXPANDED <- single.divided.data[match(age.bucket,age.labels)]

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

  # this is for budget-splitting elements
  if(tidy==TRUE) single.MODELLED <- single.MODELLED/sum(single.MODELLED)


  # 6) Plot to compare to original
  par(mfrow=c(1,2))
  plot(ageall,single.EXPANDED,pch=16,col="grey",main=paste("Single Year Estimates\n",year,"\n original [dots] vs modelled [line]"),ylim=c(0,max(single.MODELLED)),...)
  legend("topright",c(paste("k=",k)),bty="n")
  lines(ageall,single.MODELLED,col="blue",lwd=2)

  # 7) Roll up, and compare to original group total
  single.MODELLED.rolled <- tapply(single.MODELLED,age.bucket,sum)
  single.MODELLED.rolled <- single.MODELLED.rolled[match(age.labels,names(single.MODELLED.rolled))]
  if(already.split==TRUE) x.in <- age.totals*table(age.bucket)[match(names(age.totals),names(table(age.bucket)))]
  if(already.split==FALSE) x.in <- age.totals
  plot(as.numeric(x.in),main=paste("Group Totals \n",year,"\n original [dots] vs modelled [line]"),
       xaxt="n",xlab="age groups",col="grey",pch=16,ylim=c(0,max(single.MODELLED.rolled)))
  axis(1, at=1:length(x.in), labels=age.labels)
  lines(single.MODELLED.rolled,col="blue",lwd=2)
  legend("topright",c(paste("k=",k)),bty="n")
  par(mfrow=c(1,1))

  # 8) Return key variables
  names(single.MODELLED) <- age.bucket
  return(single.MODELLED)
}
