

#--- --- --- --- --- --- --- --- --- --- ---
# write function to deal with group over80
# Note, that after we expand.SINGLE, we often have an implausible situation that the by-age total doesn't deterioate with age
# so, we take the spend on 80-year-olds, and have it diminish according to the population structure
smooth.77plus <- function(spending.profile,age.structure,smoothing.age=77:100,top.group="80to100",...){

  # NB as was the case with pensions, we need to smooth back to the closest median-age (which in this case is 77)
  # what is the spending.level at age 78?
  level77 <- spending.profile[ageall==smoothing.age[1]]

  # how does the population distribution drop off?
  post77.drop <- age.structure[ageall %in% smoothing.age]/age.structure[ageall %in% smoothing.age[1]]

  # apply this drop-off rate to our level at age 80
  post77.levels <- level77*post77.drop

  # this tends to understate the overall contribution of 80+, mainly because we're missing an incidence effect
  # i.e. it's more expensive being 90 than 80

  # if we assume that the incidence rate is geometric, we can solve for it
  find.rate <- function(rate){
    new.val <- sum(post77.levels*(1+rate)^(0:(length(smoothing.age)-1)))
    old.val <- sum(spending.profile[ageall %in% smoothing.age])
    out <- abs(new.val-old.val)
    return(out)
  }

  price.rate <- optimize(f = find.rate,interval = c(0,1))$minimum

  # beef up!
  rate.vec <- (1+price.rate)^(0:(length(smoothing.age)-1))
  post77.levels.new <- post77.levels*rate.vec
  names(post77.levels.new) <- rep(top.group,length(post77.levels.new))

  # splice this back on
  out.new <- c(spending.profile[ageall %in% 0:(smoothing.age[1]-1)],post77.levels.new)

  # generate a little plot
  par(mfrow=c(1,1))
  plot(ageall,spending.profile,pch=16,col="grey",ylim=c(0,max(spending.profile)),...)
  lines(ageall,out.new,col="blue",lwd=2)

  # return
  return(out.new)
}
