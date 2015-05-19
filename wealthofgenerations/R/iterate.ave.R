


iterate.ave <- function(vec,precision=0.5,mat,weight.var,kids=TRUE){
  if("package:arm" %in% search()){
    detach("package:arm", unload=TRUE)
    detach("package:MASS", unload=TRUE)} # this is to stop dplyr getting confused on "select" commands

  # vec is the variable name, e.g. "UHLTOT"
  # precision just tells the function how long to iterate for
  # the kids option is for tax (and similar variables) where we want to explicitly make the allocation to children=0

  # first, if we don't want kids in, get rid of them from the data
  if(kids==FALSE) mat <- filter(mat,AGE_estimate>=18 & IUPOS!=3)
  # IUPOS, dependence status (3=dependent child). If kids are dependent, assume that the parents are paying the tax.
  # Note that we might want to include RELATHCF, Relationship in household (3=dependent student)). I have left this out, as it's not in all the years of the CURFs we have.

  # make all members of the household, regardless of age, weighted equally
  # (not in the sense of survey weights! This is just about dividing up a household's expenditure among its members)
  mat$weights.temp <- 1

  # set up a "difference" vector, with a large number for each element (e.g. 100)
  diff <- rep(100,nrow(mat))

  # count the number of times we iterate
  count <- 0

  # keep iterating until the diff gets to within the bounds set by the constant 'precision'
  while(max(diff)>precision){

    # set count to zero
    count <- count+1

    # start with our guess at the temporary weights
    spend.start.guess <- mat$weights.temp

    # calculate the average spending contribution, based on the weights in weights.temp
    # (note that the funky code is because dplyr hasn't yet got it together in terms of
    # passing a vector's character name into functions)
    # for more on this, see http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input; AND http://grokbase.com/t/r/r-help/141w8ep9yp/r-passing-variable-names-to-dplyr-solved

    call <- substitute(ungroup(mat) %>%
                         select(vec,ABSHID,AGE_estimate,AGE_label,weights.temp,HESHHWT) %>%
                         group_by(ABSHID) %>%
                         mutate(spending.contribution=weighted.contribution(vec,weights.temp)),
                       list(vec = as.name(vec)))

    # update mat
    mat <- eval(call)

    # based on these spending contributions, calculate the new weights.temp for each age group
    mat <- ungroup(mat) %>% group_by(AGE_label) %>% mutate(weights.temp=mean(spending.contribution))

    # calculate the difference between our old and updated guesses.
    # if the max here is less than the "precision" input, we stop the loop
    diff <- abs(spend.start.guess-mat$weights.temp)
  }

  # Save output
  out <- list(exp_by_age.ext=mat$weights.temp,
              precision=precision,
              count=count,
              # HUGH SEZ: I changed dis to 'weights.temp` but it
              # still didn't work
              #HESWEIGHTS=mat[,weight.var],
              HESWEIGHTS = dplyr::select_(mat, weight.var),
              age_label=mat$AGE_label,
              ABSHID=mat$ABSHID)

  return(out)
  require(arm)
}
