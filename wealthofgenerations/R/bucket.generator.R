

bucket.generator <- function(vec){

  # the idea here is to mechanise the creating of age buckets
  # we take a bucket list (in the form "0to9","10to55",etc)
  # this needs to be across the range 0:100
  # output is a vector of categorical age buckets across this range

  # again, define full age range
  ageall <- 0:100

  # split up ages
  age_split <- strsplit(as.character(vec),"to")

  # put it in numerical order
  ascending.order <- order(unlist(lapply(age_split,function(element) as.numeric(element[1]))))
  age_split <- age_split[ascending.order]

  # first get the full age bucket breaks
  full.bucket.breaks <- rep(NA,length(age_split)+1)
  full.bucket.breaks[1] <- age_split[[1]][1]
  for(i in 1:length(age_split)) full.bucket.breaks[i+1] <- age_split[[i]][2]

  # generate labels for full
  labs <- paste(vec[ascending.order])

  # generate full bucket
  full.out <- as.character(cut(ageall,breaks=full.bucket.breaks,include.lowest=TRUE,labels=labs))
  return(full.out)
}
