

interpolate <- function(dat,...){

  "just a tidy version, applied to data.frame of APPROX"
  "works on the APPROX function; takes APPROX arguments"
  "need a dataframe, with years formatted as XYYYY, e.g. X1984"

  if(!is.data.frame(dat)) stop("gotta input a data.frame for this function")

  # what years do we have?
  years.old <- as.numeric(gsub(pattern="[X|x]","",names(dat)))

  # what's the year range? Define "years" for every year
  years.new <- min(years.old):max(years.old)

  # setup output
  mat.out <- matrix(NA,ncol=length(years.new),nrow=nrow(dat))

  # interpolate
  list.out <- apply(dat,1,function(vec) approx(x=years.old,y=vec,method="linear",xout=years.new,...))

  # update mat.out
  for(i in 1:length(list.out)){
    mat.out[i,] <- list.out[[i]]$y
  }

  # tidy up
  colnames(mat.out) <- years.new
  row.names(mat.out) <- row.names(dat)

  return(mat.out)
}
