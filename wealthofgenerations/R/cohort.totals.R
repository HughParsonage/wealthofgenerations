#' Cohort Total Generator

cohort.totals <- function(spending.df,cohorts){
  if(is.data.frame(spending.df)) stop("Gotta have a matrix for this one")
  # get matrix of people's birthyear
  # set up matrix
  birthyear <- matrix(NA,nrow=nrow(spending.df),ncol=ncol(spending.df))
  ages <- rownames(birthyear) <- as.numeric(rownames(spending.df))
  years <- colnames(birthyear) <- as.numeric(colnames(spending.df))

  # populate birthyear
  for(i in 1:ncol(birthyear)) birthyear[,i] <- years[i]-ages

  # populate labels
  label.mat <- apply(birthyear,2,function(y) cohorts$labels[match(y,cohorts$years)])

  # now sum up by time
  out.full.by.TIME <- matrix(NA,ncol=length(years),nrow=length(unique(cohorts$labels)))
  rownames(out.full.by.TIME) <- unique(cohorts$labels)
  colnames(out.full.by.TIME) <- years
  for(i in 1:length(years)){
    add <- tapply(spending.df[,i],label.mat[,i],sum)
    out.full.by.TIME[names(add),i] <- add
  }

  # sum up by year-of-age
  out.full.by.AGE <- matrix(NA,ncol=length(ages),nrow=length(unique(cohorts$labels)))
  rownames(out.full.by.AGE) <- unique(cohorts$labels)
  colnames(out.full.by.AGE) <- ages
  for(i in 1:length(ages)){
    add <- tapply(spending.df[i,],label.mat[i,],sum)
    out.full.by.AGE[names(add),i] <- add
  }

  # Cumulative by year-of-age
  # first modify cumsum so as to deal with NAs as zeros
  cum.na <- function(x) {
    x[which(is.na(x))] <- 0
    return(cumsum(x))
  }
  out.full.by.AGE.cumulative <- t(apply(out.full.by.AGE,1,cum.na))

  # make comparisons fair, by getting rid of not-complete cohorts
  # where do the NA's kick in?
  youngest <- as.numeric(substr(rownames(out.full.by.AGE.cumulative),7,10))
  nas.from <- max(years)-youngest+2
  make.na <- function(x,pt){
    if(pt>=length(x)){
      return(x)
    } else {
      out <- x
      out[pt:length(x)] <- NA
      return(out)
    }}

  out.full.by.AGE.cumulative.trim <- out.full.by.AGE.cumulative
  for(i in 1:nrow(out.full.by.AGE.cumulative)){
    out.full.by.AGE.cumulative.trim[i,] <- make.na(out.full.by.AGE.cumulative[i,],nas.from[i])
  }

  # what is the output of our cohort totals?
  out <- list(by.TIME=out.full.by.TIME,
              by.AGE=out.full.by.AGE,
              by.AGE.cum.t=out.full.by.AGE.cumulative.trim,
              summary=rowSums(out.full.by.TIME,na.rm=T))

  # plots---
  # how did cohorts fare at different points? Plot by TIME
  plot(years,out.full.by.TIME[1,],ylim=c(0,max(out.full.by.TIME,na.rm=T)),pch="",main="Cohort Receipts/Distributions\n Across TIME")
  for(i in 1:nrow(out.full.by.TIME)) lines(years,out.full.by.TIME[i,],col=i,lwd=2)
  legend("topleft",c(paste(rownames(out.full.by.TIME))),lwd=1,lty=1,col=1:nrow(out.full.by.TIME))

  # Have a look at running totals
  plot(out$summary,ylim=c(0,max(out$summary)),xaxt="n",xlab="age groups",col="dark orange",pch=16,main="TOTAL SO FAR\nUp to 2013")
  axis(1, at=1:length(out$summary), labels=names(out$summary))

  # how are cohorts tracking? Plot by cumulative age
  plot(ages,out.full.by.AGE.cumulative.trim[1,],ylim=c(0,max(out.full.by.AGE.cumulative.trim,na.rm=T)),pch="",main="Cohort Receipts/Distributions\n By AGE\nHow are they tracking?")
  for(i in 1:nrow(out.full.by.AGE.cumulative.trim)) lines(ages,out.full.by.AGE.cumulative.trim[i,],col=i,lwd=2)
  legend("topleft",c(paste(rownames(out.full.by.AGE.cumulative.trim))),lwd=1,lty=1,col=1:nrow(out.full.by.AGE.cumulative.trim))

  return(out)
}
