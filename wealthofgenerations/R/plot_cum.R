#' Plot cumulative position wrt welfare state
#'
#'
#'

plot_cum <- function(cumulative.mat){
  plot(ageall,cumulative.mat[,1],ylim=c(min(cumulative.mat,na.rm=T),max(cumulative.mat,na.rm=T)),
       pch="",main="Net position of Cohort\n By AGE\nHow are they tracking?",ylab="% of GDP")
  for(i in 1:ncol(cumulative.mat)) lines(ageall,cumulative.mat[,i],col=i,lwd=2)
  legend("topright",c(paste(colnames(cumulative.mat))),lwd=1,lty=1,col=1:nrow(cumulative.mat))
  abline(h=0,col="gray")
}
