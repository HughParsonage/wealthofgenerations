

# --- --- --- --- --- ---
# plot age-breakdown ----
plot_agebreak <- function(df,years,...,leg.pos="topleft"){
  # get modified colheadings (get rid of 'X' where applicable)
  headings <- gsub("[X|x]","",colnames(df))

  agerange <- as.numeric(rownames(df))
  # plot
  plot(agerange,df[,1],ylim=c(0,max(df[,headings %in% years])),pch="",...)
  for(i in 1:length(years)) lines(agerange,df[,headings %in% years[i]],col=i,lwd=2)
  legend(leg.pos,c(paste(years)),col=1:length(years),lty=1)
}
