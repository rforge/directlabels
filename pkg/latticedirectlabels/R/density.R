most.likely <- function(d,debug){
  dens <- density(d$x)
  maxy <- which.max(dens$y)
  data.frame(x=dens$x[maxy],y=dens$y[maxy],hjust=0.5,vjust=0)
}
top.points <- dl.indep(most.likely)
