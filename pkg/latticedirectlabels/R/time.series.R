first <- function(d,debug=FALSE){
  data.frame(d[1,c("x","y")],hjust=1,vjust=0.5)
}
first.points <- dl.indep(first)
