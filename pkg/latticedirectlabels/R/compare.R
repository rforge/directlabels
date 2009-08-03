### there are 4 methods so far for placement of group labels on a
### scatterplot
meth.list <- c("get.means","parallel.lines","empty.grid","empty.grid.2")

compare.methods <- function
### Plot several label placement methods on the same page.
(m
### Vector of label placement function names.
 ){
  newpage <- TRUE
  L <- length(m)
  for(i in seq_along(m)){
    P <- dl(xyplot,mpgf,.resid~.fitted,factor(class),
            main=m[i],
            method=m[i])
    plot(P,split=c(1,i,1,L),newpage=newpage)
    newpage <- FALSE
  }
}
##compare.methods(meth.list)

