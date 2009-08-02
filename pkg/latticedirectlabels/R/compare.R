### there are 4 methods so far for placement of group labels on a
### scatterplot
meth.list <- c("get.means","parallel.lines","empty.grid","empty.grid.2")

compare.methods <- function(m){
  newpage <- T
  L <- length(m)
  for(i in seq_along(m)){
    FUN <- get(m[i])
    P <- xyplot(.resid~.fitted,mpgf,main=m[i],
                groups=factor(class),method=FUN,panel=direct.labels)
    plot(P,split=c(1,i,1,L),newpage=newpage)
    newpage <- FALSE
  }
}
##compare.methods(meth.list)

