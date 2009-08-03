compare.methods <- function
### Plot several label placement methods on the same page.
(m,
### Vector of label placement function names.
...
### Args to pass to dl
 ){
  mc <- match.call()
  mc[[1]] <- quote(dl)
  mc <- mc[-2]
  newpage <- TRUE
  L <- length(m)
  for(i in seq_along(m)){
    mc$main <- m[i]
    mc$method <- m[i]
    P <- eval(mc)
    plot(P,split=c(1,i,1,L),newpage=newpage)
    newpage <- FALSE
  }
}
##compare.methods(meth.list)

