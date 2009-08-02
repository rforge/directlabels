library(proto)
library(ggplot2)
data(mpg)
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
p <- dl(xyplot,mpgf,.resid~.fitted,factor(cyl))
pdfpng <- function(N,P,...){
  pdf.file <- paste(N,".pdf",sep="")
  png.file <- paste(N,".png",sep="")
  pdf(pdf.file,...)
  plot(P)
  dev.off()
  system(paste("convert",pdf.file,png.file))
}
pdfpng("scatter",p)

plot(dl(xyplot,mpgf,.resid~.fitted,factor(cyl),
        panel=function(...){panel.abline(1);panel.xyplot(...)},
        main="foobar2",
        method=parallel.lines))
plot(dl(xyplot,mpgf,.resid~.fitted,factor(cyl),debug=TRUE))
## Should fail: (default method includes parallel line calculation, which makes no sense for only 1 group per panel
plot(dl(xyplot,mpgf,.resid~.fitted|cyl,factor(cyl)))
## Should work, but not very informative:
plot(dl(xyplot,mpgf,.resid~.fitted|cyl,factor(cyl),method=empty.grid))
mpgf$cyl10 <- sapply(mpgf$cyl,function(i)paste(rep(i,l=10),collapse=""))
plot(dl(xyplot,mpgf,.resid~.fitted|cyl,factor(cyl10),method=empty.grid))
plot(dl(xyplot,mpgf,.resid~.fitted|manufacturer,factor(cyl),method=empty.grid.2))
