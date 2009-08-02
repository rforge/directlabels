library(proto)
library(ggplot2)
data(mpg)
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
plot(dl(xyplot,mpgf,.resid~.fitted,factor(cyl),
        panel=function(...){panel.abline(1);panel.xyplot(...)},main="foobar2",
        method=parallel.lines))
plot(dl(xyplot,mpgf,.resid~.fitted,factor(cyl),debug=T))
