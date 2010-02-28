data(mpg,package="ggplot2")
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
xyplot(jitter(.resid)~jitter(.fitted)|manufacturer,mpgf,groups=class)
