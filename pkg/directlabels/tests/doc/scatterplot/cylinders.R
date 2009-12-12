data(mpg,package="ggplot2")
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
xyplot(.resid~.fitted,mpgf,groups=factor(cyl))
