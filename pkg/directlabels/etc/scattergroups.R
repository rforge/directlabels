library(directlabels)
data(mpg,package="ggplot2")
## direct label simple ggplot2 scatterplot
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
slab <- direct.label(scatter,list(extreme.grid,dl.move("suv",15,15)))
print(slab)

## scatterplot in lattice:
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
mpg.scatter <- xyplot(.resid~.fitted,mpgf,groups=factor(cyl))
plot(direct.label(mpg.scatter))
plot(direct.label(mpg.scatter,list(function(d,...){
  allm <- calc.boxes(get.means(d))
  for(g in levels(d$groups)){
    x <- subset(d,groups==g)
    m <- subset(allm,groups==g)
    x$a <- x$y - m$y
    x$b <- x$x - m$x
    x$h <- sqrt(x$a^2+b^2)
    x$xt <- x$x + m$w/2 * x$b/x$h ## b/h = cos(theta)
    x$yt <- x$y + m$h/2 * x$a/x$h ## a/h = sin(theta)
    x$dist <- (x$xt-m$x)^2+(x$yt-m$y)^2
    ## consider only subset of boxes that contain no points
    ## take the box with the minimal distance
    ## add the box's 4 points to the list of points
  }
})))
