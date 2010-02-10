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
plot(direct.label(mpg.scatter,list(cex=2,function(d,...){
  allm <- calc.boxes(get.means(d))
  labtab <- data.frame()
  for(g in levels(d$groups)){
    x <- d
    m <- subset(allm,groups==g)
    x$a <- x$y - m$y
    x$b <- x$x - m$x
    x$h <- sqrt(x$a^2+x$b^2)
    x$xt <- x$x + m$w/2 * x$b/x$h ## b/h = cos(theta)
    x$yt <- x$y + m$h/2 * x$a/x$h ## a/h = sin(theta)
    x$dist <- (x$xt-m$x)^2+(x$yt-m$y)^2
    x <- transform(x,
                   left=xt-m$w/2,right=xt+m$w/2,
                   top=yt+m$h/2,bottom=yt-m$h/2)
    x$points <- apply(x,1,function(row)
                      sum(x$x>row["left"] & x$x<row["right"] &
                          x$y<row["top"] & x$y>row["bottom"]))
    ## consider only subset of boxes that contain no points
    x <- subset(x,points==0)
    ## take the box with the minimal distance
    x <- subset(x,dist==min(dist))[1,]
    labtab <- rbind(labtab,transform(x,x=xt,y=yt,groups=g))
    ## add the box's 4 points to the list of points
    browser()
    newpoints <- with(x,data.frame(x=c(left,right,right,left),
                                   groups=g,
                                   y=c(top,top,bottom,bottom)))
    d <- rbind(d,newpoints)
  }
  labtab
})))
