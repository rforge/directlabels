### Positioning Function for the first of a group of points.
first.points <-
  dl.indep(data.frame(d[which.min(d$x),],hjust=1,vjust=0.5))

### Positioning Function for the last of a group of points.
last.points <- dl.indep(data.frame(d[which.max(d$x),],hjust=0,vjust=0.5))

### Do first or last, whichever has points most spread out.
maxvar.points <- function(d,...){
  myrange <- function(x){
    if(is.factor(x))levels(x)[c(1,nlevels(x))]
    else range(x)
  }
  vars <- sapply(myrange(d$x),function(v)var(subset(d,x==v)$y))
  FUN <- if(diff(vars)<0)first.points else last.points
  FUN(d,...)
}

### Label last points, bumping labels up if they collide.
last.bumpup <- list(last.points,bumpup)

### Label first points, bumping labels up if they collide.
first.bumpup <- list(first.points,bumpup)

### Label last points from QP solver that ensures labels do not collide.
last.qp <- list(last.points,calc.boxes,qp.labels("y","h"))

### Label first points from QP solver that ensures labels do not collide.
first.qp <- list(first.points,calc.boxes,qp.labels("y","h"))

lines2 <- function
### Positioning Function for 2 groups of longitudinal data. One curve
### is on top of the other one (on average), so we label the top one
### at its maximal point, and the bottom one at its minimal
### point. Vertical justification is chosen to minimize collisions
### with the other line. This may not work so well for data with high
### variability, but then again lineplots may not be the best for
### these data either.
(d,
### The data.
 offset=0.3,
### Offset from 0 or 1 for the vjust values.
 ...
### ignored.
 ){
  top <- 0-offset
  bottom <- 1+offset
  y <- ddply(d,.(groups),function(d)mean(d$y))
  ddply(y,.(groups),function(D){
    biggest.on.average <- D$V==max(y$V)
    f <- if(biggest.on.average)max else min
    ld <- subset(d,groups==D$groups)
    pos <- ddply(subset(ld,y==f(ld$y)),.(groups),function(x)
          data.frame(x=max(x$x)-diff(range(x$x))/2,y=x$y[1]))
    other <- subset(d,groups!=D$groups)
    other.y <- other[which.min(abs(other$x-pos$x)),"y"]
    smaller.here <- pos$y<other.y
    data.frame(pos,vjust=if(biggest.on.average)
               if(smaller.here)bottom else top#bigger mean
               else if(smaller.here)bottom else top)#smaller mean
  })
}
