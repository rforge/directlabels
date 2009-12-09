### Positioning Function for the first of a group of points.
first.points <-
  dl.indep(data.frame(d[which.min(d$x),],hjust=1,vjust=0.5))
left.points <- first.points
### Positioning Function for the last of a group of points.
last.points <- dl.indep(data.frame(d[which.max(d$x),],hjust=0,vjust=0.5))
right.points <- last.points
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
### Calculate boxes around labels, for collision detection.
calc.boxes <- function(d){
  h <- as.numeric(convertHeight(stringHeight("foo"),"native"))
  w <- as.numeric(sapply(as.character(d$groups),
                         function(x)convertWidth(stringWidth(x),"native")))
  transform(d,top=y+h/2,bottom=y-h/2,right=x+w,w=w,h=h)
}
### Sequentially bump labels up, starting from the bottom, if they
### collide with the label underneath.
collide.up <- function(d,...){
  d <- calc.boxes(d)[order(d$y),]
  for(i in 2:nrow(d)){
    dif <- d$bottom[i]-d$top[i-1]
    if(dif<0){
      d$bottom[i] <- d$bottom[i]-dif
      d$top[i] <- d$top[i]-dif
      d$y[i] <- d$y[i]-dif
    }
  }
  ##print(sapply(d,class))
  d
}
### Positioning Function that draws boxes around label positions. Need
### to have previously called calc.boxes. Does not edit the data
### frame.
draw.rects <- function(d,...){
  hjust <- vjust <- 0.5
  with(d,grid.rect(x,y,w,h,hjust=hjust,vjust=vjust,
                   default.units="native",gp=gpar(col="grey")))
  d
}
### Label last points but make sure labels do not collide.
last.smart <- list(last.points,collide.up)
### Label first points but make sure labels do not collide.
first.smart <- list(first.points,collide.up)
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
