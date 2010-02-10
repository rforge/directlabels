### Positioning Function for the mean of each cluster of points.
get.means <-
  dl.indep(unique(transform(d,x=mean(x),y=mean(y))))

perpendicular.lines <- function
### Draw a line between the centers of each cluster, then draw a
### perpendicular line for each cluster that goes through its
### center. For each cluster, return the point the lies furthest out
### along this line.
(d,
### Data frame with groups x y.
 debug=FALSE,
### If TRUE will draw points at the center of each cluster and some
### lines that show how the points returned were chosen.
 ...
### ignored.
 ){
  means <- rename(get.means(d),list(x="mx",y="my",groups="groups"))
  big <- merge(d,means,by="groups")
  fit <- lm(my~mx,means)
  b <- coef(fit)[1]
  m <- coef(fit)[2]
  big2 <- transform(big,x1=(mx+x+(my-y)*m)/2)
  big3 <- transform(big2,y1=m*(x1-x)+y)
  big4 <- transform(big3,
                    d=sqrt((x-x1)^2+(y-y1)^2),
                    dm=sqrt((x-mx)^2+(y-my)^2))
  big5 <- transform(big4,ratio=d/dm)
  winners <- ddply(big5,.(groups),subset,
                   subset=seq_along(ratio)==which.min(ratio))
  ## gives back a function of a line that goes through the designated center
  f <- function(v)function(x){
    r <- means[means$groups==v,]
    -1/m*(x-r$mx)+r$my
  }
  ##dd <- ddply(means,.(groups),summarise,x=x+sdx*seq(0,-2,l=5)[-1])
  ##dd$y <- mdply(dd,function(groups,x)f(groups)(x))$x
  if(debug){
    ## First find the mean of each cluster
    grid.points(means$mx,means$my,default.units="native")
    ## myline draws a line over the range of the data for a given fun F
    myline <- function(F)
      grid.lines(range(d$x),F(range(d$x)),default.units="native")
    ## Then draw a line between these means
    myline(function(x)m*x+b)
    ## Then draw perpendiculars that go through each center
    for(v in means$groups)myline(f(v))
  }
  winners[,c("x","y","groups")]
### Data frame with groups x y, giving the point for each cluster
### which is the furthest out along the line drawn through its center.
}

empty.grid <- function
### Label placement method for scatterplots that ensures labels are
### placed in different places. A grid is drawn over the whole
### plot. Each cluster is considered in sequence and assigned to the
### point on this grid which is closest to the point given by
### loc.fun().
(d,
### Data frame of points on the scatterplot with columns groups x y.
 debug=FALSE,
### Show debugging info on the plot? This is passed to loc.fun.
 loc.fun=get.means,
### Function that takes d and returns a data frame with 1 column for
### each group, giving the point we will use to look for a close point
### on the grid, to put the group label.
 ...
### ignored.
 ){
  ##browser()
  loc <- loc.fun(d,debug)
  NREP <- 10
  gl <- function(v){
    s <- seq(from=min(d[,v]),to=max(d[,v]),l=NREP)
    list(centers=s,diff=(s[2]-s[1])/2)
  }
  L <- sapply(c("x","y"),gl,simplify=FALSE)
  g <- expand.grid(x=L$x$centers,y=L$y$centers)
  g2 <- transform(g,
                  left=x-L$x$diff,
                  right=x+L$x$diff,
                  top=y+L$y$diff,
                  bottom=y-L$y$diff)
  if(debug){
    gridlines <- with(g2,list(x=unique(c(left,right)),y=unique(c(top,bottom))))
    drawlines <- function(a,b,c,d)
      grid.segments(a,b,c,d,"native",gp=gpar(col="grey"))
    with(gridlines,drawlines(min(x),y,max(x),y))
    with(gridlines,drawlines(x,min(y),x,max(y)))
  }
  inbox <- function(x,y,left,right,top,bottom)
    c(data=sum(d$x>left & d$x<right & d$y>bottom & d$y<top))
  count.tab <- cbind(mdply(g2,inbox),expand.grid(i=1:NREP,j=1:NREP))
  if(debug){
    count.mat <- matrix(count.tab$data,nrow=NREP,ncol=NREP,
                        byrow=TRUE,dimnames=list(1:NREP,1:NREP))[NREP:1,]
    mode(count.mat) <- "character"
  }
  g3 <- transform(subset(count.tab,data==0))
  res <- data.frame()
  for(v in loc$groups){
    r <- subset(loc,groups==v)
    len <- sqrt((r$x-g3$x)^2+(r$y-g3$y)^2)
    i <- which.min(len) ## the box to use for this group
    if(debug)
      count.mat[as.character(g3[i,"j"]),
                as.character(g3[i,"i"])] <- paste("*",v,sep="")
    res <- rbind(res,g3[i,c("x","y")])
    therow <- g3[i,]
    ## select new subset of boxes for next label
    g3 <- subset(g3,!(abs(i-therow$i)<=1 & j==therow$j))
  }
  if(debug)print(count.mat)
  cbind(res,groups=loc$groups)
### Data frame with columns groups x y, 1 line for each group, giving
### the positions on the grid closest to each cluster.
}

### Start with points from empty.grid then attempt to adjust labels so
### that they do not collide with any points.
empty.grid.collide <- function(d,...){
  loc <- calc.boxes(empty.grid(d,...))
  draw.rects(loc)
  print(loc)
}  

### Label the points furthest from the origin for each group.
extreme.points <- dl.indep({
  d <- transform(d,d=sqrt(x^2+y^2))
  d[which.max(d$d),]
})

### Use empty.grid with perpendicular.lines.
empty.grid.2 <- empty.grid.fun(perpendicular.lines)

### Use empty.grid with extreme.points.
extreme.grid <- empty.grid.fun(extreme.points)

follow.points <- function
### Scatterplot positioning function that draws a line between each
### center and every point, then follows the line out far enough to
### give a box that will not collide with it. Out of all the boxes
### constructed in this way that do not contain any points, take the
### one which has the smallest distance to the center. FIXME: does not
### work with ggplot2 since the ggplot2 backend doesn't yet have
### support of actually knowing how big the text bounding box is.
(d,debug=FALSE,...){
  allm <- enlarge.box(calc.boxes(get.means(d)))
  if(debug)draw.rects(allm)
  labtab <- data.frame()
  for(g in levels(d$groups)){
    x <- d
    m <- subset(allm,groups==g)
    x$a <- x$y - m$y
    x$b <- x$x - m$x
    x$h <- sqrt(x$a^2+x$b^2) ## hypotenuse of triangle, not box height!
    x$x <- x$x + m$w/2 * x$b/x$h *1.01 ## b/h = cos(theta)
    x$y <- x$y + m$h/2 * x$a/x$h *1.01 ## a/h = sin(theta)
    x$dist <- (x$x-m$x)^2+(x$y-m$y)^2
    x <- transform(x,
                   left=x-m$w/2,right=x+m$w/2,
                   top=y+m$h/2,bottom=y-m$h/2)
    x$points <- sapply(1:nrow(x),function(i)inside(d,x[i,]))
    ## consider only subset of boxes that contain no points
    x <- subset(x,points==0)
    ## take the box with the minimal distance
    x <- subset(x,dist==min(dist))[1,]
    labtab <- rbind(labtab,transform(x,x=x,y=y,groups=g))
    ## add the box's 4 points to the list of points
    newpoints <- d[1:4,]
    newpoints$x <- c(x$left,x$right,x$right,x$left)
    newpoints$groups <- g
    newpoints$y <- c(x$top,x$top,x$bottom,x$bottom)
    d <- rbind(d,newpoints)
  }
  labtab
}
