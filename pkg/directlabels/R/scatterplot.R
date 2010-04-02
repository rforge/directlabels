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
  loc <- loc.fun(d,debug)
  NREP <- 10
  gridpts <- d
  gl <- function(v){
    s <- seq(min(gridpts[,v]),max(gridpts[,v]),l=NREP)
    if(expand){
      dif <- s[2]-s[1]
      s <- seq(min(gridpts[,v])-expand*dif,
               max(gridpts[,v])+expand*dif,
               l=NREP+2*expand)
    }
    list(centers=s,diff=s[2]-s[1])
  }
  hgrid <- function(x,w){
    hboxes <- floor(diff(range(gridpts[,x]))/r[,w])
    (-expand:(hboxes+expand-1))*r[,w]+min(gridpts[,x])+r[,w]/2
  }
  if(debug)with(loc,grid.points(x,y,default.units="native"))
  draw <- function(g){
    gridlines <- with(g,list(x=unique(c(left,right)),y=unique(c(top,bottom))))
    drawlines <- function(a,b,c,d)
      grid.segments(a,b,c,d,"native",gp=gpar(col="grey"))
    with(gridlines,drawlines(min(x),y,max(x),y))
    with(gridlines,drawlines(x,min(y),x,max(y)))
  }
  res <- data.frame()
  for(v in loc$groups){
    r <- subset(loc,groups==v)
    no.points <- data.frame()
    expand <- 0
    while(nrow(no.points)==0){
      boxes <- if("left"%in%names(loc)){
        list(x=hgrid("x","w"),y=hgrid("y","h"),w=r$w,h=r$h)
      }else{
        L <- sapply(c("x","y"),gl,simplify=FALSE)
        list(x=L$x$centers,y=L$y$centers,w=L$x$diff,h=L$y$diff)
      }
      boxes <- calc.borders(do.call(expand.grid,boxes))
      boxes <- cbind(boxes,data=inside(boxes,d))
      no.points <- transform(subset(boxes,data==0))
      expand <- expand+1 ## look further out if we can't find any labels inside
    }
    if(debug)draw(boxes)
    no.points <- transform(no.points,len=(r$x-x)^2+(r$y-y)^2)
    best <- subset(no.points,len==min(len))[1,]
    res <- rbind(res,transform(r,x=best$x,y=best$y))
    ## add points to cloud
    newpts <- with(best,data.frame(x=c(left,left,right,right,x,x,x,left,right),
                                   y=c(bottom,top,top,bottom,top,bottom,y,y,y)))
    newpts <- data.frame(newpts,
                         subset(d,select=-c(x,y))[1,,drop=FALSE],
                         row.names=NULL)
    d <- rbind(d,newpts[,names(d)])
  }
  if(debug)with(d,grid.points(x,y))
  res
### Data frame with columns groups x y, 1 line for each group, giving
### the positions on the grid closest to each cluster.
}

### Label the points furthest from the origin for each group.
extreme.points <- dl.indep({
  d <- transform(d,d=sqrt(x^2+y^2))
  d[which.max(d$d),]
})

### Use bounding box information with a small empty.grid. TODO: does
### not work with ggplot2 since the backend does not support bounding
### box calculation.
smart.grid <- empty.grid.fun(big.boxes)

### Use empty.grid with perpendicular.lines.
empty.grid.2 <- empty.grid.fun(perpendicular.lines)

### Use empty.grid with extreme.points.
extreme.grid <- empty.grid.fun(extreme.points)

follow.points <- function
### Draws a line between each center and every point, then follows the
### line out far enough to give a box outside the cloud. Out of all
### the boxes constructed in this way that do not contain any points,
### take the one which has the smallest distance to the center. FIXME:
### does not work with ggplot2 since the ggplot2 backend doesn't yet
### have support of actually knowing how big the text bounding box is.
(d,debug=FALSE,...){
  allm <- big.boxes(dl.jitter(d))
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
    x$points <- inside(x,d)
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
