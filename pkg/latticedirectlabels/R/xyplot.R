get.means <- function(d,debug=F)
  ddply(d,.(groups),summarise,x=mean(x),y=mean(y))
parallel.lines <- function(d,debug=F){
  means <- rename(get.means(d),c(x="mx",y="my"))
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
}
empty.grid <- function(d,debug=F,loc.fun=get.means){
  loc <- loc.fun(d,debug)
  gl <- function(v){
    s <- seq(from=min(d[,v]),to=max(d[,v]),l=10)
    list(centers=s,diff=(s[2]-s[1])/2)
  }
  L <- sapply(c("x","y"),gl,simplify=F)
  g <- expand.grid(x=L$x$centers,y=L$y$centers)
  g2 <- transform(g,
                  left=x-L$x$diff,
                  right=x+L$x$diff,
                  top=y+L$y$diff,
                  bottom=y-L$y$diff)
  inbox <- function(x,y,left,right,top,bottom)
    c(data=sum(d$x%inside%c(left,right) & d$y%inside%c(bottom,top)))
  g3 <- transform(subset(mdply(g2,inbox),data==0))
  res <- data.frame()
  for(v in loc$groups){
    r <- subset(loc,groups==v)
    len <- sqrt((r$x-g3$x)^2+(r$y-g3$y)^2)
    i <- which.min(len) ## the box to use for this group
    res <- rbind(res,g3[i,c("x","y")])
    g3 <- g3[-i,]
  }
  cbind(res,groups=loc$groups)
}
empty.grid.2 <- function(d,debug)empty.grid(d,debug,parallel.lines)

