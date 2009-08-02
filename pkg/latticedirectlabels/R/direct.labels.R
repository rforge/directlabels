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

### there are 4 methods so far for placement of group labels on a
### scatterplot
meth.list <- c("get.means","parallel.lines","empty.grid","empty.grid.2")

compare.methods <- function(m){
  newpage <- T
  L <- length(m)
  for(i in seq_along(m)){
    FUN <- get(m[i])
    P <- xyplot(.resid~.fitted,mpgf,main=m[i],
                groups=factor(class),method=FUN,panel=direct.labels)
    plot(P,split=c(1,i,1,L),newpage=newpage)
    newpage <- FALSE
  }
}
##compare.methods(meth.list)

dl.panel <- function
### Convert a normal panel into a direct label panel function
(panel
### The panel function to transform.
 ){
  function(...){
    panel(...)
    direct.labels(...)
  }
### A new panel function that first calls panel, then calls
### direct.labels.
}
direct.labels <- function
### Panel function that draws text labels for groups.
(x,
### x values of points to draw.
 y,
### y values of points to draw.
 subscripts,
### subscripts of groups to consider.
 groups,
### vector of groups.
 debug=FALSE,
### logical indicating whether debug annotations should be added to
### the plot.
 method=parallel.lines,
### function used to choose position of labels.
 ...
### ignored.
 ){
  groups <- groups[subscripts]
  d <- data.frame(x,y,groups)
  labs <- method(d,debug)
  just <- labs$just
  if(is.null(just))just <- "centre"
  Col <-
    if("col"%in%names(labs))labs$col
    else trellis.par.get("superpose.symbol")$col #FIXME
  grid.text(labs$groups,labs$x,labs$y,
            gp=gpar(col=Col),
            just=just,
            default.units="native")
}
dl <- function
### Lattice plot using direct labels.
(p,
### High-level lattice plot function to use.
 data,
### Data set to be passed to lattice.
 x,
### Plot formula to be passed to lattice.
 groups,
### To pass to high-level plot function.
 method=NULL,
### Method for direct labeling --- this is a function that accepts 2 arguments: d a data frame of the points to plot with columns x y groups, and debug a logical flag indicating if debug output should be shown. NULL indicates to make a logical choice based on the high-level plot function chosen.
 panel=NULL,
### Panel function to use. Defaults to corresponding default panel
### function for the high-level plot function.
 ...
### Other arguments to pass to the high-level plot function.
 ){
  m <- match.call()
  if(is.null(panel))panel <- get(paste("panel.",m$p,sep=""))
  if(is.null(method))method <- 
    switch(paste(m$p),
           xyplot=empty.grid.2
           )
  pdx <- dl.panel(panel)
  m$panel <- pdx
  m$method <- method
  m[[1]] <- m[[2]]
  eval(m[-2])
}
library(proto)#,lib="~/lib")
library(ggplot2)#,lib="~/lib")
data(mpg)
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
plot(dl(xyplot,mpgf,.resid~.fitted,factor(cyl),panel=function(...){panel.abline(1);panel.xyplot(...)},main="foobar2",method=parallel.lines))
