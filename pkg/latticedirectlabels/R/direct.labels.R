library(lattice)
library(grid)
library(nlme)
data(BodyWeight)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
top.points <- function(...){
  browser()
}
direct.labels <- function(x, ..., group.number) {
  panel.densityplot(x=x, ...)
  d <- density(x)
  maxy <- which.max(d$y)
  grid.text(levels(loci$type)[group.number],
            d$x[maxy], d$y[maxy],
            default.units="native",just="bottom")
}
densityplot(~ppp,loci,groups = type,panel = direct.labels,
            method=top.points,
            show.points=F,n=500)
direct.labels2 <- function(x,y,...,group.number){
  panel.xyplot(x=x,y=y,...)
  i <- 1
  f <- get("groups",env=parent.frame())
  lab <- levels(f)[group.number]
  grid.text(lab,x[i],y[i],default.units="native",just="right")
}









## verify if we matched everything up right
sapply(levels(BodyWeight$Rat),function(id)length(BodyWeight[BodyWeight$Rat==id,"weight"]))
reshape(BodyWeight,idvar=c("Rat","Diet"),timevar="Time",dir="wide")


library(plyr)
ddply(BodyWeight,.(Rat),summarise,mean=mean(weight))
## Conclusion: ddply is the most concise way of doing this.


get.means <- function(d,debug=F)ddply(d,.(groups),summarise,x=mean(x),y=mean(y))
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
direct.labels <- function(x,y,subscripts,groups,debug=F,method=parallel.lines,...){
  panel.superpose(x,y,subscripts,groups,...)
  groups <- groups[subscripts]
  d <- data.frame(x,y,groups)
  labs <- method(d,debug)
  print(labs)
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
empty.grid.2 <- function(d,debug)empty.grid(d,debug,parallel.lines)
library(ggplot2)
data(mpg)
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
xyplot(.resid~.fitted,mpgf,groups=factor(cyl),method=get.means,panel=direct.labels)
xyplot(.resid~.fitted,mpgf,groups=factor(cyl),method=parallel.lines,panel=direct.labels)
xyplot(.resid~.fitted,mpgf,groups=factor(cyl),method=empty.grid,panel=direct.labels)
xyplot(.resid~.fitted,mpgf,groups=factor(cyl),method=empty.grid.2,panel=direct.labels)
xyplot(.resid~.fitted,mpgf,groups=factor(class),method=get.means,panel=direct.labels)
xyplot(.resid~.fitted,mpgf,groups=factor(class),method=parallel.lines,panel=direct.labels)
xyplot(.resid~.fitted,mpgf,groups=factor(class),method=empty.grid,panel=direct.labels)
xyplot(.resid~.fitted,mpgf,groups=factor(class),method=empty.grid.2,panel=direct.labels)
## there are 4 methods so far for placement of group labels on a
## scatterplot:
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
debug(compare.methods)
compare.methods(meth.list)

first.point <- function(d,debug=F){
  ddply(d,.(groups),summarise,x=x[1],y=y[1])
}
xyplot(weight~Time|Diet,BodyWeight,type='l',groups=Rat,layout=c(3,1),
       panel=direct.labels,method=first.point)
