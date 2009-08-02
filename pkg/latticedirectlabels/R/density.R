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


