concours <- read.csv("concours.csv")
p <- xyplot(Tx.Sup ~ Tx.TB, concours,group=Concours, type="o")
library(directlabels)
direct.label(update(p,xlim=c(5,100)),list(last.qp))
## special pos method for this data set
angled.midpoints <- dl.indep({
  within(d,{
    dx <- x[1]-x[2]
    dy <- y[1]-y[2]
    rot <- atan(dy/dx)/pi*180
    x <- mean(x)
    y <- mean(y)
  })[1,]
})
direct.label(p,angled.midpoints,TRUE)
