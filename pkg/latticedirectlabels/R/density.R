most.likely <- function(d,debug){
  dens <- density(d$x)
  maxy <- which.max(dens$y)
  data.frame(x=dens$x[maxy],y=dens$y[maxy])
}
top.points <- dl.indep(most.likely)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
print(dl(densityplot,loci,~ppp,type))
