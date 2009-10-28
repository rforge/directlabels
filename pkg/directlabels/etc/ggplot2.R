library(ggplot2)


loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
dp <- qplot(ppp,data=loci,colour=type,geom="density")
dp
pdf("densityplot-ggplot2.pdf",width=5,height=5)
direct.label(dp,method=list(trans.densityplot,top.points))
dev.off()
system("convert densityplot-ggplot2.pdf densityplot-ggplot2.png")
system("display densityplot-ggplot2.png")

data(BodyWeight,package="nlme")
p <- ggplot(BodyWeight, aes(Time, weight, colour = Rat)) +
  geom_line()  +
  ##geom_text(aes(label = Rat), subset = .(Time == min(Time)),hjust=1) +
  facet_grid(~ Diet) +
  ##theme_bw()+
  scale_colour_identity()
pdl <- direct.label(p,last.points)
pdl
qpdl <- qplot(Time,weight,data=BodyWeight,colour=Rat,geom="line",facets=.~Diet)
qpdl
direct.label(qplot(Time,weight,data=BodyWeight,colour=Rat,geom="line",facets=.~Diet),last.points)
## error, ok since no colour specified:
direct.label(qplot(Time,weight,data=BodyWeight,group=Rat,geom="line",facets=.~Diet),last.points)


library(ggplot2)
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
p2 <- qplot(deaths,age,data=vad,group=demographic,geom="line",colour=demographic)+xlim(5,80)
p2
direct.label(p2,list(last.points,rot=30),TRUE)
## Compare with manual addition of direct labels:
p2+geom_text(aes(label=demographic),subset=.(age==levels(age)[nlevels(age)]),hjust=0,angle=30)+opts(legend.position="none")
direct.label(p2,function(d,...)data.frame(subset(d,y==max(y)),hjust=0,rot=30))
direct.label(p2,list(dl.indep(d[which.max(d$x),]),hjust=0,rot=30))
direct.label(p2,list(last.points,rot=30))
ggsave("ex.pdf");system("xpdf ex.pdf")
