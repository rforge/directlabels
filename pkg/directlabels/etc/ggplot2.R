library(ggplot2)


loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
direct.label(
             qplot(ppp,data=loci,colour=type,geom="density")
             ,method=list(trans.densityplot,top.points))


library(nlme)
data(BodyWeight)
p <- ggplot(BodyWeight, aes(Time, weight, colour = Rat)) +
  geom_line()  +
  ##geom_text(aes(label = Rat), subset = .(Time == min(Time)),hjust=1) +
  facet_grid(~ Diet) +
  ##theme_bw()+
  scale_colour_identity()
pdl <- direct.label(p,last.points)
pdl
direct.label(qplot(Time,weight,data=BodyWeight,colour=Rat,geom="line",facets=.~Diet),last.points)+scale_colour_identity()
## error, ok since no colour specified:
direct.label(qplot(Time,weight,data=BodyWeight,group=Rat,geom="line",facets=.~Diet),last.points)+scale_colour_identity()



vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
p2 <- qplot(deaths,age,data=vad,group=demographic,geom="line",colour=demographic)
p2
p3 <- direct.label(p2,list(last.points,rot=30),TRUE)
p3
p3+scale_colour_identity()## error, how do we hide the legend here?

