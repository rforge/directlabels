library(ggplot2)
library(nlme)
data(BodyWeight)
p <- ggplot(BodyWeight, aes(Time, weight, colour = Rat)) +
  geom_line()  +
  geom_text(aes(label = Rat), subset = .(Time == min(Time)),hjust=1) +
  facet_grid(~ Diet) +
  ##theme_bw()+
  scale_colour_identity()
pp <- function(w){
  f <- paste(w,'png',sep='.')
  png(f,w=w)
  print(p)
  dev.off()
  system(paste("eog",f))
}
pp(500)
pp(1000)

vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
p2 <- qplot(deaths,age,data=vad,group=demographic,geom="line",colour=demographic)
direct.label(p2)
direct.label(p2,list(last.points,rot=30))
