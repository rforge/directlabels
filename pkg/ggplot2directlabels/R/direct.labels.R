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
