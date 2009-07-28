library(ggplot2)
library(nlme)
data(BodyWeight)
ggplot(BodyWeight, aes(Time, weight, colour = Rat)) +
  geom_line()  +
  geom_text(aes(label = Rat), subset = .(Time == min(Time)),hjust=1) +
  facet_grid(~ Diet) +
  ##theme_bw()+
  scale_colour_identity()
