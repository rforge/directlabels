library(ggplot2)
library(nlme)
data(BodyWeight)

ggplot(BodyWeight, aes(Time, weight, colour = subject)) +
  geom_line()  +
  geom_text(aes(label = subject), subset = .(Time == min(Time))) +
  facet_wrap(~ treatment)
