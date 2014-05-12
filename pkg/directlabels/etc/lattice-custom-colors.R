## Test dotplot directlabels
## La couleur des labels ne correspond pas à celle de la courbe à laquelle il se rapporte
## Jean-Luc Flot, 2014-05-10

library(lattice)
library(directlabels)
##library(quadprog)
library(RColorBrewer)
palette(brewer.pal(10, "RdYlGn"))
palette()
setwd("C:/")
load(file=paste("test dotplot directlabel.RData"))

ls()

w

v <- dotplot(mediane~date, w, groups = cluster, ylab = "médiane par cluster" ,type = "o", col=1:10 ,las = 2 , lwd = 3 )
print(v)
print(direct.label(v ))
