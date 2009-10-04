data(VADeaths)
print(VADeaths)
library(lattice)
library(latticedl)
dlvad <- direct.label(
             dotplot(VADeaths,type="o")
             ,method=function(...)data.frame(last.points(...),rot=30))
library(latticeExtra)
c(dotplot(VADeaths,type="o",auto.key=list(space="right")),dlvad) #for combining lattice plots on the same page.
