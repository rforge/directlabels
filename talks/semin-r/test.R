data(VADeaths)
print(VADeaths)
library(lattice)
library(latticedl)
direct.label(
             dotplot(VADeaths,type="o")
             ,method=function(...)data.frame(last.points(...),rot=30))
library(latticeExtra)
c(l1,l2) #for combining lattice plots on the same page.
