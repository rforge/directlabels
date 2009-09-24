data(VADeaths)
library(lattice)
library(latticedl)
direct.label(
             dotplot(VADeaths,type="o")
             ,method=function(...)data.frame(last.points(...),rot=30))
