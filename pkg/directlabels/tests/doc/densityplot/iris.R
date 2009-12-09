iris2 <- melt(iris,id="Species")
densityplot(~value|variable,iris2,groups=Species,scales="free")
