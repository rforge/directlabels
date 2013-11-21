works_with_R("3.0.2", R.matlab="2.0.6",
             reshape2="1.2.2", plyr="1.8")

timeProjection <- readMat("timeProjection1_0.10.mat")
dimnames(timeProjection$runTime) <-
  list(vector.length=timeProjection$elementRange,
       seed=NULL,
       method=c("Random","Heap","Sort"))
molt <- melt(timeProjection$runTime, value.name="seconds")
projectionSeconds <- ddply(molt, .(vector.length, method), summarize,
                           mean=mean(seconds), sd=sd(seconds),
                           min=min(seconds), max=max(seconds))
save(projectionSeconds, file="../data/projectionSeconds.RData")
