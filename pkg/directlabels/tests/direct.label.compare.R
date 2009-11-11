library(directlabels)
dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
ddf <- melt(as.data.frame(dts),id="time")
names(ddf) <- c("time","sex","deaths")
plots <- list(lattice=
              xyplot(deaths~time,ddf,groups=sex,type="l",xlim=c(-10,80)),
              ggplot2=
              qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-10,80))
pos.funs <- list("first.points","lines2")
direct.label.compare(plots,pos.funs)
direct.label.compare(plots[1],pos.funs[1])
direct.label.compare(plots[1],pos.funs)
direct.label.compare(plots,pos.funs[1])
named.funs <- list(first.points=first.points,lines2=lines2)
mixed.funs <- list("first.points",lines2=lines2,last.points)
not.named <- structure(named.funs,names=NULL)
unlabeled.plots <- structure(plots,names=NULL)
direct.label.compare(plots,mixed.funs[3])
## this should work with all combinations, with varying labels
for(ml in list(pos.funs,named.funs,mixed.funs,not.named))
  for(pl in list(plots,unlabeled.plots))direct.label.compare(pl,ml))
