
## Easy fix for confusing legend: direct labels
library(latticedl)
long <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
direct.label(long)

## Even works in black and white
direct.label(update(long,par.settings=standard.theme(color=FALSE)))

## Change label positions with the method argument
direct.label(long,method=last.points)

## Make your own positioning function using dl.indep
direct.label(long,method=dl.indep(d[which.max(d$x),]))

## You can change text parameters (see ?grid::grid.text for full list)
direct.label(dots2,method=list("last.points",rot=30))

## Load some data on car fuel efficiency
data(mpg,package="ggplot2")
head(mpg)

## Plot city versus highway fuel efficiency
xyplot(cty~hwy,mpg,aspect=1)

## Add a reference line x=y
panel.xyref <- function(...){
  panel.xyplot(...)
  panel.abline(0,1)
}
xyplot(cty~hwy,mpg,aspect=1,panel=panel.xyref)

## Jitter the data to see all the points
xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,panel=panel.xyref)

## Group data by number of cylinders in the engine
direct.label(xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,panel=panel.xyref,groups=factor(cyl)))

## Group data by car class
direct.label(xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,panel=panel.xyref,groups=class))

## Compare direct labeling methods
compare.methods(c("empty.grid","empty.grid.2"),xyplot,mpg,jitter(cty)~jitter(hwy),class,aspect=1,panel=panel.xyref)

