## Easy fix for confusing legend: direct labels
library(latticedl)
qqm <- qqmath(~gcsescore,Chem97,groups=gender,f.value=ppoints(100),type="l")
direct.label(qqm)

## For longitudinal data we also label the first or last points
long <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
direct.label(long)

## Change label positions with the method argument
direct.label(long,method=last.points)

## Labelling method can also be specified as a list
direct.label(dots2,method=list("last.points",rot=45))

## Or as a fixed data frame for a specific plot
direct.label(qqm,method=data.frame(x=c(-2,0),y=c(6,4),groups=c("F","M")))

## Works for lots of colors
direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),type=c('l','g'),f.value=ppoints(100)))

## And even in black and white
direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),type=c('l','g'),f.value=ppoints(100),par.settings=standard.theme(color=FALSE)))

## Load some data on car fuel efficiency
data(mpg,package="ggplot2")
head(mpg)

## Plot city versus highway fuel efficiency
xyplot(cty~hwy,mpg,aspect=1)

## Jitter the data to see all the points
xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1)

## Group data by number of cylinders in the engine
direct.label(xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,groups=factor(cyl)))

## Group data by car class
direct.label(xyplot(jitter(cty)~jitter(hwy),mpg,aspect=1,groups=class))

## Compare direct labeling methods
compare.methods(c("empty.grid","empty.grid.2"),xyplot,mpg,jitter(cty)~jitter(hwy),class,aspect=1)
