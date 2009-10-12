## Easy fix for confusing legend: direct labels
library(latticedl)
qqm <- qqmath(~gcsescore,Chem97,groups=gender,f.value=ppoints(100),type="l")
direct.label(qqm)

## For longitudinal data we also label the first or last points
long <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
direct.label(long)

## Change label positions with the method argument
direct.label(long,method=last.points)

## Make your own positioning function
endpoint <- function(data.points,...){
  label.pos <- NULL
  for(g in unique(data.points$groups)){
    d <- subset(data.points,groups==g)
    label.pos <- rbind(label.pos,d[which.max(d$x),])
  };return(label.pos)}
direct.label(long,method=endpoint)

## Simplify the definition using dl.indep
endpoint2 <- dl.indep({
  d[which.max(d$x),]
})
direct.label(long,method=endpoint2)

## Labelling method can also be specified as a list
direct.label(long,method=list("endpoint2",hjust=0))

## You can change other text parameters (see ?grid.text for full list)
direct.label(update(dots2,xlim=c(min(vad$deaths),80)),method=list("last.points",rot=30))

## You can also use fixed label positions for a specific plot
direct.label(qqm,method=list(x=c(-2,0),y=c(6,4),groups=c("F","M")))

## Works for lots of colors
direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),type=c('l','g'),f.value=ppoints(100)))

## And even in black and white
direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),type=c('l','g'),par.settings=standard.theme(color=FALSE),f.value=ppoints(100)))

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
