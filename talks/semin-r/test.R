## Load a data set
data(Chem97,package="mlmRev")
head(Chem97)

## Simple histogram
library(lattice)
histogram(~gcsescore,Chem97)

## Histograms conditional on a categorical variable
histogram(~gcsescore|factor(score),Chem97)

## Conditioned plots of kernel density estimates
densityplot(~gcsescore|factor(score),Chem97)

## Conditioned and grouped density plots
densityplot(~gcsescore|factor(score),Chem97,groups=gender)

## Add a legend with the auto.key argument
densityplot(~gcsescore|factor(score),Chem97,groups=gender,auto.key=list())

## Hide the actual points with the plot.points argument
densityplot(~gcsescore|factor(score),Chem97,groups=gender,auto.key=list(),plot.points=FALSE)

## Legend layout with the columns argument
densityplot(~gcsescore|factor(score),Chem97,groups=gender,auto.key=list(columns=2),plot.points=FALSE)

## Legend positioning with the space argument
densityplot(~gcsescore|factor(score),Chem97,groups=gender,auto.key=list(columns=2,space="bottom"),plot.points=FALSE)

## Show all default settings
show.settings()

## Show settings good for printout
show.settings(col.whitebg())

## Change the settings
rb <- simpleTheme(col=c("black","red"))
show.settings(rb)

## Change group colors with par.settings
densityplot(~gcsescore|factor(score),Chem97,groups=gender,auto.key=list(columns=2,space="bottom"),par.settings=list(superpose.line=list(col=c("black","red"))))

## Query graphical parameters
names(trellis.par.get())

## Query graphical parameters for grouped lineplots
trellis.par.get("superpose.line")

## Shorter specification using simpleTheme
densityplot(~gcsescore|factor(score),Chem97,groups=gender,auto.key=list(columns=2,space="bottom"),par.settings=)

## simple one-time positioning function:
qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)),method=function(d,...)data.frame(x=c(-2,0),y=c(6,4),groups=c("M","F")))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)),method=data.frame(x=c(-2,0),y=c(6,4),groups=c("M","F")))
direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),type=c('l','g'),f.value=ppoints(100)))

qq(gender~gcsescore|factor(score),Chem97,type=c("p","g"),aspect=1)
bwplot(factor(score)~gcsescore|gender,Chem97)
bwplot(gcsescore~gender|factor(score),Chem97,layout=c(6,1))

head(quakes)
stripplot(depth~factor(mag),quakes,
          main="Depth of earthquake epicenters by magnitude",
          xlab="Magnitude (Richter)",
          ylab="Depth (km)")
xyplot(depth~factor(mag),quakes,
          main="Depth of earthquake epicenters by magnitude",
          xlab="Magnitude (Richter)",
          ylab="Depth (km)")

## Load a tabular data set
print(VADeaths)

## Convert to data frame to work with lattice
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
head(vad)

## Plot using bars
barchart(age~deaths|demographic,vad)

## Set the bar origin to 0 (less confusing)
barchart(age~deaths|demographic,vad,origin=0)

## Arrange the plots vertically to facilitate comparison
barchart(age~deaths|demographic,vad,layout=c(1,4),origin=0)

## Dotplots also work well for these data
dotplot(age~deaths|demographic,vad,layout=c(1,4))

## Connect the dots with the "o" type
dotplot(age~deaths|demographic,vad,layout=c(1,4),type="o")

## Use grouping instead of conditioning to facilitate comparison
dots <- dotplot(age~deaths,vad,groups=demographic,type="o")
dots

## Add a confusing legend ... how to label better?
update(dots,auto.key=list())

## Direct label the different groups
library(latticedl)
direct.label(dots)
direct.label(dots,method=function(...)data.frame(last.points(...),rot=30))
direct.label(dots,method=list(rot=90,hjust=-0.1))

## Load some earthquake measurements
data(Earthquake,package="nlme")
head(Earthquake)

## Scatter plot with xyplot
xyplot(accel~distance,Earthquake)

## Log scales with scales argument
xyplot(accel~distance,Earthquake,scales=list(log=TRUE))

## Type "p" is the default
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p"))

## Type
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p","g"))
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),
       type=c("p","g","smooth"))


x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
head(grid)
levelplot(z~x*y,grid)
levelplot(z~x*y,grid,cuts=50)
my.colors <- sapply(0:100,function(l)hcl(l=l))
levelplot(z~x*y,grid,cuts=50,col.regions=my.colors)

head(volcano)
levelplot(volcano)
levelplot(volcano,col.regions=my.colors)
wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10))



library(latticedl)
direct.label(densityplot(~gcsescore,Chem97,groups=gender))

library(lattice)
library(latticedl)
dlvad <- direct.label(
             dotplot(VADeaths,type="o")
             ,method=function(...)data.frame(last.points(...),rot=30))
library(latticeExtra)
c(dotplot(VADeaths,type="o",auto.key=list(space="right")),dlvad) #for combining lattice plots on the same page.
## simple one-time positioning function:
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g")))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)),method=function(d,...)data.frame(x=c(-2,0),y=c(6,4),groups=c("M","F")))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)),method=data.frame(x=c(-2,0),y=c(6,4),groups=c("M","F")))
direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),type=c('l','g'),f.value=ppoints(100)))
