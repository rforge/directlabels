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
show.settings(standard.theme(color=FALSE))

## Change the settings
br <- simpleTheme(col=c("black","red"))
show.settings(rb)

## Change group colors with par.settings
densityplot(~gcsescore|factor(score),Chem97,groups=gender,auto.key=list(columns=2,space="bottom"),par.settings=br)

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
dotplot(age~deaths,vad,groups=demographic)

## Plots can be saved as R objects
dots <- dotplot(age~deaths,vad,groups=demographic)
dots

## Saved plots can be updated later
dots2 <- update(dots,type="o")
dots2

## Add a confusing legend ... how to label better?
update(dots2,auto.key=list())

## Load some earthquake measurements
data(Earthquake,package="nlme")
head(Earthquake)

## Scatterplot with xyplot
xyplot(accel~distance,Earthquake)

## Log scales with scales argument
xyplot(accel~distance,Earthquake,scales=list(log=TRUE))

## Type "p" is the default
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p"))

## Type "g" adds a grid
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p","g"))

## Type "smooth" adds a smooth line
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p","g","smooth"))

## Add some labels
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p","g","smooth"),xlab="Distance from epicenter (km)",ylab="Maximum horizontal acceleration (g)",main="Larger quakes are felt closer to the epicenter",sub="Data on log scale")

## Make some 3d data
x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
head(grid)

## Plot 3 categorical variables using color
levelplot(z~x*y,grid)

## Add more levels to the color scale
levelplot(z~x*y,grid,cuts=50)

## Use a different color scale
my.colors <- sapply(0:100,function(l)hcl(l=l))
levelplot(z~x*y,grid,cuts=50,col.regions=my.colors)

## Data in matrix form of volcano heights
dim(volcano)
volcano[1:5,1:5]

## Plot volcano elevations in a matrix using color
levelplot(volcano,col.regions=my.colors)

## Use 3d wireframe plots
wireframe(volcano,drape=TRUE,col.regions=my.colors)

## Combine plots using latticeExtra
library(latticeExtra)
trellis.par.set(regions=list(col=my.colors))
c(wireframe(volcano,drape=TRUE),levelplot(volcano))

## Box and whisker plots
bwplot(gcsescore~gender|factor(score),Chem97,layout=c(6,1))

## Compare the distribution (sample quantiles) of 2 variables
qq(gender~gcsescore|factor(score),Chem97)

## Force aspect ratio to be equal
qq(gender~gcsescore|factor(score),Chem97,aspect=1)

## Determine the normality of a variable
qqmath(~gcsescore,Chem97,groups=gender,auto.key=list(space="right"))

## Only plot 100 probability points (faster)
qqmath(~gcsescore,Chem97,groups=gender,auto.key=list(space="right"),f.value=ppoints(100))

## Use lines instead of points ... legend?
qqmath(~gcsescore,Chem97,groups=gender,auto.key=list(space="right"),f.value=ppoints(100),type="l")

## Easy fix for confusing legend: direct labels
library(latticedl)
direct.label(qqmath(~gcsescore,Chem97,groups=gender,f.value=ppoints(100),type="l"))


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
## Direct label the different groups
## Add a confusing legend ... how to label better?
update(dots2,auto.key=list())

library(latticedl)
direct.label(dots)
direct.label(dots,method=function(...)data.frame(last.points(...),rot=30))
direct.label(dots,method=list(rot=90,hjust=-0.1))

