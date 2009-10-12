## Lattice allows easy visualization of many variables
options(width=60)
library(lattice)
dotplot(variety ~ yield | site, data = barley, groups = year,auto.key=list(space="right"),layout=c(1,6),xlab = "Barley Yield (bushels/acre)")

## Aspect ratio in scatterplots is important
xyplot(sunspot.year~1700:1988,xlab="Year",type="l",scales=list(x=list(alternating=2)),main = "Yearly Sunspots")

## Lattice also automatically calculates aspect ratio for optimal decoding
xyplot(sunspot.year~1700:1988,xlab="Year",type="l",scales=list(x=list(alternating=2)),main = "Yearly Sunspots",aspect="xy")

## Load a data set
data(Chem97,package="mlmRev")
head(Chem97)

## Simple histogram
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
show.settings(br)

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

## Plot 3 continuous variables using color
levelplot(z~x*y,grid)

## Use a different color scale
my.colors <- sapply(0:100,function(l)hcl(l=l))
levelplot(z~x*y,grid,col.regions=my.colors)

## Data in matrix form of volcano heights
dim(volcano)
print(volcano[1:5,1:5])

## Plot volcano elevations in a matrix using color
levelplot(volcano,col.regions=my.colors)

## Use 3d wireframe plots
wireframe(volcano,drape=TRUE,col.regions=my.colors)

## Combine plots using latticeExtra
library(latticeExtra)
both <- c(wireframe(volcano,drape=TRUE),levelplot(volcano))
both

## Globally change the plot parameters
trellis.par.set(regions=list(col=my.colors))
both

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

## Change legend appearance
qqmath(~gcsescore,Chem97,groups=gender,auto.key=list(lines=TRUE,points=FALSE,space="right"),f.value=ppoints(100),type="l")

## Longitudinal data
data(BodyWeight,package="nlme")
head(BodyWeight)

## Even more confusing legend
xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1),auto.key=list(space="right"))





