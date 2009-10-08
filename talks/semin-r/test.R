data(Chem97,package="mlmRev")
head(Chem97)

library(lattice)
histogram(~gcsescore,Chem97)
histogram(~gcsescore|factor(score),Chem97)
densityplot(~gcsescore|factor(score),Chem97,groups=gender)
## simple one-time positioning function:
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g")))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)))
direct.label(qqmath(~gcsescore,Chem97,groups=gender,type=c("p","g"),f.value=ppoints(100)),method=function(d,...)data.frame(x=c(-2,0),y=c(6,4),groups=c("M","F")))
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

VADeaths
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","rate")
head(vad)
barchart(age~rate|demographic,vad,layout=c(4,1),origin=0)
dotplot(age~rate|demographic,vad,layout=c(4,1))
dots <- dotplot(age~rate,vad,groups=demographic,type="o")
dots
direct.label(dots)
direct.label(dots,method=function(...)data.frame(last.points(...),rot=30))
direct.label(dots,extra=list(rot=90,hjust=-0.1))

data(Earthquake,package="nlme")
head(Earthquake)
xyplot(accel~distance,Earthquake)
xyplot(accel~distance,Earthquake,scales=list(log=TRUE))
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),
       type=c("p"))
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),
       type=c("p","g"))
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
