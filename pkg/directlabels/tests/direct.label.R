library(directlabels)

data(mpg,package="ggplot2")
## direct label simple ggplot2 scatterplot
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
slab <- direct.label(scatter,list(extreme.grid,dl.move("suv",15,15)))
print(slab)

## scatterplot in lattice:
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
mpg.scatter <- xyplot(.resid~.fitted,mpgf,groups=factor(cyl))
plot(direct.label(mpg.scatter))
## debug=TRUE shows more output:
plot(direct.label(mpg.scatter,debug=TRUE))
## try custom panel functions and title:
mpgs2 <- update(mpg.scatter,
                panel=function(...){panel.abline(1);panel.xyplot(...)},
                main="foobar2")
plot(direct.label(mpgs2))

## Should plot but show direct label placement error in each panel:
## default method includes perpendicular line calculation, which makes
## no sense for only 1 group per panel
trellised <- xyplot(.resid~.fitted|cyl,mpgf,groups=factor(cyl))
plot(direct.label(trellised))
## should work, but not very informative:
plot(direct.label(trellised,method=empty.grid,TRUE))


data(Chem97,package="mlmRev")
qqm <- qqmath(~gcsescore,Chem97,groups=gender,
              type=c("p","g"),f.value=ppoints(25),auto.key=TRUE)
plot(direct.label(qqm))
static.labels <- data.frame(x=c(-2,0),y=c(6,4),groups=c("F","M"))
plot(direct.label(qqm,method=function(d,...)static.labels,debug=TRUE))
plot(direct.label(qqm,method=static.labels,debug=TRUE))
## Should work: static.labels overwrites values from
## last.points. Applying last.points should change the hjust as well:
plot(direct.label(qqm,method=c("last.points",static.labels),debug=TRUE))
plot(direct.label(qqm,method=c(static.labels,"last.points"),debug=TRUE))
plot(direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),
                    type=c('l','g'),f.value=ppoints(100))))

## densityplot labeling
plot(direct.label(densityplot(~gcsescore,Chem97,groups=factor(score))))
## Try with several panels:
plot(direct.label(densityplot(~gcsescore|gender,Chem97,
                              groups=factor(score),layout=c(1,2))))
iris2 <- melt(iris,id="Species")
direct.label(densityplot(~value|variable,iris2,groups=Species,scales="free"))
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
plot(direct.label(
             densityplot(~ppp,loci,groups=type,n=500)
             ))
## Not very informative but it should work:
plot(direct.label(
             densityplot(~ppp|type,loci,groups=type,n=500)
             ))


## dotplot:
angled.endpoints <- list("last.points",rot=30)
plot(direct.label(dotplot(VADeaths,type="o"),method=angled.endpoints))
## Try the same plot with ggplot2
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
p2 <- qplot(deaths,age,data=vad,
            group=demographic,geom="line",colour=demographic)
print(direct.label(p2,angled.endpoints)+xlim(5,80))

## label 2 groups of longitudinal data:
dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
ddf <- melt(as.data.frame(dts),id="time")
names(ddf) <- c("time","sex","deaths")
plots <- list(lattice=xyplot(deaths~time,ddf,groups=sex,type="l"),
              ggplot2=qplot(time,deaths,data=ddf,colour=sex,geom="line"))
for(p in plots)print(direct.label(p)) ## should default to lines2

## lineplot labeling
sprayplot <- xyplot(decrease~treatment,OrchardSprays,groups=rowpos,type="a")
direct.label(sprayplot) ## should be last.points
direct.label(sprayplot,last.smart) ## better
data(BodyWeight,package="nlme")
ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
print(direct.label(ratplot))
print(direct.label(ratplot,last.points))
print(direct.label(ratplot,last.smart)) ## not so good actually
## Say we want to use a simple linear model to explain rat body weight:
fit <- lm(weight~Time+Diet+Rat,BodyWeight)
bw <- fortify(fit,BodyWeight)
## And we want to use this panel function to display the model fits:
panel.model <- function(x,subscripts,col.line,...){
  panel.xyplot(x=x,subscripts=subscripts,col.line=col.line,...)
  llines(x,bw[subscripts,".fitted"],col=col.line,lty=2)
}
## Just specify the custom panel function as usual:
rat2 <- update(ratplot,
               panel=panel.superpose,
               panel.groups=panel.model)
print(direct.label(rat2,last.points))

## complicated ridge regression lineplot ex. fig 3.8 from Elements of
## Statistical Learning, Hastie et al.
myridge <- function(f,data,lambda=c(exp(-seq(-15,15,l=200)),0)){
  require(MASS)
  fit <- lm.ridge(f,data,lambda=lambda)
  X <- data[-which(names(data)==as.character(f[[2]]))]
  Xs <- svd(scale(X)) ## my d's should come from the scaled matrix
  dsq <- Xs$d^2
  ## make the x axis degrees of freedom
  df <- sapply(lambda,function(l)sum(dsq/(dsq+l)))
  D <- data.frame(t(fit$coef),lambda,df) # scaled coefs
  molt <- melt(D,id=c("lambda","df"))
  ## add in the points for df=0
  limpts <- transform(subset(molt,lambda==0),lambda=Inf,df=0,value=0)
  rbind(limpts,molt)
}
data(prostate,package="ElemStatLearn")
pros <- subset(prostate,train==TRUE,select=-train)
m <- myridge(lpsa~.,pros)
p <- xyplot(value~df,m,groups=variable,type="o",pch="+",
            panel=function(...){
              panel.xyplot(...)
              panel.abline(h=0)
              panel.abline(v=5,col="grey")
            },
            main="Ridge regression shrinks least squares coefficients",
            ylab="scaled coefficients",
            sub="grey line shows coefficients chosen by cross-validation",
            xlab=expression(df(lambda)))
##pdf("figure3.8.pdf",h=5,w=5)
direct.label(update(p,xlim=c(0,9.25)),
             list(last.smart,cex=0.75,dl.trans(x=x+0.1)))
##dev.off()
##system("xpdf figure3.8.pdf")
