\name{direct.label}
\Rdversion{1.1}
\alias{direct.label}
\title{direct label}
\description{Add direct labels to a grouped lattice plot. The idea is that we parse the trellis object returned by the high level plot function and return it changed such that it will plot direct labels.}
\usage{
direct.label(p, method = NULL, extra = list(), debug = FALSE)
}
\arguments{
  \item{p}{The lattice plot (result of a call to a high-level lattice function).}
  \item{method}{Method for direct labeling --- this is a function that accepts 2 arguments: d a data frame of the points to plot with columns x y groups, and debug a logical flag indicating if debug output should be shown. NULL indicates to make a logical choice based on the high-level plot function chosen.}
  \item{extra}{extra variables to be added to the label position data frame. This is useful for concisely specifying direct label placement parameters that are not a function of the data (ie if all the labels should be rotated by 30 degrees, use extra=list(rot=30)).}
  \item{debug}{Show debug output?}
}
\details{
}
\value{The lattice plot.}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>}
\note{
}


\seealso{
}
\examples{library(latticedl)
library(proto)
library(ggplot2)
data(mpg)
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
mpg.scatter <- xyplot(.resid~.fitted,mpgf,groups=factor(cyl))
plot(direct.label(mpg.scatter))
plot(direct.label(mpg.scatter,debug=TRUE))
plot(direct.label(
        xyplot(.resid~.fitted,mpgf,groups=factor(cyl),
               panel=function(...){panel.abline(1);panel.xyplot(...)},
               main="foobar2")
        ,method=perpendicular.lines))
## Should plot but show direct label placement error in each panel:
## default method includes perpendicular line calculation, which makes
## no sense for only 1 group per panel
trellised <- xyplot(.resid~.fitted|cyl,mpgf,groups=factor(cyl))
plot(direct.label(trellised))
## Should work, but not very informative:
plot(direct.label(trellised,method=empty.grid))
mpgf$cyl10 <- sapply(mpgf$cyl,function(i)paste(rep(i,l=10),collapse=""))
plot(direct.label(
        xyplot(.resid~.fitted|cyl,mpgf,groups=factor(cyl10))
        ,method=empty.grid))
## Some label placements fail, some dont:
plot(direct.label(
        xyplot(.resid~.fitted|manufacturer,mpgf,groups=factor(cyl))
        ,method=empty.grid.2))

data(BodyWeight,package="nlme")
print(direct.label(
         xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
         ))
## Say we want to use a simple linear model to explain rat body weight:
fit <- lm(weight~Time+Diet+Rat,BodyWeight)
bw <- fortify(fit,BodyWeight)
## And we want to use this panel function to display the model fits:
panel.model <- function(x,subscripts,col.line,...){
  panel.xyplot(x=x,subscripts=subscripts,col.line=col.line,...)
  llines(x,bw[subscripts,".fitted"],col=col.line,lty=2)
}
## Just specify the custom panel function as usual:
print(direct.label(
         xyplot(weight~Time|Diet,bw,groups=Rat,type='l',layout=c(3,1),
                panel=panel.superpose,panel.groups=panel.model)
         ))

## Fails: default method for scatterplot doesn't make sense here
##print(direct.label(xyplot,BodyWeight,weight~Time|Diet,Rat))
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
plot(direct.label(
             densityplot(~ppp,loci,groups=type,n=500)
             ))
## Not very informative but it should work:
plot(direct.label(
             densityplot(~ppp|type,loci,groups=type,n=500)
             ))}