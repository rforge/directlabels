\name{dlcompare}
\alias{dlcompare}
\title{dlcompare}
\description{Compare several plots and/or label placement methods. This creates a custom grid graphics display based on lattice and/or ggplot2 output. This is possible because the direct.label function is generic. The result will be a plot matrix with plots on the columns and positioning methods on the rows. Names from the lists will be used to annotate the plot. If label placement method list elements are not named, but are given as a character string designating a Positioning Function, we will use this name for the label.}
\usage{
dlcompare(plots, pos.funs)
}
\arguments{
  \item{plots}{List of ggplot2 or lattice plots.}
  \item{pos.funs}{List of label placement methods to apply to each plot.}
}
\details{
}
\value{
}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@inria.fr>}
\note{
}


\seealso{
}
\examples{library(directlabels)
dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
ddf <- melt(as.data.frame(dts),id="time")
names(ddf) <- c("time","sex","deaths")
plots <- list(lattice=
              xyplot(deaths~time,ddf,groups=sex,type="l",xlim=c(-10,80)),
              ggplot2=
              qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-10,80))
pos.funs <- list("first.points","lines2")
dlcompare(plots,pos.funs)
dlcompare(plots[1],pos.funs[1])
dlcompare(plots[1],pos.funs)
dlcompare(plots,pos.funs[1])
named.funs <- list(first.points=first.points,lines2=lines2)
mixed.funs <- list("first.points",lines2=lines2,last.points)
not.named <- structure(named.funs,names=NULL)
unlabeled.plots <- structure(plots,names=NULL)
dlcompare(plots,mixed.funs[3])
## this should work with all combinations, with varying labels
for(ml in list(pos.funs,named.funs,mixed.funs,not.named))
  for(pl in list(plots,unlabeled.plots))dlcompare(pl,ml)}