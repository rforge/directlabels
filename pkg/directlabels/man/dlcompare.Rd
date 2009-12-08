\name{dlcompare}
\alias{dlcompare}
\title{Direct label comparison plot}
\description{Compare several plots and/or label placement methods. This creates
a custom grid graphics display based on lattice and/or ggplot2
output. This is possible because the direct.label function is
generic. Plots will be on the columns and positioning methods will
be on the rows.}
\usage{
dlcompare(plots, pos.funs)
}
\arguments{
  \item{plots}{List of ggplot2 or lattice plots. List names will be used to
annotate the plot.}
  \item{pos.funs}{List of label placement methods to apply to each plot. List names,
or function names if specified as character strings, will be used
to annotate the plot.}
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
              xyplot(deaths~time,ddf,groups=sex,type="l",xlim=c(-15,80)),
              ggplot2=
              qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-10,80))
pos.funs <- list("first.points","lines2")
##pdf("compare.pdf",width=10,height=10)
dlcompare(plots,pos.funs)
##dev.off();system("xpdf compare.pdf")

## Try some more exotic labeling options.
exotic <- list(lines2,
               rot=c(0,180),
               fontsize=c(10,20),
               fontface=c("bold","italic"),
               fontfamily=c("mono","serif"),
               alpha=c(0.25,1))
## Currently ggplot2 backend doesn't support face and family.
dlcompare(plots,list(exotic))

## All of these subsets should produce valid comparison plots.
dlcompare(plots[1],pos.funs[1])
dlcompare(plots[1],pos.funs)
dlcompare(plots,pos.funs[1])
named.funs <- list(first.points=first.points,lines2=lines2)
mixed.funs <- list("first.points",lines2=lines2,last.points)
not.named <- structure(named.funs,names=NULL)
unlabeled.plots <- structure(plots,names=NULL)
dlcompare(plots,mixed.funs[3])

## this should work with all combinations, with varying labels
for(ml in list(mixed.funs,not.named))
  for(pl in list(plots,unlabeled.plots))dlcompare(pl,ml)

## Compare scatterplot labeling methods.
data(BodyWeight,package="nlme")
dlcompare(list(plots[[1]],xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type="l",layout=c(3,1))),list("first.points","lines2"))
scatters <- list(xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1),
                 xyplot(Sepal.Length~Petal.Length,iris,groups=Species))
##pdf("scattercompare.pdf",width=10,height=10)
dlcompare(scatters,list("empty.grid","empty.grid.2"))
##dev.off();system("xpdf scattercompare.pdf")}
