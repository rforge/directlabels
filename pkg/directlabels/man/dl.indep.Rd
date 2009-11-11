\name{dl.indep}
\alias{dl.indep}
\title{ Direct label groups independently}
\description{Makes a function you can use to specify the location of each group independently.}
\usage{
dl.indep(expr)
}
\arguments{
  \item{expr}{Expression that takes a subset of the d data frame, with data from only a single group, and returns the direct label position.}
}
\details{
}
\value{A Positioning Function.}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@inria.fr>}
\note{
}


\seealso{
}
\examples{library(directlabels)
complicated <- list(dl.trans(x=x+10),
                    dl.indep(d[-2,]),
                    rot=c(30,180))
direct.label(
             dotplot(VADeaths,type="o")
             ,method=complicated)}