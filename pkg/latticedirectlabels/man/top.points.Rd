\name{top.points}
\Rdversion{1.1}
\alias{top.points}
\title{top.points}
\description{For density plots}
\usage{
top.points(d, debug)
}
\arguments{
  \item{d}{
}
  \item{debug}{
}
}
\details{
}
\value{
}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>}
\note{
}


\seealso{
}
\examples{loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
library(latticedl)
print(dl(densityplot,loci,~ppp,type,n=500))
## Not very informative but it should work:
print(dl(densityplot,loci,~ppp|type,type,n=500))}