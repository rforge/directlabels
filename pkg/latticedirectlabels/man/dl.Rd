\name{dl}
\Rdversion{1.1}
\alias{dl}
\title{dl}
\description{Lattice plot using direct labels.}
\usage{
dl(p, data, x, groups, method = NULL, panel = NULL, ...)
}
\arguments{
  \item{p}{High-level lattice plot function to use.}
  \item{data}{Data set to be passed to lattice.}
  \item{x}{Plot formula to be passed to lattice.}
  \item{groups}{To pass to high-level plot function.}
  \item{method}{Method for direct labeling --- this is a function that accepts 2 arguments: d a data frame of the points to plot with columns x y groups, and debug a logical flag indicating if debug output should be shown. NULL indicates to make a logical choice based on the high-level plot function chosen.}
  \item{panel}{Panel function to use. Defaults to corresponding default panel function for the high-level plot function.}
  \item{\dots}{
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