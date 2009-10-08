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
\examples{}