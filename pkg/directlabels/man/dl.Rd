\name{dl}
\Rdversion{1.1}
\alias{dl}
\title{ Quick lattice direct label plot}
\description{Shortcut for a lattice plot with direct labels. This is a convenience function so you do not have to explicitly type groups=. This simply constructs the lattice plot and then calls direct.label on it.}
\usage{
dl(lattice.fun, data, x, groups, method = NULL, debug = FALSE, ...)
}
\arguments{
  \item{lattice.fun}{High-level lattice plot function to use.}
  \item{data}{Data to use.}
  \item{x}{Lattice model formula.}
  \item{groups}{To be passed to lattice as groups= argument.}
  \item{method}{Method for direct labeling as described in ?label.positions.}
  \item{debug}{Show debug output?}
  \item{\dots}{Other arguments to be passed to lattice.fun.}
}
\details{
}
\value{The lattice plot.}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@inria.fr>}
\note{
}


\seealso{
}
\examples{}