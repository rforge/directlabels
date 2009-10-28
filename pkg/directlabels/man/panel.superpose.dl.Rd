\name{panel.superpose.dl}
\Rdversion{1.1}
\alias{panel.superpose.dl}
\title{panel superpose dl}
\description{Call panel.superpose for the data points and then for the direct labels. This is a proper lattice panel function that behaves much like panel.superpose.}
\usage{
panel.superpose.dl(x, y = NULL, subscripts, groups, panel.groups, method = NULL, .panel.superpose = panel.superpose, type = "p", ...)
}
\arguments{
  \item{x}{Vector of x values.}
  \item{y}{Vector of y values.}
  \item{subscripts}{Subscripts of x,y,groups.}
  \item{groups}{Vector of group ids.}
  \item{panel.groups}{To be parsed for default labeling method, and passed to panel.superpose.}
  \item{method}{Method for direct labeling as described in ?label.positions. NULL indicates to choose a Positioning Function based on the panel.groups function.}
  \item{.panel.superpose}{The panel function to use for drawing data points.}
  \item{type}{Plot type, used for default method dispatch.}
  \item{\dots}{Additional arguments to panel.superpose.}
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
\examples{}