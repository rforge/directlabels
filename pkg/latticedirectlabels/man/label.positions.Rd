\name{label.positions}
\Rdversion{1.1}
\alias{label.positions}
\title{label positions}
\description{Calculates table of positions of each label. This can be thought of as a lattice panel function. It takes the same sort of arguments but does not draw anything. Instead it is called for its return value.}
\usage{
label.positions(x, y, subscripts, groups, debug = FALSE, method = perpendicular.lines, extra = list(), ...)
}
\arguments{
  \item{x}{x values of points to draw.}
  \item{y}{y values of points to draw.}
  \item{subscripts}{subscripts of groups to consider.}
  \item{groups}{vector of groups.}
  \item{debug}{logical indicating whether debug annotations should be added to the plot.}
  \item{method}{function used to choose position of labels.}
  \item{extra}{extra variables for the returned data frame. This is useful for concisely specifying direct label placement parameters that are not a function of the data (ie if all the labels should be rotated by 30 degrees, use extra=list(rot=30)).}
  \item{\dots}{passed to positioning method}
}
\details{
}
\value{Data frame describing where direct labels should be positioned.}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>}
\note{
}


\seealso{
}
\examples{}