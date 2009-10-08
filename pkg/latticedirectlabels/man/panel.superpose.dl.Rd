\name{panel.superpose.dl}
\Rdversion{1.1}
\alias{panel.superpose.dl}
\title{panel superpose dl}
\description{Call panel.superpose with the data and then the direct labels.}
\usage{
panel.superpose.dl(x, y = NULL, subscripts, groups, panel.groups, method = NULL, extra = list(), debug = FALSE, .panel.superpose = panel.superpose, type = "p", ...)
}
\arguments{
  \item{x}{vector of x values.}
  \item{y}{vector of y values.}
  \item{subscripts}{subscripts of x,y,groups.}
  \item{groups}{vector of group ids.}
  \item{panel.groups}{To be parsed for default labeling method, and passed to panel.superpose.}
  \item{method}{Positioning Function to use for label placement.}
  \item{extra}{extra variables to be added to the label position data frame. This is useful for concisely specifying direct label placement parameters that are not a function of the data (ie if all the labels should be rotated by 30 degrees, use extra=list(rot=30)).}
  \item{debug}{Show debug output?}
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
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>}
\note{
}


\seealso{
}
\examples{}