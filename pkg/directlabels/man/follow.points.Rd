\name{follow.points}
\alias{follow.points}
\title{follow points}
\description{Scatterplot positioning function that draws a line between each
center and every point, then follows the line out far enough to
give a box that will not collide with it. Out of all the boxes
constructed in this way that do not contain any points, take the
one which has the smallest distance to the center. FIXME: does not
work with ggplot2 since the ggplot2 backend doesn't yet have
support of actually knowing how big the text bounding box is.}
\usage{
follow.points(d, debug = FALSE, ...)
}
\arguments{
  \item{d}{
}
  \item{debug}{
}
  \item{\dots}{
}
}



\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





