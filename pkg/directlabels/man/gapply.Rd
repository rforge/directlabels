\name{gapply}
\alias{gapply}
\title{gapply}
\description{apply a function to every group. works like ddply from plyr
package, but the grouping column is always called groups.}
\usage{
gapply(d, FUN, ...)
}
\arguments{
  \item{d}{data frame with column groups.}
  \item{FUN}{function to apply to every group separately.}
  \item{\dots}{additional arguments for FUN.}
}

\value{data frame of results after applying FUN to each group in d.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





