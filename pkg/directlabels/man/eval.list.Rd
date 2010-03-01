\name{eval.list}
\alias{eval.list}
\title{Evaluate Positioning Function list}
\description{Run all the Positioning Functions on a given data set. This is
useful since it is often much less verbose to define Positioning
Methods in list form instead of function form, ex lasso.labels.}
\usage{
eval.list(method, d, debug = FALSE, ...)
}
\arguments{
  \item{method}{Positioning Function list.}
  \item{d}{Data frame.}
  \item{debug}{Show debugging output?}
  \item{\dots}{Passed to Positioning Functions.}
}

\value{The final data frame returned after applying all of the items in
the Positioning Function list.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





