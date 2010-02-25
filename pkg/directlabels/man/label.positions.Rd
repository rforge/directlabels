\name{label.positions}
\alias{label.positions}
\title{label positions}
\description{Calculates table of positions of each label based on input data
for each panel and Positioning Functions. This is meant for
internal use inside a direct.label method. This function contains
all the logic for parsing the method= argument and sequentially
applying the Positioning Functions to the input data to obtain the
label positions.}
\usage{
label.positions(d, method, debug = FALSE, ...)
}
\arguments{
  \item{d}{Data frame to which we will sequentially apply the Positioning
Functions.}
  \item{method}{Method for direct labeling, specified in one of the following
ways: (1) a Positioning Function, (2) the name of a Positioning
Function as a character string, or (3) a list containing any
number of (1), (2), or additionally named values. Starting from
the data frame of points to plot for the panel, the elements of
the list are applied in sequence, and each row of the resulting
data frame is used to draw a direct label. See examples in
?direct.label and ?positioning.functions.}
  \item{debug}{Show debug output? If TRUE, the resulting table of label positions
will be printed.}
  \item{\dots}{Passed to Positioning Function(s).}
}

\value{Data frame of direct label positions. Each row describes the
position of 1 label to be drawn later.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





