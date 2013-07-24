\name{apply.method}
\alias{apply.method}
\title{Apply a Positioning Method}
\description{Run a Positioning Method list on a given data set. This function
contains all the logic for parsing a Positioning Method and
sequentially applying its elements to the input data to obtain the
label positions.}
\usage{apply.method(method, d, columns.to.check = c("x", "y", "groups"), 
    ..., debug = FALSE)}
\arguments{
  \item{method}{Direct labeling Positioning Method. Starting from the data frame
of points to plot for the panel, the elements of the Positioning
Method list are applied in sequence, and then each row of the
resulting data frame is used to draw a direct label. The
interpretation of Positioning Method list is described below.
\itemize{
\item a Positioning Function is any function(d,...) which takes a
data.frame d with columns x,y,groups and returns another
data.frame representing the positions of the desired direct
labels. For a description of all the columns that are interpreted
for drawing direct labels, see ?drawDetails.dlgrob. For example,
maxvar.points is a Positioning Function that returns a data.frame
with columns x,y,groups,hjust,vjust.
\item a character vector of length 1 is treated as the name of an
R object. For example, specifying "maxvar.points" means to look up
the variable called maxvar.points and use that. Using the name of
a Positioning Function is preferable to specifying the Positioning
Function itself, since then the name is visible in the Positioning
Method list, which is more interpretable when debugging.
\item a named list element is used to add or update variables in the
data.frame of direct labels to plot. For example cex=1.5 means set the
cex column of the direct label data.frame to 1.5.
\item an element of a Positioning Method list can be another
Positioning Method list, in which case the elements of the inner
list are applied.
}}
  \item{d}{Data frame to which we apply the Positioning Method.}
  \item{columns.to.check}{After applying each Positioning Function, we check for the
presence of these columns, and if not found we stop with an error.}
  \item{\dots}{Named arguments, passed to Positioning Functions.}
  \item{debug}{If TRUE, print each Positioning Method and the direct label
data.frame that results from its evaluation.}
}

\value{The final data frame returned after applying all of the items in
the Positioning Method list.}

\author{Toby Dylan Hocking}





