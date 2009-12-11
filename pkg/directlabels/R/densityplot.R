### Positioning Function for the top of a group of points.
top.points <-
  dl.indep(data.frame(d[which.max(d$y),],hjust=0.5,vjust=0))

high.points <- top.points
