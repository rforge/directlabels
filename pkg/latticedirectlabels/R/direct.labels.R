dl.panel <- function
### Convert a normal panel into a direct label panel function.
(panel
### The panel function to transform.
 ){
  function(...){
    panel(...)
    direct.labels(...)
  }
### A new panel function that first calls panel, then calls
### direct.labels.
}
direct.labels <- function
### Panel function that draws text labels for groups.
(x,
### x values of points to draw.
 y,
### y values of points to draw.
 subscripts,
### subscripts of groups to consider.
 groups,
### vector of groups.
 debug=FALSE,
### logical indicating whether debug annotations should be added to
### the plot.
 method=parallel.lines,
### function used to choose position of labels.
 ...
### ignored.
 ){
  groups <- groups[subscripts]
  d <- data.frame(x,groups)
  if(!missing(y))d$y <- y
  labs <- method(d,debug)
  for(p in c("hjust","vjust"))
    labs[,p] <- if(p %in% names(labs))as.character(labs[,p]) else 0.5
  print(labs)
  Col <-
    if("col"%in%names(labs))labs$col
    else trellis.par.get("superpose.symbol")$col #FIXME
  grid.text(labs$groups,labs$x,labs$y,
            gp=gpar(col=Col),
            hjust=labs$hjust,vjust=labs$vjust,
            default.units="native")
}
dl <- function
### Lattice plot using direct labels.
(p,
### High-level lattice plot function to use.
 data,
### Data set to be passed to lattice.
 x,
### Plot formula to be passed to lattice.
 groups,
### To pass to high-level plot function.
 method=NULL,
### Method for direct labeling --- this is a function that accepts 2 arguments: d a data frame of the points to plot with columns x y groups, and debug a logical flag indicating if debug output should be shown. NULL indicates to make a logical choice based on the high-level plot function chosen.
 panel=NULL,
### Panel function to use. Defaults to corresponding default panel
### function for the high-level plot function.
 ...
### Other arguments to pass to the high-level plot function.
 ){
  m <- match.call()
  if(is.null(panel))panel <- get(paste("panel.",m$p,sep=""))
  if(is.null(method))method <- 
    switch(paste(m$p),
           xyplot=empty.grid.2,
           densityplot=top.points,
           stop("No default direct label placement method for ",
                m$p,".\nPlease specify method."))
  pdx <- dl.panel(panel)
  m$panel <- pdx
  m$method <- method
  m[[1]] <- m[[2]]
  m <- m[-2]
  print(m)
  eval(m)
}
dl.indep <- function
### makes a function you can use to specify the location of each group
### independently.
(FUN
### function that takes a subset of the d data frame, with data from
### only a single group, and returns the direct label position.
 ){
  function(d,debug){
    ddply(d,.(groups),FUN,debug)
  }
}
