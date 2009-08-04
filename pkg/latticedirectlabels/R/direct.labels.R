dl.panel <- function
### Convert a normal panel into a direct label panel function.
(panel
### The panel function to transform.
 ){
  function(...){
    panel.superpose(panel.groups=panel,...)
    labs <- direct.labels(...)
    panel.superpose(panel.groups=dl.text,labs=labs,...)
  }
### A new panel function that first calls panel, then calls
### direct.labels.
}
dl.text <- function
### To be used as panel.groups= argument in panel.superpose. Analyzes
### arguments to determine correct text color for this group, and then
### draws the direct label text.
(labs,
### table of labels and positions constructed by direct.labels
 group.number,
### which group we are currently plotting, according to levels(labs$groups)
 col.line=NULL,
### line color
 col.points=NULL,
### point color
 col=NULL,
### general color
 col.symbol=NULL,
### symbol color
 type=NULL,
### plot type
 ...
### ignored
 ){
  ##print(cbind(col,col.line,col.points,col.symbol,type))
  col.text <- switch(type,p=col.symbol,l=col.line,col.line)
  g <- labs[levels(labs$groups)[group.number]==labs$groups,]
  grid.text(g$groups,g$x,g$y,
            hjust=g$hjust,vjust=g$vjust,
            gp=gpar(col=col.text),
            default.units="native")
}
direct.labels <- function
### Calculates table of positions of each label for each panel.
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
 method=perpendicular.lines,
### function used to choose position of labels.
 ...
### ignored.
 ){
  groups <- groups[subscripts]
  d <- data.frame(x,groups)
  if(!missing(y))d$y <- y
  if(class(method)=="character")method <- get(method)
  labs <- try(method(d,debug))
  if(class(labs)=="try-error")stop("direct label placement method failed")
  for(p in c("hjust","vjust"))
    labs[,p] <- if(p %in% names(labs))as.character(labs[,p]) else 0.5
  ##print(labs)
  labs
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
  type <- if(is.null(m$type))"" else m$type
  if(is.null(method))method <- 
    switch(paste(m$p),
           xyplot=switch(type,l="first.points","empty.grid.2"),
           densityplot="top.points",
           stop("No default direct label placement method for ",
                m$p,".\nPlease specify method."))
  m$panel <- dl.panel(panel)
  m$method <- method
  m[[1]] <- m[[2]]
  m <- m[-2]
  if(class(m$groups)=="name")m$groups <- call("factor",m$groups)
  m$data <- eval(m$data,parent.frame())
  for(r in names(m))if(class(m[[r]])=="call"){
    R <- try(eval(m[[r]],parent.frame()),TRUE)
    if(class(R)!="try-error")m[[r]] <- R
  }
  eval(m)
}

compare.methods <- function
### Plot several label placement methods on the same page.
(m,
### Vector of label placement function names.
...
### Args to pass to dl
 ){
  mc <- match.call()
  mc[[1]] <- quote(dl)
  mc <- mc[-2]
  newpage <- TRUE
  L <- length(m)
  for(i in seq_along(m)){
    mc$main <- m[i]
    mc$method <- m[i]
    P <- eval(mc)
    plot(P,split=c(1,i,1,L),newpage=newpage)
    newpage <- FALSE
  }
}
