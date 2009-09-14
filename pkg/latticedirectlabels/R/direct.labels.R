dl.text <- function
### To be used as panel.groups= argument in panel.superpose. Analyzes
### arguments to determine correct text color for this group, and then
### draws the direct label text.
(labs,
### table of labels and positions constructed by label.positions
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
direct.label <- function
### Add direct labels to a grouped lattice plot. The idea is that we
### parse the trellis object returned by the high level plot function
### and return it changed such that it will plot direct labels.
(p,
### The lattice plot (result of a call to a high-level lattice function).
 method=NULL,
### Method for direct labeling --- this is a function that accepts 2
### arguments: d a data frame of the points to plot with columns x y
### groups, and debug a logical flag indicating if debug output should
### be shown. NULL indicates to make a logical choice based on the
### high-level plot function chosen.
 debug=FALSE
### Show debug output?
 ){
  type <- p$panel.args.common$type
  if(is.null(type))type <- "NULL"
  lattice.fun.name <- paste(p$call[[1]])
  if(is.null(method))method <- 
    switch(lattice.fun.name,
           xyplot=switch(type,l="first.points","empty.grid.2"),
           densityplot="top.points",
           stop("No default direct label placement method for ",
                lattice.fun.name,". Please specify method."))
  old.panel <- if(class(p$panel)=="character")get(p$panel) else p$panel
  p$panel <- function(...,panel.groups){
    if("panel.groups"%in%names(p$panel.args.common))
      ## If they specified panel.groups, then old.panel is probably
      ## already panel.superpose (or some modification thereof), thus
      ## we should call it:
      old.panel(...,panel.groups=panel.groups)
    ## If panel.groups is unspecified, then we call panel.superpose
    ## ourself with the panel function they specified:
    else panel.superpose(panel.groups=old.panel,...)
    ## Add direct labels:
    labs <- label.positions(...,method=method,debug=debug)
    panel.superpose(panel.groups=dl.text,labs=labs,...)
  }
  p
### The lattice plot.
}
make.panel.groups.fun <- function
### Direct-label-ify a function you would normally use as the
### panel.groups argument to panel.superpose. Note that this technique
### only works with Positioning Functions that are group-independent.
(old.panel,
### Panel function to be converted to show direct labels, and to be
### used as panel.groups.
 method=get.means,
 debug=FALSE
 ){
  function(...){
    old.panel(...)
    labs <- label.positions(...,groups=1,method=method,debug=debug)
    dl.text(labs,...)
  }
}
dl <- function
### Shortcut for direct label lattice plots.
(lattice.fun,
### High-level lattice plot function to use.
 data,
### Data frame to use.
 x,
### Lattice model formula.
 groups,
### To be passed to lattice as groups= argument.
 method=NULL,
### Label placement method to be passed to direct.label.
 debug=FALSE,
### Show debugging output? to be passed to direct.label.
 ...
### Other arguments to be passed to lattice.fun.
 ){
  m <- match.call()
  m <- m[!names(m)%in%c("method","debug")]
  m <- m[-1]
  p <- eval(m)
  direct.label(p,method,debug)
### The lattice plot.
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
