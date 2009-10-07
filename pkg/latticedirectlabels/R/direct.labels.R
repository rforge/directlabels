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
  ##debugging output:
  ##print(cbind(col,col.line,col.points,col.symbol,type))
  col.text <- switch(type,p=col.symbol,l=col.line,col.line)
  g <- labs[levels(as.factor(labs$groups))[group.number]==labs$groups,]
  grid.text(g$groups,g$x,g$y,
            hjust=g$hjust,vjust=g$vjust,rot=g$rot,
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
 extra=list(),
### extra variables to be added to the label position data frame. This
### is useful for concisely specifying direct label placement
### parameters that are not a function of the data (ie if all the
### labels should be rotated by 30 degrees, use extra=list(rot=30)).
 debug=FALSE
### Show debug output?
 ){
  old.panel <- if(class(p$panel)=="character")get(p$panel) else p$panel
  lattice.fun.name <- paste(p$call[[1]])
  p$panel <-
    function(panel.groups=paste("panel.",lattice.fun.name,sep=""),...){
      panel.superpose.dl(panel.groups=panel.groups,extra=extra,
                         .panel.superpose=old.panel,
                         method=method,
                         debug=debug,
                         ...)
  }
  p
### The lattice plot.
}
panel.superpose.dl <- function
### Call panel.superpose with the data and then the direct labels.
(x,
### vector of x values.
 y=NULL,
### vector of y values.
 subscripts,
### subscripts of x,y,groups.
 groups,
### vector of group ids.
 panel.groups,
### To be parsed for default labeling method, and passed to
### panel.superpose.
 method=NULL,
### Positioning Function to use for label placement.
 extra=list(),
### extra variables to be added to the label position data frame. This
### is useful for concisely specifying direct label placement
### parameters that are not a function of the data (ie if all the
### labels should be rotated by 30 degrees, use extra=list(rot=30)).
 debug=FALSE,
### Show debug output?
 .panel.superpose=panel.superpose,
### The panel function to use for drawing data points.
 type="p",
### Plot type.
 end=0.03,
### Rug end value in npc.
 ...
### Additional arguments to panel.superpose.
 ){
  ## FIXME: this is a total hack:
  tryCatch(.panel.superpose(x=x,y=y,subscripts=subscripts,
                            groups=groups,type=type,
                            panel.groups=panel.groups,...),
           error=function(e).panel.superpose(x=x,y=y,subscripts=subscripts,
             groups=groups,type=type,...))
  subs <-
    if(is.character(panel.groups))panel.groups else substitute(panel.groups)
  lattice.fun.name <-
    if(is.character(subs))sub("panel.","",subs) else ""
  if(is.null(type))type <- "NULL"
  if(is.null(method))method <- 
    switch(lattice.fun.name,
           dotplot="last.points",
           xyplot=switch(type,l="first.points","empty.grid.2"),
           densityplot="top.points",
           rug=function(d,debug)
             ddply(d,.(groups),function(d,debug)
                   data.frame(x=mean(d$x),
                              y=as.numeric(convertY(unit(end,"npc"),"native")),
                              vjust=0)
                   ,debug),
           stop("No default direct label placement method for ",
                lattice.fun.name,". Please specify method."))
  labs <- label.positions(method=method,debug=debug,groups=groups,extra=extra,
                          subscripts=subscripts,x=x,y=y,...)
  panel.superpose(panel.groups=dl.text,labs=labs,type=type,x=x,
                  groups=groups,subscripts=subscripts,...)
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
