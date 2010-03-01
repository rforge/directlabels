uselegend.trellis <- function
### Add a legend to a trellis plot, for comparison.
(p,
### The trellis object.
 ...
### Ignored.
 ){
  if(is.null(p$legend))update(p,auto.key=TRUE)
  else p
}
 
### Functions which need translation before applying Positioning Function.
need.trans <- c("qqmath","densityplot")
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
            gp=gpar(col=col.text,fontsize=g$fontsize,fontfamily=g$fontfamily,
              fontface=g$fontface,lineheight=g$lineheight,cex=g$cex,
              alpha=g$alpha),
            default.units="native")
}
direct.label.trellis <- function
### Add direct labels to a grouped lattice plot. This works by parsing
### the trellis object returned by the high level plot function, and
### returning it with a new panel function that will plot direct
### labels using the specified method.
(p,
### The lattice plot (result of a call to a high-level lattice
### function).
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  old.panel <- if(class(p$panel)=="character")get(p$panel) else p$panel
  lattice.fun.name <- paste(p$call[[1]])
  p$panel <-
    function(panel.groups=paste("panel.",lattice.fun.name,sep=""),...){
      panel.superpose.dl(panel.groups=panel.groups,
                         .panel.superpose=old.panel,
                         method=method,
                         ...)
  }
  p$legend <- NULL
  update(p,debug=debug)
### The lattice plot.
}
panel.superpose.dl <- function
### Call panel.superpose for the data points and then for the direct
### labels. This is a proper lattice panel function that behaves much
### like panel.superpose.
(x,
### Vector of x values.
 y=NULL,
### Vector of y values.
 subscripts,
### Subscripts of x,y,groups.
 groups,
### Vector of group ids.
 panel.groups,
### To be parsed for default labeling method, and passed to
### panel.superpose.
 method=NULL,
### Method for direct labeling as described in ?label.positions. NULL
### indicates to choose a Positioning Function based on the
### panel.groups function.
 .panel.superpose=panel.superpose,
### The panel function to use for drawing data points.
 type="p",
### Plot type, used for default method dispatch.
 ...
### Additional arguments to panel.superpose.
 ){
  rgs <- list(x=x,subscripts=subscripts,groups=groups,type=type,`...`=...)
  if(!missing(y))rgs$y <- y
  ## FIXME: this is a total hack:
  tryCatch(do.call(".panel.superpose",c(rgs,panel.groups=panel.groups))
           ,error=function(e)do.call(".panel.superpose",rgs))
  if(missing(panel.groups))panel.groups <- "panel.xyplot" #lattice default
  subs <-
    if(is.character(panel.groups))panel.groups else substitute(panel.groups)
  lattice.fun.name <-
    if(is.character(subs))sub("panel.","",subs) else ""
  if(is.null(type))type <- "NULL"
  if(is.null(method)){
    picker <- getOption("directlabels.defaultpf.lattice")
    if(is.null(picker))picker <- defaultpf.lattice
    method <- do.call(picker,as.list(environment()))
  }
  ## maybe eventually allow need.trans to be specified in options()??
  if(lattice.fun.name%in%need.trans)method <-
    c(paste("trans.",lattice.fun.name,sep=""),method)
  groups <- as.factor(groups)
  groups <- groups[subscripts]
  d <- data.frame(x,groups)
  if(!missing(y))d$y <- y
  labs <- label.positions(d,method,...)
  type <- type[type!="g"] ## printing the grid twice looks bad.
  panel.superpose(panel.groups=dl.text,labs=labs,type=type,x=x,
                  groups=groups,subscripts=seq_along(groups),...)
}
defaultpf.lattice <- function
### If no Positioning Function specified, choose a default using this
### function. The idea is that this is called with all the variables
### in the environment of panel.superpose.dl, and this can be
### user-customizable by setting the directlabels.defaultpf.lattice
### option to a function like this.
(lattice.fun.name,groups,type,...){
  ldefault <- if(nlevels(groups)==2)"lines2" else "maxvar.points"
  ## maybe eventually scan options("directlabels.default.lattice") for
  ## a list that specifies a manual override to these defaults
  switch(lattice.fun.name,
         dotplot=ldefault,
         xyplot=switch(type,p="smart.grid",ldefault),
         densityplot="top.points",
         qqmath=ldefault,
         rug="rug.mean",
         stop("No default direct label placement method for '",
              lattice.fun.name,"'.\nPlease specify method."))
}
