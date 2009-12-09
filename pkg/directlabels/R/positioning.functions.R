direct.label <- function
### Add direct labels to a plot. This is a S3 generic and there are
### appropriate methods for "trellis" and "ggplot" objects.
(p,
### The plot to which you would like to add direct labels.
 method=NULL,
### The direct label placement method as described in
### ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  if(!is.null(method)&&class(method)=="character"&&method=="legend")return(p)
  UseMethod("direct.label")
### The plot object, with direct labels added.
}
label.positions <- function
### Calculates table of positions of each label. It does not draw
### anything, but is called for its return value. Normally you don't
### have to call label.positions explicitly. Instead, it is called for
### you by direct.label, for each panel.
(x,
### x values of points to draw.
 y,
### y values of points to draw.
 subscripts,
### Subscripts of groups to consider.
 groups,
### Vector of groups.
 debug=FALSE,
### Show debug output? If TRUE, the resulting table of label positions
### will be printed.
 method,
### Method for direct labeling, specified in one of the following
### ways: (1) a Positioning Function, (2) the name of a Positioning
### Function as a character string, or (3) a list containing any
### number of (1), (2), or additionally named values. Starting from
### the data frame of points to plot for the panel, the elements of
### the list are applied in sequence, and each row of the resulting
### data frame is used to draw a direct label. See examples in
### ?direct.label and ?positioning.functions.
 ...
### Passed to Positioning Function(s).
 ){
  groups <- as.factor(groups)
  levs <- levels(groups)
  groups <- groups[subscripts]
  d <- data.frame(x,groups)
  if(!missing(y))d$y <- y
  if(class(method)=="function")method <- list(method)
  while(length(method)){
    m <- method[[1]]
    while(class(m)=="list"){
      method <- c(m,method[-1])
      m <- method[[1]]
    }
    m.var <- names(method)[1]
    if(!(is.null(m.var)||m.var==""))d[[m.var]] <- m else{
      if(class(m)=="character"){
        method.name <- paste(m," ",sep="")
        m <- get(m)
      }else method.name <- ""
      d <- try(m(d,debug=debug,...))
      if(class(d)=="try-error")
        stop("direct label placement method ",method.name,"failed")
    }
    method <- method[-1]
  }
  ## rearrange factors in case pos fun messed up the order:
  d$groups <- factor(as.character(d$groups),levs)
  ## defaults for grid parameter values:
  for(p in c("hjust","vjust"))
    d[,p] <- if(p %in% names(d))as.character(d[,p]) else 0.5
  if(!"rot"%in%names(d))d$rot <- 0
  d <- unique(d)
  if(debug)print(d)
  d
### Data frame of direct label positions. Each row describes the
### position of 1 label to be drawn later.
}

### Transformation function for 1d densityplots.
trans.densityplot <- dl.indep({
  dens <- density(d$x)
  data.frame(x=dens$x,y=dens$y)
})
trans.density <- trans.densityplot
### Transformation function for 1d qqmath plots.
trans.qqmath <- dl.indep({
  r <- prepanel.default.qqmath(d$x,...)
  data.frame(x=r$x,y=r$y)
})

### Positioning Function for the bottom of a group of points.
bottom.points <-
  dl.indep(data.frame(d[which.min(d$y),],hjust=0.5,vjust=1))
low.points <- bottom.points
### Place points on top of the mean value of the rug.
rug.mean <- function(d,...,end)
  ddply(d,.(groups),function(d)
        data.frame(x=mean(d$x),
                   y=as.numeric(convertY(unit(end,"npc"),"native")),
                   vjust=0))


