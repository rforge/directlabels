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
  if(!is.null(method)&&class(method)=="character"&&method=="legend")
    UseMethod("uselegend")
  else UseMethod("direct.label")
### The plot object, with direct labels added.
}
label.positions <- function
### Calculates table of positions of each label based on input data
### for each panel and Positioning Functions. This is meant for
### internal use inside a direct.label method. This function contains
### all the logic for parsing the method= argument and sequentially
### applying the Positioning Functions to the input data to obtain the
### label positions.
(d,
### Data frame to which we will sequentially apply the Positioning
### Functions.
 method,
### Method for direct labeling, specified in one of the following
### ways: (1) a Positioning Function, (2) the name of a Positioning
### Function as a character string, or (3) a list containing any
### number of (1), (2), or additionally named values. Starting from
### the data frame of points to plot for the panel, the elements of
### the list are applied in sequence, and each row of the resulting
### data frame is used to draw a direct label. See examples in
### ?direct.label and ?positioning.functions.
 debug=FALSE,
### Show debug output? If TRUE, the resulting table of label positions
### will be printed.
 ...
### Passed to Positioning Function(s).
 ){
  ## make sure input data is in good format
  d <- transform(d,
                 x=as.numeric(x),
                 groups=as.factor(groups))
  if("y"%in%names(d))d <- transform(d,y=as.numeric(y))
  ##save original levels for later in case PFs mess them up.
  levs <- levels(d$groups)
  if(class(method)=="function")method <- list(method)
  isconst <- function(){
    m.var <- names(method)[1]
    !(is.null(m.var)||m.var=="")
  }
  islist <- function()class(method[[1]])=="list"
  isref <- function()(!isconst())&class(method[[1]])=="character"
  while(length(method)){
    method.name <- ""
    ## Resolve any PF names or nested lists
    while(islist()||isref()){
      if(islist()){
        method <- c(method[[1]],method[-1])
        method.name <- ""
      }else{ #must be character -> get the fun
        method.name <- paste(method[[1]]," ",sep="")
        method <- c(get(method[[1]]),method[-1])
      }
    }
    if(isconst())d[[names(method)[1]]] <- method[[1]] else{
      d <- try(method[[1]](d,debug=debug,...))
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

### Place points on top of the mean value of the rug.
rug.mean <- function(d,...,end)
  ddply(d,.(groups),function(d)
        data.frame(x=mean(d$x),
                   y=as.numeric(convertY(unit(end,"npc"),"native")),
                   vjust=0))


