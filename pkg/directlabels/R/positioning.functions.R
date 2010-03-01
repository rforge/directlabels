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
  d <- eval.list(method,d,debug,...)
  ## rearrange factors in case pos fun messed up the order:
  d$groups <- factor(as.character(d$groups),levs)
  ## defaults for grid parameter values:
  for(p in c("hjust","vjust")){
    d[,p] <- if(p %in% names(d))as.character(d[,p]) else NA
    d[is.na(d[,p]),p] <- 0.5
  }
  if(!"rot"%in%names(d))d$rot <- NA
  d$rot[is.na(d$rot)] <- 0
  d <- unique(d)
  if(debug)print(d)
  d
### Data frame of direct label positions. Each row describes the
### position of 1 label to be drawn later.
}

eval.list <- function # Evaluate Positioning Function list
### Run all the Positioning Functions on a given data set. This is
### useful since it is often much less verbose to define Positioning
### Methods in list form instead of function form, ex lasso.labels.
(method,
### Positioning Function list.
 d,
### Data frame.
 debug=FALSE,
### Show debugging output?
 ...
### Passed to Positioning Functions.
 ){
  if(!is.list(method))method <- list(method)
  isconst <- function(){
    m.var <- names(method)[1]
    !(is.null(m.var)||m.var=="")
  }
  islist <- function()class(method[[1]])=="list"
  isref <- function()(!isconst())&class(method[[1]])=="character"
  while(length(method)){
    ## Resolve any PF names or nested lists
    while(islist()||isref()){
      if(islist()){
        method <- c(method[[1]],method[-1])
      }else{ #must be character -> get the fun(s)
        method <- c(lapply(method[[1]],get),method[-1])
      }
    }
    if(isconst())
      d[[names(method)[1]]] <- method[[1]]
    else
      d <- method[[1]](d,debug=debug,...)
    method <- method[-1]
  }
  d
### The final data frame returned after applying all of the items in
### the Positioning Function list.
}


### Transformation function for 1d densityplots.
trans.densityplot <- dl.indep({
  dens <- density(d$x,na.rm=TRUE)
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

### Label points at the top, making sure they don't collide.
top.qp <- list(top.points,calc.boxes,qp.labels("x","w"))

### Label points at the zero before the first nonzero y value.
lasso.labels <-
  list(rot=60,
       dl.indep({
         d <- d[order(d$x),]
         i <- which(d$y!=0)[1]
         hjust <- as.integer(d[i,"y"]>0)
         data.frame(d[i-1,],hjust,vjust=hjust)
       }),
       calc.boxes,
       ## calculate how wide the tilted box is
       dl.trans(h.inches=convertHeight(unit(h,"native"),"inches",TRUE)),
       dl.trans(hyp.inches=h.inches/sin(2*pi*rot/360)),
       dl.trans(hyp=convertWidth(unit(hyp.inches,"inches"),"native",TRUE)),
       ## avoid collisions between tilted boxes
       qp.labels("x","hyp"))

default.picker <- function
### Look at options() for a user-defined default Positioning Function
### picker, and use that (or the hard-coded default picker), with the
### calling environment to figure out a good default.
(f
### Object class to look for (trellis or ggplot).
 ){
  varname <- paste("defaultpf.",f,sep="")
  p <- getOption(paste("directlabels.",varname,sep=""))
  if(is.null(p))p <- get(varname)
  do.call(p,as.list(parent.frame()))
}

