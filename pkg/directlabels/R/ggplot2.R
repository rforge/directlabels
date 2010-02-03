### Geoms which need translation before applying Positioning Function.
need.trans.ggplot <- c("density")
direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  ##lvar <- if("group" %in% names(p$mapping))"group" else "colour"
  if(missing(method)){
    varnames <- c(groups="colour",x="x")
    if("y" %in% names(p$mapping))varnames <- c(varnames,y="y")
    rename.vec <- sapply(p$mapping[varnames],deparse)
    rename.vec <- gsub("[a-z]+[(]([^)]+)[)]","\\1",rename.vec)
    d <- structure(p$data[,rename.vec],names=names(varnames))
    geom <- p$layers[[1]]$geom$objname
    ldefault <- if(nlevels(d$groups)==2)"lines2" else "maxvar.points"
    if(is.null(method))method <-
      switch(geom,
             density="top.points",
             line=ldefault,
             point="empty.grid.2",
             stop("No default label placement for this type of ggplot."))
    if(geom%in%need.trans.ggplot)method <-
      c(paste("trans.",geom,sep=""),method)
  }
  ##print(p$layers[[1]]$mapping)
  dlgeom <- geom_text(position=position_dl(list(method),debug,p),
                      stat=p$layers[[1]]$stat)
  ##browser()
  ##print(dlgeom)
  p+dlgeom+scale_colour_discrete(legend=FALSE)
### The ggplot object with direct labels added.
}
### Position class for direct label placement.
PositionDl <- proto(ggplot2::Position,{
  method <- NULL
  debug <- FALSE
  orig <- NULL
  new <- function(., method=NULL, debug=FALSE, orig=NULL) {
    .$proto(method=method,debug=debug,orig=orig)
  }
  adjust <- function(.,data,scales){
    ##print(head(data))
    ##browser()
    if(is.null(data$colour)){
      colvar <- .$orig$layers[[1]]$mapping$colour
      if(!is.null(colvar)){
        colvar <- gsub("^[.][.](.*)[.][.]$","\\1",colvar)
        data$colour <- data[,colvar]
      }else stop("Need colour aesthetic to direct label.")
    }
    labtab <- label.positions(x=data$x,y=data$y,groups=factor(data$colour),
                              subscripts=1:nrow(data),method=.$method[[1]],
                              debug=.$debug)
    targs <- list(label="groups",
                  group="groups",
                  colour="groups",
                  angle="rot",
                  size="fontsize",
                  ##face="fontface",
                  ##family="fontfamily",
                  alpha="alpha")
    targs <- targs[targs%in%names(labtab)]
    targs <- sapply(targs,as.name)
    r <- do.call("transform",c(list(labtab),targs))
    ##browser()
    ##print(head(r))
    r
  }
  objname <- "dl"
})
### Position for internal use with geom_text.
position_dl <- PositionDl$build_accessor()

