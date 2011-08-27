uselegend.ggplot <- function
### Show the ggplot2 legend, for comparison.
(p,
### The ggplot object.
 ...
### Ignored.
 ){
  p
}

### Geoms which need translation before applying Positioning Method.
need.trans.ggplot <- c()

geom_dl <- structure(function(mapping=NULL,method,...){
  require(ggplot2)
  ## Geom for direct labeling that creates dlgrobs in the draw()
  ## method.
  GeomDirectLabel <- proto(Geom, {
    draw_groups <- function(., ...) .$draw(...)
    draw <- function(., data, scales, coordinates,
                     method=NULL,debug=FALSE, ...) {
      data$rot <- as.integer(data$angle)
      data$groups <- data$label
      dlgrob(coordinates$transform(data, scales),method,debug=debug)
    }
    draw_legend <- function(.,data,...){
      data <- aesdefaults(data,.$default_aes(),list(...))
      with(data,{
        textGrob("dl",0.5,0.5,rot=angle,
                 gp=gpar(col=alpha(colour,alpha),fontsize=size*.pt))
      })
    }
    objname <- "dl"
    desc <- "Direct labels"
    default_stat <- function(.) StatIdentity
    required_aes <- c("x", "y", "label")
    default_aes <- function(.)
      aes(colour="black", size=5 , angle=0, hjust=0.5, vjust=0.5, alpha = 1)
  })
  GeomDirectLabel$new(mapping,method=method,...)
},ex=function(){
  vad <- as.data.frame.table(VADeaths)
  names(vad) <- c("age","demographic","deaths")
  ## no color, just direct labels!
  p <- ggplot(vad,aes(deaths,age))+
    geom_line(aes(group=demographic))+
    geom_dl(aes(label=demographic),method="top.qp")
  print(p)
  ## add color:
  p+aes(colour=demographic)+
    scale_colour_discrete(legend=FALSE)
  ## add linetype:
  p+aes(linetype=demographic)+
    scale_linetype(legend=FALSE)
  ## no color, just direct labels
  data(BodyWeight,package="nlme")
  bw <- ggplot(BodyWeight,aes(Time,weight,label=Rat))+
    geom_line(aes(group=Rat))+
    facet_grid(~Diet)+
    geom_dl(method="last.qp")
  print(bw)
  ## add some more direct labels
  bw2 <- bw+geom_dl(method="first.qp")
  print(bw2)
  ## add color
  bw2+aes(colour=Rat)+
    scale_colour_discrete(legend=FALSE)
})
direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  require(proto)
  require(ggplot2)
  SCALE <- scale_colour_discrete
  maps <- lapply(p$layers,function(L){
    m <- p$mapping
    m[names(L$mapping)] <- L$mapping
    m
  })
  ##TODO: need to design framework around other aesthetics besides
  ##colour. ie fill and linetype, see etc/rod.R
  cvars <- lapply(maps,"[[","colour")
  has.colour <- !sapply(cvars,is.null)
  if(any(has.colour)){
    i <- which(has.colour)[1] ##just pick the first one
    L <- p$layers[[i]]
    colvar <- cvars[[i]]
    ## FIXME: kind of a hack.
    if(colvar=="..level..")SCALE <- scale_colour_continuous
    colvar <- gsub("^[.][.](.*)[.][.]$","\\1",colvar)
  }else stop("Need colour aesthetic to direct label.")
  ## Position class for direct label placement.
  PositionDl <- proto(ggplot2:::Position,{
    method <- NULL
    debug <- FALSE
    new <- function(.,method=NULL,debug=FALSE){
      .$proto(method=method,debug=debug)
    }
    adjust <- function(.,data,...){
      d <- transform(data,groups=if("piece"%in%names(data))piece else colour)
      labtab <- label.positions(d,.$method,.$debug,class="ggplot")
      targs <- list(label=if("colour"%in%names(labtab))"colour" else "groups",
                    angle="rot",
                    size="fontsize",
                    ##size="cex", ##DONT DO THIS --- will cause disappearing!!
                    ##face="fontface",
                    ##family="fontfamily",
                    alpha="alpha")
      possibly.missing <- c("colour","group")
      toadd <- possibly.missing[(!possibly.missing%in%names(labtab))]
      targs[toadd] <- "groups"
      targs <- targs[targs%in%names(labtab)]
      targs <- sapply(targs,as.name)
      r <- do.call("transform",c(list(labtab),targs))
      if(is.numeric(data$colour)&&!is.numeric(r$colour))
        r$colour <- as.numeric(as.character(r$colour))
      r
    }
    objname <- "dl"
  })
  geom <- L$geom$objname
  if(is.null(method))method <- default.picker("ggplot")
  if(geom%in%need.trans.ggplot)method <-
    list(paste("trans.",geom,sep=""),method)
  pos.inst <- PositionDl$new(method=list(method),debug=debug)
  dlgeom <- geom_dl(aes(groups=demographic),method=)
  scale.types <- sapply(p$scales$.scales,"[[",".output")
  scale.i <- which("colour"==scale.types)
  if(length(scale.i)){
    p$scales$.scales[[scale.i[1]]]$legend <- FALSE
  }else{
    p <- p+SCALE(legend=FALSE)
  }
  p+dlgeom
### The ggplot object with direct labels added.
}

defaultpf.ggplot <- function
### Default method selection method for ggplot2 plots.
(geom,p,...){
  switch(geom,
         density="top.points",
         line={
           varnames <- c(groups="colour",x="x")
           if("y" %in% names(p$mapping))varnames <- c(varnames,y="y")
           rename.vec <- sapply(p$mapping[varnames],deparse)
           rename.vec <- gsub("[a-z]+[(]([^)]+)[)]","\\1",rename.vec)
           d <- structure(p$data[,rename.vec],names=names(varnames))
           if(nlevels(d$groups)==2)"lines2" else "maxvar.points"
         },
         point="extreme.grid",
         path="bottom.points",
         stop("No default label placement for this type of ggplot."))
}

