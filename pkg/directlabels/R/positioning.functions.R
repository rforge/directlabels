### Process data points using the Positioning Method and draw the
### resulting direct labels. This is called for every panel with
### direct labels, every time the plot window is resized.
drawDetails.dlgrob <- function(x,recording){
  ## calculate x and y position in cm --- by this time we should have
  ## done any preprocessing necessary to convert 1d data to 2d data!
  cm.data <- transform(x$data,
                       x=convertX(unit(x,"native"),"cm",valueOnly=TRUE),
                       y=convertY(unit(y,"native"),"cm",valueOnly=TRUE),
                       groups=factor(groups))
  ## save original levels for later in case Positioning Methods mess
  ## them up.
  levs <- unique(cm.data[,c("groups","colour")])
  code <- as.character(cm.data$colour)
  names(code) <- as.character(cm.data$groups)
  ## apply ignore.na function -- these points are not plotted
  cm.data <- ignore.na(cm.data)
  cm.data <- apply.method(x$method,cm.data,
                          debug=x$debug,axes2native=x$axes2native)
  if(nrow(cm.data)==0)return()## empty data frames can cause many bugs
  ## rearrange factors in case Positioning Methods messed up the
  ## order:
  cm.data$col <- code[as.character(cm.data$groups)]
  ## defaults for grid parameter values:
  defaults <- list(hjust=0.5,vjust=0.5,rot=0)
  for(p in names(defaults)){
    if(!p %in% names(cm.data))cm.data[,p] <- NA
    cm.data[is.na(cm.data[,p]),p] <- defaults[[p]]
  }
  cm.data <- unique(cm.data)
  gpargs <- c("cex","alpha","fontface","fontfamily","col")
  gp <- do.call(gpar,cm.data[names(cm.data)%in%gpargs])
  if(x$debug){
    print(cm.data)
    ##browser()
  }
  with(cm.data,{
    grid.text(groups,x,y,hjust=hjust,vjust=vjust,rot=rot,default.units="cm",
              gp=gp)
  })
}

dlgrob <- function
### Make a grid grob that will draw direct labels.
(data,
### Data frame including points to plot in native coordinates.
 method,
### Positioning Method.
 debug=FALSE,
 axes2native=identity,
 ...
 ){
  grob(data=data,method=method,debug=debug,axes2native=axes2native,
       cl="dlgrob",
       name=if(is.character(method)){
         sprintf("GRID.dlgrob.%s",method[1])
       }else{
         NULL
       },...)
}

direct.label <- structure(function # Direct labels for color decoding
### Add direct labels to a plot, and hide the color legend. Modern
### plotting packages like lattice and ggplot2 show automatic legends
### based on the variable specified for color, but these legends can
### be confusing if there are too many colors. Direct labels are a
### useful and clear alternative to a confusing legend in many common
### plots.
(p,
### The "trellis" or "ggplot" object with things drawn in different
### colors.
 method=NULL,
### Positioning Method, which determines the positions of the direct
### labels as a function of the plotted data. If NULL, we examine the
### plot p and try to choose an appropriate default. See ?apply.method
### for more information about Positioning Methods.
 debug=FALSE
### Show debug output?
 ){
  ##alias<< directlabels
  if(is.character(method)&&method[1]=="legend")
    UseMethod("uselegend")
  else
    UseMethod("direct.label")
### A plot with direct labels and no color legend.
},ex=function(){
  library(ggplot2)
  ## direct label simple ggplot2 scatterplot
  scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                   main="Fuel efficiency depends on car size")
  print(direct.label(scatter))
  ## Use a different Positioning Method, and edit one of the label
  ## positions by hand
  print(direct.label(scatter,list("extreme.grid",dl.move("suv",15,15))))

  ## direct labels are not as good when there are multiple panels, but
  ## as long as not every class appears in every panel, it still
  ## should be clearer than using a legend.
  carpanels <- xyplot(jitter(hwy)~jitter(cty)|manufacturer,mpgf,groups=class,
  main="City and highway fuel efficiency depends on manufacturer and car class")
  print(direct.label(carpanels))


  if(require(mlmRev)){
    data(Chem97)
    qqm <- qqmath(~gcsescore,Chem97,groups=gender,
                  f.value=ppoints(25),auto.key=list())
    direct.label(qqm)
    ## default for points is different for default for lines
    plot(direct.label(update(qqm,type=c("l","g"))))
  }

  loci <-
    data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
               type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
  plot(direct.label(densityplot(~ppp,loci,groups=type,n=500)))
  lplot <- qplot(ppp,data=loci,colour=type,geom="density")
  print(direct.label(lplot))
  ## the manual color scale is respected by directlabels. 
  lmanual <- lplot+scale_colour_manual(values=c("red","black","blue"))
  print(direct.label(lmanual))
  
  ## dotplots of matrices are easy to do in lattice but you need to
  ## manually specify a Positioning Method.
  dp <- dotplot(VADeaths,type="o")
  plot(direct.label(dp,"angled.endpoints"))
  custom.method <- list(cex=1.2,dl.trans(y=y+0.1),"top.qp")
  plot(direct.label(dp,"custom.method"))
  ## To alter the direct label text, alter the factor levels, or in
  ## this case the column names.
  VAD2 <- VADeaths
  colnames(VAD2) <- sub(" ","\n",colnames(VAD2))
  plot(direct.label(dotplot(VAD2,type="o"),"custom.method"))

  ## contour plot --- labels each piece, not each group!
  if(require(reshape2)){
    volcano3d <- melt(volcano)
    names(volcano3d) <- c("x", "y", "z")
    v <- ggplot(volcano3d, aes(x, y, z = z,colour=..level..))+
      stat_contour()
    print(direct.label(v))
  }

  ## direct labels for lineplots that do not overlap and do not go off
  ## the plot.
  data(BodyWeight,package="nlme")
  ratplot <-
    xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
  print(direct.label(ratplot))#default is maxvar.qp
  print(direct.label(ratplot,"last.qp")) ## same side

  lattice.options(oldopt)
})

default.picker <- function
### Look at options() for a user-defined default Positioning Method
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

