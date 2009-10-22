direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  lvar <- if("group" %in% names(p$mapping))"group" else "colour"
  rename.vec <- sapply(p$mapping[c(lvar,"x","y")],deparse)
  d <- structure(p$data[,rename.vec],names=c("groups","x","y"))
  labtab <- label.positions(d$x,d$y,1:nrow(d),d$groups,debug,method)
  dlgeom <- geom_text(position=position_directlabel(method))
  p+dlgeom
### The ggplot object with direct labels added.
}
position_directlabel <- function(method)proto(Position,{
  adjust <- function(.,data,scales){
    labtab <- label.positions(x=data$x,y=data$y,groups=data$colour,
                              subscripts=1:nrow(data),method=.$.dlmethod)
    transform(labtab,label=groups,group=groups,colour=groups,angle=rot)
  }
  objname <- "dl"
},.dlmethod=method)
