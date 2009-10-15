direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  rename.vec <- sapply(p$mapping[c("group","x","y")],deparse)
  d <- structure(p$data[,rename.vec],names=c("groups","x","y"))
  labtab <- label.positions(d$x,d$y,1:nrow(d),d$groups,debug,method)
  dlgeom <- geom_text(aes(x=x,y=y,group=groups,colour=groups,label=groups,
                          angle=rot,hjust=hjust,vjust=vjust),labtab)
  p+dlgeom
### The ggplot object with direct labels added.
}
