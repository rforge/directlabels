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
  dlgeom <- geom_text(position=position_dl(list(method),debug))
  ##print(dlgeom)
  p+dlgeom
### The ggplot object with direct labels added.
}
PositionDl <- proto(Position,{
  method <- NULL
  debug <- FALSE
  new <- function(., method=NULL, debug=FALSE) {
    .$proto(method=method,debug=debug)
  }
  adjust <- function(.,data,scales){
    ##print(data)
    labtab <- label.positions(x=data$x,y=data$y,groups=data$colour,
                              subscripts=1:nrow(data),method=.$method[[1]],
                              debug=.$debug)
    r <- transform(labtab,label=groups,group=groups,colour=groups,angle=rot)
    ##print(r)
    r
  }
  objname <- "dl"
})
position_dl <- PositionDl$build_accessor()

