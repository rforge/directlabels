works_with_R("3.0.1",directlabels="2013.7.24",
             grImport2="0.1.0",grConvert="0.1.0")
scatter <- ggplot(USArrests,aes(Murder, Assault))+
  geom_point()
svgs <- list()
seconds <- c()
for(state in rownames(USArrests)){
  print(state)
  fn <- sprintf("data/%s.svg", state)
  converted <- sprintf("data/%s-converted.svg", state)
  convertPicture(fn, converted)
  seconds[[state]] <- system.time({
    svgs[[state]] <- readPicture(converted)
  })["elapsed"]
}
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
                          debug=x$debug,
                          axes2native=x$axes2native,
                          images=x$images)
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
  ## Positioning Methods finished, print result!
  if(x$debug)print(cm.data)
  ## Split data into image labels and text labels
  has.image <- cm.data$groups%in%names(x$images)
  text.labels <- cm.data[!has.image,]
  image.labels <- cm.data[has.image,]
  if(nrow(text.labels)){
    gpargs <- c("cex","alpha","fontface","fontfamily","col")
    gp <- do.call(gpar,text.labels[names(text.labels)%in%gpargs])
    with(text.labels,{
      grid.text(groups,x,y,hjust=hjust,vjust=vjust,rot=rot,default.units="cm",
                gp=gp)
    })
  }
  for(i in seq_along(image.labels$groups)){
    irow <- image.labels[i,]
    
  }
}
### needed to add images argument to draw
GeomDirectLabel <- proto(ggplot2:::Geom, {
  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates,
                   method=NULL,debug=FALSE,images=list(), ...) {
    if(!is.list(images))stop("images must be a named list of images")
    data$rot <- as.integer(data$angle)
    data$groups <- data$label
    ctrans <- function(d){
      coord_transform(coordinates, d, scales)
    }
    dlgrob(subset(ctrans(data),select=-group),
           method,debug=debug,images=images,
           axes2native=ctrans)
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

### change so that the height and width of the box is stored
empty.grid <- function
### Label placement method for scatterplots that ensures labels are
### placed in different places. A grid is drawn over the whole
### plot. Each cluster is considered in sequence and assigned to the
### point on this grid which is closest to the point given by
### the input data points. Makes use of attr(d,"orig.data").
(d,
### Data frame of target points on the scatterplot for each label.
 debug=FALSE,
### Show debugging info on the plot?
 ...
### ignored.
 ){
  NREP <- 10
  all.points <- attr(d,"orig.data")[,c("x","y")]
  if(any(table(d$groups)>1))d <- get.means(d)
  label.targets <- d
  ranges <- list(x=convertX(unit(c(0,1),"npc"),"cm",valueOnly=TRUE),
                 y=convertY(unit(c(0,1),"npc"),"cm",valueOnly=TRUE))
  gl <- function(v){
    s <- seq(min(all.points[,v]),max(all.points[,v]),l=NREP)
    if(expand){
      dif <- s[2]-s[1]
      s <- seq(min(ranges[[v]])-expand*dif,
               max(ranges[[v]])+expand*dif,
               l=NREP+2*expand)
    }
    list(centers=s,diff=s[2]-s[1])
  }
  hgrid <- function(x,w){
    hboxes <- floor(diff(ranges[[x]])/r[,w])
    (-expand:(hboxes+expand-1))*r[,w]+r[,w]/2+min(ranges[[x]])
  }
  if(debug)with(label.targets,{
    grid.points(x,y,default.units="cm",gp=gpar(col="green"))
  })
  draw <- function(g){
    gridlines <- with(g,list(x=unique(c(left,right)),y=unique(c(top,bottom))))
    drawlines <- function(a,b,c,d)
      grid.segments(a,b,c,d,"cm",gp=gpar(col="grey"))
    with(gridlines,drawlines(min(x),y,max(x),y))
    with(gridlines,drawlines(x,min(y),x,max(y)))
  }
  res <- data.frame()
  label.targets <-
    label.targets[order(nchar(as.character(label.targets$groups))),]
  for(v in label.targets$groups){
    r <- subset(label.targets,groups==v)
    no.points <- data.frame()
    expand <- 0
    while(nrow(no.points)==0){
      boxes <- if("left"%in%names(label.targets)){
        list(x=hgrid("x","w"),y=hgrid("y","h"),w=r$w,h=r$h)
      }else{
        L <- sapply(c("x","y"),gl,simplify=FALSE)
        list(x=L$x$centers,y=L$y$centers,w=L$x$diff,h=L$y$diff)
      }
      boxes <- calc.borders(do.call(expand.grid,boxes))
      boxes <- cbind(boxes,data=inside(boxes,all.points))
      no.points <- transform(subset(boxes,data==0))
      expand <- expand+1 ## look further out if we can't find any labels inside
    }
    if(debug)draw(boxes)
    no.points <- transform(no.points,len=(r$x-x)^2+(r$y-y)^2)
    best <- no.points[which.min(no.points$len),]
    res <- rbind(res,transform(r,
                               x=best$x,y=best$y,
                               w=best$w,h=best$h,
                               hjust=best$hjust,vjust=best$vjust))
    ## add points to cloud
    newpts <- with(best,{
      expand.grid(x=seq(left,right,l=3),
                  y=seq(top,bottom,l=3))
    })
    all.points <- rbind(all.points,newpts)
  }
  if(debug)with(all.points,grid.points(x,y,default.units="cm"))
  res
### Data frame with columns groups x y, 1 line for each group, giving
### the positions on the grid closest to each cluster.
}

p <- ggplot(iris,aes(jitter(Petal.Length),jitter(Sepal.Length),colour=Species))+
  geom_point()+
  theme(legend.position="none")+
  coord_equal()
p+GeomDirectLabel$new(aes(label=Species),method="smart.grid")
p+GeomDirectLabel$new(aes(label=Species),method=function(x,...)x)
## strategy to to find a box to put the image in, then push a viewport
## into that box, and call grid.raster inside
p+GeomDirectLabel$new(aes(label=Species),
                      method=list(empty.grid,rot=30),
                      images=iris.photos[1])
labeled <- p+
  GeomDirectLabel$new(aes(label=Species),
                      method=empty.grid,
                      images=iris.photos)
png("image-labels.png",w=1100,h=700)
print(labeled)
dev.off()
pdf("image-labels.pdf",w=11,h=7)
print(labeled)
dev.off()
## NULL is invalid -- images must be a list!
print(p+GeomDirectLabel$new(aes(label=Species),method=empty.grid,images=NULL))

## Idea: take a few states and plot all the tornadoes in those states,
## with a flag next to each state cluster.
data(UStornadoes, package="animint")
four <- subset(UStornadoes, state %in% c("CA","TX","IL","NY"))
ggplot()+
  geom_point(aes(log10(trackLength), log10(trackWidth), colour=state),
             data=four)
## PROBLEM: there are no clusters, so direct labels don't work well
## here.
