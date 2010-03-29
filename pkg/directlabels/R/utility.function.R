dl.combine <- function # Combine output of several methods
### Apply several Positioning methods to the original data frame.
(...
### Several Positioning Functions.
 ){
  FUNS <- list(...)
  function(d,...){
    dfs <- lapply(FUNS,eval.list,d)
    res <- data.frame()
    for(df in dfs){
      if(nrow(res))res <- merge(df,res,all=TRUE)
      else res <- df
    }
    res
  }
### A Positioning Function that returns the combined data frame after
### applying each specified Positioning Function.
}

dl.indep <- function # Direct label groups independently
### Makes a function you can use to specify the location of each group
### independently.
(expr
### Expression that takes a subset of the d data frame, with data from
### only a single group, and returns the direct label position.
 ){
  foo <- substitute(expr)
  f <- function(d,...)eval(foo)
  src <- paste("dl.indep(",paste(deparse(foo),collapse="\n"),")",sep="")
  structure(function(d,...)ddply(d,.(groups),f,...),"source"=src)
### A Positioning Function.
}

dl.trans <- function # Direct label data transform
### Make a function that transforms the data. This is for conveniently
### making a function that calls transform on the data frame, with the
### arguments provided. See examples.
(...
### Arguments to pass to transform.
 ){
  L <- as.list(match.call())[-1]
  function(d,...)do.call("transform",c(list(d),L))
### A Positioning Function.
}

dl.move <- function # Manually move a direct label
### Sometimes there is 1 label that is placed oddly by another
### Positioning Function. This function can be used to manually place
### that label in a good spot.
(groups,
 x,
 y
 ){
  function(d,...){
    v <- sapply(groups,function(g)which(d$groups==g))
    d[v,"x"] <- x
    d[v,"y"] <- y
    d
  }
### A Positioning Function that moves a label into a good spot.
}

### Make a Positioning Function with empty.grid, that calculates label
### position targets using f.
empty.grid.fun <- function(f)
  function(d,debug,...)empty.grid(d,debug,f)

### Jitter the label positions.
dl.jitter <- dl.trans(x=jitter(x),y=jitter(y))

### Calculate boxes around labels, for collision detection.
calc.boxes <- function(d,debug=FALSE,...){
  vp <- current.viewport()
  convert <- function(worh){
    conv <- get(paste("convert",worh,sep=""))
    stri <- get(paste("string",worh,sep=""))
    with(d,sapply(seq_along(groups),function(i){
      if("cex"%in%names(d))vp$gp <- gpar(cex=cex[i])
      pushViewport(vp)
      if(debug)grid.rect() ##highlight current viewport
      w <- conv(stri(as.character(groups[i])),"native")
      popViewport()
      w
    }))
  }
  w <- convert("Width")
  h <- convert("Height")
  calc.borders(transform(d,w=w,h=h))
}

### Calculate big boxes around the means of each cluster.
big.boxes <- function(d,...)enlarge.box(calc.boxes(get.means(d)))

calc.borders <- function
### Calculate bounding box based on newly calculated width and height.
(d,
### Data frame of point labels, with new widths and heights in the w
### and h columns.
 ...
### ignored.
 ){
  hjust <- vjust <- 0.5 ##defaults in case unassigned in d
  transform(d,
            top=y+(1-vjust)*h,bottom=y-vjust*h,
            right=x+(1-hjust)*w,left=x-hjust*w,
            h=h,w=w)
}

### Positioning Function that draws boxes around label positions. Need
### to have previously called calc.boxes. Does not edit the data
### frame.
draw.rects <- function(d,...){
  ## easy way -- not correct, doesn't use calc'ed borders
  ##with(d,grid.rect(x,y,w,h,hjust=hjust,vjust=vjust,
  ##                 default.units="native",gp=gpar(col="grey")))
  d_ply(d,.(groups),function(D){
    with(D,grid.lines(c(left,left,right,right,left),
                      c(bottom,top,top,bottom,bottom),
                      "native",gp=gpar(col="grey")))
  })
  d
}

### Sequentially bump labels up, starting from the bottom, if they
### collide with the label underneath.
bumpup <- function(d,...){
  d <- calc.boxes(d)[order(d$y),]
  for(i in 2:nrow(d)){
    dif <- d$bottom[i]-d$top[i-1]
    bpts <- with(d[i,],data.frame(y=bottom,x=c(left,right)))
    n.in <- in1box(bpts,d[i-1,])
    if(dif<0&&n.in>0){
      d$bottom[i] <- d$bottom[i]-dif
      d$top[i] <- d$top[i]-dif
      d$y[i] <- d$y[i]-dif
    }
  }
  ##print(sapply(d,class))
  d
}

### Use a QP solver to find the best places to put the points on a
### line, subject to the constraint that they should not overlap
qp.labels <- function(var,spacer)function(d,...){
  if(!spacer%in%names(d))stop("need to have calculated ",spacer)
  require(quadprog)
  d <- d[order(d[,var],decreasing=TRUE),]
  ## sorts data so that m_1 is on top, m_n on bottom.
  n <- nrow(d)
  D <- diag(rep(1,n))
  A <- diag(rep(1,n))[,-n]-rbind(0,diag(rep(1,n-1)))
  h <- d[,spacer]
  b0 <- (h[-n]+h[-1])/2
  sol <- solve.QP(D,d[,var],A,b0)
  d[,var] <- sol$solution
  d
}

### Make text bounding box larger by some amount.
enlarge.box <- function(d,...){
  if(!"h"%in%names(d))stop("need to have already calculated height and width.")
  h <- unit(d$h,"native")
  d$h <- d$h*2
  d$w <- d$w+as.numeric(convertWidth(convertHeight(h,"inches"),"native"))
  calc.borders(d)
}

in1which <- function
### Calculate which points fall in a box.
(p,
### data frame of points with columns x and y and many rows.
 box
### data frame of 1 row with columns left right top bottom.
 ){
  p$x>box$left & p$x<box$right & p$y<box$top & p$y>box$bottom
}

### Calculate how many points fall in a box.
in1box <- function(p,box)sum(in1which(p,box))

inside <- function
### Calculate for each box how many points are inside.
(boxes,
### Data frame of box descriptions, each row is 1 box, need columns
### left right top bottom.
 points
### Data frame of points, each row is 1 point, need columns x y.
 ){
  sapply(1:nrow(boxes),function(i)in1box(points,boxes[i,]))
### Vector of point counts for each box.
}
