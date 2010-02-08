dl.indep <- function # Direct label groups independently
### Makes a function you can use to specify the location of each group
### independently.
(expr
### Expression that takes a subset of the d data frame, with data from
### only a single group, and returns the direct label position.
 ){
  foo <- substitute(expr)
  f <- function(d,...)eval(foo)
  src <- paste("dl.indep(",deparse(foo),")",sep="")
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
calc.boxes <- function(d,...){
  vp <- current.viewport()
  convert <- function(worh){
    conv <- get(paste("convert",worh,sep=""))
    stri <- get(paste("string",worh,sep=""))
    with(d,sapply(seq_along(groups),function(i){
      if("cex"%in%names(d))vp$gp <- gpar(cex=cex[i])
      pushViewport(vp)
      w <- conv(stri(as.character(groups[i])),"native")
      popViewport()
      w
    }))
  }
  w <- convert("Width")
  h <- convert("Height")
  hjust <- vjust <- 0.5 ##defaults in case unassigned in d
  d <- transform(d,
                 top=y+(1-vjust)*h,bottom=y-vjust*h,
                 right=x+(1-hjust)*w,left=x-hjust*w,
                 h=h,w=w)
}

### Positioning Function that draws boxes around label positions. Need
### to have previously called calc.boxes. Does not edit the data
### frame.
draw.rects <- function(d,...){
  ##browser()
  ## easy way
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
    if(dif<0){
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
### (assumes they all have the same height/width -- NEED TO FIX).
qp.labels <- function(var,spacer)list(calc.boxes,function(d,...){
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
})
