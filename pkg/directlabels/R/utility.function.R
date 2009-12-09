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
### Use empty.grid with perpendicular.lines.
empty.grid.2 <- empty.grid.fun(perpendicular.lines)
### Use empty.grid with extreme.points.
extreme.grid <- empty.grid.fun(extreme.points)
### Jitter the label positions.
dl.jitter <- dl.trans(x=jitter(x),y=jitter(y))
### Label the points furthest from the origin for each group.
extreme.points <- dl.indep({
  d <- transform(d,d=sqrt(x^2+y^2))
  d[which.max(d$d),]
})
