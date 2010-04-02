dlcompare <- function # Direct label comparison plot
### Compare several plots and/or label placement methods. This creates
### a custom grid graphics display based on lattice and/or ggplot2
### output. This is possible because the direct.label function is
### generic. Plots will be on the columns and positioning methods will
### be on the rows.
(plots,
### List of ggplot2 or lattice plots. List names will be used to
### annotate the plot.
 pos.funs,
### List of label placement methods to apply to each plot. List names,
### or function names if specified as character strings, will be used
### to annotate the plot.
 rects=TRUE,
### Draw rectangles around each plot, creating a grid?
 row.items="plots",
### If "plots" then put plots on the rows and method on the
### columns. Otherwise, do the opposite.
 debug=FALSE
### Show debug output?
 ){
  ## Augment positioning method list names if possible
  names(pos.funs) <- sapply(seq_along(pos.funs),function(i){
    N <- names(pos.funs)[i]
    f <- pos.funs[[i]]
    if(!is.null(N)&&N!="")N
    else if(class(f)=="character")f
    else ""
  })
  if(sum(names(pos.funs)!="")==0)names(pos.funs) <- NULL
  grid.newpage()

  standard <- row.items=="plots"
  row.items <- if(standard)plots else pos.funs
  col.items <- if(standard)pos.funs else plots
  
  rowadd <- if(is.null(names(col.items)))0 else 1
  widths <- rep("null",l=length(col.items))
  if(!is.null(names(row.items)))widths <- c(widths,"cm")
  heights <- rep("null",l=length(row.items))
  if(rowadd)heights <- c("cm",heights)
  the.layout <-
    grid.layout(length(heights),length(widths),
                widths=unit(1,widths),
                heights=unit(1,heights))
  pushViewport(viewport(layout=the.layout))

  for(col in seq_along(col.items)){
    if(!is.null(names(col.items))){
      pushViewport(viewport(layout.pos.col=col,layout.pos.row=1))
      grid.text(names(col.items)[col])
      popViewport()
    }
    for(row in seq_along(row.items)){
      if(col==1&&!is.null(names(row.items))){
        pushViewport(viewport(layout.pos.col=length(col.items)+1,
                              layout.pos.row=row+rowadd))
        grid.text(names(row.items)[row],rot=-90)
        popViewport()
      }
      pushViewport(viewport(layout.pos.col=col,layout.pos.row=row+rowadd))
      p <- if(standard)
        direct.label(row.items[[row]],col.items[[col]],debug=debug)
      else direct.label(col.items[[col]],row.items[[row]],debug=debug)
      print(p,newpage=FALSE)
      if(rects)grid.rect()
      popViewport()
    }
  }
  popViewport()
}
