direct.label.compare <- function
### Compare several plots and/or label placement methods. This creates
### a custom grid graphics display based on lattice and/or ggplot2
### output. This is possible because the direct.label function is
### generic. The result will be a plot matrix with plots on the
### columns and positioning methods on the rows. Names from the lists
### will be used to annotate the plot.
(plots,
### List of ggplot2 or lattice plots.
 pos.funs
### List of label placement methods to apply to each plot.
 ){
  grid.newpage()
  the.layout <-
    grid.layout(length(pos.funs)+1,length(plots)+1,
                widths=unit(1,c(rep("null",l=length(plots)),"cm")),
                heights=unit(1,c("cm",rep("null",l=length(pos.funs)))))
  pushViewport(viewport(layout=the.layout))
  for(col in seq_along(plots)){
    pushViewport(viewport(layout.pos.col=col,layout.pos.row=1))
    grid.text(names(plots)[col])
    popViewport()
    for(row in seq_along(pos.funs)){
      if(col==1){
        pushViewport(viewport(layout.pos.col=length(plots)+1,
                              layout.pos.row=row+1))
        grid.text(pos.funs[[row]],rot=-90)
        popViewport()
      }
      pushViewport(viewport(layout.pos.col=col,layout.pos.row=row+1))
      print(direct.label(plots[[col]],pos.funs[[row]]),newpage=FALSE)
      grid.rect()
      popViewport()
    }
  }
  popViewport()
}
