library(tikzDevice)
library(grid)
tikz("test.tex",1,1,standAlone=TRUE)
s <- "$\\hat\\lambda$"

grid.newpage()
grid.text(s,0,0,hjust=0,vjust=0)
x <- stringWidth(s)
y <- stringHeight(s)
grid.rect(0,0,x,y,hjust=0,vjust=0)

dev.off()
system("pdflatex test && evince test.pdf")
