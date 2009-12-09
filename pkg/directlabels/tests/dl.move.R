library(directlabels)
data(mpg,package="ggplot2")
scatter <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1)
dlcompare(list(scatter),
          list("extreme.grid",
               `+dl.move`=list(extreme.grid,dl.move("suv",15,15))))
