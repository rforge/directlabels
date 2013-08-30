works_with_R("3.0.1",ggplot2="0.9.3.1.99",directlabels="2013.7.24")

## The point of this script is to demonstrate that there is no need
## for spacing between panels with theme_bw() in ggplot2.

p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point()+
  facet_grid(vs ~ am)+
  theme_bw()

## Example from help(contourplot)
require(stats)
require(lattice)
attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
               parametric = c("radiation", "wind"), span = 1, degree = 2)
w.marginal <- seq(min(wind), max(wind), length.out = 50)
t.marginal <- seq(min(temperature), max(temperature), length.out = 50)
r.marginal <- seq(min(radiation), max(radiation), length.out = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
                     radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
detach(environmental)
library(ggplot2)
p <- ggplot(grid,aes(wind,temperature,z=fit))+
  stat_contour(aes(colour=..level..))+
  facet_wrap(~radiation)+
  theme_bw()

library(grid)
space <- function(x)p+theme(panel.margin=unit(x,"lines"))
plots <- list(space=space(0.25), no.space=space(0))
png("space-nospace.png",width=1000)
dlcompare(plots,pos.funs=list(bottom.pieces),
          row.items="posfuns",rects=FALSE)
dev.off()
