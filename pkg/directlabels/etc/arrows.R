
library(ggplot2)
library(directlabels)

draw.arrows <- function(d,...){
  # called perpendicular.grid,
  #print (x)
  #then calc.boxes
  d <- calc.boxes(d)
  if(is.null(d$box.color))d$box.color <- "black"
  if(is.null(d$fill))d$fill <- "white"
  for(i in 1:nrow(d)){
    with(d[i,],{
      grid.segments(x0=left, y0=top,
                    x1=right, y1=bottom,
                    default.units="cm", arrow=arrow(),
                    gp = gpar(), draw = TRUE, vp = NULL)
    })
  }
  d
}

draw.rects <- function(d,...){
  if(is.null(d$box.color))d$box.color <- "black"
  if(is.null(d$fill))d$fill <- "white"
  for(i in 1:nrow(d)){
    with(d[i,],{
      grid.polygon(c(left,left,right,right),
                   c(bottom,top,top,bottom),
                   default.units="cm",
                   gp=gpar(col=d$box.color,fill=fill))
    })
  }
  d
}

par = c(0.34442397, -2.13692332, -0.71227700, 0.26051846, -0.09703043,
-0.36318124)

print (par)

shf <- function(pars, p, h) {
  r <- (pars[1]+pars[2]*log10(p))*(h ^ (pars[3] + pars[4]*(p))) + (pars[5]
+ pars[6]*log10(p))
  r <- ifelse(r > 1, 1, ifelse(r < 0, 0, r))
  return (r)
}

h <- c(seq(1,10,1), seq(20,100, 10), seq(200, 1000, 100))
p_s <- seq(0.05, 0.5, 0.05)

p_st <- data.frame()
for (i in p_s) {
  for (j in h) {
    p_st <- rbind(p_st, c(j, i, shf(par, i, j)))
  }
}
names(p_st)[1] <- paste("h")
names(p_st)[2] <- paste("p")
names(p_st)[3] <- paste("w")

arrow.points.to <- function(d,...){
  d[nrow(d),]
}

draw.arrows.from.labels.to.points <- function(Labels, Points){
  stopifnot(all(Labels$groups == Points$groups))
  Labels <- calc.boxes(Labels)
  grid.segments(Labels$left, Labels$top,
                Points$x, Points$y,
                default.units="cm", arrow=arrow(),
                gp=gpar(col=Labels$col))
}

perp.arrows <- function(d,...){
  Labels <- apply.method("get.means",d)
  pts <- gapply(d, arrow.points.to)
  draw.arrows.from.labels.to.points(Labels, pts)
  Labels
}

p2 <- ggplot(p_st, aes_string(y="h", x="w", group="p", color="p")) +
  geom_path(size=0.5) +
  labs(x = "W", y ="H") +
  scale_x_continuous(limits = c(0,1)) +
  scale_colour_gradientn(colours = rev(rainbow(3)), limits=c(0,0.5)) #+
  #geom_dl(aes_string(label="p"), list("perpendicular.grid", cex=0.6))
#smart.grid
#print (p2)

direct.label(p2, perpendicular.grid, draw.arrows)
#################################################
