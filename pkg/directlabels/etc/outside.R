library(reshape)
library(ggplot2)
library(directlabels)

df=data.frame(
  x = 1:3,
  One=c(12, 8, 13),
  Two=c(13, 7, 11),
  Threeeeeeeeee=c(11, 9, 11))

df.d.melt = melt(df[,c("x","One","Two","Threeeeeeeeee")], id.vars="x")
df.d.melt$variable1 = df.d.melt$variable
levels(df.d.melt$variable1) = paste("","Lable for",levels(df.d.melt$variable1))

p = ggplot(df.d.melt, aes(x=x, y=value, color=variable)) + 
  geom_line(size=1.1) +
  geom_text(aes(x =3.4, y=8, label="Custom Outside\nChart Annotation"), show_guide=FALSE) + 
  coord_cartesian(xlim=c(1,3)) +
  geom_dl(aes(label=variable1), method=list("last.qp", cex=1), show_guide=FALSE) + 
  theme(legend.position="top",plot.margin = unit(c(0, 4, 0, 0), "cm")) 

p1 <- ggplot_gtable(ggplot_build(p))
p1$layout$clip[p1$layout$name=="panel"] <- "off"
grid.draw(p1)

my.reduce.cex <- function(d,...){
  old.vp <- current.vpPath()
  while(!is.null(current.vpPath())){
    upViewport()
  }
  l <- xlimits()
  downViewport(old.vp)
  positive.part <- function(x)ifelse(x>0,x,0)
  right <- positive.part(d$right-l[2])
  left <- positive.part(l[1]-d$left)
  w <- d$right-d$left
  if(is.null(d$cex)){
    d$cex <- 1
  }
  d$cex <- (w-right)/w * (w-left)/w * d$cex
  d <- calc.boxes(d)
  draw.rects(d)
}
outside.reduce <-
  list(cex=3, "last.points","calc.boxes","my.reduce.cex",
       qp.labels("y","bottom","top",make.tiebreaker("x","y"),ylimits),
       "draw.rects")
p = ggplot(df.d.melt, aes(x=x, y=value, color=variable)) + 
  geom_line(size=1.1) +
  geom_text(aes(x =3.4, y=8, label="Custom Outside\nChart Annotation"), show_guide=FALSE) + 
  coord_cartesian(xlim=c(1,3)) +
  geom_dl(aes(label=variable1), method="outside.reduce") +
  guides(color="none")+
  theme(legend.position="top",plot.margin = unit(c(0, 4, 0, 0), "cm")) 

p1 <- ggplot_gtable(ggplot_build(p))
p1$layout$clip[p1$layout$name=="panel"] <- "off"
grid.draw(p1)
