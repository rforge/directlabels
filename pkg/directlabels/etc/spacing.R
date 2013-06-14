works_with_R("3.0.1",directlabels="2013.6.14",ggplot2="0.9.3.1")

data(BodyWeight,package="nlme")
ratplot <- ggplot(BodyWeight,aes(Time,weight,colour=Rat))+
  facet_grid(.~Diet)+
  geom_line()
print(ratplot)
last.qp.spaced <-
  list("last.points", #start at the endpoint of each line.
       dl.trans(x=x+0.1), #move to the right 0.1cm.
       cex=1.5, #target text size if there is enough space on the right.
       ##Set the bounding box height to 7/6 the text height:
       "calc.boxes",
       dl.trans(h=7/6*h),"calc.borders",
       ##Move labels up and down to avoid collisions:
       qp.labels("y","bottom","top",make.tiebreaker("x","y"),ylimits),
       ##"calc.boxes","draw.rects", #uncomment to show bounding box
       reduce.cex.lr)#decrease text size if not enough space on the right.
direct.label(ratplot,"last.qp.spaced")

