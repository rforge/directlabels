data(path,package="directlabels")
library(ggplot2)
p <- ggplot(joined,aes(x,y))+
  geom_path(aes(group=row),colour="grey")+
  geom_point(aes(size=lambda))+
  geom_point(aes(colour=class),data=pts,pch=21,fill="white")+
  coord_equal()
direct.label(p)
