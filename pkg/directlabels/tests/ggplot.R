library(ggplot2)
library(directlabels)
data(mpg,package="ggplot2")
plots <-
  list(qplot=qplot(hwy,cty,data=mpg,colour=class),
       ggplot=ggplot(mpg,aes(hwy,cty,colour=class))+geom_point(),
       aes2=ggplot(,aes(hwy,cty))+geom_point(aes(colour=class),data=mpg),
       aes22=ggplot(,aes(colour=class))+geom_point(aes(hwy,cty),data=mpg),
       geom=ggplot()+geom_point(aes(hwy,cty,colour=class),data=mpg),
       mix=ggplot(mpg)+geom_point(aes(hwy,cty,colour=class)),
       mix2=ggplot(,aes(hwy,cty,colour=class))+geom_point(data=mpg))
for(i in seq_along(plots)){
  p <- plots[[i]]
  p.name <- names(plots)[[i]]
  print(p.name)
  dl <- direct.label(p)
  print(dl)
}
