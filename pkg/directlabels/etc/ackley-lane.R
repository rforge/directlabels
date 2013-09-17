library(ggplot2)
library(directlabels)
r = read.csv ("Random.csv", header = T) # using the attached random data

r1<- qplot(TREAT, RAND, data = r, stat="summary", fun.y = "mean" ,
group=MALE,color=MALE, width=1.5,)
r2<- qplot(TREAT, RAND, data = r, stat="summary", fun.y = "mean" ,
group=MALE, width=1.5,)

##But I can only add direct labels to plot "r1" and not "r2"
my.method <- list ( "last.qp" , hjust = -1 , vjust = 0 , cex = 1 , rot= 20 )
direct.label (r1, my.method)   # works with plot "r1" but not plot "r2"
r2+geom_dl(aes(label=MALE), my.method, stat="summary", fun.y="mean")
