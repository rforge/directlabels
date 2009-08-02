first.point <- function(d,debug=F){
  ddply(d,.(groups),summarise,x=x[1],y=y[1])
}
data(BodyWeight,package="nlme")
xyplot(weight~Time|Diet,BodyWeight,type='l',groups=Rat,layout=c(3,1),
       panel=direct.labels,method=first.point)
