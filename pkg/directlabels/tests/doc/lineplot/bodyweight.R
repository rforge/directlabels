data(BodyWeight,package="nlme")
xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',
       layout=c(3,1),xlim=c(-10,75))
