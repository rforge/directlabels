data(BodyWeight,package="nlme")
print(dl(xyplot,BodyWeight,weight~Time|Diet,Rat,
         type='l',layout=c(3,1)))
## Fails: default method for scatterplot doesn't make sense here
print(dl(xyplot,BodyWeight,weight~Time|Diet,Rat))
