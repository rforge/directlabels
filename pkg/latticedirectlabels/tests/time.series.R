data(BodyWeight,package="nlme")
pdfpng("longitudinal",dl(xyplot,BodyWeight,weight~Time|Diet,Rat,
         type='l',layout=c(3,1)),h=7,w=14)

## Fails: default method for scatterplot doesn't make sense here
##print(dl(xyplot,BodyWeight,weight~Time|Diet,Rat))
