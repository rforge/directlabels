pred <- read.table("lines-data2.txt")
## I would like to have a Positioning Method like dl.move but that
## allows you to specify only 1 of the 2 coordinates, the other one is
## calculated automatically based on the data. here i would like to do
## dl.move('kif',x=0)... problem: need to do this first before we
## forget all the data points.
direct.label(xyplot(rate~log10(gamma)|replicate+nu,pred,groups=data,type="l",
  main="1-SVM fit to 50% GL2 controls for each replicate, predicting other 50% and KIF11 controls",
  ylab="proportion of data contained in 1-SVM hypersphere",
  xlim=c(min(log10(pred$gamma)),7),
  xlab="log10(gamma) - shape parameter of gaussian kernel on 4 important variables"),
             list(last.points,dl.move("kif",0,-0.025)))

