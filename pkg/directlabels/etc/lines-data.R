pred <- read.table("lines-data.txt")
direct.label(xyplot(rate~log10(gamma)|replicate+nu,pred,groups=data,type="l"))
