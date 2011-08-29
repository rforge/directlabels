library(lars)
data(diabetes)
fit <- with(diabetes,lars(x,y,type="lasso"))
beta <- coef(fit)
arclength <- rowSums(abs(beta))
path <- data.frame(melt(beta),arclength)
names(path)[1:3] <- c("step","variable","beta")
ggplot(path,aes(arclength,beta,colour=variable))+
  geom_line(aes(group=variable))
