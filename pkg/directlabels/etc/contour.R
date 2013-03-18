
##Setup code:
library(ggplot2)
library(directlabels)

x = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)
y = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
x2 = x*y
df_test = data.frame(a=x, b=y, c=x2)

##This unfortunately don't work:
plot = ggplot(df_test, aes(x=a, y=b, fill=c)) +  geom_tile() +
geom_contour(aes(z=c))
direct.label(plot)
# Error in direct.label.ggplot(plot) :
#  Need colour aesthetic to infer default direct labels.

##Adding a color aestetic to geom_contour (and a method to direct.label)
##also produces a strange error:
plot = ggplot(df_test, aes(x=a, y=b, fill=c)) +  geom_tile() +
geom_contour(aes(z=c, colour=..level..))
plot <- ggplot(df_test, aes(x=a, y=b, z=c)) +
  ##geom_tile() +
  stat_contour(aes(colour=..level..))
direct.label(plot, method="bottom.pieces")
# Error: stat_contour requires the following missing aesthetics: z

##I'm not sure how that could happen, as z is specified in the
##geom_contour(aes(z=c,..)) -> Bug?

##Anyway: adding it to the default aes() works, kind of:
plot = ggplot(df_test, aes(x=a, y=b, fill=c, z=c)) +   geom_tile() +
geom_contour(aes(z=c, colour=..level..))
direct.label(plot, method="bottom.pieces")
## Works but contour and labels are unreadable

##I can change the contour colors but the labels are still not readable
plot = ggplot(df_test, aes(x=a, y=b, fill=c, z=c)) +   geom_tile() +
geom_contour(aes(z=c, colour=..level..), colour="black")
direct.label(plot, method="bottom.pieces")
## Visible contours, but labels are still not readable
##-> Is there a way to specify label colors?

##Unfortunately, geom_dl() is not useable for this case, as it exepcts a
##aes(label=...) but "..levels.." are not available to geom_dl()  and
##'c' als does not work:
plot = ggplot(df_test, aes(x=a, y=b, fill=c, z=c)) +   geom_tile() +
geom_contour(aes(z=c, colour=..level..), colour="black")
plot + geom_dl(aes(label=..level..), method="bottom.pieces")
# Error in eval(expr, envir, enclos) : Objekt 'level' not found
plot + geom_dl(aes(label=c), method="bottom.pieces")
# Error in split.default(x = seq_len(nrow(x)), f = f, drop = drop, ...) :
#  Grouplength is 0 but datalength is > 0

plot <- ggplot(df_test, aes(x=a, y=b, z=c)) +
  stat_contour(aes(colour=..level..),breaks=c(3,10))
direct.label(plot, method="bottom.pieces")

p <- qplot(a,data=df_test,geom="bar",binwidth=2)
