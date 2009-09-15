<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<h2>Longitudinal data for body weight of 16 rats and 3 different medical treatments</h2>
<pre>
library(latticedl)
data(BodyWeight,package="nlme")
dl(xyplot,BodyWeight,weight~Time|Diet,Rat,
   type='l',layout=c(3,1))
</pre>
<img src="longitudinal.png" />

<h2>Residuals versus fitted values for a linear model for some cars, grouped by number of cylinders in the engine</h2>
<pre>
library(proto)
library(ggplot2)
library(latticedl)
data(mpg)
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
dl(xyplot,mpgf,.resid~.fitted,factor(cyl))
</pre>
<img src="scatter.png" />

<h2>Custom panel and panel.groups functions for the rat body weight data</h2>
<pre>
## Say we want to use a simple linear model to explain rat body weight:
fit <- lm(weight~Time+Diet+Rat,BodyWeight)
bw <- fortify(fit,BodyWeight)

## Custom panel function which highlights min and max values:
panel.range <- function(y,...){
  panel.abline(h=range(y))
  panel.xyplot(y=y,...)
}
dl(xyplot,bw,weight~Time|Diet,Rat,type="l",layout=c(3,1),panel=panel.range)
</pre>

<img src="longitudinal-custom-panel.png" />

<pre>
## This panel.groups function will display the model fits:
panel.model <- function(x,subscripts,col.line,...){
  panel.xyplot(x=x,subscripts=subscripts,col.line=col.line,...)
  llines(x,bw[subscripts,".fitted"],col=col.line,lty=2)
}
dl(xyplot,bw,weight~Time|Diet,Rat,type="l",layout=c(3,1),
   panel=panel.superpose,panel.groups=panel.model,method=first.points)
</pre>

<img src="longitudinal-custom-panel-groups.png" />

<pre>
## Custom panel and panel.groups functions:
dl(xyplot,bw,weight~Time|Diet,Rat,type="l",layout=c(3,1),
   panel=panel.range,panel.groups=panel.model,method=first.points)
</pre>
<img src="longitudinal-custom-both.png" />

<h2>Comparing methods for label positioning on scatterplots</h2>
<pre>
library(latticedl)
compare.methods(c("get.means","parallel.lines","empty.grid","empty.grid.2"),
                xyplot,mpgf,.resid~.fitted,factor(class))
</pre>
<img src="compare.png" />

<pre>
compare.methods(c("first.points","last.points"),
                xyplot,BodyWeight,weight~Time|Diet,Rat,type="l",layout=c(3,1))
</pre>
<img src="compare-long.png" />

<h2>Using the panel.superpose.dl panel function</h2>

<p>The panel.superpose.dl function can be used in place of
panel.superpose in your lattice plots. It behaves just like
panel.superpose.dl, but it intelligently adds direct labels. If you
specify panel.groups as a character rather than a function, then we
can guess a Positioning Function. If not, you can always specify a
Positioning Function with the method= argument.</p>

<pre>
xyplot(weight~Time|Diet,bw,groups=Rat,type="l",layout=c(3,1),
       panel=panel.superpose.dl,panel.groups="panel.xyplot")
</pre>
<img src="longitudinal.png" />

<p>Here we use a custom panel.groups function so the Positioning
Function must be specified using the method= argument:</p>

<pre>
xyplot(weight~Time|Diet,bw,groups=Rat,type="l",layout=c(3,1),
       panel=panel.superpose.dl,panel.groups=panel.model,method=first.points)
</pre>
<img src="longitudinal-custom-panel-groups.png" />

<a href="index.php">Back to site index</a>

</body>

</html>
