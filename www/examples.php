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
panel.superpose, but it intelligently adds direct labels. If you
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

<h2>Specifying the positioning method as a list</h2>

<p>Since the input and output of a Positioning Function is the same
sort of data frame, you can chain Positioning Functions together. To
illustrate how this works, consider the following contrived
example:</p>

<pre>
complicated <- list(dl.trans(x=x+10), ## add 10 to every x value
                    dl.indep(d[-2,]), ## delete the 2nd point of every group
                    rot=c(30,180)) ## rotate by alternately 30 and 180 degrees
direct.label(dotplot(VADeaths,type="o"),method=complicated)
</pre>

<img src="method2.png" alt="contrived direct labels" />

<p>When method is specified as a list, the arguments will be applied
in sequence, starting from the data frame of all plotted points. In
this example, </p>

<ol>

<li>the first element is a function that adds 10 to every x value,
thus shifting the labels to the right relative to the points.</li>

<li>the second element is a call to dl.indep, which lets you specify
an expression to apply to each group, as a function of the data d. In
this case d[-2,] means delete the second row from each group.</li>

<li>named elements are copied to the data frame, so the third element,
rot=c(30,180), adds a rot column to the data frame, with values 30 and
180 for every other row. This has the effect of rotating every label
by 30 or 180 degrees. Text display parameters fontsize, fontfamily,
fontface, lineheight, and cex can also be specified in this manner
(see the help page for grid::grid.text).
</li>

</ol>

<a href="index.php">Back to site index</a>

</body>

</html>
