<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title>directlabels - advanced examples</title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>
				   
<body>

<h2>Custom panel and panel.groups functions for the rat body weight data</h2>

<p>If you want to use a custom display in lattice panels, you need to
write a custom panel or panel.groups function. For something you want
to draw once for each panel, use a custom panel function, and for
something that you want to draw for each group, use a custom
panel.groups function. When you use a custom panel.groups function,
you need to explicitly specify the Positioning Function for the direct
labels. This series of examples should should illustrate how to
effectively create custom direct labeled displays.</p>

<pre>
library(directlabels)
data(BodyWeight,package="nlme")
## Say we want to use a simple linear model to explain rat body weight:
fit &lt;- lm(weight~Time+Diet+Rat,BodyWeight)
bw &lt;- fortify(fit,BodyWeight)

## Custom panel function which highlights min and max values:
panel.range &lt;- function(y,...){
  panel.abline(h=range(y))
  panel.superpose(y=y,...)
}
direct.label(xyplot(weight~Time|Diet,bw,groups=Rat,type="l",
                    layout=c(3,1),panel=panel.range))
</pre>

<img alt="direct labeled display with custom panel function"
src="longitudinal-custom-panel.png" />

<pre>
## This panel.groups function will display the model fits:
panel.model &lt;- function(x,subscripts,col.line,...){
  panel.xyplot(x=x,subscripts=subscripts,col.line=col.line,...)
  llines(x,bw[subscripts,".fitted"],col=col.line,lty=2)
}
x &lt;- xyplot(weight~Time|Diet,bw,groups=Rat,type="l",layout=c(3,1),
            panel=panel.superpose,panel.groups=panel.model)
direct.label(x,"first.points")
</pre>

<img alt="direct labeled display with custom panel.groups function"
src="longitudinal-custom-panel-groups.png" />

<pre>
## Custom panel and panel.groups functions:
x &lt;- xyplot(weight~Time|Diet,bw,groups=Rat,type="l",layout=c(3,1),
            panel=panel.range,panel.groups=panel.model)
direct.label(x,"first.points")
</pre>
<img alt="direct labeled diplay with custom panel and panel.groups
functions" src="longitudinal-custom-both.png" />

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
<img alt="you can add direct labels using the panel.superpose.dl panel
function" src="longitudinal.png" />

<p>Here we use a custom panel.groups function so the Positioning
Function must be specified using the method= argument:</p>

<pre>
xyplot(weight~Time|Diet,bw,groups=Rat,type="l",layout=c(3,1),
       panel=panel.superpose.dl,panel.groups=panel.model,method="first.points")
</pre>
<img alt="you must explicitly specify the Positioning Function if you
use a custom panel.groups function"
src="longitudinal-custom-panel-groups.png" />

<h2>Specifying the positioning method as a list</h2>

<p>Since the input and output of a Positioning Function is the same
sort of data frame, you can chain Positioning Functions together. To
illustrate how this works, consider the following contrived
example:</p>

<pre>
complicated &lt;- list(dl.trans(x=x+10), ## add 10 to every x value
                    dl.indep(d[-2,]), ## delete the 2nd point of every group
                    rot=c(30,180)) ## rotate by alternately 30 and 180 degrees
direct.label(dotplot(VADeaths,type="o"),complicated)
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
by 30 or 180 degrees. Text display parameters alpha, fontsize,
fontfamily, fontface, lineheight, and cex can also be specified in
this manner (see the help pages for grid::gpar for an exhaustive
description and directlabels::dlcompare for an example).
</li>

</ol>

<h2>Compare several plots and methods</h2>

<p>Several different labeling methods can be compared for any mix of
ggplot2 and/or lattice plots.</p>

<pre>
library(directlabels)
dts &lt;- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
ddf &lt;- melt(as.data.frame(dts),id="time")
names(ddf) &lt;- c("time","sex","deaths")
plots &lt;- list(lattice=
              xyplot(deaths~time,ddf,groups=sex,type="l",xlim=c(-15,80)),
              ggplot2=
              qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-10,80))
pos.funs &lt;- list("first.points","lines2")
dlcompare(plots,pos.funs)
</pre>

<img alt="lattice and ggplot2 plots can be compared"
src="compare-new.png" />

<pre>
scatters &lt;- list(xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1),
                 xyplot(Sepal.Length~Petal.Length,iris,groups=Species))
dlcompare(scatters,list("empty.grid","empty.grid.2"))
</pre>

<img alt="several plots and Positioning Functions can be compared"
src="scattercompare.png" />

  <!--
  <h2>Direct labeling several datasets</h2>

  <p>This is a situation that does not occur very often, but if you
  need to direct label 2 different datasets, here is how you should go
  about doing it.</p>

  <ul>

  <li>ggplot2: Instead of the usual direct.label(plot)</li>

  </ul>
  -->

<center>
<table>
<tr><td>Please send email
to <a href="http://r-forge.r-project.org/sendmessage.php?touser=1571">Toby
Dylan Hocking</a> if you are using <a href="http://directlabels.r-forge.r-project.org/">directlabels</a> or have ideas to
contribute, thanks!</td>
</tr>
<tr>
<td align="center">
    <a href="http://validator.w3.org/check?uri=referer">validate</a>
</td>
</tr>
</table>

</center>

</body>

</html>
