
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body class="normal">

<!-- R-Forge Logo
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>
 -->

<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>
 -->
<!-- end of project description -->

<h1>Intuitive figures using direct labels instead of legends</h1>

<p>The idea is very simple. You just made a figure where you drew a
bunch of lines or points in different colors, according to some
categorical variable. Now when you look at the figure, how do you
figure out which color corresponds to which value of that
variable?</p>

<p>In most statistical packages the answer to this question is given
by a <b>legend or key</b> that you have to decode. Legends can be at
best, hard to decode, or at worst, downright confusing. Here is a very
confusing example that motivates the use of direct labeling:

<pre>
library(lattice)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
densityplot(~ppp,loci,groups=type,auto.key=list(space="top",columns=3))
</pre>
<img src="confusing.png" />

<p>Look closely. Is the curve for BAL on the left or right? An easy
fix for this problem would be putting the label right next to the
colored lines. Then we would be using the data for label positioning,
which is inherently more intuitive and obvious to decode.</p>

<p>"But," you say, "lattice makes it so easy to makes these legends!
Direct labeling is a lot of tedious work! I can live with these
confusing legends!"</p>

<h2>Do not live with confusing legends any longer. Instead, use direct
labels.</h2>

<p>This package is an attempt to make direct labeling
a reality in everyday statistical practice by making available a body
of useful functions that make direct labeling of common plots easy to
do. The first working examples are based on lattice graphics:</p>

<pre>
install.packages("latticedl")
library(latticedl)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
## Just add direct.label() around your lattice plot:
direct.label(densityplot(~ppp,loci,groups=type,n=500))
</pre>
<img src="density.png" />
<p>
<a href="examples.php">More examples...</a>
</p>

<h2>An extensible framework for thinking about direct labeling
problems</h2>

<p>Direct labeling a plot can be decomposed into 2 steps: calculating
label positions, then drawing the labels. Drawing the labels will
always be taken care of for you, using the color of the corresponding
group. Calculating label positions is also done for you for common
plot types. For example, with the density plot above, we placed the
each label above the mode of the corresponding density estimate.</p>

<p>If default label positions are not satisfactory, you can always
specify your own label placement method, using the method= argument to
direct.label. For example, we can label longitudinal data either on
the left or right of the lines:</p>

<pre>
library(latticedl)
data(BodyWeight,package="nlme")
p <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type="l",layout=c(3,1))
direct.label(p,method=first.points)
direct.label(p,method=last.points)
</pre>

<p>Here first.points and last.points are Positioning Functions of the
form function(d,...){return(data.frame(x=,y=,groups=))}, where d is
all the data to plot, as a data frame with columns <tt>x y
groups</tt>. first.points simply returns the rows of the data frame
which correspond to the first points for each group. latticedl plots a
direct label for each row returned by the positioning method.</p>

<img src="compare-long.png" alt="direct label longitudinal data" />

<p>Also note that the drawing functions in package latticedl are
totally linked to the lattice graphics framework, but we can
potentially use Positioning Functions with other plotting frameworks,
i.e. ggplot2. Here is how direct labeling will work for ggplot2, using
experimental functions in the directlabels package:</p>

<pre>
dp <- qplot(ppp,data=loci,colour=type,geom="density")
direct.label(dp,method=list(trans.densityplot,top.points))
</pre>

<img src="densityplot-ggplot2.png" alt="densityplot in ggplot2" />

<h2>Talks</h2>

<p><a href="http://www.mnhn.fr/semin-r/">semin-r</a>, 15 oct 2009. "<a
href="HOCKING-latticedl-semin-r.pdf">Visualizing multivariate data
using lattice and direct labels</a>" with <a
href="HOCKING-latticedl-semin-r.R">R code examples</a>.</p>

<p>Please send email to <a
href="http://r-forge.r-project.org/sendmessage.php?touser=1571">Toby
Dylan Hocking</a> if you are using this package or have ideas to
contribute, thanks!</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
