
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
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<h1>Intuitive figures using direct labels instead of legends</h1>

<p>The idea is very simple. You just made a figure where you drew a
bunch of lines or points in different colors, according to some
categorical variable. Now when you look at the figure, how do you
figure out which color corresponds to which value of that
variable?</p>

<p>In most statistical packages the answer to this question is given
by a <b>legend or key</b> that you have to decode. <b>This sucks</b>
because it can be at best, hard to decode, or at worst, downright
confusing. Here's an example <b>that sucks:</b></p>

<pre>
library(lattice)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
densityplot(~ppp,loci,groups = type,auto.key=list(space="top",columns=3))
</pre>
<img src="confusing.png" />

<p>Look closely. Is the curve for BAL on the left or right? An easy
fix for this problem would be putting the label right next to the
colored lines. Then we're using the data for label positioning, which
is inherently more intuitive and obvious to decode.</p>

<p>"But," you say, "lattice makes it so easy to makes these legends!
Direct labeling is a lot of tedious work! I can live with these shitty
legends!"</p>

<h2>Don't live with confusing legends any longer. Instead, use
direct labels.</h2>

<p>This package is an attempt to make direct labeling
a reality in everyday statistical practice by making available a body
of useful functions that make direct labeling of common plots easy to
do. The first working examples are based on lattice graphics:</p>

<pre>
install.packages("ggplot2") # for dependencies on CRAN
install.packages("latticedl", repos="http://R-Forge.R-project.org")
library(latticedl)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
dl(densityplot,loci,~ppp,type,n=500)
</pre>
<img src="density.png" />
<p>
<a href="examples.php">More examples...</a>
</p>

<h2>An extensible framework for thinking about direct labeling
problems</h2>

<p> Direct labeling a plot basically consists of 2 steps:
</p>

<ul> 

<li><b>Label positioning:</b> calculating where you want to put the
labels based on the data points. This is done by the Positioning
Function, specified by the method= argument to dl.</li>

<li><b>Drawing the labels:</b> we will always take care of drawing the
labels for you, using the right color.</li>

</ul>

<p>Label positioning is something that changes from graphic to
graphic. Thus we introduce the concept of a Positioning
Function. Change the Positioning Function, and you change the position
of your direct labels. For example, we can label longitudinal data
either on the left or right of the lines, using included Positioning
Functions as follows:</p>

<pre>
library(latticedl)
data(BodyWeight,package="nlme")
dl(xyplot,BodyWeight,weight~Time|Diet,Rat,type="l",layout=c(3,1),
   method=first.points)
dl(xyplot,BodyWeight,weight~Time|Diet,Rat,type="l",layout=c(3,1),
   method=last.points)
</pre>
<img src="compare-long.png" />

<p>The package comes with several built-in positioning functions, and
tries to choose the best one to use based on your choice of high-level
lattice function. The defaults can't be right for every one of your
graphs, but you can always make the labels appear at the right place
by writing your own Positioning Functions tailored to your data. If
you want to position your labels yourself, write a Positioning
Function that takes the input data points as a data frame with columns
<tt>x y groups</tt>, and returns a data frame with columns <tt>x y
groups</tt> that describes where the labels should be positioned for
each group.</p>

<p>Also note that the drawing functions are totally linked to the
lattice graphics framework, but we can use Positioning Functions with
other plotting frameworks, i.e. ggplot2.</p>

<p>Please send email to <a
href="http://r-forge.r-project.org/sendmessage.php?touser=1571">Toby
Dylan Hocking</a> if you are using this package, thanks!</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
