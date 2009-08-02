
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
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

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
confusing.</p>

<p><b>Instead, use direct labels.</b> That is, just put the label
right next to the color lines/points, in the same color. So then it's
obvious and clear what you're plotting.</p>

<p>This package is an attempt to make direct labeling a reality in
everyday statistical practice by making available a body of useful
functions that make direct labeling of common plots easy to do. The
first working examples are based on lattice graphics:</p>

<h2>Density estimates for some simulated PPP-values under 3 conditions</h2>
<pre>
library(latticedl)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
dl(densityplot,loci,~ppp,type,
   n=500)
</pre>
<img src="density.png" />

<h2>Longitudinal data for body weight of 18 rats and 3 different medical treatments</h2>
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
plot(dl(xyplot,mpgf,.resid~.fitted,factor(cyl)))
</pre>
<img src="scatter.png" />



<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
