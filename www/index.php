
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

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<div dir="ltr"><a name="top"> </a>


<!-- 1.- HEADER:-->
<table style="background-color:#47697E" width="100%">
            
<tbody>
<tr>
<td width="10">&nbsp;</td>
<td valign="middle" width="30px">
<img alt="Fernando Colchero" border="0" height="100px" src="https://sites.google.com/site/fernandocolchero/ferlogo.jpg?height=100px" style="margin:5px">
</td>
                
<td width="20">&nbsp;</td>
<td style="vertical-align:middle">
                    
<p style="white-space:nowrap;vertical-align:middle;font-family:verdana;margin-top:0px;margin-bottom:0px;font-size:38px;letter-spacing:0.25em;color:#FFCC33">BaSTA</p>
                    
<p style="white-space:nowrap;vertical-align:middle;font-family:verdana;font-size:24px;color:#FCF1D1">Bayesian Survival Trajectory Analysis</p>
                </td>
            </tr>
        </tbody>
</table>
 

<!-- 2.- LINKS:-->
<!--<table width="100%">
<tbody>
<tr valign="middle">
<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
<a href="https://sites.google.com/site/fernandocolchero/home" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">HOME</a>
</td>
<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
<a href="https://sites.google.com/site/fernandocolchero/cv" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">CV</a>
</td>
<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
<a href="https://sites.google.com/site/fernandocolchero/projects" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">PROJECTS</a>
</td>
<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
<a href="https://sites.google.com/site/fernandocolchero/links" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">LINKS</a>
</td>
</tr>
</tbody>
</table>

<p><br>
</p>
-->
  
<!-- 3.- LOCAL LINKS:-->
<p><br></p>
<table>
<tbody>
<tr>
 
<td align="center" valign="middle" width="290">
    <img alt="Fernando Colchero" border="1" src="https://sites.google.com/site/fernandocolchero/sootyBand.jpg">
  </td>
  
<td width="20">&nbsp;</td>
  
<td width="400">
    
<h2 style="font-family:verdana;font-size:28px;color:#84002E;letter-spacing:0.2em">BaSTA</h2>
    
  
<!--<p style="font-family:verdana"><b>Bayesian Survival Trajectory Analysis</b></p>-->
    
    
<p style="font-family:verdana;font-size:14px;text-align:justify">Bayesian inference on age-specific survival for capture-recapture/recovery data with censored and truncated records</p>


<p style="font-family:verdana;font-size:14px"><b>Authors:</b><br>Fernando Colchero <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Colchero@demogr.mpg.de</a><br>Owen R. Jones <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Jones@demogr.mpg.de</a><br>Maren Rebke <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Rebke@demogr.mpg.de</a></p>
 
</td>
</tr>
</tbody>
</table>

<p style="font-size:small"><br>
</p>

<!-- 4.- BODY:-->
  
<table align="left" bgcolor="#A3C586" style="border-top:1px solid rgb(192, 192, 192);border-bottom:1px solid rgb(192, 192, 192)" valign="middle" width="100%">
<tbody>
<tr>
<td>
<h3 style="letter-spacing:0.2em;color:#38472A;font-family:verdana">Project description</h3>
</td>
</tr>
</tbody>
</table>
<p><br>
<br>
</p>

<table width="100%">
<tbody>
<tr>
<td style="vertical-align:middle" width="500px">
<p style="text-align:justify;font-family:verdana">BaSTA is an R package (R Development Core Team 2011) which allows drawing inference on age-specific survival and mortality patterns from capture-recapture/recovery data when a large number of individuals (or all) have missing age information (Colchero, Jones and Rebke <i>in prep</i>). BaSTA is based on a model developed by Colchero and Clark (<i>in press</i>), which extends inference from parameter estimates to the estimation of unknown (i.e. latent) times of birth and death. The package also allows testing the effect of categorical and continuous individual covariates on mortality and survival (see figure). Although the package is still on a development phase, we will be able to launch it in a few weeks.<br>
<br>
The model is based on a Monte Carlo Markov Chain algorithm that combines Metropolis sampling for survival parameters and latent states (i.e. unknown times of birth and death) and direct sampling for recapture probabilities (Clark 2007, Colchero and Clark <i>in press</i>). The current version of the package includes:</p>
<ul style="font-family:verdana">
    <li>Estimates of age-specific survival parameters.</li>
    <li>Estimates of yearly recapture probabilities.</li>
    <li>Estimates latent (i.e. unknown) times of birth and death.</li>
    <li>Allows testing three different mortality functions (Gompertz, Gompertz-Makeham and Siler)(Gompertz 1925, Pletcher 1999, Siler 1979).</li>
    <li>Evaluates the effects of time-independent categorical and continuous covariates on survival.</li>
    <li>Runs multiple simulations either in parallel (using package snowfall; Knaus 2010) or in series.</li>
    <li>Uses multiple simulations to estimate convergence (i.e. potential scale reduction factor; Gelman <i>et al.</i>2004).</li>
    <li>Calculates basic measures for model selection (DIC; Spiegelhalter <i>et al.</i>2002).</li>

</ul>

<p style="text-align:justify;font-family:verdana">Future versions will include:</p>
<ul style="font-family:verdana">
    <li>Time-dependent covariates.</li>
    <li>Covariates on recapture probabilities.</li>
    <li>Model selection through Reversible Jump MCMC (RJMCMC; King and Brooks 2002; Gimenez <i>et al.</i> 2009).</li>
    <li>Additional mortality models (i.e. logistic, Weibull, etc.).</li>
</ul>

</td>
<td align="center">


<p align="center" style="font-size:10"><img height="350px" src="https://sites.google.com/site/fernandocolchero/kestrelGO.png?height=350px"><br>
</p>
<p align="center" style="font-size:11px;font-family:verdana">BaSTA model output for sex differences in Kestrel mortality<br>
using a Gompertz survival model (Rebke <i>et al. in prep</i>).</p>

<div style="text-align:center"></div>

</td>
</tr>
</tbody>
</table>






<p style="font-size:12px"><a href="#top" style="font-size:12px;color:#84002E;font-family:verdana">Back to top</a></p>

  
<table align="left" bgcolor="#A3C586" style="border-top:1px solid rgb(192, 192, 192);border-bottom:1px solid rgb(192, 192, 192)" valign="middle" width="100%">
<tbody>
<tr>
<td>
<h3 style="letter-spacing:0.2em;color:#38472A;font-family:verdana">References</h3>
</td>
</tr>
</tbody>
</table>

<p><br>
<br>
</p>

<ul style="font-family:verdana">

  <li>Clark, J.S. (2007) <b>Models for ecological data</b>. Princeton University Press, Princeton, New Jersey, USA.</li>
  <br>
  <li>Colchero, F. and J. S. Clark (<i>in press</i>) <b>Bayesian inference on age-specific survival for censored and truncated data.</b> <i>Journal of Animal Ecology</i>.
</li>
<br>
  <li>Colchero, F., O.R. Jones and M. Rebke (<i>In progress</i>) <b>BaSTA: an R package to estimate survival and mortality from incomplete mark-recapture data with covariates.</b></li>
<br>
  <li>Gimenez, O., Bonner, S., King, R., Parker, R.A., Brooks, S.P., Jamieson, L.E., Grosbois, V., Morgan, B.J.T., Thomas, L. (2009) <b>WinBUGS for population ecologists: Bayesian modeling using Markov Chain Monte Carlo methods</b>. <u>In</u><i> Modeling Demographic Processes in Marked Populations. Ecological and Environmental Statistics Series, vol 3</i> (eds D.L. Thomson, E.G. Cooch &amp; M.J. Conroy), pp. 883-915. Springer, Berlin, Germany.</li>
  <br>
  <li>Gompertz, B. (1825) <b>On the nature of the function expressive of the law of human mor- tality, and on a new mode of determining the value of life contingencies</b>. <i>Philosophical Transactions of the Royal Society of London</i>, 115, 513�583.</li>
  <br>
  <li> King, R. and Brooks, S.P. (2002) <b>Bayesian model discrimination for multiple strata capture-recapture data</b>. <i>Biometrika</i>, 89, 785-806</li>
<br>
  <li>Knaus, J. (2010). <b>snowfall: Easier cluster computing (based on snow)</b>. R package version 1.84.
  <a href="http://CRAN.R-project.org/package=snowfall" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/package=snowfall</a></li>
  <br>
  <li>Pletcher, S. (1999) <b>Model fitting and hypothesis testing for age-specific mortality data</b>. <i>Journal of Evolutionary Biology</i>, 12, 430-439.</li>
  <br>
<li>R Development Core Team (2011). <b>R: A language and environment for statistical computing</b>. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL <a href="http://CRAN.R-project.org/" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/</a>.</li>
 <br>
  <li>Siler, W. (1979) <b>A competing-risk model for animal mortality</b>. <i>Ecology</i>, 60, 750-757.</li>
 
</ul>


<p style="font-size:12px"><a href="#top" style="font-size:12px;color:#84002E;font-family:verdana">Back to top</a></p>
</div>

</body>
</html>
