
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
	<title>BaSTA</title>
	<link href="http://r-forge.r-project.org/themes/rforge/styles/estilo1.css" rel="stylesheet" type="text/css" /> 
  </head>

<body>
<div dir="ltr"><a name="top"> </a>

<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->


<!-- 1.- HEADER:-->
<table style="background-color:#47697E" width="1050"  style="table-layout:fixed">
	<tbody>
		<tr>
			<td width="10px">&nbsp;</td>
			<td valign="middle" width="30px">
				<img alt="BaSTA logo" border="0" height="100px" src="bastalogo.jpg?height=100px" style="margin:5px">
			</td>
			<td width="20px">&nbsp;</td>
			<td width="990px" style="vertical-align:middle">
				<p style="white-space:nowrap;vertical-align:middle;font-family:verdana;margin-top:0px;margin-bottom:0px;font-size:38px;letter-spacing:0.25em;color:#FFCC33">BaSTA</p>
				<p style="white-space:nowrap;vertical-align:middle;font-family:verdana;font-size:24px;color:#FCF1D1">Bayesian Survival Trajectory Analysis</p>
			</td>
        </tr>
    </tbody>
</table>
 

<!-- 2.- LINKS:-->
<table width="1050px" style="table-layout:fixed">
	<tbody>
		<tr valign="middle">
			<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
				<a href="#summary" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">SUMMARY</a>
			</td>
			<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
				<a href="#description" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">PACKAGE</a>
			</td>
			<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
				<a href="#model" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">MODEL</a>
			</td>
			<td style="text-align:center;vertical-align:middle;height:30px;background-color:#A3C586;width:200px">
				<a href="#refs" style="color:#38472A;text-decoration:none;font-size:20px;font-family:verdana">REFERENCES</a>
			</td>
		</tr>
	</tbody>
</table>


  
<!-- 3.- LOCAL LINKS:-->
<table width="700px" style="table-layout:fixed" cellpadding="10">
	<tbody>
		<tr>
			<td align="center" valign="top" width="250">
				<p><br></p>
				<img alt="Sooty tern banding" border="1" src="sootyBand.jpg">
				<p align="left"><br><br><br><br><br>&nbsp<a href="http://r-forge.r-project.org/"><img height="30px" src="http://r-forge.r-project.org/themes/rforge/images/logo.png"" border="0" alt="R-Forge Logo"></a></p>
			</td>
			<td width="50">&nbsp;</td>
			<td width="450" valign="top">
			  <p style="font-family:verdana;font-size:32px;color:#84002E;letter-spacing:0.2em"><b>BaSTA</b></p>
				<p style="font-family:verdana;font-size:16px;line-height:1.25"><b>Authors:</b><br><br>Fernando Colchero <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Colchero@demogr.mpg.de</a><br>Owen R. Jones <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Jones@demogr.mpg.de</a><br>Maren Rebke <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Rebke@demogr.mpg.de</a><br></p>
				<p style="font-family:verdana;font-size:16px;line-height:1.25;text-align:justify"><br><b>Developed at:</b><br><br><a href="http://www.demogr.mpg.de" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Max Planck Institute for Demographic Research</a><br>(<i>Laboratory of Evolutionary Biodemography and Independent Research Group Modeling the Evolution of Aging</i>).</p>
			</td>
		</tr>
	</tbody>
</table>

<p style="font-size:small"><br></p>

<!-- 4.- SECTIONS:-->
	<!-- 4.1- Summary:-->
  
<table align="left" bgcolor="#A3C586" valign="middle" width="1050px" style="table-layout:fixed">
	<tbody>
		<tr>
			<td>
				<p style="letter-spacing:0.2em;color:#38472A;font-family:verdana;font-size:24px;line-height:20%;text-indent:20px"><a name="summary">Project summary</a></p>
			</td>
		</tr>
	</tbody>
</table>
<p><br><br></p>

<table width="1050px" style="table-layout:fixed" cellpadding="40">
  <tbody>
    <tr>
		<td style="vertical-align:middle" width="420px">
		<p style="text-align:justify;font-family:verdana;line-height:1.75;font-size:18px"><b>BaSTA</b> is an R package (R Development Core Team 2011) that allows drawing inference on age-specific survival and mortality patterns from capture-recapture/recovery data when a large number of individuals (or all) have missing age information (Colchero, Jones and Rebke <i>in prep</i>). <b>BaSTA</b> is based on a model developed by Colchero and Clark (<i>in press</i>), which extends inference from parameter estimates to the estimation of unknown (i.e. latent) times of birth and death. The package also allows testing the effect of categorical and continuous individual covariates on mortality and survival (for an example see Fig. 1). Although the package is still on a development phase, we will be able to launch it in a few weeks.</p>
	  </td>
		<td style="vertical-align:top" width="420px">
		<p><img width="400px" align="right" src="kestrelGO.png?width=400px"><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br></p>
		<p align="center" style="font-size:12px;text-align:center;font-family:verdana">Fig. 1. <b>BaSTA</b> model output for sex differences in Kestrel mortality<br>using a Gompertz survival model (Rebke <i>et al. in prep</i>). The left<br>panel shows posterior distributions for the two survival parameters<br>while the right panel shows the resulting survival probabilities and the <br>mortality rates for males and females.</p>
	  </td>
    </tr>
  </tbody>
</table>

<p style="font-size:12px"><a href="#top" style="font-size:12px;color:#84002E;font-family:verdana">Back to top</a></p>


	<!-- 4.2- Description:-->

<table align="left" bgcolor="#A3C586" valign="middle" width="1050px" style="table-layout:fixed">
	<tbody>
		<tr>
			<td>
				<p style="letter-spacing:0.2em;color:#38472A;font-family:verdana;font-size:24px;line-height:20%;text-indent:20px"><a name="description">Package description</a></p>
			</td>
		</tr>
	</tbody>
</table>
<p><br><br></p>

<table width="1050px" style="table-layout:fixed" cellpadding="40">
  <tbody>
    <tr>
		<td style="vertical-align:middle" width="420px">
			<p style="text-align:justify;font-family:verdana;line-height:1.75;font-size:18px"><b>BaSTA</b> requires conventional data input (equivalent to other common capture-recapture softwares) and minimum user input (Fig. 2). The data required consist of a single table, in data frame format (see <a href="http://cran.r-project.org/doc/manuals/R-intro.html#Lists-and-data-frames"  rel="nofollow" style="color:#84002E;text-decoration:none">Introduction to R</a> on how to build data frames), where each row corresponds to an individual history and where the first two columns include the times of birth and death respectively, followed by the traditional capture history matrix, with one column for each year of the study and that assigns 1 when individuals were detected and 0 otherwise. The last columns are optional, and should include covariates such as sex, location id, birth weight, etc.<br><br>The current version includes:</p>
			<ul style="font-family:verdana;text-align:justify;line-height:1.75;font-size:18px">
				<li>Functions for data formatting.</li>
				<li>Data checking and correction of common data errors.</li>
				<li>Estimates of age-specific survival parameters.</li>
				<li>Estimates of yearly recapture probabilities.</li>
				<li>Estimates latent (i.e. unknown) times of birth and death.</li>
				<li>Allows testing three different mortality functions (Gompertz, Gompertz-Makeham and Siler)(Gompertz 1925, Pletcher 1999, Siler 1979).</li>
				<li>Evaluates the effects of time-independent categorical and continuous covariates on survival.</li>
				<li>Runs multiple simulations either in parallel (using package snowfall; Knaus 2010) or in series.</li>
			</ul>
		</p>
		</td>
		<td style="vertical-align:top" width="420px">
		    <p align="center"  style="font-size:10"><img width="500px" align="right" src="BastaGeneralChart.jpg?width=500px"><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
</p>
			<p align="center" style="font-size:12px;font-family:verdana">Fig. 2. <b>BaSTA</b> general package chart.</p><br>
			<ul style="font-family:verdana;text-align:justify;line-height:1.75;font-size:18px">
				<li>Calculates basic diagnostics on MCMC performance such as parameter update rates and serial autocorrelation.</li>
				<li>Uses multiple simulations to estimate convergence (i.e. potential scale reduction factor; Gelman <i>et al.</i>2004).</li>
				<li>Calculates basic measures for model selection (DIC; Spiegelhalter <i>et al.</i>2002).</li>
			</ul>
			<p style="text-align:justify;font-family:verdana;line-height:1.75;font-size:18px">Future versions will include:</p>
			<ul style="font-family:verdana;text-align:justify;line-height:1.75;font-size:18px">
				<li>Time-dependent covariates.</li>
				<li>Covariates on recapture probabilities.</li>
				<li>Model selection through Reversible Jump MCMC (RJMCMC; King and Brooks 2002; Gimenez <i>et al.</i> 2009).</li>
				<li>Additional mortality models (i.e. logistic, Weibull, etc.).</li>
			</ul>
		</p>
	  </td>
    </tr>
  </tbody>
</table>

<p style="font-size:12px"><a href="#top" style="font-size:12px;color:#84002E;font-family:verdana">Back to top</a></p>

	<!-- 4.3- Model:-->

<table align="left" bgcolor="#A3C586" valign="middle" width="1050px" style="table-layout:fixed">
	<tbody>
		<tr>
			<td>
				<p style="letter-spacing:0.2em;color:#38472A;font-family:verdana;font-size:24px;line-height:20%;text-indent:20px"><a name="model">Model description</a></p>
			</td>
		</tr>
	</tbody>
</table>
<p><br><br></p>

<table width="1050px" style="table-layout:fixed" cellpadding="40">
  <tbody>
    <tr>
		<td style="vertical-align:middle" width="420px">
		<p style="text-align:justify;font-family:verdana;line-height:1.75;font-size:18px">The core of the model is a Monte Carlo Markov Chain (MCMC) algorithm that combines Metropolis sampling for survival parameters and latent states (i.e. unknown times of birth and death) and direct sampling for recapture probabilities (Clark 2007, Colchero and Clark <i>in press</i>). The main function performs several diagnostics on the user's inputs such as checking that the data is consistent with the model's requirements, verifies that the number of iterations, the burnin sequence and the thinning gap are consistent and finally verifies that the initial parameters and jumps provided by the user allow the model to run properly (Fig. 3). After running these diagnostics, the function can run multiple MCMC simulations. In case convergence is not acheived or some or all simulations failed, neither convergence nor model selection diagnostics are calculated.</p>
	  </td>
		<td style="vertical-align:top" width="420px">
	      <p align="center" style="font-size:10"><img width="500px" align="right" border="0" src="bastaChart.jpg?width=500px"><br>
</p>
	      <p align="center" style="font-size:12px;font-family:verdana">Fig. 3. <b>BaSTA</b> model chart.</p>
	  </td>
    </tr>
  </tbody>
</table>

<p style="font-size:12px"><a href="#top" style="font-size:12px;color:#84002E;font-family:verdana">Back to top</a></p>

 
<table align="left" bgcolor="#A3C586" valign="middle" width="1050px" style="table-layout:fixed">
	<tbody>
		<tr>
			<td>
				<p style="letter-spacing:0.2em;color:#38472A;font-family:verdana;font-size:24px;line-height:20%;text-indent:20px"><a name="refs">References</a></p>
			</td>
		</tr>
	</tbody>
</table>

<p><br>
<br>
</p>

<table width="1050px" style="table-layout:fixed" cellpadding="20">
	<tbody>
		<tr>
			<td style="vertical-align:middle" width="420px">
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">
				Clark, J.S. (2007) <b>Models for ecological data</b>. Princeton University Press, Princeton, New Jersey, USA.<br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">Colchero, F. and J. S. Clark (<i>in press</i>) <b>Bayesian inference on age-specific survival for censored and truncated data.</b> <i>Journal of Animal Ecology</i>.<br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">Colchero, F., O.R. Jones and M. Rebke (<i>In progress</i>) <b>BaSTA: an R package to estimate survival and mortality from incomplete mark-recapture data with covariates.</b><br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">Gimenez, O., Bonner, S., King, R., Parker, R.A., Brooks, S.P., Jamieson, L.E., Grosbois, V., Morgan, B.J.T., Thomas, L. (2009) <b>WinBUGS for population ecologists: Bayesian modeling using Markov Chain Monte Carlo methods</b>. <u>In</u><i> Modeling Demographic Processes in Marked Populations. Ecological and Environmental Statistics Series, vol 3</i> (eds D.L. Thomson, E.G. Cooch &amp; M.J. Conroy), pp. 883-915. Springer, Berlin, Germany.<br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">Gompertz, B. (1825) <b>On the nature of the function expressive of the law of human mortality, and on a new mode of determining the value of life contingencies</b>. <i>Philosophical Transactions of the Royal Society of London</i>, 115, 513-583.<br></p>
			</td>
			<td style="vertical-align:top" width="420px">
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">King, R. and Brooks, S.P. (2002) <b>Bayesian model discrimination for multiple strata capture-recapture data</b>. <i>Biometrika</i>, 89, 785-806.<br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">Knaus, J. (2010). <b>snowfall: Easier cluster computing (based on snow)</b>. R package version 1.84.
					<a href="http://CRAN.R-project.org/package=snowfall" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/package=snowfall</a><br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">Pletcher, S. (1999) <b>Model fitting and hypothesis testing for age-specific mortality data</b>. <i>Journal of Evolutionary Biology</i>, 12, 430-439.<br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">R Development Core Team (2011). <b>R: A language and environment for statistical computing</b>. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL <a href="http://CRAN.R-project.org/" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/</a>.<br><br></p>
				<p style="font-family:verdana;text-align:justify;margin-left:.5in;text-indent:-.5in;line-height:1.75;font-size:16px">Siler, W. (1979) <b>A competing-risk model for animal mortality</b>. <i>Ecology</i>, 60, 750-757.<br></p>
			</td>
		</tr>
	</tbody>
</table>

<p style="font-size:12px"><a href="#top" style="font-size:12px;color:#84002E;font-family:verdana">Back to top</a></p>
</div>

</body>
</html>
