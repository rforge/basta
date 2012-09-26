
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
    <link rel="stylesheet" type="text/css" href="bastastyle.css" /> 
	<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-4964902-4']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

  </script>
  </head>

  <body style="background:#E5F5E0">
    <a name="top"></a>
    <table style="vertical-align:middle;background:#E5F5E0;padding:0px;outline-width:0px" width="100%" height="100%">
      <tbody>
        <tr align="center">
          <td>
            <table style="vertical-align:middle;background:#FFFFFF" width="1050px">
              <tbody><tr align="center"><td>
            <!-- 1.- HEADER:-->
            <table width="1050"  style="table-layout:fixed">
              <tbody>
                <tr>
                  <td style="padding:0px;outline-width:0px"><img alt="BaSTA logo" border="0" width="1045px" src="bastalogo.jpg?"></td>
                </tr>
              </tbody>
            </table>
            <!-- 2.- LINKS:-->
            <table width="1050px" style="table-layout:fixed;vertical-align:middle;background:#FFFFFF" cellspacing="5px">
              <tbody>
                <tr>
                  <td class="button"><a href="#summary" class="button">SUMMARY</a></td>
                  <td class="button"><a href="#description" class="button">PACKAGE</a></td>
                  <td class="button"><a href="#use" class="button">APPLICATION</a></td>
                  <td class="button"><a href="#bugs" class="button">BUG FIXES</a></td>
                  <td class="button"><a href="#refs" class="button">REFERENCES</a></td>
                </tr>
              </tbody>
            </table>
            <!-- 3.- LOCAL LINKS:-->
            <table width="1045px" style="table-layout:fixed;background:#FFFFFF" cellpadding="20">
              <tbody>
                <tr>
                  <td align="center" valign="top" width="350">
                    <img alt="Sooty tern banding" border="0" width="400px" style="vertical-align:top;outline-width:0px" src="BastaPhotos.jpg">
                    <p align="left">&nbsp<a href="http://r-forge.r-project.org/"><img height="30px" src="Rforgelogo.png" border="0" alt="R-Forge Logo"></a></p>
                  </td>
                  <td width="400" valign="top">
                    <!--<h1><b>BaSTA</b></h1>-->
                    <p style="font-size:16px;line-height:1.25"><b>Authors:</b><br><a href="http://www.colchero.com" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Fernando Colchero</a><br><a href="http://www.owenjon.es" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Owen R. Jones</a><br><a href="http://www.demogr.mpg.de/en/institute/staff_directory_1899/maren_rebke_1430.htm" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Maren Rebke</a><br></p>
                    <p style="font-size:16px;line-height:1.25;text-align:justify"><br><b>Developed at:</b><br><a href="http://www.demogr.mpg.de" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Max Planck Institute for Demographic Research</a><br></p>
                    <p style="font-size:16px;line-height:1.25;text-align:justify"><br><b>Mailing list:</b><br>For enquiries, comments or bug reports, please register to the <a href="mailto:Basta-users@lists.r-forge.r-project.org"  rel="nofollow" style="color:#84002E">BaSTA Users mailing</a></p>
                  </td>
                  <td width="50px"></td>
                </tr>
              </tbody>
            </table>
            <!-- 4.- SECTIONS:-->
            <!-- 4.1- Summary:-->
            <a name="summary"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="Summary" border="0" width="1045px" src="summary.jpg?"></td>
							</tbody>
						</table>
            <table  valign="top" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag"><b>BaSTA</b> is an R package (R Development Core Team 2011) that allows users to draw inference on age-specific survival and mortality patterns from capture-recapture/recovery data when a large number of individuals (or all) have missing age information (Colchero, Jones and Rebke 2012). <b>BaSTA</b> is based on a model developed by Colchero and Clark (2012), which extends inference from parameter estimates to the estimation of unknown (i.e. latent) times of birth and death. The package also allows testing the effect of categorical and continuous individual covariates on mortality and survival (for an example see Fig. 1). Below we feature a video hosted by the journal Methods in Ecology and Evolution, that explains the context in which BaSTA can be useful and the way the package can be implemented.                   </td>
                  <td class="main">
                    <p><br><img style="margin:0px;padding:0px;border:none" width="400px" align="middle" src="Sheep.png"><br><br></p>
                    <p class="caption">Fig. 1. <b>BaSTA</b> model output for sex differences in Soay sheep mortality using a Siler survival model (Colchero & Clark 2012). The left panel shows posterior distributions for the survival and recapture parameters while the right panel shows the resulting survival probabilities and the mortality rates for males and females.</p>
                  </td>
                </tr>
              </tbody>
            </table>
            <table  valign="top" width="1050px" style="table-layout:fixed;background:#FFFFFF;padding-top:0px;padding-bottom:0px">
              <tbody>
                <tr style="background:#FFFFFF">
                  <td class="main">
										<p class="parag">Version 1.3 of the package is now availale on CRAN and can be installed as source by typing the following line of code into the R console: <br></p>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;install.packages("BaSTA")</code>
<br></p>
										<p class="parag">Also, the latest version of <b>BaSTA</b> (vers. 1.4) can be installed from R forge by typing:<br></p>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;install.packages("BaSTA", repos="http://R-Forge.R-project.org", type = "source")</code>
<br></p>
										<p class="parag">We have set up a <a href="mailto:Basta-users@lists.r-forge.r-project.org"  rel="nofollow" style="color:#84002E">BaSTA Users mailing</a> list so users can ask questions or provide comments, suggestions or criticism that can help us improve the package. Users can register by <a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/basta-users"  rel="nofollow" class="intext">clicking here</a>. Also, you can see below a video hosted by the journal <a href="http://www.methodsinecologyandevolution.org/" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Methods in Ecology and Evolution</a> where we explain the rational behind the package and some general applications.<br><br></p> 
										<p><br></p>
										<iframe width="320" height="180" src="http://www.youtube.com/embed/OLFzY3vHVOQ" frameborder="0" allowfullscreen style="display:block;margin:5px auto;text-align:center;"></iframe>
                    <p class="caption"><b>BaSTA</b> video hosted by the journal Methods in Ecology and Evolution.</p>
										<p><br></p>
                    <a href="#top" class="totop">Back to top</a>
                    <p></p>
									</td>
                </tr>
              </tbody>
            </table>
            <!-- 4.2- Package:-->
            <a name="description"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="Package" border="0" width="1045px" src="package.jpg?"></td>
							</tbody>
						</table>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag">The core of the model is a Monte Carlo Markov Chain (MCMC) algorithm that combines Metropolis sampling for survival parameters and latent states (i.e. unknown times of birth and death) and direct sampling for recapture probabilities (Clark 2007, Colchero and Clark <i>in press</i>). The main function performs several diagnostics on the user's inputs such as checking that the data is consistent with the model's requirements, verifies that the number of iterations, the burnin sequence and the thinning gap are consistent and finally verifies that the initial parameters and jumps provided by the user allow the model to run properly (Fig. 2).
                  </td>
                  <td style="vertical-align:top;padding-top:0px;padding-bottom:0px;padding-left:40px;padding-right:40px" width="420px">
                    <p class="main" align="left"><br><img align="left" style="margin:0px;padding:0px;border:none;width:475px" align="middle" src="BastaGeneralChart.jpg?"></p>
                    <p class="caption">Fig. 2. <b>BaSTA</b> general package chart.</p><br>
                  </td>
                </tr>
              </tbody>
            </table>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag">After running these diagnostics, <b>BaSTA</b> uses a dynamic procedure to find adequate jump standard deviations for mortality parameters. This procedure runs before the core of the analysis is performed, keeping the user from having to find these jump sd's by trial and error. After apropriate jumps are found, multiple MCMC simulations can be ran either in parallel or in series. In case convergence is not acheived or some or all simulations failed, neither convergence nor model selection diagnostics are calculated.<br><br>The current version includes:</p>
                    <ul>
                      <li>Functions for data formatting.</li>
                      <li>Data checking and correction of common data errors.</li>
                      <li>Estimates of age-specific survival parameters.</li>
                      <li>Estimates of yearly recapture probabilities.</li>
                      <li>Estimates latent (i.e. unknown) times of birth and death.</li>
                      <li>Allows testing four different mortality functions (Exponential, Gompertz, Weibull and logistic) and to extend the model to Makeham or bathtub shapes (Gompertz 1925, Siler 1979, Cox and Oakes 1984, Pletcher 1999).</li>
                      <li>Evaluates the effects of time-independent categorical and continuous covariates on survival.</li>
                      <li>Runs multiple simulations either in parallel (using package snowfall; Knaus 2010) or in series.</li>
                      <li>Calculates basic diagnostics on MCMC performance such as parameter update rates and serial autocorrelation.</li>
                      <li>Uses multiple simulations to estimate convergence (i.e. potential scale reduction factor; Gelman <i>et al.</i> 2004).</li>
                      <li>Calculates basic measures for model selection (DIC; Spiegelhalter <i>et al.</i> 2002).</li>
                      <li>Performs dynamic update of jump standard deviations, improving convergence without the need of several trials.</li>
                    </ul>
                    <p style="text-align:justify;line-height:1.75;font-size:18px">Future versions will include:</p>
                    <ul style="text-align:justify;line-height:1.75;font-size:18px">
                      <li>Time-dependent covariates.</li>
											<li>Cohort effects on mortality</li>
                      <li>Covariates on recapture and recovery probabilities.</li>
                      <li>Model selection either through Reversible Jump MCMC (RJMCMC; King and Brooks 2002; Gimenez <i>et al.</i> 2009) or by adapting Link and Barker's (2009) model averaging approach.</li>
                    </ul>
										<p><br></p>
                    <a href="#top" class="totop">Back to top</a>
                    <p></p>
                  </td>
                </tr>
              </tbody>
            </table>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
            <!-- 4.3- Apply BaSTA:-->
            <a name="use"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="Application" border="0" width="1045px" src="use.jpg?"></td>
							</tbody>
						</table>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag"><b>BaSTA</b> is considerably easy to apply. The BaSTA vignette, which includes a step by step tutorial, can be accessed <a href="BaSTAoverview.pdf" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">here</a>. This vignette provides information on how to setup the dataset in an appropriate format for BaSTA and steps on how to perform a range of analyses. <br><br>Here we provide a very general overview on the different analyses that can be performed with BaSTA and the types of outputs that the user should expect to find. As we mentioned above, BaSTA allows users to test a range of models and functional forms for the mortality function (Fig. 4). <br></p>
										<p style="text-align:center"><br><img style="margin:0px;padding:0px;border:none;width:700px" align="middle" src="mortFuns.jpg?width=700px"></p>
                    <p class="caption" style="text-align:center">Fig. 4. Functional forms that can be tested with <b>BaSTA</b>. The general types include: a) Exponential (the name is based on the survival probability); b) Gompertz; c) Weibull; and d) logistic. Additional shapes include Makeham (green dashed lines) and bathtub (red continous lines)</p><br>
                    <p class="parag">In addition, it is possible to test the effect of categorical and continuous covariates. In Fig. 5 we show examples on which effects can be tested with a combination of a categorical and a continuous covariates. <br></p>
										<p style="text-align:center"><br><img style="margin:0px;padding:0px;border:none;width:700px" align="middle" src="covarsStruct.jpg?width=700px"></p>
                    <p class="caption" style="text-align:center">Fig. 5. Covariate effects that can be tested with <b>BaSTA</b>. Given two sets of covariates, one categorical (e.g. females vs males) and one continuous (e.g. weight) the effects that can be tested include: a) fused (i.e. categorical covariates affecting mortality parameters and continous covariates as proportional hazards); b) proportional hazards (both types of covariates as proportional hazards); and c) all in mortality (both covariates affecting mortality parameters; currently only implemented with Gompertz models)</p><br>
                    <p class="parag">After setting up the dataset, which we will call <code  style="color:#006600">myDataset</code>, and defining the years of start and end of the study, say, 1995 and 2005 respectively, a basic BaSTA analysis with a Gompertz mortality model (default) can be performed by typing into the R gui the following command: <br></p>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out <- basta(object = myDataset, studyStart = 1995, studyEnd = 2005)</code>
<br></p>
                    <p class="parag">In this case, BaSTA runs a single simulation for 50,000 iterations. To visualize the result, the user only needs to type:</p>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;summary(out)</code>
<br></p>
										<p class="parag">Which prints to the screen the relevant information such as the call of the model, the coefficients with standard errors and lower and upper bounds, if the model converged and if so, the value of model fit (DIC, which can be used for model selection). To plot the resulting traces for the parameters, just type on the R console:<br></p>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;plot(out)</code>
<br></p>
										<p class="parag">And to visualize the resulting mortality and survival probability functions, the function <code style="color:#006600">plot()</code> can be modified as:<br></p>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;plot(out, plot.trace = FALSE)</code>
<br></p>
										<p class="parag">As we mentioned above, convergence can only be estimated by running more than one simulation. When running multiple simulations, we strongly recomend to use the routine that updates jump sd's. This will make the run longer, but will greatly increase the chances of getting convergence from the first try. To run 4 simulations of the same model as shown above with the jumps update routine on, the user only needs to type:<br></p>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out <- basta(object = myDataset, studyStart = 1995, studyEnd = 2005, </code><br>
										<p><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>nsim = 4, parallel = TRUE, ncpus = 4, updateJumps = TRUE</b>)</code>
<br></p>
										<p class="parag">The argument <code style="color:#006600">parallel</code> makes use of package <code style="color:#006600">snowfall</code> (Knaus 2010), which allows BaSTA to run all 4 simulations in parallel, reducing computing time by a quarter of the time it would take to run each simulation one after the other. <br></p>
										<p class="parag">Additional arguments such as <code style="color:#006600">model</code> or <code style="color:#006600">shape</code> can be used to test different functional forms for the mortality functions (Fig. 4). For instance, to run a logistic model with bathtub shape for 4 simulations in parallel, the code is: <br></p>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out <- basta(object = myDataset, studyStart = 1995, studyEnd = 2005, <b>model = "LO"</b>,</code><br>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>shape = "bathtub"</b>, nsim = 4, parallel = TRUE, ncpus = 4, </code>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;updateJumps = TRUE)</code>
<br></p>
										<p class="parag">If covariates were included in the dataset, the default is to run them as "fused" (see Fig. 5). To test different covariate effects, the user only needs to change argument <code style="color:#006600">covarsStruct</code>. For example, to test the same model as above under a proportional hazards covariate structure the code should be modified as folows:<br></p>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out <- basta(object = myDataset, studyStart = 1995, studyEnd = 2005, model = "LO",</code><br>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape = "bathtub", <b>covarsStruct = "prop.haz"</b>, nsim = 4, </code>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;parallel = TRUE, ncpus = 4, updateJumps = TRUE)</code>
<br></p>
										<p class="parag">We recomend users to test different models, shapes and covariate structures and use the measures of model fit (i.e. DIC) for model comparison. In case the model does not converge, make sure that there are not too many missing records for younger individuals. This is commonly an issue with studies on birds, for which many juvenile individuals do not return to the breeding grounds when they reach maturity. To fix this without loosing information, BaSTA provides argument <code style="color:#006600">minAge</code> that can be used to specify this minimum age. For example, the code for the model above with a minimum age of 1 is:<br></p>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out <- basta(object = myDataset, studyStart = 1995, studyEnd = 2005, model = "LO",</code><br>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;shape = "bathtub", <b>minAge = 1</b>, covarsStruct = "prop.haz", nsim = 4, </code>
										<p><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;parallel = TRUE, ncpus = 4, updateJumps = TRUE)</code>
<br></p>
										<p class="parag">In case things are not working or you have any doubts or problems, please contact us by registering to the <a href="mailto:Basta-users@lists.r-forge.r-project.org"  rel="nofollow" style="color:#84002E">BaSTA Users mailing</a>.<br></p>
										<p><br></p>
                    <a href="#top" class="totop">Back to top</a>
                    <p></p>
									</td>
                </tr>
              </tbody>
            </table>
            <!-- 4.4- Bug Fixes:-->
            <a name="bugs"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="Application" border="0" width="1045px" src="bugs.jpg?"></td>
							</tbody>
						</table>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag">the latest version, <b>BaSTA 1.4</b>, includes the following updates: <br></p>
										<ul>
											<li>Update jump routine for multiple simulations runs before the main analysis, this improves convergence and consistency between multiple simulations.</li>
											<li>Improved convergence by fixing several bugs in the MCMC routine.</li>
											<li>Fixed reporting of DIC values with function <code style="color:#006600">summary()</code>.</li>
											<li>Function <code style="color:#006600">summary()</code> ouputs the basic information, which can be stored instead of the main output. This reduces storage problems due to the size of the output.</li>
										</ul>
										<p><br></p>
                    <a href="#top" class="totop">Back to top</a>
                    <p></p>
									</td>
                </tr>
              </tbody>
            </table>
            <!-- 4.4- References:-->
            <a name="refs"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF;padding:0px;outline-width:0px">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="References" border="0" width="1045px" src="references.jpg?"></td>
							</tbody>
						</table>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <tr style="background:#FFFFFF">
                  <td style="vertical-align:middle;padding:40px" width="420px">
                    <p class="Refs">Clark, J.S. (2007) <b>Models for ecological data</b>. Princeton University Press, Princeton, New Jersey, USA.<br><br></p>
                    <p class="Refs">Colchero, F. and J.S. Clark (2012) <b>Bayesian inference on age-specific survival for censored and truncated data.</b> <i>Journal of Animal Ecology,</i> 81, 139-149 (<a href="http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2011.01898.x/abstract" rel="nofollow" style="color:#84002E" target="_blank">publication</a>).<br><br></p>
                    <p class="Refs">Colchero, F., O.R. Jones and M. Rebke (2012) <b>BaSTA: an R package for Bayesian estimation of age-specific survival from incomplete mark-recapture/recovery data with covariates.</b> <i>Methods in Ecology and Evolution</i>. DOI: 10.1111/j.2041-210X.2012.00186.x (<a href="http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2012.00186.x/abstract" rel="nofollow" style="color:#84002E" target="_blank">publication</a>).<br><br></p>
										<p class="Refs">Cox, D. R., and Oakes D. (1984) <b>Analysis of Survival Data</b>. Chapman and Hall, London.</b><br><br></p>
                    <p class="Refs">Gimenez, O., Bonner, S., King, R., Parker, R.A., Brooks, S.P., Jamieson, L.E., Grosbois, V., Morgan, B.J.T., Thomas, L. (2009) <b>WinBUGS for population ecologists: Bayesian modeling using Markov Chain Monte Carlo methods</b>. <u>In</u><i> Modeling Demographic Processes in Marked Populations. Ecological and Environmental Statistics Series, vol 3</i> (eds D.L. Thomson, E.G. Cooch &amp; M.J. Conroy), pp. 883-915. Springer, Berlin, Germany.<br><br></p>
                    
                    <a href="#top" class="totop">Back to top</a>
                  </td>
                  <td style="vertical-align:top;padding:40px" width="420px">
										<p class="Refs">Gompertz, B. (1825) <b>On the nature of the function expressive of the law of human mortality, and on a new mode of determining the value of life contingencies</b>. <i>Philosophical Transactions of the Royal Society of London</i>, 115, 513-583.<br></p>
                    <p class="Refs">King, R. and Brooks, S.P. (2002) <b>Bayesian model discrimination for multiple strata capture-recapture data</b>. <i>Biometrika</i>, 89, 785-806.<br><br></p>
                    <p class="Refs">Knaus, J. (2010). <b>snowfall: Easier cluster computing (based on snow)</b>. R package version 1.84. <a href="http://CRAN.R-project.org/package=snowfall" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/package=snowfall</a><br><br></p>
                    <p class="Refs">Pletcher, S. (1999) <b>Model fitting and hypothesis testing for age-specific mortality data</b>. <i>Journal of Evolutionary Biology</i>, 12, 430-439.<br><br></p>
                    <p class="Refs">R Development Core Team (2011). <b>R: A language and environment for statistical computing</b>. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL <a href="http://CRAN.R-project.org/" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/</a>.<br><br></p>
                    <p class="Refs">Siler, W. (1979) <b>A competing-risk model for animal mortality</b>. <i>Ecology</i>, 60, 750-757.<br></p>
                  </td>
                </tr>
              </tbody>
            </table>
          </td>
        </tr>
      </tbody>
    </table>
  </body>
</html>
