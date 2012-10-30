
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
            <table style="vertical-align:middle;background:#FFFFFF" width="900px">
              <tbody>
								<tr align="center">
									<td>
										<!-- 1.- HEADER:-->
										<table style="table-layout:fixed">
											<tbody>
												<tr>
													<td style="padding:0px;outline-width:0px">
														<img alt="BaSTA logo" border="0" width="900px" src="bastalogo.jpg?">
													</td>
												</tr>
											</tbody>
										</table>
										<table width="100%" style="table-layout:fixed;vertical-align:middle;background:#FFFFFF">
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
										<table style="table-layout:fixed;background:#FFFFFF">
											<tbody>
												<tr>
													<td align="center" valign="top" width="360">
														<p></p>
														<p align="left" style="padding-left:20px;padding-bottom:60px"><img alt="BaSTA photos" border="0" width="320px" align="left" style="vertical-align:top;outline-width:0px" src="BastaPhotos.jpg"><br><br><br><br><br><br><br><br><br><br><br><br></p>
															<p align="left" valign="bottom" style="padding-left:20px"><a href="http://r-forge.r-project.org/"><img height="30px" src="Rforgelogo.png" border="0" alt="R-Forge Logo"></a></p>
													</td>
													<td width="500" valign="top" style="padding-right:40px">
														<p align="left" style="font-size:16px;line-height:1.25"><b>Authors:</b><br><a href="http://www.colchero.com" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Fernando Colchero</a><br><a href="http://www.owenjon.es" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Owen R. Jones</a><br><a href="http://www.demogr.mpg.de/en/institute/staff_directory_1899/maren_rebke_1430.htm" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Maren Rebke</a><br></p>
														<p align="left" style="font-size:16px;line-height:1.25;text-align:justify"><br><b>Developed at:</b><br><a href="http://www.demogr.mpg.de" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Max Planck Institute for Demographic Research</a><br></p>
														<p align="left" style="font-size:16px;line-height:1.25;text-align:justify"><br><b>Mailing list:</b><br>For enquiries, comments or bug reports,<br>please register to the <a href="http://lists.r-forge.r-project.org/mailman/listinfo/basta-users"  rel="nofollow" style="color:#84002E">BaSTA Users mailing list</a></p>
														<p align="left" style="font-size:16px;line-height:1.25;text-align:justify"><br><b>Cite as:</b><br>Colchero, F., Jones, O.R and Rebke, M. (2012) BaSTA: an R package for Bayesian estimation of age-specific survival from incomplete mark-recapture/recovery data with covariates. <i>Methods in Ecology and Evolution</i>. 3: 466-470.</p>
														<p></p>
													</td>
												</tr>
											</tbody>
										</table>
										<table style="table-layout:fixed;background:#FFFFFF">
											<tbody>
												<tr>
													<td>
													<a name="summary"></a>
													<img alt="Summary" border="0" width="900px" src="summary.jpg?">
													<p class="parag"><b>BaSTA</b> is an R package (R Development Core Team 2012) that allows users to draw inference on age-specific survival and mortality patterns from capture-recapture/recovery data when a large number of individuals (or all) have missing age information (Colchero, Jones and Rebke 2012). <b>BaSTA</b> is based on a model developed by Colchero and Clark (2012), which extends inference from parameter estimates to the estimation of unknown (i.e. latent) times of birth and death. The package also allows testing the effect of categorical and continuous individual covariates on mortality and survival (for an example see Fig. 1).</p>
													<p style="text-align:center"><br><img style="margin:0px;padding:0px;border:none" width="400px" align="middle" src="Sheep.png"><br></p>
													<p class="caption">Fig. 1. <b>BaSTA</b> model output for sex differences in Soay sheep mortality using a Siler survival model (Colchero & Clark 2012). The left panel shows posterior distributions for the survival and recapture parameters while the right panel shows the resulting survival probabilities and the mortality rates for males and females.</p>
													<p class="parag">Version 1.5 of the package is now availale on CRAN and can be installed by typing the following line of code into the R console: <br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;install.packages("BaSTA")</code><br></p>
													<!--<p class="parag">However, we strongly recomend users to install the latest stable version, <b>BaSTA</b> (vers. 1.5), which should be available on CRAN in the next few days. This last version can be currently installed either from from R forge by typing:<br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;install.packages("BaSTA", repos="http://R-Forge.R-project.org",</code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;type = "source")</code><br></p> -->
													<p class="parag">A normal installation of BaSTA 1.5 from CRAN is now possible. <b>This last version has important updates and improvements, so we stronlgy recomend users to install it</b>.<br></p>
													<p class="parag">BaSTA 1.6 can be downloaded by clicking <a href="BaSTA_1.6.tar.gz" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">here</a>. Save the file in a folder of your choice, say ''C:/Documents/BaSTAtemp/'' for windows users, and then type on the R console:<br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;install.packages("C:/Documents/BaSTAtemp/BaSTA_1.6.tar.gz"</code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;type = "source")</code><br></p>
													<p class="parag">We have set up a <a href="http://lists.r-forge.r-project.org/mailman/listinfo/basta-users"  rel="nofollow" style="color:#84002E">BaSTA Users mailing list</a> so users can ask questions or provide comments, suggestions or criticism that can help us improve the package. Users can register by <a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/basta-users"  rel="nofollow" class="intext">clicking here</a>. Also, you can see below a video hosted by the journal <a href="http://www.methodsinecologyandevolution.org/" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Methods in Ecology and Evolution</a> where we explain the rational behind the package and some general applications.<br></p>
													<p><br></p>
													<p style="text-align:center"><iframe width="480" height="270" src="http://www.youtube.com/embed/OLFzY3vHVOQ" frameborder="0" allowfullscreen style="display:block;margin:5px auto"></iframe></p>
													<p class="caption" style="text-align:center"><b>BaSTA</b> video hosted by the journal Methods in Ecology and Evolution.</p>
													<p></p>
													<a href="#top" class="totop">Back to top</a>
													<p></p>
													<a name="description"></a>
													<img alt="Package" border="0" width="900px" src="package.jpg?">
													<p class="parag">The core of the model is a Monte Carlo Markov Chain (MCMC) algorithm that combines Metropolis sampling for survival parameters and latent states (i.e. unknown times of birth and death) and direct sampling for recapture probabilities (Clark 2007, Colchero and Clark 2012). The main function performs several diagnostics on the user's inputs such as checking that the data is consistent with the model's requirements and verifies that the number of iterations, the burnin sequence and the thinning gap are consistent (Fig. 2).</p>
													<p style="text-align:center"><br><img style="margin:0px;padding:0px;border:none;width:475px" align="middle" src="BastaGeneralChart.jpg?"><br></p>
													<p class="caption" style="text-align:center">Fig. 2. <b>BaSTA</b> general package chart.<br></p>
													<p class="parag">After running these diagnostics, <b>BaSTA</b> uses a dynamic procedure to find adequate jump standard deviations for mortality parameters. This procedure runs before the core of the analysis is performed, keeping the user from having to find these jump sd's by trial and error. After apropriate jumps are found, multiple MCMC simulations can be ran either in parallel or in series. In case convergence is not acheived or some or all simulations failed, neither convergence nor model selection diagnostics are calculated.<br><br><b>The current version includes:</b></p>
													<ul>
														<li>Functions for data formatting.</li>
														<li>Data checking and correction of common data errors.</li>
														<li>Estimates of age-specific survival parameters.</li>
														<li>Estimates of yearly recapture probabilities.</li>
														<li>Estimates of latent (i.e. unknown) times of birth and death.</li>
														<li>Allows testing four different mortality functions (Exponential, Gompertz, Weibull and logistic) and to extend the model to Makeham or bathtub shapes (Gompertz 1925, Siler 1979, Cox and Oakes 1984, Pletcher 1999).</li>
														<li>Evaluates the effects of time-independent categorical and continuous covariates on survival.</li>
														<li>Runs multiple simulations either in parallel (using package snowfall; Knaus 2010) or in series.</li>
														<li>Calculates basic diagnostics on MCMC performance such as parameter update rates and serial autocorrelation.</li>
														<li>Uses multiple simulations to estimate convergence (i.e. potential scale reduction factor; Gelman <i>et al.</i> 2004).</li>
														<li>Calculates basic measures for model fit (DIC; Spiegelhalter <i>et al.</i> 2002).</li>
														<li>Finds jump standard deviations automatically through adaptive independent Metropolis (Roberts and Rosenthal 2009).</li>
													</ul>
													<p class="parag"><b>Future versions will include:</b></p>
													<ul>
														<li>Time-dependent covariates.</li>
														<li>Cohort effects on mortality</li>
														<li>Covariates on recapture and recovery probabilities.</li>
														<li>Model selection either through Reversible Jump MCMC (RJMCMC; King and Brooks 2002; Gimenez <i>et al.</i> 2009) or by adapting Barker and Link's (2010) Bayes factors approach.</li>
													</ul>
													<p></p>
													<a href="#top" class="totop">Back to top</a>
													<p></p>
													<a name="use"></a>
													<img alt="Application" border="0" width="900px" src="use.jpg?">
													<p class="parag"><b>BaSTA</b> is considerably easy to apply. The BaSTA vignette, which includes a step by step tutorial, can be accessed <a href="BaSTAoverview.pdf" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">here</a>. This vignette provides information on how to setup the dataset in an appropriate format for BaSTA and steps to run a range of analyses. <br><br>Here we provide a very general overview on the different analyses that can be performed with BaSTA and the types of outputs that the user should expect to find. As we mentioned above, BaSTA allows users to test a range of models and functional forms for the mortality function (Fig. 3). <br></p>
													<p style="text-align:center"><br><img style="margin:0px;padding:0px;border:none;width:600px" align="middle" src="mortFuns.jpg?"></p>
													<p class="caption">Fig. 3. Functional forms that can be tested with <b>BaSTA</b>. The general types include: a) Exponential (the name is based on the survival probability); b) Gompertz; c) Weibull; and d) logistic. Additional shapes include Makeham (green dashed lines) and bathtub (red continous lines)</p>
													<p class="parag">In addition, it is possible to test the effect of categorical and continuous covariates. In Fig. 4 we show examples on which effects can be tested with a combination of a categorical and a continuous covariates. <br></p>
													<p style="text-align:center"><br><img style="margin:0px;padding:0px;border:none;width:500px" align="middle" src="covarsStruct.jpg?"></p>
													<p class="caption">Fig. 4. Covariate effects that can be tested with <b>BaSTA</b>. Given two sets of covariates, one categorical (e.g. females vs males) and one continuous (e.g. weight) the effects that can be tested include: a) fused (i.e. categorical covariates affecting mortality parameters and continous covariates as proportional hazards); b) proportional hazards (both types of covariates as proportional hazards); and c) all in mortality (both covariates affecting mortality parameters; currently only implemented with Gompertz models)</p>
													<p class="parag">After setting up the dataset, which we will call <code style="color:#006600;font-size:18px">myDataset</code>, and defining the years of start and end of the study, say, 1995 and 2005 respectively, a basic BaSTA analysis with a Gompertz mortality model (default) can be performed by typing into the R gui the following command: <br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out &lt;- basta(object = myDataset, studyStart = 1995, studyEnd = 2005)</code><br></p>
													<p class="parag">In this case, BaSTA runs a single simulation for 11,000 iterations with a burn-in of 1001 steps. As we mentioned above, convergence can only be estimated by running more than one simulation. When running multiple simulations, we strongly recomend to use the routine that updates jump sd's. This will make the analysis sligthly longer, but will greatly increase the chances of getting convergence from the first try. To run 4 simulations of the same model as shown above with the jumps update routine on, the user only needs to type:<br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out &lt;- basta(object = myDataset, studyStart = 1995, studyEnd = 2005, </code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code  style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>nsim = 4, parallel = TRUE, ncpus = 4, updateJumps = TRUE</b>)</code><br></p>
													<p class="parag">Argument <code style="color:#006600;font-size:18px">parallel</code> makes use of package <code style="color:#006600;font-size:18px">snowfall</code> (Knaus 2010), which allows BaSTA to run all 4 simulations in parallel, reducing computing time by a quarter of the time it would take to run each simulation one after the other.<br></p>
													<p class="parag">Additional arguments such as <code style="color:#006600;font-size:18px">model</code> or <code style="color:#006600;font-size:18px">shape</code> can be used to test different functional forms for the mortality functions (Fig. 3). For instance, to run a logistic model with bathtub shape for 4 simulations in parallel, the code is: <br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out &lt;- basta(object = myDataset, studyStart = 1995, studyEnd = 2005,</code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>model = "LO", shape = "bathtub"</b>, nsim = 4, parallel = TRUE, </code></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ncpus = 4, updateJumps = TRUE)</code><br></p>
													<p class="parag">If covariates were included in the dataset, the default is to run them as "fused" (see Fig. 4). To test different covariate effects, the user only needs to change argument <code style="color:#006600;font-size:18px">covarsStruct</code>. For example, to test the same model as above under a proportional hazards covariate structure the code should be modified as folows:<br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out &lt;- basta(object = myDataset, studyStart = 1995, studyEnd = 2005, </code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;model = "LO", shape = "bathtub", <b>covarsStruct = "prop.haz"</b>, </code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nsim = 4, parallel = TRUE, ncpus = 4, updateJumps = TRUE)</code><br></p>
													<p class="parag">We recomend users to test different models, shapes and covariate structures and use the measures of model fit (i.e. DIC) for model comparison. In case the model does not converge, make sure that there are not too many missing records for younger individuals. This is commonly an issue with studies on birds, for which many juvenile individuals do not return to the breeding grounds when they reach maturity. To fix this without loosing information, BaSTA provides argument <code style="color:#006600;font-size:18px">minAge</code> that can be used to specify this minimum age. For example, the code for the model above with a minimum age of 1 is:<br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;out &lt;- basta(object = myDataset, studyStart = 1995, studyEnd = 2005, </code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;model = "LO", shape = "bathtub", <b>minAge = 1</b>, </code></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;covarsStruct = "prop.haz", nsim = 4, parallel = TRUE,</code><br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ncpus = 4, updateJumps = TRUE)</code><br></p>
													<p class="parag">To visualize the result, the user only needs to type either <code  style="color:#006600;font-size:18px">out</code> or, for additional information, function <code style="color:#006600;font-size:18px">summary(out)</code>, which prints to the screen the relevant information such as the call of the model, the coefficients with standard errors and lower and upper bounds, if the model converged and if so, the value of model fit (DIC, which can be used for model selection; Fig. 5).<br></p>
													<p style="text-align:center"><img align="center" style="margin:0px;padding:0px;border:none;width:700px" align="middle" src="printOutput.png?"></p>
													<p class="caption">Fig. 5. BaSTA output printed by typing <code style="color:#006600">out</code> into the R console. Additional information can be obtained by typing <code style="color:#006600;font-size:18px">summary(out)</code>.</p>
													<p class="parag">To visually assess convergence, the resulting traces for the parameters can be plotted by typing on the R console <code style="color:#006600;font-size:18px">plot(out)</code> (Fig. 6), which, as an example, produces the following figure for a Weibull model with bathtub shape and two covariates (males-M; females-F):<br></p>
													<p style="text-align:center"><img align="center" style="margin:0px;padding:0px;border:none;width:450px" align="middle" src="kestrelTraces.jpg?"></p>
													<p class="caption">Fig. 6. Example of an output from a Weibull model with bathtub shape on a kestrel (<i>Falco tinnunculus</i>) dataset (Jones <i>et al.</i> 2008). The plot shows the traces for the mortality parameters for males (M) and females (F). The plot was produced with the R built-in function <code style="color:#006600;font-size:18px">plot()</code>.</p>
													<p class="parag">Also, the resulting mortality and survival probability trajectories (Fig. 7) can be plotted by typing <code style="color:#006600;font-size:18px">plot(out, plot.trace = FALSE)</code>, producing the following plot for the same example:</p>
													<p style="text-align:center"><img align="center" style="margin:0px;padding:0px;border:none;width:400px" align="middle" src="kestrelTrajsCI.jpg?"></p>
													<p class="caption">Fig. 7. Survival probability and mortality trajectories for male (M) and female (F) kestrels (<i>Falco tinnunculus</i>).</p>
													<p class="parag">In case the survival and mortality plots become too crowded, which can happen if several categorical covariates are evaluated, function <code style="color:#006600;font-size:18px">plot()</code> can be modified using arguments <code  style="color:#006600;font-size:18px">xlim</code>, which reduces the range of ages over which the plots are produced, and argument <code style="color:#006600;font-size:18px">noCI</code>, which eliminates credible intervals and only plots the mean expected trajectories (Fig. 8):<br></p>
													<p style="font-size:16px;padding-left:60px;padding-right:60px"><code style="color:#006600">&nbsp;&nbsp;&nbsp;&nbsp;plot(out, plot.trace = FALSE, xlim = c(0, 3), noCI = TRUE)</code><br></p>
													<p style="text-align:center"><img align="center" style="margin:0px;padding:0px;border:none;width:400px" align="middle" src="kestrelTrajsNoCI.jpg?"></p>
													<p class="caption">Fig. 8. Zoom in on the survival probability and mortality trajectories for male (M) and female (F) kestrels (<i>Falco tinnunculus</i>).</p>
													<p class="parag">In case models are not working properly or you have any doubts or problems, please contact us by registering to the <a href="http://lists.r-forge.r-project.org/mailman/listinfo/basta-users"  rel="nofollow" style="color:#84002E">BaSTA Users mailing list</a>.<br></p></p>

													<p></p>
													<a href="#top" class="totop">Back to top</a>
													<p></p>
													<a name="bugs"></a>
													<img alt="Upgrades" border="0" width="900px" src="bugs.jpg?">
													<p class="parag">The newest version, <b>BaSTA 1.6</b>, not yet available on CRAN, includes the following updates: <br></p>
													<ul>
														<li>Fixed a bug that prevented to calculate quantiles for predicted mortality and survival when only continuous covariates were included.</li>
														<li>Fixed a series of bugs when evaluating only continuous covariates inMort.</li>
														<li>Fixed a bug that prevented to plot all the traces from proportional hazard parameters in the same plotting window.</li>
														<li>Minor bug fixes.</li>
													</ul>
													<p></p>
													<hr>
													<p class="parag">The current version in CRAN, <b>BaSTA 1.5</b>, includes the following updates: <br></p>
													<ul>
														<li>Great improvement on the updateJumps algorithm and the estimation of mortality parameters. This upgrade greatly reduces the number of iterations needed for convergence and the time required to run the analysis.</li>
														<li>Fixed an issue when assigning times of birth and death that prevented the model to estimate deaths between ages 0 and 1.</li>
														<li>Improved the <code style="color:#006600">plot()</code> function to allow zooming to different ranges of ages when plotting survival and mortality and to plot these trajectories with or without credible intervals.</li>
														<li>Minor bug fixes.</li>
													</ul>
													<p class="parag">We strongly recomend all users to switch to BaSTA 1.5. You won't be disapointed!<br></p>
													<p></p>
													<hr>
													<p class="parag">The previous stable version, <b>BaSTA 1.4</b>, included the following updates: <br></p>
													<ul>
														<li>Update jump routine for multiple simulations runs before the main analysis, this improves convergence and consistency between multiple simulations.</li>
														<li>Improved convergence by fixing several bugs in the MCMC routine.</li>
														<li>Fixed reporting of DIC values with function <code style="color:#006600">summary()</code>.</li>
														<li>Function <code style="color:#006600">summary()</code> ouputs the basic information, which can be stored instead of the main output. This reduces storage problems due to the size of the output.</li>
													</ul>
													<p></p>
													<a href="#top" class="totop">Back to top</a>
													<p></p>
													<a name="refs"></a>
													<img alt="References" border="0" width="900px" src="references.jpg?">
														<p class="Refs">Clark, J.S. (2007) <b>Models for ecological data</b>. Princeton University Press, Princeton, New Jersey, USA.<br></p>
														<p class="Refs">Colchero, F. and J.S. Clark (2012) <b>Bayesian inference on age-specific survival for censored and truncated data.</b> <i>Journal of Animal Ecology,</i> 81, 139-149 (<a href="http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2011.01898.x/abstract" rel="nofollow" style="color:#84002E" target="_blank">publication</a>).<br></p>
														<p class="Refs">Colchero, F., O.R. Jones and M. Rebke (2012) <b>BaSTA: an R package for Bayesian estimation of age-specific survival from incomplete mark-recapture/recovery data with covariates.</b> <i>Methods in Ecology and Evolution</i>. 3: 466-470 (<a href="http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2012.00186.x/abstract" rel="nofollow" style="color:#84002E" target="_blank">publication</a>).<br></p>
														<p class="Refs">Cox, D. R., and Oakes D. (1984) <b>Analysis of Survival Data</b>. Chapman and Hall, London.<br></p>
														<p class="Refs">Gelman, A., Carlin, J.B., Stern, H.S. & Rubin, D.B. (2004). <b>Bayesian data analysis</b>. 2nd edn. Boca Raton: Chapman &
Hall/CRC.<br></p>
														<p class="Refs">Gimenez, O., Bonner, S., King, R., Parker, R.A., Brooks, S.P., Jamieson, L.E., Grosbois, V., Morgan, B.J.T., Thomas, L. (2009) <b>WinBUGS for population ecologists: Bayesian modeling using Markov Chain Monte Carlo methods</b>. <u>In</u><i> Modeling Demographic Processes in Marked Populations. Ecological and Environmental Statistics Series, vol 3</i> (eds D.L. Thomson, E.G. Cooch &amp; M.J. Conroy), pp. 883-915. Springer, Berlin, Germany.<br></p>
														<p class="Refs">Gompertz, B. (1825) <b>On the nature of the function expressive of the law of human mortality, and on a new mode of determining the value of life contingencies</b>. <i>Philosophical Transactions of the Royal Society of London</i>, 115, 513-583.<br></p>
														<p class="Refs">Jones, O.R., Coulson, T., Clutton-Brock T. & Godfray, H.C.J. (2008) <b>A web resource for the UK's long-term individual-based time-series (LITS) data</b>. <i>Journal of Animal Ecology</i> 77: 612-615<br></p>
														<p class="Refs">King, R. and Brooks, S.P. (2002) <b>Bayesian model discrimination for multiple strata capture-recapture data</b>. <i>Biometrika</i>, 89, 785-806.<br></p>
														<p class="Refs">Knaus, J. (2010). <b>snowfall: Easier cluster computing (based on snow)</b>. R package version 1.84. <a href="http://CRAN.R-project.org/package=snowfall" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/package=snowfall</a><br></p>
														<p class="Refs">Barker, R.J. and Link, W.A (2010) <b>Posterior model probabilities computed from model-specific Gibbs output</b>. <i>arXiv preprint</i> arXiv:1012.0073.<br></p>
														<p class="Refs">Pletcher, S. (1999) <b>Model fitting and hypothesis testing for age-specific mortality data</b>. <i>Journal of Evolutionary Biology</i>, 12, 430-439.<br></p>
														<p class="Refs">R Development Core Team (2012). <b>R: A language and environment for statistical computing</b>. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL <a href="http://CRAN.R-project.org/" rel="nofollow" style="color:#84002E" target="_blank">http://CRAN.R-project.org/</a>.<br></p>
														<p class="Refs">Roberts, G.O. and Rosenthal, J.S. (2009) <b>Examples of adaptive MCMC</b>. <i>Journal of Computational and Graphical Statistics</i>, 18, 349-367.<br></p>
														<p class="Refs">Siler, W. (1979) <b>A competing-risk model for animal mortality</b>. <i>Ecology</i>, 60, 750-757.<br></p>
														<p class="Refs">Spiegelhalter, D.J., Best, N.G., Carlin, B.P. & van der Linde, A. (2002) <b>Bayesian measures of model complexity and fit</b>. <i>Journal of the Royal Statistical Society: Series B</i>, 64, 583-639.
														<p><br></p>
														<a href="#top" class="totop">Back to top</a>
														<p></p>
													</td>
												</tr>
											</tbody>
										</table>
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
