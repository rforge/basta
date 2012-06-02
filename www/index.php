
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
    <table style="vertical-align:middle;background:#E5F5E0" width="100%" height="100%">
      <tbody>
        <tr align="center">
          <td>
            <table style="vertical-align:middle;background:#1F1209" width="1055px">
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
            <table width="1050px" style="table-layout:fixed;vertical-align:middle" cellspacing="5px">
              <tbody>
                <tr>
                  <td class="button"><a href="#summary" class="button">SUMMARY</a></td>
                  <td class="button"><a href="#description" class="button">PACKAGE</a></td>
                  <td class="button"><a href="#model" class="button">MODEL</a></td>
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
                    <p style="font-size:16px;line-height:1.25"><b>Authors:</b><br><br>Fernando Colchero <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Colchero@demogr.mpg.de</a><br>Owen R. Jones <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Jones@demogr.mpg.de</a><br>Maren Rebke <a href="mailto:Colchero@demogr.mpg.de" rel="nofollow" style="color:#84002E">Rebke@demogr.mpg.de</a><br></p>
                    <p style="font-size:16px;line-height:1.25;text-align:justify"><br><b>Developed at:</b><br><br><a href="http://www.demogr.mpg.de" rel="nofollow" style="color:#84002E;font-family:verdana" target="_blank">Max Planck Institute for Demographic Research</a><br>(<i>Max Planck Independent Research Group Modeling the Evolution of Aging and Laboratory of Evolutionary Biodemography</i>).</p>
                  </td>
                  <td width="50px"></td>
                </tr>
              </tbody>
            </table>
            <!-- 4.- SECTIONS:-->
            <!-- 4.1- Summary:-->
            <a name="summary"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#1F1209">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="Summary" border="0" width="1045px" src="summary.jpg?"></td>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag"><b>BaSTA</b> is an R package (R Development Core Team 2011) that allows users to draw inference on age-specific survival and mortality patterns from capture-recapture/recovery data when a large number of individuals (or all) have missing age information (Colchero, Jones and Rebke 2012). <b>BaSTA</b> is based on a model developed by Colchero and Clark (2012), which extends inference from parameter estimates to the estimation of unknown (i.e. latent) times of birth and death. The package also allows testing the effect of categorical and continuous individual covariates on mortality and survival (for an example see Fig. 1). Below we feature a video hosted by the journal Methods in Ecology and Evolution, that explains the context in which BaSTA can be useful and the way the package can be implemented. Version 1.1 of the package is now availale on CRAN and can be installed as source by typing the following line of code into the R console: <br></p>
										<p><code>install.packages("BaSTA")</code>
<br></p>
										<p class="parag">Also, the latest version of <b>BaSTA</b> (vers. 1.2) can be installed from R forge by typing:<br></p>
										<p><code>install.packages("BaSTA", repos="http://R-Forge.R-project.org")</code>
<br></p>

                    <a href="#top" class="totop">Back to top</a>
                  </td>
                  <td class="main">
                    <p><img style="margin:0px;padding:0px;border:none" width="400px" align="middle" src="sheep.png?width=400px"><br><br></p>
                    <p class="caption">Fig. 1. <b>BaSTA</b> model output for sex differences in Soay sheep mortality using a Siler survival model (Colchero & Clark 2012). The left panel shows posterior distributions for the survival and recapture parameters while the right panel shows the resulting survival probabilities and the mortality rates for males and females.</p>
										<p class="parag">We have set up a <a href="mailto:Basta-users@lists.r-forge.r-project.org"  rel="nofollow" style="color:#84002E">BaSTA Users mailing</a> list so users can ask questions or provide comments, suggestions or criticism that can help us improve the package. Users can register by <a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/basta-users"  rel="nofollow" class="intext">clicking here</a>.<br></p>
                  </td>
                </tr>
              </tbody>
            </table>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#FFFFFF">
              <tbody>
                <tr style="background:#FFFFFF">
									<td>
										<iframe width="640" height="360" src="http://www.youtube.com/embed/OLFzY3vHVOQ" frameborder="0" allowfullscreen style="display:block;margin:5px auto;text-align:center;"></iframe>
									</td>
                </tr>
              </tbody>
            </table>
            <!-- 4.2- Package:-->
            <a name="description"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#1F1209">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="Package" border="0" width="1045px" src="package.jpg?"></td>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag"><b>BaSTA</b> requires conventional data input (equivalent to other common capture-recapture softwares) and minimum user input (Fig. 2). The data consist of a single table, where each row corresponds to an individual history. This table consist of a first column with individual id's, the next two columns for the times of birth and death, respectively, followed by the traditional capture history matrix. This capture history includes one column for each year of the study and assigns 1 when individuals are detected and 0 otherwise. The last columns are optional, and should include covariates such as sex, location id, birth weight, etc.<br><br>The current version includes:</p>
                    <ul>
                      <li>Functions for data formatting.</li>
                      <li>Data checking and correction of common data errors.</li>
                      <li>Estimates of age-specific survival parameters.</li>
                      <li>Estimates of yearly recapture probabilities.</li>
                      <li>Estimates latent (i.e. unknown) times of birth and death.</li>
                      <li>Allows testing four different mortality functions (Exponential, Gompertz, Weibull and logistic) and to extend the model to Makeham or bathtub shapes (Gompertz 1925, Siler 1979, Cox and Oakes 1984, Pletcher 1999).</li>
                      <li>Evaluates the effects of time-independent categorical and continuous covariates on survival.</li>
                    </ul>
                    <p><br></p>
                    <a href="#top" class="totop">Back to top</a>
                  </td>
                  <td style="vertical-align:top;padding:40px" width="420px">
                    <img style="margin:0px;padding:0px;border:none;width:420px" align="middle" src="BastaGeneralChart.jpg?width=420px">
                    <p class="caption">Fig. 2. <b>BaSTA</b> general package chart.</p><br>
                    <ul>
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
                      <li>Model selection through Reversible Jump MCMC (RJMCMC; King and Brooks 2002; Gimenez <i>et al.</i> 2009).</li>
                    </ul>
                  </td>
                </tr>
              </tbody>
            </table>
            <!-- 4.3- Model:-->
            <a name="model"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#1F1209">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="Model" border="0" width="1045px" src="model.jpg?"></td>
                <tr style="background:#FFFFFF">
                  <td class="main">
                    <p class="parag">The core of the model is a Monte Carlo Markov Chain (MCMC) algorithm that combines Metropolis sampling for survival parameters and latent states (i.e. unknown times of birth and death) and direct sampling for recapture probabilities (Clark 2007, Colchero and Clark <i>in press</i>). The main function performs several diagnostics on the user's inputs such as checking that the data is consistent with the model's requirements, verifies that the number of iterations, the burnin sequence and the thinning gap are consistent and finally verifies that the initial parameters and jumps provided by the user allow the model to run properly (Fig. 3). After running these diagnostics, the function can run multiple MCMC simulations. In case convergence is not acheived or some or all simulations failed, neither convergence nor model selection diagnostics are calculated.<br></p>
                    <a href="#top" class="totop">Back to top</a>
                  </td>
                  <td style="vertical-align:top;padding:40px" width="420px">
                    <img width="420px" align="middle" border="0" src="bastaChart.jpg?width=420px">
                    <p class="caption">Fig. 3. <b>BaSTA</b> model chart.</p>
                  </td>
                </tr>
              </tbody>
            </table>
            <!-- 4.4- References:-->
            <a name="refs"></a>
            <table  valign="middle" width="1050px" style="table-layout:fixed;background:#1F1209">
              <tbody>
                <td style="padding:0px;outline-width:0px"><img alt="References" border="0" width="1045px" src="references.jpg?"></td>
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
            <p><br><br></p>
          </td>
        </tr>
      </tbody>
    </table>
  </body>
</html>
