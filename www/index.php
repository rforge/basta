
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
										<!-- 3.- LOCAL LINKS:-->
										<table style="table-layout:fixed;background:#FFFFFF">
											<tbody>
												<tr>
													<td align="center" valign="top" width="360"></td>
														<p style="font-size: 200%;text-align: center">The BaSTA website has been moved <a href="http://www1.imada.sdu.dk/~colchero/basta/" rel="nofollow" style="color:#84002E" target="_blank">here</a></p>
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
