<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:template match="testsuites">
		<html>
			<head>
				<title>JCHRONOSS Result Review Platform</title>
				<meta charset="utf-8" />
				<meta name="viewport" content="width=device-width, initial-scale=1" />
				<link rel="stylesheet" href="../styles/dep/bootstrap.min.css" />
				<link rel="stylesheet" href="../styles/custom.css" />
				<script src="../scripts/dep/sorttable.js" type="text/javascript"></script>
				<script src="../scripts/dep/jquery.min.js" type="text/javascript"></script>
				<script src="../scripts/dep/bootstrap.min.js" type="text/javascript"></script>
				<script src="../scripts/custom.js" type="text/javascript"></script>
			</head>
			<body style='padding-top:50px;' class="col-lg-offset-1 col-lg-10">
				<div >
					<div class='navbar navbar-inverse navbar-fixed-top'>
						<div class="navbar-header">
							<a class="navbar-brand" target="_blank" href="http://jchronoss.hpcframework.com/">JCHRONOSS</a>
						</div>
						<ul class='nav navbar-nav'>
							<li class="active" ><a href='main.html'>Home</a></li>
							<li><a href='errors.html'>Errors</a></li>
							<li><a href='diff-main.html'>Compare</a></li>
							<li><a href='realtime.html'>Real-Time</a></li>
						</ul>
					</div>
					<div class="jumbotron text-center">
						<h1>JCHRONOSS Result Viewer</h1>
						<p>
							Displays your results in a fancy way when no other reviewing platform are available.
						</p> 
						<img class="img-responsive center-block" src="../images/logo.png" alt="JCHRONOSS Logo" style="width:35%" /> 
					</div>

					<div id='body'>
						<h1 class="text-center alert alert-danger">Total number of failures : <xsl:value-of select="count(//testsuite/testcase)" /></h1>
						<div class='container-fluid text-center'>
							<div class="input-group"> <span class="input-group-addon">Filter</span>

								<input id="filter" type="text" class="form-control" placeholder="Type here..." />
								</div>
							<table class='table table-hover table-striped sortable table-condensed'>
								<thead>
									<tr>
										<th>Name</th>
										<th>Time (s)</th>
									</tr>
								</thead>
								<tbody class="searchable">
									<xsl:apply-templates select="testsuite/testcase"/>
								</tbody>
							</table>
						</div>
					</div>
					<a id="scroll-top" href="#" data-toggle="tooltip" data-placement="right" class="btn btn-primary btn-lg scroll-top" role="button">Back To Top</a>
					<div class="alert alert-info">
						<strong>Tip: </strong>You can browse more detailed view per package by clicking wherever you want within the row and the complete table is sortable (asc,dec) by clicking on any table header.
					</div>
					<div class="container-fluid text-center" style="background-color:lightgrey;"><strong>JCHRONOSS</strong> Result Viewer, so called <em>"The WebView"</em><br/>Distributed under the CeCILL-C license (LGPL-Compatible), available <a href="http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.html">here.</a></div>
				</div>
			</body>
		</html>
	</xsl:template>
	<xsl:template match="testcase">
		<tr>
			<xsl:attribute name='class'>clickable-row danger</xsl:attribute>
			<xsl:attribute name='data-href'><xsl:value-of select='@htmlfile'/>#<xsl:value-of select="@name"/></xsl:attribute>
			<td class='name'>
				<strong><xsl:value-of select='@name'/></strong>
			</td>
			<td class='time'><xsl:value-of select='@time'/></td>
		</tr>
	</xsl:template>
</xsl:stylesheet>
