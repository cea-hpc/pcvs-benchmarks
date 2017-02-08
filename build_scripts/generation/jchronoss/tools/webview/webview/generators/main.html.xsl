<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:template match="global">
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
						<xsl:apply-templates select='total'/>
						<xsl:apply-templates select='details'/>
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

	<xsl:template match="total">
		<div class='container-fluid text-center'>
			<h2>Validation Overview <button type="button" class="btn btn-primary btn-sm pull-right" data-toggle="collapse" data-target="#detail-summary">Details</button></h2>
			<div class='progress'>
				<div class='progress-bar progress-bar-danger' role="progressbar" aria-valuemin="0" aria-valuemax="100">
					<xsl:variable name="errors_ratio" select="round (100 * (errors + failures) div tests)" />
					<xsl:attribute name='aria-valuenow'><xsl:value-of select='$errors_ratio'/></xsl:attribute>
					<xsl:attribute name='style'>width: <xsl:value-of select='$errors_ratio'/>%;</xsl:attribute>
					<xsl:value-of select="$errors_ratio" />%
				</div>
				<div class='progress-bar progress-bar-warning' role="progressbar" aria-valuemin="0" aria-valuemax="100">
					<xsl:variable name="skipped_ratio" select="round (100 * (skipped) div tests)" />
					<xsl:attribute name='aria-valuenow'><xsl:value-of select='$skipped_ratio'/></xsl:attribute>
					<xsl:attribute name='style'>width: <xsl:value-of select='$skipped_ratio'/>%;</xsl:attribute>
					<xsl:value-of select="$skipped_ratio" />%
				</div>
				<div class='progress-bar progress-bar-success' role="progressbar" aria-valuemin="0" aria-valuemax="100">
					<xsl:variable name="successes_ratio" select="round (100 * (tests - skipped - errors - failures) div tests)" />
					<xsl:attribute name='aria-valuenow'><xsl:value-of select='$successes_ratio'/></xsl:attribute>
					<xsl:attribute name='style'>width: <xsl:value-of select='$successes_ratio'/>%;</xsl:attribute>
					<xsl:value-of select="$successes_ratio" />%
				</div>
			</div>

			<div id="detail-summary" class="collapse">
				<ul class="list-group">
					<li class="list-group-item">
						<span class="badge"><xsl:value-of select="tests" /></span>
						Total number of tests
					</li>
					<li class="list-group-item">
						<span class="badge"><xsl:value-of select="success" /></span>
						Total number of Successes
					</li>
					<li class="list-group-item">
						<span class="badge"><xsl:value-of select="failures" /></span>
						Total number of Failures
					</li>
					<li class="list-group-item">
						<span class="badge"><xsl:value-of select="skipped" /></span>
						Number of Skipped tests due to dependency non-resolution
					</li>
					<li class="list-group-item">
						<span class="badge"><xsl:value-of select="errors" /></span>
						Number of aborted tests for any other reason
					</li>
				</ul>
			</div>
		</div>
	</xsl:template>

	<xsl:template match="details">
		<div class='container-fluid text-center'>
			<table class='table table-hover table-striped sortable table-condensed'>
				<thead>
					<tr>
						<th>Status Bar</th>
						<th>Package</th>
						<th>Errors</th>
						<th>Failed</th>
						<th>Skipped</th>
						<th>Success</th>
						<th>Total</th>
					</tr>
				</thead>
				<tbody>
					<xsl:apply-templates select="testdir"/>
				</tbody>
			</table>
		</div>
	</xsl:template>

	<xsl:template match="testdir">
		<tr>
			<xsl:attribute name='class'>clickable-row</xsl:attribute>
			<xsl:attribute name='data-href'><xsl:value-of select='htmlfile'/></xsl:attribute>
			<td>
				<div class='progress' style="vertical-align:middle;  margin-bottom: 0px" >
					<div class='progress-bar progress-bar-danger' role="progressbar" aria-valuemin="0" aria-valuemax="100">
						<xsl:variable name="errors_ratio" select="round (100 * (errors + failures) div tests)" />
						<xsl:attribute name='aria-valuenow'><xsl:value-of select='$errors_ratio'/></xsl:attribute>
						<xsl:attribute name='style'>width: <xsl:value-of select='$errors_ratio'/>%;</xsl:attribute>
						<xsl:value-of select="$errors_ratio" />%
					</div>
					<div class='progress-bar progress-bar-warning' role="progressbar" aria-valuemin="0" aria-valuemax="100">
						<xsl:variable name="skipped_ratio" select="round (100 * (skipped) div tests)" />
						<xsl:attribute name='aria-valuenow'><xsl:value-of select='$skipped_ratio'/></xsl:attribute>
						<xsl:attribute name='style'>width: <xsl:value-of select='$skipped_ratio'/>%;</xsl:attribute>
						<xsl:value-of select="$skipped_ratio" />%
					</div>
					<div class='progress-bar progress-bar-success' role="progressbar" aria-valuemin="0" aria-valuemax="100">
						<xsl:variable name="successes_ratio" select="round (100 * (tests - skipped - errors - failures) div tests)" />
						<xsl:attribute name='aria-valuenow'><xsl:value-of select='$successes_ratio'/></xsl:attribute>
						<xsl:attribute name='style'>width: <xsl:value-of select='$successes_ratio'/>%;</xsl:attribute>
						<xsl:value-of select="$successes_ratio" />%
					</div>
				</div>
			</td>
			<td style='vertical-align:middle;'>
				<strong><xsl:value-of select='name'/></strong>
			</td>
			<td style='vertical-align:middle;'><xsl:value-of select='errors'/></td>
			<td style='vertical-align:middle;'><xsl:value-of select='failures'/></td>
			<td style='vertical-align:middle;'><xsl:value-of select='skipped'/></td>
			<td style='vertical-align:middle;'><xsl:value-of select='success'/></td>
			<td style='vertical-align:middle;'><xsl:value-of select='tests'/></td>
		</tr>
	</xsl:template>

</xsl:stylesheet>
