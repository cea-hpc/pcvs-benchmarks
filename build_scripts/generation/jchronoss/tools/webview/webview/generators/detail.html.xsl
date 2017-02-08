<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:template match="testsuite">
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

					<div id=''>
						<div class='container-fluid text-center'>
							<h2><strong>Package "<xsl:value-of select="@name" />"</strong> <button type="button" class="btn btn-primary btn-sm pull-right" data-toggle="collapse" data-target="#detail-summary">Details</button></h2>
							<div class='progress'>
								<div class='progress-bar progress-bar-danger' role="progressbar" aria-valuemin="0" aria-valuemax="100">
									<xsl:variable name="errors_ratio" select="round (100 * (count(//error) + count(//failure) ) div count(//testcase))" />
									<xsl:attribute name='aria-valuenow'><xsl:value-of select='$errors_ratio'/></xsl:attribute>
									<xsl:attribute name='style'>width: <xsl:value-of select='$errors_ratio'/>%;</xsl:attribute>
									<xsl:value-of select="$errors_ratio" />%
								</div>
								<div class='progress-bar progress-bar-warning' role="progressbar" aria-valuemin="0" aria-valuemax="100">
									<xsl:variable name="skipped_ratio" select="round (100 * (count(//skipped) ) div count(//testcase))" />
									<xsl:attribute name='aria-valuenow'><xsl:value-of select='$skipped_ratio'/></xsl:attribute>
									<xsl:attribute name='style'>width: <xsl:value-of select='$skipped_ratio'/>%;</xsl:attribute>
									<xsl:value-of select="$skipped_ratio" />%
								</div>
								<div class='progress-bar progress-bar-success' role="progressbar" aria-valuemin="0" aria-valuemax="100">
									<xsl:variable name="successes_ratio" select="round (100 * (count(//testcase) - count(//skipped) - count(//error) - count(//failure)) div count(//testcase))" />
									<xsl:attribute name='aria-valuenow'><xsl:value-of select='$successes_ratio'/></xsl:attribute>
									<xsl:attribute name='style'>width: <xsl:value-of select='$successes_ratio'/>%;</xsl:attribute>
									<xsl:value-of select="$successes_ratio" />%
								</div>
							</div>

							<div id="detail-summary" class="collapse">
								<ul class="list-group">
									<li class="list-group-item">
										<span class="badge"><xsl:value-of select='count(//testcase)'/></span>
										Total number of tests
									</li>
									<li class="list-group-item">
										<span class="badge"><xsl:value-of select='count(//testcase) - count(//failure) - count(//error) - count(//skipped)'/></span>
										Total number of Successes
									</li>
									<li class="list-group-item">
										<span class="badge"><xsl:value-of select='count(//failure)'/></span>
										Total number of Failures
									</li>
									<li class="list-group-item">
										<span class="badge"><xsl:value-of select='count(//skipped)'/></span>
										Number of Skipped tests due to dependency non-resolution
									</li>
									<li class="list-group-item">
										<span class="badge"><xsl:value-of select='count(//error)'/></span>
										Number of aborted tests for any other reason
									</li>
								</ul>
							</div>
						</div>
						<div class='container-fluid text-center'>
							<!--<div class="input-group"> <span class="input-group-addon">Filter</span>-->

								<!--<input id="filter" type="text" class="form-control" placeholder="Type here..." />-->
								<!--</div>-->
							<ul class="nav nav-pills nav-justified" style="padding-bottom:20px">
								<li class="active"><a><strong>Group By :</strong></a></li>
								<li><a href="#" onclick="showOnly('*')"><strong>All</strong></a></li>
								<li><a href="#" onclick="showOnly('succeed')"><strong>Success</strong></a></li>
								<li><a href="#" onclick="showOnly('skipped')"><strong>Skipped</strong></a></li>
								<li><a href="#" onclick="showOnly('failed')"><strong>Failures</strong></a></li>
								<li><a href="#" onclick="showOnly('error')"><strong>Errors</strong></a></li>
							</ul>
							<table class='table table-hover table-striped table-condensed'>
								<thead>
									<tr>
										<!--<th></th>-->
										<th>Test Name</th>
										<th class="col-xs-1">Status</th>
										<th class="col-xs-1">Time</th>
									</tr>
								</thead>
								<tbody class="searchable">
									<xsl:apply-templates select="testcase"/>
								</tbody>
							</table>
						</div>
					</div>
					<a id="scroll-top" href="#" data-toggle="tooltip" data-placement="right" class="btn btn-primary btn-lg scroll-top" role="button">Back To Top</a>
					<div class="container-fluid text-center" style="background-color:lightgrey;"><strong>JCHRONOSS</strong> Result Viewer, so called <em>"The WebView"</em><br/>Distributed under the CeCILL-C license (LGPL-Compatible), available <a href="http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.html">here.</a></div>
				</div>
			</body>
		</html>
	</xsl:template>

	<xsl:template name="compute-testcase-status-name">
		<xsl:choose>
			<xsl:when test="error">Error</xsl:when>
			<xsl:when test="failure">Failure</xsl:when>
			<xsl:when test="skipped">Skipped</xsl:when>
			<xsl:otherwise>Success</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="testcase">
		<tr>
			<xsl:call-template name="toogle-visibility"/>
			<xsl:attribute name="class">
				<xsl:choose>
					<xsl:when test="error">error danger</xsl:when>
					<xsl:when test="failure">failed danger</xsl:when>
					<xsl:when test="skipped">skipped warning</xsl:when>
					<xsl:otherwise>succeed success</xsl:otherwise>
				</xsl:choose>
			</xsl:attribute>
			<xsl:attribute name="id"><xsl:value-of select='@name'/></xsl:attribute>
			<td class='name'><strong><xsl:value-of select='@name'/></strong></td>
			<td class='status'><em><xsl:call-template name='compute-testcase-status-name'/></em></td>
			<td class='time'><xsl:value-of select='@time'/></td>
		</tr>
		<xsl:apply-templates select='error|failure|skipped|success'/>
	</xsl:template>

	<xsl:template name="toogle-visibility">
		<xsl:attribute name='onclick'>toggle_visibility("<xsl:value-of select="@name"/>-details")</xsl:attribute>
	</xsl:template>

	<xsl:template match="error">
		<tr class='error-details danger' style='display: none;'>
			<xsl:attribute name='id'><xsl:value-of select='../@name'/>-details</xsl:attribute>
			<td class='error-details' colspan='4'>
				<h4>Command : </h4>
				<pre><xsl:value-of select='@message'/></pre>
				<h4>Error : </h4>
				<pre><xsl:value-of select='../system-out'/></pre>
			</td>
		</tr>
	</xsl:template>
	<xsl:template match="failure">
		<tr class='failed-details danger'>
			<xsl:attribute name='id'><xsl:value-of select='../@name'/>-details</xsl:attribute>
			<td class='failed-details' colspan='4'>
				<h4>Command : </h4>
				<pre><xsl:value-of select='@message'/></pre>
				<h4>Failure : </h4>
				<pre><xsl:value-of select='../system-out'/></pre>
			</td>
		</tr>
	</xsl:template>
	<xsl:template match="success">
		<tr class='succeed-details success' style='display: none;'>
			<xsl:attribute name='id'><xsl:value-of select='../@name'/>-details</xsl:attribute>
			<td class='succeed-details' colspan='4'>
				<h4>Command : </h4>
				<pre><xsl:value-of select='@message'/></pre>
				<xsl:if test="../system-out">
					<h4>Log : </h4>
					<pre><xsl:value-of select='../system-out'/></pre>
				</xsl:if>
			</td>
		</tr>
	</xsl:template>
	<xsl:template match="skipped">
		<tr class='skipped-details warning' style="display: none;">
			<xsl:attribute name='id'><xsl:value-of select='../@name'/></xsl:attribute>
			<td class='skipped-details' colspan='4'>
				<h4>Command : </h4>
				<pre><xsl:value-of select='@message'/></pre>
				<h4>Log : </h4>
				<pre><xsl:value-of select='../system-out'/></pre>
			</td>
		</tr>
	</xsl:template>
</xsl:stylesheet>
