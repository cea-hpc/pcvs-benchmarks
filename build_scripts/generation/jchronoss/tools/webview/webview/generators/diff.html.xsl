<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="diffTestSuite">
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
					<a class="navbar-brand" href="#">JCHRONOSS</a>
				</div>
				<ul class='nav navbar-nav'>
					<li class="active" ><a href='main.html'>Home</a></li>
					<li><a href='errors.html'>Errors</a></li>
					<li><a href='#'>Compare</a></li>
					<li><a href='#'>Real-Time</a></li>
					<li><a href='#'>About</a></li>
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
				<h1 class='title'><xsl:value-of select='@name'/></h1>
				<xsl:call-template name='summary'/>
				<div class='section'>
					<h2><xsl:value-of select="@name"/> <span id="title_results" style="font-size:70%; font-style=italic;"></span></h2>
					<table class='test-results'>
						<tr>
							<th></th>
							<th>Name</th>
							<th>Status</th>
							<th>Delta (s)</th>
							<th>Delta (%)</th>
						</tr>
						<xsl:apply-templates select="diffTestCase"/>
					</table>
				</div>
			</div>
			<div class="alert alert-info">
				<strong>Tip: </strong>You can browse more detailed view per package by clicking wherever you want within the row and the complete table is sortable (asc,dec) by clicking on any table header.
			</div>  
			<a id="scroll-top" href="#" data-toggle="tooltip" data-placement="right" class="btn btn-primary btn-lg scroll-top" role="button">Back To Top</a>
			<div class="container-fluid text-center" style="background-color:lightgrey;"><strong>JCHRONOSS</strong> Result Viewer, so called <em>"The WebView"</em><br/>Distributed under the CeCILL-C license (LGPL-Compatible), available <a href="http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.html">here.</a></div>
		</div>
	</body>
</html>
</xsl:template>

<xsl:template name="summary">
	<div class='section'>
		<h2>Summary</h2>
		<table class='summary'>
			<tr>
				<th></th>
				<th>Status</th>
				<th>Nombre</th>
				<th colspan='2'>Rapport</th>
			</tr>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">unchanged_success</xsl:with-param>
				<xsl:with-param name="description">Still Succeded</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">unchanged_failure</xsl:with-param>
				<xsl:with-param name="description">Still Failed</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">fixed</xsl:with-param>
				<xsl:with-param name="description">Fixed</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">regress_perf</xsl:with-param>
				<xsl:with-param name="description">Perf regression</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">regress_failure</xsl:with-param>
				<xsl:with-param name="description">Failure regression</xsl:with-param>
			</xsl:call-template>
			<tr class='total'>
				<td>
					<a alt='show' href="javascript:showOnly('*');"><img src='../images/icon-view.png' alt='show'/></a>
				</td>
				<td>
					<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/></xsl:attribute>
					total
				</td>
				<td id="nb_total">
					<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/></xsl:attribute>
				</td>
				<td>
				</td>
				<td>
					<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/></xsl:attribute>
					100 %
				</td>
			</tr>
		</table>
	</div>
</xsl:template>

<xsl:template name="compute-global-status-name">
	<xsl:choose>
		<xsl:when test="count(//error) != 0">error</xsl:when>
		<xsl:when test="count(//failure) != 0">failed</xsl:when>
		<xsl:otherwise>success</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template name="summary-line">
	<xsl:param name="status"/>
	<xsl:param name="description"/>
	<xsl:param name="value"/>
	<tr>
		<xsl:attribute name='class'><xsl:value-of select='$status'/>-line</xsl:attribute>
		<td>
			<a alt='show'>
				<xsl:attribute name='href'>javascript:showOnly('<xsl:value-of select='$status'/>');</xsl:attribute>
				<img src='../images/icon-view.png' alt='show'/>
			</a>
		</td>
		<td><xsl:value-of select="$description"/></td>
		<td><xsl:attribute name="id">nb_<xsl:value-of select="$status"/></xsl:attribute></td>
		<td class='rapport_progress'>
			<div class='progress'>
				<div class='bar'>
					<xsl:attribute name="id">bar_<xsl:value-of select='$status'/></xsl:attribute>
				</div>
			</div>
		</td>
		<td><xsl:attribute name="id">percent_<xsl:value-of select='$status'/></xsl:attribute> %</td>
	</tr>
</xsl:template>
<xsl:template match="diffTestCase">
	<tr>
		<xsl:attribute name='class'><xsl:call-template name='compute-testcase-status-name'/></xsl:attribute>
		<td class='icon'>
			<img>
				<xsl:attribute name="src">../images/status-<xsl:call-template name='compute-testcase-status-name'/>.png</xsl:attribute>
				<xsl:call-template name="toggle_visibility"/>
			</img>
		</td>
		<td class='name'><xsl:value-of select='@name'/></td>
		<td class='status'><xsl:call-template name='compute-testcase-status-display'/></td>
		<td class='time'><xsl:value-of select='format-number(current/testcase/@time - reference/testcase/@time, "#.##")'/></td>
		<td class='time'><xsl:value-of select='format-number((current/testcase/@time - reference/testcase/@time) div reference/testcase/@time, "#.## %")'/></td>
	</tr>
	<xsl:call-template name="chooseStatus"/>
</xsl:template>

<xsl:template name="chooseStatus">
	<xsl:choose>
		<xsl:when test="@status = 'unchanged_failure'"><xsl:call-template name="unchanged_failure"/></xsl:when>
		<xsl:when test="@status = 'regress_fail'"><xsl:call-template name="regress_fail"/></xsl:when>
	</xsl:choose>
</xsl:template>

<xsl:template name="compute-testcase-status-name">
	<xsl:choose>
		<xsl:when test="@status = 'unchanged_success'">unchanged_success</xsl:when>
		<xsl:when test="@status = 'unchanged_failure'">unchanged_failure</xsl:when>
		<xsl:when test="@status = 'regress_fail'">regress_failure</xsl:when>
		<xsl:when test="@status = 'regress_perf'">regress_perf</xsl:when>
		<xsl:otherwise>fixed</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template name="compute-testcase-status-display">
	<xsl:choose>
		<xsl:when test="@status = 'unchanged_success'">Still succeeded</xsl:when>
		<xsl:when test="@status = 'unchanged_failure'">Still failed</xsl:when>
		<xsl:when test="@status = 'regress_fail'">Failure regression</xsl:when>
		<xsl:when test="@status = 'regress_perf'">Perf regression</xsl:when>
		<xsl:otherwise>Fixed</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template name="toggle_visibility">
	<xsl:if test="1"><!--@status = unchanged_failure or @status = regress">-->
		<xsl:attribute name='onclick'>toggle_visibility("<xsl:value-of select="@name"/>")</xsl:attribute>
	</xsl:if>
</xsl:template>

<xsl:template name="unchanged_success">
	<tr class='regress_failure-details'>
		<xsl:attribute name='id'><xsl:value-of select='@name'/></xsl:attribute>
		<td class='unchanged_success-details' colspan='5'>
			<h3>Message : </h3>
			<pre><xsl:value-of select='current/testcase/success/@message'/></pre>
		</td>
	</tr>
</xsl:template>
<xsl:template name="regress_perf">
	<tr class='regress_perf-details'>
		<xsl:attribute name='id'><xsl:value-of select='@name'/></xsl:attribute>
		<td class='regress_perf-details' colspan='5'>
			<h3>Message : </h3>
			<pre><xsl:value-of select='current/testcase/success/@message'/></pre>
		</td>
	</tr>
</xsl:template>
<xsl:template name="unchanged_failure">
	<tr class='unchanged_failure-details'>
		<xsl:attribute name='id'><xsl:value-of select='@name'/></xsl:attribute>
		<td class='unchanged_failure-details' colspan='2'>
			<h3> Current Message : </h3>
			<pre><xsl:value-of select='current/testcase/failure/@message'/></pre>
			<h3>Log : </h3>
			<pre><xsl:value-of select='current/testcase/system-out'/></pre>
		</td>
		<td class='unchanged_failure-details' colspan='3'>
			<h3>Reference Message : </h3>
			<pre><xsl:value-of select='reference/testcase/failure/@message'/></pre>
			<h3>Log : </h3>
			<pre><xsl:value-of select='reference/testcase/system-out'/></pre>
		</td>
	</tr>
</xsl:template>
<xsl:template name="regress_fail">
	<tr class='regress_failure-details'>
		<xsl:attribute name='id'><xsl:value-of select='@name'/></xsl:attribute>
		<td class='regress_failure-details' colspan='5'>
			<h3>Message : </h3>
			<pre><xsl:value-of select='current/testcase/failure/@message'/></pre>
			<h3>Failure : </h3>
			<pre><xsl:value-of select='current/testcase/system-out'/></pre>
		</td>
	</tr>
</xsl:template>

</xsl:stylesheet>
