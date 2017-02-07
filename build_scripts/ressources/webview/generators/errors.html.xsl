<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="testsuites">
	<html>
	<head>
		<title>All errors</title>
		<link rel="stylesheet" type="text/css" href="../styles/global-layout.css" />
		<link rel="stylesheet" type="text/css" href="../styles/summary.css" />
		<link rel="stylesheet" type="text/css" href="../styles/test-results.css" />
		<link rel="stylesheet" type="text/css" href="../styles/menu.css" />
		<script language='javascript' src='../scripts/select.js'></script>
		</head>
		<body>
			<div id='page'>
				<div id='header'></div>
				<div id='menu-container'>
					<ul id='menu'>
						<li><a href='main.html'>Summary</a></li>
						<li><a href='errors.html'>Errors</a></li>
					</ul>
				</div>
				<div id='body'>
					<h1 class='title'>All errors</h1>
					<xsl:apply-templates select='testsuite'/>
				</div>
				<div id='footer'></div>
			</div>
		</body>
	</html>
</xsl:template>

<xsl:template match="testsuite">
	<div class='section'>
		<h2><xsl:value-of select="@name"/></h2>
		<table class='test-results'>
			<tr>
				<th></th>
				<th>Name</th>
				<th>Status</th>
				<th>Time (s)</th>
			</tr>
			<xsl:apply-templates select="testcase"/>
		</table>
	</div>
</xsl:template>

<xsl:template match="testcase">
	<tr>
		<xsl:attribute name='class'><xsl:call-template name='compute-testcase-status-name'/></xsl:attribute>
		<td class='icon'>
			<img>
				<xsl:attribute name='src'>../images/status-<xsl:call-template name='compute-testcase-status-name'/>.png</xsl:attribute>
			</img>
		</td>
		<td class='name'>
			<a>
				<xsl:attribute name='href'><xsl:value-of select='@htmlfile'/></xsl:attribute>
				<xsl:value-of select='@name'/>
			</a>
			</td>
		<td class='status'><xsl:call-template name='compute-testcase-status-name'/></td>
		<td class='time'><xsl:value-of select='@time'/></td>
	</tr>
</xsl:template>

<xsl:template name="compute-testcase-status-name">
	<xsl:value-of select='@status'/>
</xsl:template>

</xsl:stylesheet>
