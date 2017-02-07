<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="global">
	<html>
	<head>
		<title>Web visualizer from JUnit Results</title>
		<link rel="stylesheet" type="text/css" href="../styles/global-layout.css" />
		<link rel="stylesheet" type="text/css" href="../styles/summary.css" />
		<link rel="stylesheet" type="text/css" href="../styles/details-list.css" />
		<link rel="stylesheet" type="text/css" href="../styles/menu.css" />
		<!--<script language='javascript' src='.js'></script>-->
		<script src="../scripts/sorttable.js" type="text/javascript"></script>
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
					<h1 class='title'>JUnit Results</h1>
					<xsl:apply-templates select='total'/>
					<xsl:apply-templates select='details'/>
				</div>
				<div id='footer'></div>
			</div>
		</body>
	</html>
</xsl:template>

<xsl:template match="total">
	<div class='section'>
		<h2>Summary</h2>
		<table class='summary'>
			<tr>
				<th>Status</th>
				<th>Nombre</th>
				<th colspan='2'>Rapport</th>
			</tr>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">success</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='success'/></xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">failed</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='failures'/></xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">error</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='errors'/></xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">skipped</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='skipped'/></xsl:with-param>
			</xsl:call-template>
			<tr class='total'>
				<td>
					<xsl:attribute name='class'><xsl:value-of select='status'/></xsl:attribute>
					total
				</td>
				<td>
					<xsl:attribute name='class'><xsl:value-of select='status'/></xsl:attribute>
					<xsl:value-of select='tests'/>
				</td>
				<td>
					<xsl:attribute name='class'><xsl:value-of select='status'/></xsl:attribute>
					<font>
						<xsl:attribute name='class'><xsl:value-of select='status'/></xsl:attribute>
						<xsl:value-of select='status'/>
					</font>
				</td>
				<td>
					<xsl:attribute name='class'><xsl:value-of select='status'/></xsl:attribute>
					100 %
				</td>
			</tr>
		</table>
	</div>
</xsl:template>

<xsl:template name="summary-line">
	<xsl:param name="status"/>
	<xsl:param name="value"/>
	<tr>
		<xsl:attribute name='class'><xsl:value-of select='$status'/>-line</xsl:attribute>
		<td><xsl:value-of select="$status"/></td>
		<td><xsl:value-of select='$value'/></td>
		<td class='rapport_progress'>
			<div class='progress'>
			<div class='bar'>
				<xsl:attribute name='style'>width: <xsl:value-of select='round(100 * $value div tests)'/>%;</xsl:attribute>
			</div>
			</div>
		</td>
		<td>
			<xsl:value-of select='round(100 * $value div tests)'/> %
		</td>
	</tr>
</xsl:template>

<xsl:template match="details">
	<div class='section'>
		<h2>Test groups</h2>
		<table class='details-list sortable'>
			<tr>
				<th>Directory</th>
				<th>Errors</th>
				<th>Failures</th>
				<th>Skipped</th>
				<th>Success</th>
				<th>Total</th>
				<th>% failed</th>
				<th>Status</th>
			</tr>
			<xsl:apply-templates select="testdir"/>
		</table>
	</div>
</xsl:template>

<xsl:template match="testdir">
	<tr>
		<xsl:attribute name='class'><xsl:value-of select='status'/></xsl:attribute>
		<td class='name'>
			<a>
				<xsl:attribute name='href'><xsl:value-of select='htmlfile'/></xsl:attribute>
				<xsl:value-of select='name'/>
			</a>
		</td>
		<td class='errors'><xsl:value-of select='errors'/></td>
		<td class='failures'><xsl:value-of select='failures'/></td>
		<td class='skipped'><xsl:value-of select='skipped'/></td>
		<td class='success'><xsl:value-of select='success'/></td>
		<td class='tests'><xsl:value-of select='tests'/></td>
		<td><xsl:value-of select='round(100 * (errors+failures) div tests)' /></td>
		<td class='status'><xsl:value-of select='status'/></td>
	</tr>
</xsl:template>

</xsl:stylesheet>
