<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="testsuite">
	<html>
	<head>
		<title><xsl:value-of select='@name'/></title>
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
						<li><a>
							<xsl:attribute name="href">diff-<xsl:value-of select='translate(@name, "\.", "\-")'/>.html</xsl:attribute>
							Diff</a></li>
					</ul>
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
								<th>Time (s)</th>
							</tr>
							<xsl:apply-templates select="testcase"/>
						</table>
					</div>
				</div>
				<div id='footer'></div>
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
				<xsl:with-param name="status">success</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='count(//testcase) - count(//failure) - count(//error) - count(//skipped)'/></xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">failed</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='count(//failure)'/></xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">error</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='count(//error)'/></xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="summary-line">
				<xsl:with-param name="status">skipped</xsl:with-param>
				<xsl:with-param name="value"><xsl:value-of select='count(//skipped)'/></xsl:with-param>
			</xsl:call-template>
			<tr class='total'>
				<td>
					<a alt='show' href="javascript:showOnly('*');"><img src='../images/icon-view.png' alt='show'/></a>
				</td>
				<td>
					<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/>-line</xsl:attribute>
					total
				</td>
				<td>
					<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/>-line</xsl:attribute>
					<xsl:value-of select='count(//testcase)'/>
				</td>
				<td>
					<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/>-line</xsl:attribute>
					<font>
						<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/>-line</xsl:attribute>
						<xsl:call-template name='compute-global-status-name'/>
					</font>
				</td>
				<td>
					<xsl:attribute name='class'><xsl:call-template name='compute-global-status-name'/>-line</xsl:attribute>
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
	<xsl:param name="value"/>
	<tr>
		<xsl:attribute name='class'><xsl:value-of select='$status'/>-line</xsl:attribute>
		<td>
			<a alt='show'>
				<xsl:attribute name='href'>javascript:showOnly('<xsl:value-of select='$status'/>');</xsl:attribute>
				<img src='../images/icon-view.png' alt='show'/>
			</a>
		</td>
		<td><xsl:value-of select="$status"/></td>
		<td><xsl:value-of select='$value'/></td>
		<td class='rapport_progress'>
			<div class='progress'>
			<div class='bar'>
				<xsl:attribute name='style'>width: <xsl:value-of select='round(100 * $value div count(//testcase))'/>%;</xsl:attribute>
			</div>
			</div>
		</td>
		<td>
			<xsl:value-of select='round(100 * $value div count(//testcase))'/> %
		</td>
	</tr>
</xsl:template>

<xsl:template match="testcase">
	<tr>
		<xsl:attribute name='class'><xsl:call-template name='compute-testcase-status-name'/></xsl:attribute>
		<td class='icon'>
			<img>
				<xsl:attribute name='src'>../images/status-<xsl:call-template name='compute-testcase-status-name'/>.png</xsl:attribute>
				<xsl:call-template name='toogle-visibility'/>
			</img>
		</td>
		<td class='name'><xsl:value-of select='@name'/></td>
		<td class='status'><xsl:call-template name='compute-testcase-status-name'/></td>
		<td class='time'><xsl:value-of select='@time'/></td>
	</tr>
	<xsl:apply-templates select='failure|skipped|success'/>
</xsl:template>

<xsl:template name="compute-testcase-status-name">
	<xsl:choose>
		<xsl:when test="error">error</xsl:when>
		<xsl:when test="failure">failed</xsl:when>
		<xsl:when test="skipped">skipped</xsl:when>
		<xsl:otherwise>success</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template name="toogle-visibility">
		<xsl:attribute name='onclick'>toggle_visibility("<xsl:value-of select="@name"/>")</xsl:attribute>
</xsl:template>

<xsl:template match="failure">
	<tr class='failed-details'>
		<xsl:attribute name='id'><xsl:value-of select='../@name'/></xsl:attribute>
		<td class='failed-details' colspan='4'>
			<h3>Message : </h3>
			<pre><xsl:value-of select='@message'/></pre>
			<h3>Failure : </h3>
			<pre><xsl:value-of select='../system-out'/></pre>
		</td>
	</tr>
</xsl:template>
<xsl:template match="success">
	<tr class='success-details' style="display: none;">
		<xsl:attribute name='id'><xsl:value-of select='../@name'/></xsl:attribute>
		<td class='success-details' colspan='4'>
			<h3>Message : </h3>
			<pre><xsl:value-of select='@message'/></pre>
			<xsl:if test="../system-out">
				<h3>Log : </h3>
				<pre><xsl:value-of select='../system-out'/></pre>
			</xsl:if>
		</td>
	</tr>
</xsl:template>
<xsl:template match="skipped">
	<tr class='skipped-details' style="display: none;">
		<xsl:attribute name='id'><xsl:value-of select='../@name'/></xsl:attribute>
		<td class='skipped-details' colspan='4'>
			<h3>Message : </h3>
			<pre><xsl:value-of select='@message'/></pre>
			<h3>Log : </h3>
			<pre><xsl:value-of select='../system-out'/></pre>
		</td>
	</tr>
</xsl:template>
</xsl:stylesheet>
