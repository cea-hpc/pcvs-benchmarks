<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output mode="xml" indent="yes"/>
<xsl:param name='current_dir'>Unknown</xsl:param>

<xsl:template match="/">
	<html>
	<head>
		<title><xsl:value-of select='$current_dir'/></title>
		<link rel="stylesheet" type="text/css" href="../styles/global-layout.css" />
		<link rel="stylesheet" type="text/css" href="../styles/summary.css" />
		<link rel="stylesheet" type="text/css" href="../styles/test-results.css" />
		<link rel="stylesheet" type="text/css" href="../styles/menu.css" />
		<script language='javascript' src='../scripts/select.js'></script>
		</head>
		<body onload="generate_diff_summary()">
			<div id='page'>
				<div id='header'></div>
				<div id='menu-container'>
					<ul id='menu'>
						<li><a href='main.html'>Summary</a></li>
						<li><a href='errors.html'>Errors</a></li>
						<li><a>
							<xsl:attribute name="href"><xsl:value-of select='translate($current_dir, "\/", "\-")'/>-output.html</xsl:attribute>
						Details</a></li>
					</ul>
				</div>
				<div id='body'>
					<h1 class='title'><xsl:value-of select='$current_dir'/></h1>
					<p> we are sorry but we haven't found a reference output file for this tests group. </p>
				</div>
				<div id='footer'></div>
			</div>
		</body>
	</html>
</xsl:template>
</xsl:stylesheet>
