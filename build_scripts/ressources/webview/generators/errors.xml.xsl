<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:param name='htmlfile'>Unknown</xsl:param>

<xsl:template match="testsuites">
	<xsl:apply-templates select="testsuite"/>
</xsl:template>

<xsl:template match="testsuite">
	<xsl:if test='testcase/failure or testcase/error'>
		<testsuite>
			<xsl:attribute name='name'><xsl:value-of select='@name'/></xsl:attribute>
			<xsl:apply-templates select="testcase"/>
		</testsuite>
	</xsl:if>
</xsl:template>

<xsl:template match="testcase">
	<xsl:if test='failure or error'>
		<testcase>
			<xsl:attribute name='name'><xsl:value-of select='@name'/></xsl:attribute>
			<xsl:attribute name='status'>failed</xsl:attribute>
			<xsl:attribute name='time'><xsl:value-of select='@time'/></xsl:attribute>
			<xsl:attribute name='htmlfile'><xsl:value-of select='$htmlfile'/></xsl:attribute>
		</testcase>
	</xsl:if>
</xsl:template>

</xsl:stylesheet>
