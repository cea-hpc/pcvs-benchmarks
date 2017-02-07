<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" indent="yes"/>
	<xsl:template match="comparison">
		<diffTestSuite>
		<xsl:apply-templates select="checked/testsuite/testcase"/>
		</diffTestSuite>
	</xsl:template>

	<xsl:template match="checked/testsuite/testcase">
		<diffTestCase>
			<xsl:attribute name="classname"><xsl:value-of select="@classname"/></xsl:attribute>
			<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<current><xsl:copy-of select="."/></current>
			<reference>
				<xsl:call-template name="loopReference">
					<xsl:with-param name="current_name"><xsl:value-of select="@name"/></xsl:with-param>
					<xsl:with-param name="current_classname"><xsl:value-of select="@classname"/></xsl:with-param>
				</xsl:call-template>
			</reference>
		</diffTestCase>
	</xsl:template>
	
	<xsl:template name="loopReference">
		<xsl:param name="current_name"/>
		<xsl:param name="current_classname"/>
		<xsl:for-each select="/comparison/reference/testsuite/testcase">
			<xsl:if test="$current_name = @name and $current_classname = @classname">
				<xsl:copy-of select="."/>
			</xsl:if> 
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
