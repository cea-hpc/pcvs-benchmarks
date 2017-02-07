<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="diffTestSuite">
	<diffTestSuite name="{@name}">
    	<xsl:apply-templates select="diffTestCase"/>
    </diffTestSuite>
</xsl:template>

<xsl:template match="diffTestCase">
        <xsl:copy>
		<xsl:choose>
        		<xsl:when test='count(current/testcase/failure) &gt; 0  and count(reference/testcase/failure) &gt; 0'>
                		<xsl:attribute name='status'>unchanged_failure</xsl:attribute> 
        		</xsl:when>
   		        <xsl:when test='count(current/testcase/failure) &gt; 0  and count(reference/testcase/failure) &lt; 1'>
   		        	<xsl:attribute name='status'>regress_fail</xsl:attribute> 
   		        </xsl:when>
   		        <xsl:when test='count(current/testcase/failure) &lt; 1  and count(reference/testcase/failure) &gt; 0'>
   		        	<xsl:attribute name='status'>fixed</xsl:attribute> 
   		        </xsl:when>
			<xsl:when test='(current/testcase/@time div reference/testcase/@time) &gt; 1.10'>
   		        	<xsl:attribute name='status'>regress_perf</xsl:attribute> 
   		        </xsl:when>
   		        <xsl:when test='count(current/testcase/failure) &lt; 1  and count(reference/testcase/failure) &lt; 1'>
   		        	<xsl:attribute name='status'>unchanged_success</xsl:attribute> 
   		        </xsl:when>
		</xsl:choose>
		<xsl:apply-templates select="@*|node()"/>
	</xsl:copy>	
</xsl:template>

<xsl:template match="@*|node()">
	<xsl:copy>
		<xsl:apply-templates select="@*|node()"/>
	</xsl:copy>
</xsl:template>

</xsl:stylesheet>
