#!/bin/sh

#display root tag
echo "<?xml version='1.0'?>"

#find xml results
file_list=$(find $SOURCES_LIST -iname 'output*.xml')

#open detail balis
echo "<testsuites>"

#loop on files
for file in ${file_list}
do
	#calc htmlfilename
	htmlfile="$(egrep -o "<testsuite name=\"[^\"]+\"" $file | sed -e "s@<testsuite name\=@@g" -e "s@\"@@g" -e "s@\.@-@g" -e "s@/@-@g").html"
	#htmlfile=$(echo $file | sed -e 's#^\./##g' -e 's#/#-#g' -e 's/.xml/.html/g')
	xsltproc --stringparam htmlfile ${htmlfile} ${WEBVIEW_PATH}/webview/generators/errors.xml.xsl $file | sed -e '/<?xml version="1.0"?>/d'
done

#close detail balis
echo "</testsuites>"
