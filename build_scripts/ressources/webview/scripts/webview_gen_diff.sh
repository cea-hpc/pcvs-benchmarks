#!/bin/sh

TEMP_OUTPUT="temp.xml"
checked="$(tail -n $(( `wc -l $1 | cut -f1 -d" "` - 1 )) $1 | sed -e "s@<testsuites@<checked@g" -e "s@</testsuites@\</checked@g")"
reference="$(tail -n $(( `wc -l $2 | cut -f1 -d" "` - 1 )) $2 | sed -e "s@<testsuites@<reference@g" -e "s@</testsuites@\</reference@g")"

echo "<comparison>" > $TEMP_OUTPUT
echo "$checked" >> $TEMP_OUTPUT
echo "$reference" >> $TEMP_OUTPUT
echo "</comparison>" >> $TEMP_OUTPUT

xsltproc webview/generators/diff.xml.xsl $TEMP_OUTPUT

