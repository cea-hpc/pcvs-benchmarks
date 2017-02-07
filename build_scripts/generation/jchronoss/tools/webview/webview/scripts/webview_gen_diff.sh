#!/bin/sh

TEMP_OUTPUT="temp.xml"
checked="$(sed -e "s/^<?xml version=.*?>$//g" -e "s@<testsuite@<checked@g" -e "s@</testsuite@\</checked@g" $1)"
reference="$(sed -e "s/^<?xml version=.*?>$//g" -e "s@<testsuite@<reference@g" -e "s@</testsuite@\</reference@g" $2)"

echo "<comparison>" > $TEMP_OUTPUT
echo "$checked" >> $TEMP_OUTPUT
echo "$reference" >> $TEMP_OUTPUT
echo "</comparison>" >> $TEMP_OUTPUT

xsltproc webview/generators/diff.xml.xsl $TEMP_OUTPUT

