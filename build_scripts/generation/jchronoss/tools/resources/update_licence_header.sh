#!/bin/sh
for cur in `ls *.cpp && ls *.h`
do
echo "Doing $cur"
FILE=$cur 
cat ../tools/resources/header_cpp > $FILE.new && tail -n `echo "\`cat $FILE | wc -l\` - 24" | bc` $FILE >> $FILE.new
#mv $FILE.new $FILE
done
