#!/bin/sh


update_headers(){
	sed -i "s@Version : $1@Version : $2@g" header_cpp
	sed -i "s@Version : $1@Version : $2@g" header_sh
}

update_sources_files(){
	for file in `find ../../src/ -iname "*.cpp" -o -iname "*.h"`
	do
		sed -i "s@Version : $1@Version : $2@g" $file
	done
}

update_cmakelists(){
	for file in `find ../.. -name "CMakeLists.txt"`
	do
		sed -i "s@Version : $1@Version : $2@g" $file
	done
}

update_tests_files(){
	for file in `find ../../tests/ -iname "*.cpp"`
	do
		sed -i "s@Version : $1@Version : $2@g" $file
	done
}

update_specific_files(){

	sed -i "s@(Version $1)@(Version $2)@g" ../../README.md
	sed -i "s@Version : $1@Version : $2@g" ../../doc/*.tex
	sed -i "s@Version : $1@Version : $2@g" ../../tools/webview/webview_gen_all.sh
	sed -i "s@Version : $1@Version : $2@g" ../../tools/jsLoc/jsLoc_gen_all.sh
	sed -i "s@Version : $1@Version : $2@g" ../../tools/jsLoc/jsLoc/main.py
	sed -i "s@Version : $1@Version : $2@g" ../../tools/jsLoc/jsLoc/classes.py
	echo "$newversion" > ../../VERSION
}

update_control(){
	echo "################# NOT UPDATED ##################"
	grep -R -I "`echo "$1" | sed -e "s@\.@\\\\\.@g"`" ../../src/ ../../tests/ ../../tools/ ../../README.md ../../doc/
	echo "################################################"

}

if [ $# -eq 0 ] ; then
	echo "You must provide at least an old version number !"
	exit 1
fi
if [ $# -eq 1 ] ; then
	update_control $1
	exit 0
fi

SCRIPT_LOCATION="`dirname $0`"
oldversion="$1"
newversion="$2"

cd $SCRIPT_LOCATION

echo "Update Headers files"
update_headers $oldversion $newversion
echo "Update Sources files"
update_sources_files $oldversion $newversion
echo "Update Tests files"
update_tests_files $oldversion $newversion
echo "Updates CMakeLists.txt"
update_cmakelists $oldversion $newversion
echo "Updates specific files"
update_specific_files $oldversion $newversion

echo "Check Sum"
update_control $oldversion

cd - > /dev/null
