#!/bin/sh

if [ ! -d ./.git ] ; then
	echo "This file have to be executed from root Git Project !"
	exit 1
fi

TO_EXCLUDE="`cat ./.gitattributes | cut -f1 -d" "`"
git ls-files | grep -vF "$TO_EXCLUDE" #> list_of_files

