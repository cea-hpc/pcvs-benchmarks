#!/bin/sh
FORMAT=""
COMPRESS_COMMAND=""
VERSION=""

if [ ! -d .git ] ; then
	echo "Run this script from root git project !"
	exit 1
fi

VERSION="`cat ./VERSION`"
#VERSION="`cat ./VERSION | sed -e "s@\.@\\\\\.@g"`"
case $1 in
	zip)
		FORMAT=zip
		COMPRESS_COMMAND=""
	;;
	tgz)
		FORMAT=tar
		COMPRESS_COMMAND=gzip
	;;
	tbz2)
		FORMAT=tar
		COMPRESS_COMMAND=bzip2
	;;
	*)
		echo "Please use a format in : zip, tgz, bz2 !"
		exit 2
	;;
esac
echo "Generate sources archive for JCHRONOSS $VERSION"
if [ -z "$COMPRESS_COMMAND" ] ; then
	git archive --format=$FORMAT --prefix=JCHRONOSS-$VERSION/ HEAD > JCHRONOSS-$VERSION.$1
else
	git archive --format=$FORMAT --prefix=JCHRONOSS-$VERSION/ HEAD | $COMPRESS_COMMAND > JCHRONOSS-$VERSION.$1
fi
