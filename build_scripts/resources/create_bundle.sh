#!/bin/sh

function die()
{
	printf "$@\n" 1>&2
	exit 1
}


which git >/dev/null 2>&1 || die "Unable to git in PATH !"
which tar >/dev/null 2>&1 || die "Unable to find tar tool !"
which gzip >/dev/null 2>&1 || die "Unable to find compression tool gzip !"

# get back from build_scripts/resources/
rootdir="`dirname $0`/../../"
version="`cat $rootdir/VERSION`"

OLD=$PWD
cd $rootdir

test -f ./run_validation || die "Had troubles with moving to root directory !"
test -n "$version" || die "Unable to detect Version number for PCVS !"

# old syntax (tar.gz may not exist)
git archive --format=tar --prefix=pcvs-$version/ HEAD | gzip > $OLD/pcvs-$version.tar.gz && printf "Archive created: $OLD/pcvs-$version.tar.gz\n"
