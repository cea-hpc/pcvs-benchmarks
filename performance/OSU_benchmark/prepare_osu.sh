#!/bin/sh

NAME="`basename $1 .tar.gz`"

echo "Prepare $NAME"

tar -xzf $1
for i in collective  pt2pt; do
	mkdir -p mpi/$i
	cp $NAME/mpi/$i/*.[ch]  mpi/$i
	cp testSuite_config_$i  mpi/$i/testSuite_config
done
rm -rf $NAME

