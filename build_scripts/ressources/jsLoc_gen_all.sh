#!/bin/sh
############################# MPC License ##############################
# Wed Nov 19 15:19:19 CET 2008                                         #
# Copyright or (C) or Copr. Commissariat a l'Energie Atomique          #
#                                                                      #
# IDDN.FR.001.230040.000.S.P.2007.000.10000                            #
# This file is part of the MPC Runtime.                                #
#                                                                      #
# This software is governed by the CeCILL-C license under French law   #
# and abiding by the rules of distribution of free software.  You can  #
# use, modify and/ or redistribute the software under the terms of     #
# the CeCILL-C license as circulated by CEA, CNRS and INRIA at the     #
# following URL http://www.cecill.info.                                #
#                                                                      #
# The fact that you are presently reading this means that you have     #
# had knowledge of the CeCILL-C license and that you accept its        #
# terms.                                                               #
#                                                                      #
# Authors:                                                             #
#   - ADAM Julien julien.adam@cea.fr                                   #
#                                                                      #
########################################################################
GLOBAL_FILE="traces/global_traces"
ALLOC_FILE="traces/alloc_traces"

NBNODES=0
CHAIN="#nbTests:"

#banner
cat ./banners/jsloc_banner

. ./config.cfg || exit 1

gen_global_data(){
	NBNODES=$(echo "$NBNODES+$(egrep "^#" $1 | cut -f2 -d":")" | bc)
	echo "$(tail -n $(( $(wc -l $1 | cut -f1 -d" ") - 1 )) $1)" >> $GLOBAL_FILE
}

find_ts_end(){
	file="$(echo "$1"| cut -f1 -d":")"
	echo "$(cat $file | cut -f3 -d":" | sort -n | tail -n 1)"
}


if [ ! -d ./traces ] ; then
	echo "Error: There are no source to display !"
	exit 1
fi

TIMESTAMP="$(egrep "^#" traces/testFile-00000001.in.trace | cut -f3 -d":")"
CHAIN="$CHAIN:$TIMESTAMP"
echo "$CHAIN" > $GLOBAL_FILE

echo "Start Generation :"
for file in `find ./traces/ -iname '*.trace'`
do
    name="$(basename $file | cut -f1 -d".")"
    echo "  - Generation for $name"
    python jsLoc/main.py -d traces/$name.svg $file
    if [ -z "`echo $file | grep "testFile-00000000"`" ] ; then
    	gen_global_data $file
    fi
done

if [ ! -z "$CLUSTER_MAX_NODES" ] ; then
	NBNODES=$CLUSTER_MAX_NODES
fi
sed -i "s@$CHAIN@#nbTests:$NBNODES:$TIMESTAMP@g" $GLOBAL_FILE 
echo "  - Generation global execution trace"
python jsLoc/main.py -d $GLOBAL_FILE.svg $GLOBAL_FILE

DATAS="$(egrep "^#" traces/*.trace | grep -v testFile-00000000.in.trace)"
echo "#nbTests:$NBNODES:$TIMESTAMP" > $ALLOC_FILE
for line in $DATAS
do
	nodes="$(echo "$line" | cut -f3 -d":")"
	ts_begin="$(echo "$line" | cut -f4 -d":")"
	ts_end="$(find_ts_end $line)"
	echo "$(echo "$line" | cut -f1 -d":" | sed -e "s@traces/@@g"):$ts_begin:$ts_end:$nodes" >> $ALLOC_FILE
done
echo "  - Generation global allocation trace"
python jsLoc/main.py -d $ALLOC_FILE.svg $ALLOC_FILE
echo "Done"
echo "You can browse by opening $PWD/traces/*.svg in web browser."
