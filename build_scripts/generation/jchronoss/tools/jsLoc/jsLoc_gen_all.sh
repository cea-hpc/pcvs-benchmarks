#!/bin/bash
############################################################################
#                                                                          #
#                         Copyright or (C) or Copr.                        #
#       Commissariat a l'Energie Atomique et aux Energies Alternatives     #
#                                                                          #
# Version : 1.2                                                            #
# Date    : Tue Jul 22 13:28:10 CEST 2014                                  #
# Ref ID  : IDDN.FR.001.160040.000.S.P.2015.000.10800                      #
# Author  : Julien Adam <julien.adam@cea.fr>                               #
#           Marc Perache <marc.perache@cea.fr>                             #
#                                                                          #
# This file is part of JCHRONOSS software.                                 #
#                                                                          #
# This software is governed by the CeCILL-C license under French law and   #
# abiding by the rules of distribution of free software.  You can  use,    #
# modify and/or redistribute the software under the terms of the CeCILL-C  #
# license as circulated by CEA, CNRS and INRIA at the following URL        #
# "http://www.cecill.info".                                                #
#                                                                          #
# As a counterpart to the access to the source code and  rights to copy,   #
# modify and redistribute granted by the license, users are provided only  #
# with a limited warranty  and the software's author,  the holder of the   #
# economic rights,  and the successive licensors  have only  limited       #
# liability.                                                               #
#                                                                          #
# In this respect, the user's attention is drawn to the risks associated   #
# with loading,  using,  modifying and/or developing or reproducing the    #
# software by the user in light of its specific status of free software,   #
# that may mean  that it is complicated to manipulate,  and  that  also    #
# therefore means  that it is reserved for developers  and  experienced    #
# professionals having in-depth computer knowledge. Users are therefore    #
# encouraged to load and test the software's suitability as regards their  #
# requirements in conditions enabling the security of their systems and/or #
# data to be ensured and,  more generally, to use and operate it in the    #
# same conditions as regards security.                                     #
#                                                                          #
# The fact that you are presently reading this means that you have had     #
# knowledge of the CeCILL-C license and that you accept its terms.         #
#                                                                          #
############################################################################

##################### VARS ####################
MASTER_FILE="globals.trace"
SLAVES_FILE="slaves_block.trace"
OUT_DIR="."
SRC_DIR="."
NBRES=""
PATTERN="#nbJobs"
SLAVES_FIRST_TS=""
SLAVE8_LAST_TS=""
JSLOC_PATH="."
JSLOC_MAIN="jsLoc/main.py"

############################
# Interpret options from command line
# - $1 : chain where option is written
# - $2 : pattern to remove from chain above
read_param_value(){
	echo $1 | sed -e "s@^$2=@@g"
}


#############################
# write current file in standard input (header omitted)
# - $1 fileName to write
merge_from(){
	echo "$(tail -n $(( $(wc -l $1 | cut -f1 -d" ") - 1 )) $1)"
}

#############################
# Get begin and end timestamp in order to generate global and slaves files
# - $@ : list of files used to find min and max timestamps
find_slaves_timestamp_limits(){
	TMP="$(cat $@ | grep -v "^#")"
	
	SLAVES_FIRST_TS="$(echo "$TMP" | cut -f2 -d":" | sort -n | head -n 1)"
	SLAVES_LAST_TS="$(echo "$TMP" | cut -f3 -d":" | sort -n | tail -n 1)"
}

#############################
# generate master file (all tests included)
gen_master_data_file(){
	find_slaves_timestamp_limits $(ls ${SRC_DIR}/testFile-*.trace)
	echo "${PATTERN}:${NBRES}:${SLAVES_FIRST_TS}"
	for file in $(ls ${SRC_DIR}/testFile-*.trace) ; do
		merge_from $file
	done	
}

#############################
# generate slaves file scheduling (by worker)
gen_slaves_data_file(){
	find_slaves_timestamp_limits $(ls ${SRC_DIR}/testFile-*.trace)
	echo "${PATTERN}:${NBRES}:${SLAVES_FIRST_TS}"
	for file in $(ls ${SRC_DIR}/testFile-*.trace) ; do
		find_slaves_timestamp_limits $file
		name="$(basename $file)"
		cur_res="$(cat $file | grep "^#" | cut -f2 -d":")"
		echo "$name:$SLAVES_FIRST_TS:$SLAVES_LAST_TS:$cur_res"
	done
}

#############################
#generate final svg diagram from given file
#$1 = filename as input
gen_svg(){
	SVG_PREFIX="$(basename $1 | cut -f1 -d".")"
	python ${JSLOC_PATH}/${JSLOC_MAIN} -d ${OUT_DIR}/${SVG_PREFIX}.svg -s $1
}

#############################
#print help
printHelp(){

echo "####################### USAGE ########################"
echo "#  ./jsLoc_gen_all.sh [--in=<path>] --res=<number>   #"
echo "######################################################"
}

#############################
########### MAIN ############
#############################

#############################
#options parsing
for arg in $@ ; do
	case $arg in
		--in=*)
			SRC_DIR="$(read_param_value $arg "--in")"
		;;
		--out=*)
			OUT_DIR="$(read_param_value $arg "--out")"
		;;
		--res=*)
			NBRES="$(read_param_value $arg "--res")"
		;;
		-h|--help)
			printHelp
			exit 0
		;;
		*)
			echo "Still not implemented !!!"
			exit 1
		;;
	esac
done

#############################
# number of available resources need to be set
if [ -z "${NBRES}" ] ; then
	echo " ERROR : Unable to run JsLoc without known how many resources we have"
	echo " ERROR : Try --res=<number> option."
	exit 1
fi

if [ ! -f "${PWD}/jsLoc_gen_all.sh" ] ; then
	JSLOC_PATH="`dirname $0`"
fi

########## BANNER ###########
cat ${JSLOC_PATH}/jsLoc/jsloc_banner

#############################
# create globals file from current traces
gen_master_data_file > ${OUT_DIR}/${MASTER_FILE}
gen_slaves_data_file > ${OUT_DIR}/${SLAVES_FILE}

#############################
# Generate each file
for file in $(ls ${SRC_DIR}/testFile-*.trace); do
	echo " - Generation for $(basename $file)"
	gen_svg $file
done

#############################
# generate globals file
echo " - Generation for global traces"
gen_svg ${OUT_DIR}/${MASTER_FILE}
echo " - Generation for slaves traces"
gen_svg ${OUT_DIR}/${SLAVES_FILE}
