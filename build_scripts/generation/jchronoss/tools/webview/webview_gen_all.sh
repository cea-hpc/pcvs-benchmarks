#!/bin/sh

############################################################################
#                                                                          #
#                         Copyright or (C) or Copr.                        #
#       Commissariat a l'Energie Atomique et aux Energies Alternatives     #
#                                                                          #
# Version : 2.0                                                            #
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

SOURCES_LIST=""
WEBVIEW_PATH="."
NEW_PATH=""
DIFF_PATH=""
SKELETON=""

###################### FUNCTION ####################
# Args :
#  - output file
#  - command
generate()
{
	output=$1
	echo "  - Generate `basename $output`"
	shift
	$* > $output || exit 1
}

###################### FUNCTION ####################
# Args :
#  - directory
cleanup()
{
	echo " * Cleanup $1" 
	if [ ! -z "$1" ]; then
		rm -f $1/* || exit 1
	fi
}

###################### FUNCTION ####################
# Args:
#  - output
#  - xslt
#  - xml
generate_by_xsl()
{
	output=$1
	echo "  - Generate `basename $1`"
	shift
	#echo xsltproc $*
	xsltproc $* > $output || exit 1
}

############## CHECK SOURCES #####################
#
#
check_sources(){
	
	if [ -z "$NEW_PATH" ] ; then
		echo "You have to provide a valid path where output files are stored"
		exit 1
	fi

	SOURCES_LIST="$NEW_PATH"    
}

check_reference(){

	if [ -z "$DIFF_PATH" ]  ; then
		echo "  - No reference data submitted (or not found) : Disabling Diff capabilities"
		HAVE_REFERENCE="no"
	else 
		HAVE_REFERENCE="yes"
		mkdir -p ./reference
		tar -xf $DIFF_PATH -C ./reference
	fi
}

#check if a file in $DIFF_TARBALL match with $1 file
file_matching(){
	
	file_ref="./reference/last_results/$(echo "$1" | awk -F'last_results/' '{print $2}' 2> /dev/null)"
	echo $file_ref

	if [ -f $file_ref ] ; then
		return 0
	fi
	return 1
}

print_help(){
	echo "################################## WEBVIEW USAGE #############################"
	echo "                ./webview_gen_all.sh --new=<path> [ --old=<path> ]           #"
	echo "##############################################################################"
	echo "#  --new=<path>   Specifies an input folder (current validation)             #"
	echo "#  --old=<path>   Specifies a reference folder (previous validation)         #"
	echo "##############################################################################"
}
######################## MAIN ######################

if [ ! -f "${PWD}/webview_gen_all.sh" ] ; then
	WEBVIEW_PATH="`dirname $0`"
fi

## BANNER ##
cat ${WEBVIEW_PATH}/webview/webview_banner

for arg in $*
do
	case $arg in
		--skeleton)
			SKELETON="yes"
			;;
		--new=*)
			NEW_PATH="$(echo "$arg" | sed -e "s@^--new=@@g")"
		;;
		--old=*)
			DIFF_PATH="$(echo "$arg" | sed -e "s@^--old=@@g")"
		;;
		-h|--help)
			print_help
			exit 0
		;;
		*)
			echo "Ignored : --> $arg"
		;;
	esac
done

if test "$SKELETON" = "yes" ; then
	SOURCES_LIST=""
else
	echo " * Check sources"
	check_sources
	echo " * Check reference sources"
	check_reference
fi

#cleanup of mkdir
if [ -d ${WEBVIEW_PATH}/webview/generated ]; then
	cleanup ${WEBVIEW_PATH}/webview/generated
else
	echo "create webview/generated"
	mkdir ${WEBVIEW_PATH}/webview/generated || exit 1
fi

export SOURCES_LIST
export WEBVIEW_PATH

echo " * Start generation :"
#generate summary
generate ${WEBVIEW_PATH}/webview/generated/main.xml ${WEBVIEW_PATH}/webview/scripts/webview_gen_global_stats.sh

#generate html pages
generate_by_xsl ${WEBVIEW_PATH}/webview/generated/main.html ${WEBVIEW_PATH}/webview/generators/main.html.xsl ${WEBVIEW_PATH}/webview/generated/main.xml
cp ${WEBVIEW_PATH}/webview/generators/realtime.html ${WEBVIEW_PATH}/webview/generated/realtime.html
cp ${WEBVIEW_PATH}/webview/generators/diff-main.html ${WEBVIEW_PATH}/webview/generated/diff-main.html

if test -z $SOURCES_LIST; then
	echo " * Website Skeleton Generated !";
	exit 0;
fi

generate ${WEBVIEW_PATH}/webview/generated/errors.xml ${WEBVIEW_PATH}/webview/scripts/webview_gen_fail_list.sh
generate_by_xsl ${WEBVIEW_PATH}/webview/generated/errors.html ${WEBVIEW_PATH}/webview/generators/errors.html.xsl ${WEBVIEW_PATH}/webview/generated/errors.xml

for file in $(find $SOURCES_LIST -iname 'output*.xml' )
do
	out_filename="$(egrep -o "<testsuite name=\"[^\"]+\"" $file | sed -e "s@<testsuite name\=@@g" -e "s@\"@@g" -e 's#\.#-#g' -e "s#/#-#g").html"
	generate_by_xsl ${WEBVIEW_PATH}/webview/generated/$out_filename ${WEBVIEW_PATH}/webview/generators/detail.html.xsl $file

	diff_out_filename="$(egrep -o "<testsuite name=\"[^\"]+\"" $file | sed -e "s@<testsuite name\=@@g" -e "s@\"@@g" -e 's#\.#-#g' -e "s#/#-#g").xml"
	if [ "${HAVE_REFERENCE}" = "yes" ] ; then
		file_ref="`file_matching "$file"`"
		if [ $? -eq  0 ] ; then
			generate ${WEBVIEW_PATH}/webview/generated/diff-$diff_out_filename ${WEBVIEW_PATH}/webview/scripts/webview_gen_diff.sh $file $file_ref
			rm -f temp.xml
			generate_by_xsl ${WEBVIEW_PATH}/webview/generated/diff-computed-$diff_out_filename ${WEBVIEW_PATH}/webview/generators/diff-compute.xml.xsl ${WEBVIEW_PATH}/webview/generated/diff-$diff_out_filename
			generate_by_xsl ${WEBVIEW_PATH}/webview/generated/diff-$out_filename ${WEBVIEW_PATH}/webview/generators/diff.html.xsl ${WEBVIEW_PATH}/webview/generated/diff-computed-$diff_out_filename
		
		else
			generate_by_xsl ${WEBVIEW_PATH}/webview/generated/diff-$out_filename ${WEBVIEW_PATH}/webview/generators/diff-file-not-found.html.xsl $file 
			echo "    /!\ WARNING: file $file_ref NOT FOUND, Diff not generated"
		fi
	else
		generate_by_xsl ${WEBVIEW_PATH}/webview/generated/diff-$out_filename ${WEBVIEW_PATH}/webview/generators/diff-file-not-found.html.xsl $file 
	fi
done

#finish
echo ""
###################### FINISH ######################
echo "You can browse by opening $WEBVIEW_PATH/webview/index.html in your browser"
echo "or manually start a small web server : cd webview && python -m SimpleHTTPServer"
