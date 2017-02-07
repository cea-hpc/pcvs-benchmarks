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
#   - VALAT Sebastien sebastien.valat@cea.fr                           #
#                                                                      #
########################################################################

###################### FUNCTION ####################
# Args :
#  - output file
#  - command
generate()
{
	output=$1
	echo "  - Generate $output"
	shift
	$* > $output || exit 1
}

###################### FUNCTION ####################
# Args :
#  - directory
cleanup()
{
	echo "  - Cleanup $1"
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
	echo "  - Generate $1"
	shift
	#echo xsltproc $*
	xsltproc $* > $output || exit 1
}

############## CHECK SOURCES #####################
#
#
check_sources(){
	if [ -z "$1" ] ; then
		PREFIX="."
	else PREFIX="$1"
	fi
	if [ ! -z "$NEW_TARBALL" ] ; then
            	#backup
		if [ -d ./test_suite ] ; then
            		mv -T test_suite test_suite.backup
		fi
		if [ -d ./build_check ] ; then
            		mv -T build_check build_check.backup
        	fi

		rm -rf last_results/*
		tar -xzf $NEW_TARBALL last_results/test_suite/ last_results/build_check/ 2> /dev/null
		mv last_results/* .
	fi

	if [ ! -d ./test_suite ] && [ ! -d ./build_check ]  ; then
		echo "Error: There are no source to display !"
		exit 1
	else 
            	if [ -d ./test_suite ] ; then
			SOURCES_LIST="$SOURCES_LIST $PREFIX/test_suite/"    
            	fi
		if [ -d ./build_check ] ; then
			SOURCES_LIST="$SOURCES_LIST $PREFIX/build_check/"    
        	fi
	fi
}

check_reference(){
	rm -rf reference/*

	if [ -z "${DIFF_TARBALL}" ] && [ -f ./config.cfg ] ; then
		. ./config.cfg	
	fi
	if [ ! -z "${DIFF_TARBALL}" ] && [ -f ${DIFF_TARBALL} ] ; then
		mkdir -p reference 
		rm -rf reference/* && cd reference
		tar -xzf ${DIFF_TARBALL} last_results/test_suite/ last_results/build_check/ 2> /dev/null 
		cd ..
	fi

	if [ ! -d ./reference/last_results/test_suite ] && [ ! -d ./reference/last_results/build_check ]  ; then
		echo "  - No reference tarball submitted (or not found) : Disabling Diff capabilities"
		HAVE_REFERENCE="no"
	else HAVE_REFERENCE="yes"
	fi;
}

#check if a file in $DIFF_TARBALL match with $1 file
file_matching(){
	
	file_ref="./reference/last_results/$(echo "$1" | awk -F'^./' '{print $2}' 2> /dev/null)"
	
	echo $file_ref

	if [ -f $file_ref ] ; then
		return 0
	fi
	return 1
}

print_help(){
	echo "################################## WEBVIEW USAGE #############################"
	echo "                         ./webview_gen_all.sh [OPTIONS]                      #"
	echo "##############################################################################"
	echo "#  --new=<tarball>   Specifies an input tarball (current validation)         #"
	echo "#  --old=<tarball>   Specifies a reference tarball (previous validation)     #"
	echo "#                                                                            #"
	echo "# By default, webview script generates webview without diff option except    #"
	echo "# if DIFF_TARBALL is defined (use --diff=<tarball> option in mpc_validation) #"
	echo "##############################################################################"
}
######################## MAIN ######################
#first message

cat ./banners/webview_banner

SOURCES_LIST=""
execbase_filter='.*'

for arg in $*
do
	case $arg in
		--new=*)
			NEW_TARBALL="$(echo "$arg" | sed -e "s@^--new=@@g")"
		;;
		--old=*)
			DIFF_TARBALL="$(echo "$arg" | sed -e "s@^--old=@@g")"
		;;
		-h|--help)
			print_help
			exit 0
		;;
        -p|--procbase)
            execbase_filter="_procbase_"
        ;;
        -t|--taskbase)
            execbase_filter="_taskbase_"
        ;;
        -a|--alone)
            execbase_filter="_alonebase_"
        ;; 
		*)
			echo "Ignored : --> $arg"
		;;
	esac
done

echo " * Check sources"
check_sources
echo " * Check reference tarball"
check_reference
echo " * Start generation :"

#cleanup of mkdir
if [ -d webview/generated ]; then
	cleanup webview/generated
else
	echo "create webview/generated"
	mkdir webview/generated || exit 1
fi

export SOURCES_LIST

#generate summary
generate webview/generated/main.xml ./webview/scripts/webview_gen_global_stats.sh
generate webview/generated/errors.xml ./webview/scripts/webview_gen_fail_list.sh

#generate html pages
generate_by_xsl webview/generated/main.html webview/generators/main.html.xsl webview/generated/main.xml
generate_by_xsl webview/generated/errors.html webview/generators/errors.html.xsl webview/generated/errors.xml

for file in $(find $SOURCES_LIST -iname 'output*.xml' )
do
	out_filename=$(echo $file | sed -e 's#^\./##g' -e 's#/#-#g' -e "s/.xml/.html/g")
	current_dir=$(dirname $file | sed -e 's#^\./##g')
	generate_by_xsl webview/generated/$out_filename --stringparam current_dir ${current_dir} --stringparam execbase_filter ${execbase_filter} webview/generators/detail.html.xsl $file

	diff_out_filename=$(echo $file | sed -e 's#^\./##g' -e 's#/#-#g')
	
	if [ "${HAVE_REFERENCE}" = "yes" ] ; then
		file_ref="`file_matching "$file"`"
		if [ $? -eq  0 ] ; then
			generate webview/generated/diff-$diff_out_filename webview/scripts/webview_gen_diff.sh $file $file_ref
			rm -f temp.xml
			generate_by_xsl webview/generated/diff-computed-$diff_out_filename --stringparam current_dir ${current_dir} webview/generators/diff-compute.xml.xsl webview/generated/diff-$diff_out_filename
			generate_by_xsl webview/generated/diff-$out_filename --stringparam current_dir ${current_dir} webview/generators/diff.html.xsl webview/generated/diff-computed-$diff_out_filename
		
		else
			generate_by_xsl webview/generated/diff-$out_filename --stringparam current_dir  ${current_dir} webview/generators/diff-file-not-found.html.xsl $file 
			echo "    /!\ WARNING: file $file_ref NOT FOUND, Diff not generated"
		fi
	else
		generate_by_xsl webview/generated/diff-$out_filename --stringparam current_dir ${current_dir} webview/generators/diff-file-not-found.html.xsl $file 
	fi
done

#some post-actions
rm -rf last_results reference 
if [ -d test_suite.backup ] ; then
	rm -rf test_suite
	mv -T test_suite.backup test_suite
fi
if [ -d build_check.backup ] ; then
	rm -rf build_check
	mv -T build_check.backup build_check
fi
#finish
echo "Done"
echo ""
###################### FINISH ######################
echo "You can browse by opening $PWD/webview/index.html in your browser"
echo "or manually start a small web server : cd webview && python -m SimpleHTTPServer"
