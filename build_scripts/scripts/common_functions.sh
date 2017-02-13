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
#   - VALAT Sebastien sebastien.valat@cea.fr                           #
#                                                                      #
########################################################################


common_escapement_xml()
{
	#weak check : if we are on bash, speed up esaping with built-in optimization
	if [ "$TYPE_SHELL" = "bash" ] ; then
		#This is an optimised way but supported only by bash, not strict sh
		out="$2"
		out="${out//&/&amp;}"
		out="${out//</&lt;}"
		out="${out//>/&gt;}"
		out="${out//\"/&quot;}"
	else
		#very slow with a lot of tests (around 5 process forks for one test)
		out="$(echo "$2" | sed -e "s/\&/\&amp;/g" -e "s/</\&lt;/g" -e "s/>/\&gt;/g" -e "s/\"/\&quot;/g")"
	fi
	eval "$1=\"${out}\""
}

##################### SAFE EXEC #########################
# Display the command and execute it. On failure, exit the script.
# Params:
#   - $1 : Line to display about the command
#   - $* : command and arguments
common_safe_exec() {
	echo " * $1"
	shift
	eval "$@"
	if test "$?" != "0" ; then
		exit 1
	fi
}

##################### SAFE WARN #########################
# Display the command and execute it. On failure, exit the script.
# Params:
#   - $1 : Line to display about the command
#   - $* : command and arguments
common_safe_warn() {
	echo " * $1"
	shift
	eval "$@"
	if test "$?" != "0" ; then
		echo "Warning unable to finish $@"
	fi
}

##################### PRINT ERROR #######################
# Print an error message.
# Params:
#   - $* : Message to display
common_print_error() {
	echo ${ECHO_OPTIONS} "${IMPORTANT_COLOR}----------------------------------- /!\ ERROR /!\ ---------------------------------" 1>&2
	echo "|  $*" 1>&2
	echo "|  See ./mpc_validation -h|--help for further information" 1>&2
	echo ${ECHO_OPTIONS} "-----------------------------------------------------------------------------------${DEFAULT_COLOR}" 1>&2
}

#################### INSERT TEST ##########################
# Add a new command rule to execute while running tests
# Please use this function by this way we can abstract the
# Format of the generated file
# Params:
#   - $1 : test name
#   - $2 : Extected exit status
#   - $3 : Command
#   - $4 : deps on other tests, separated by commas
#   - $4 : required number or resources by the test
common_insert_test()
{	
	#Format is {NAME}:{DEPS}:{EXPECTED_EXIT_STATUS}:{COMMAND}
	if common_test_requirement ; then
		#do some escaping
		common_escapement_xml 'name' "$1"
		common_escapement_xml 'rc' "$2"
		common_escapement_xml 'command' "$3"
		common_escapement_xml 'deps' "$4"

		#test info
		echo "	<job>"
		echo "		<name>$name</name>"
		echo "		<rc>$rc</rc>"

		#Write deps
		echo "		<deps>"
		if [ ! -z "$deps" ]; then
			if [ "$TYPE_SHELL" = "bash" ] ; then
				#CAUTION, This replacement tricks work only on bash
				echo "			<dep>${deps//[,;]/</dep><dep>}</dep>"
			else
				echo "			<dep>$(echo "$deps" | sed -e "s@[,;]@</dep><dep>@g")</dep>"
			fi
		fi
		echo "		</deps>"
	
		#command and finish
		echo "		<command>$command</command>"
		echo "          <constraints>"
		if [ ! -z "`echo "$name" | egrep "^compile_"`" ] ; then
				echo "                  <constraint>compilation</constraint>"
		fi
		echo "          </constraints>"
		if [ ! -z "$5" ] ; then
			echo "          <resources>$5</resources>"
		fi
		echo "	</job>"
	fi	
}

#################### READ PARAM VALUE #####################
# Extract the value of a given parameter using the format --name=value.
# Params:
#   - $1 : Define the string from which to extract.
#   - $2 : Define the parameter string (eg. --prefix)
common_read_param_value() {
	echo "$1" | sed -e "s/^$2=//g"
}

##########################################################
# Get first value if smaller than a limit given by the second one.
# It set variable "value"
# Params:
#  - $1 : value to keep if possible
#  - $2 : limit
common_get_first_if_smaller()
{
	if [ $1 -le $2 ]; then
		value=$1
	else
		value=$2
	fi
}

############### HELPER TEST REQUIREMENT #########################
# Before instert each test, apply last filters
common_test_requirement()
{
	# check OpenMP requirements
	if [ "${HAVE_MPC_GCC}" = "false" ] && [ "${REQUIRE_OPENMP}" = "true" ] ; then
		return 1
	fi
	
	# if this occurs, all requirements are passed
	return 0
}

######################## VERIFICATION FAST TEST FILE ####################
#return true if filename in $1 exists
common_check_existing_files(){
	if [ ! -f "$1" ] ; then
		common_print_error "The input file $1 is missing. Check file existence."; exit 2;
	fi
}

###################### CLEAN PATHS  ####################
# Clean all paths contained in tarball
common_clean_path()
{
	find . -exec sed -i "s@${MPC_INSTALL_PREFIX}@MPC_INSTALL_PATH@g" {} \;     2> /dev/null
	find . -exec sed -i "s@${PCVS_SOURCE_DIR}@TESTSUITE_PATH@g" {} \;      2> /dev/null
	find . -exec sed -i "s@${HOME}@HOME_PATH@g" {} \;                         2> /dev/null
	find . -exec sed -i "s@/cea/local/opt@MODULES_PATH@g" {} \;               2> /dev/null
	find . -exec sed -i "s@`whoami`@USER@g" {} \;                              2> /dev/null
	if [ -z "$HOSTNAME" ] ; then
		HOSTNAME="`hostname`"
	fi
	find . -exec sed -i "s@$HOSTNAME@HOSTNAME@g" {} \;                        2> /dev/null
}

############ COMMON_SET_TO_ENV #################
# Set $1 to $2, $1 will be a environment variable, it will erase any previous value
common_set_to_env()
{
	var="$1"; shift
	arg="$@"
	eval "$var=\"$arg\""
}

common_append_to_env()
{
	var="$1"; shift
	old_arg="`eval echo "$"$var`"
	arg="$@"
	eval "$var=\"$old_arg $arg\""
}

common_prepend_to_env()
{
	var="$1"; shift
	old_arg="`eval echo "$"$var`"
	arg="$@"
	eval "$var=\"$arg $old_arg\""
}

common_remove_to_env()
{
	var="$1"; shift
	old_arg="`eval echo "$"$var`"
	new_arg="`echo "$old_arg" | sed -e "s@$@@@g"`"
	common_set_to_env "$var" "$new_arg"
}
