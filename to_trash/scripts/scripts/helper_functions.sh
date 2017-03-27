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

################## DEFAULT FOR VARS #######################
EXPECTED_EXIT_CODE=0
COMMAND_OPTIONS=""

####################### SET IF VALID #####################
# this function will set the var named as $1 with $2 only if $1 is not empty
# $1 : env var name
# $2 : value
helper_set_if_valid()
{
	content=`eval $1`
	if test -n $content; then
		eval="$1=\"$content$2\""
	fi
}

###################### PREFIX FILES #######################
# Add a PCVS_CSOURCE_DIR as prefix for all files given in parameter and print
# Params:
#  - $* : list of files to prefix
# Args:
#  - PCVS_CSOURCE_DIR
helper_make_absolute_source_paths()
{
	for tmp in $*
	do
		if [ -z "$(echo "${tmp}" | grep '^/')" ]; then
			echo "${PCVS_CSOURCE_DIR}/${tmp}"
		else
			echo "$tmp"
		fi
	done
}

################# LIST_TESTS_NAME FROM FILES ##############
# Display list of test files obtained by using wildcards.
# Globals:
#   - PCVS_CSOURCE_DIR
helper_wildcard_test_list()
{
	find "${PCVS_CSOURCE_DIR}" -iname "*.c" -type f
	find "${PCVS_CSOURCE_DIR}" -iname "*.cu" -type f
	find "${PCVS_CSOURCE_DIR}" -iname "*.cpp" -type f
	find "${PCVS_CSOURCE_DIR}" -iname "*.cxx" -type f
	find "${PCVS_CSOURCE_DIR}" -iname "*.f" -type f
	find "${PCVS_CSOURCE_DIR}" -iname "*.f90" -type f
}

###################### GET COMPILER #######################
# Place compiler to use in 'compiler' variable
# Params:
#  - $1 : First file to compile to detect language if COMPILER is not defined
# Globals:
#  - COMPILER : Define the compiler to use. If not defined, try to find the good one depending on first file language.
#  - CC : C compiler to use
#  - CXX : CXX compiler to use.
#  - F77 : Fotran compiler to use.
helper_get_compiler()
{
	#source a potential configuration file
	if test -n "${PCVS_COMPILER}"; then
		file=$PCVS_INTERNALS_DIR/configuration/compilers/${PCVS_COMPILER}.conf

		if test -f ${file}; then
			. ${file}
		fi
	fi

	if [ -z "$COMPILER" ]; then
		case "$1" in
			*.c)   compiler="$TS_CC ${CC_ARGS}";;
			*.C)   compiler="$TS_CXX ${CXX_ARGS}";;
			*.cpp) compiler="$TS_CXX ${CXX_ARGS}";;
			*.cxx) compiler="$TS_CXX ${CXX_ARGS}";;
			*.f)   compiler="$TS_F77 ${F77_ARGS}";;
			*.F)   compiler="$TS_F77 ${F77_ARGS}";;
			*.f90) compiler="$TS_F77 ${F77_ARGS}";;
			*.F90) compiler="$TS_F77 ${F77_ARGS}";;
			*.cu)  compiler="$TS_NV  ${NV_ARGS}";;
			*)
				common_print_error "Unknown language to select compiler for file : $1";
				exit 1
				;;
		esac
	else
		compiler="$COMPILER"
	fi
}

################### GEN COMPILE COMMAND ###################
# Generate compile command for the given file. Command is placed into 'command' variable
# Params:
#  - $1 : Output file
#  - $2 $3 ... : The files to compile, first one is use to determine which compiler to use.
# Globals:
#  - COMPILER : Define the compiler to use. If not defined, try to find the good one depending on first file language.
#  - CC : C compiler to use
#  - CXX : CXX compiler to use.
#  - F77 : Fotran compiler to use.
#  - CFLAGS
#  - LDFLAGS
helper_gen_compile_command()
{
	#select compiler
	helper_get_compiler "$2"

	#generate command
	command="${compiler} ${CFLAGS} -o $* ${LDFLAGS}"
}

####################### CONFIG LOOP #######################
# Insert test for all configurations
# Args :
#  - $1 : base test name
#  - $2 : executable
#  - $3 : optional dependency
# Globals:
#  - MPC_NET_LIST
#  - MPC_THREAD_LIST
#  - MPC_PROC_NB_LIST
#  - MPC_TASK_NB_LIST
#  - EXPECTED_EXIT_CODE
#  - MPCRUN
#  - MPCRUN_ARGS
#  - MPC_AUTO_KILL_TIMEOUT
#  - COMMAND_OPTIONS
helper_configuration_loop()
{
	#loop on exec configurations
	for net in ${MPC_NET_LIST}
	do
		for thread in ${MPC_THREAD_LIST}
		do
			for proc in ${MPC_PROC_NB_LIST}
			do
				for tasks in ${MPC_TASK_NB_LIST}
				do
					if [ $proc -le $tasks ] ; then
						if [ $proc -gt $CLUSTER_MAX_NODES ]  ; then
							nbNodes="$CLUSTER_MAX_NODES"
							cores="$((($CLUSTER_MAX_CORES_PER_NODE*$CLUSTER_MAX_NODES)/$proc))"
						else 
							nbNodes="$proc"
							cores="$CLUSTER_MAX_CORES_PER_NODE"
						fi
                        
                        if [ $proc -eq 1 ] && [ $tasks -eq 1 ]; then
                           exec_mode="alonebase"
                        elif [ $proc -eq 1 ]; then 
                           exec_mode="taskbase"  
                        elif [ $tasks -eq $proc ]; then
                           exec_mode="procbase"
                        else
                           exec_mode="hybridbase"
                        fi

						#gen options
						opts="--clean --autokill=${MPC_AUTO_KILL_TIMEOUT} -N=$nbNodes -n=${tasks} --share-node -p=${proc} -net=${net} -m=${thread} -c=$cores"
						#gen command
						command="${MPCRUN} ${opts} ${MPCRUN_ARGS} ${2} ${COMMAND_OPTIONS}"
						#insert test
						common_insert_test "${1}_${thread}_${exec_mode}_${net}_proc_nb_${proc}_${tasks}" "${EXPECTED_EXIT_CODE}" "$command" "${3}" "$nbNodes"
					fi
				done
			done
		done
	done
}

#################### INSERT TEST ##########################
# Insert test for all configurations. It will fill variable 'command'.
# Args :
#  - $1 : executable
#  - $2 : mpcrun args extra arguement (in additions to default MPCRUN_ARGS)
# Globals:
#  - MPC_AUTO_KILL_TIMEOUT
#  - MPCRUN
#  - MPCRUN_ARGS
#  - COMMAND_OPTIONS
helper_gen_exec_command()
{
	#gen options
	opts="--clean --autokill=${MPC_AUTO_KILL_TIMEOUT} --share-node"
	#gen command
	command="${MPCRUN} ${opts} ${MPCRUN_ARGS} ${2} ${1} ${COMMAND_OPTIONS}"
}

