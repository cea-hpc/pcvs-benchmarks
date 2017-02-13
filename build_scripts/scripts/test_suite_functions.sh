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
#   - Julien Adam <adamj@paratools.com>                                   #
#   - VALAT Sebastien sebastien.valat@cea.fr                           #
#                                                                      #
########################################################################

######################## DEFAULTS #######################
COMMON_FILES=""
TEST_ARGS=""

#################### SOURCES FILES ######################
. ${PCVS_INTERNALS_DIR}/scripts/common_functions.sh
. ${PCVS_INTERNALS_DIR}/scripts/helper_functions.sh

################## LOAD CONFIG FILE #####################
. ${PCVS_INTERNALS_DIR}/scripts/load_config.sh

############## SINGLE CONFIGURATION LOADING #############
# Parameters :
#    - $* : List of files, if empty, try to find by using *.cpp, *.c and *.C
# Globals    :
#  - COMPILER : Define the compiler to use. If not defined, try to find the good one depending on first file language.
#  - CC : C compiler to use
#  - CXX : CXX compiler to use.
#  - F77 : Fotran compiler to use.
#  - MPC_TEST_EXE_PATH : To know where to put the output file
#  - CFLAGS
#  - LDFLAGS
#  - COMMON_FILES : Extra source files to add to compile with all tests
#  - EXPECTED_EXIT_CODE
#  - MPCRUN
#  - MPCRUN_ARGS
#  - MPC_AUTO_KILL_TIMEOUT
#  - MPCRUN_CONFIGURATION : Will be added to MPCRUN_ARGS, used to define -n/-p ....
#  - TEST_ARGS
single_configuration()
{
	#if not files in params, try to find
	if [ -z "$*" ]; then
		files="$(helper_wildcard_test_list)"
	else
		files="$(helper_make_absolute_source_paths $*)"
	fi

	#check errors
	if [ -z "${files}" ]; then
		common_print_error "No source files found in ${PCVS_CSOURCE_DIR}"
		exit 1
	fi

	#ensure to use all core for OpenMP tests, otherwise srun may allocate only 1 core
	MPCRUN_ARGS="-c=${CLUSTER_MAX_CORES_PER_NODE} ${MPCRUN_ARGS}"

	#loop on all tests
	for test_file in ${files}
	do
		#calc test name by removing file extention
		test_name=$(basename ${test_file} | sed -r -e 's/\.[a-zA-Z0-9+]+$//g')
		test_exe="${MPC_TEST_EXE_PATH}/${test_name}"

		#generate compile command
		helper_gen_compile_command "${test_exe}" "${test_file}" "${COMMON_FILES}"
		compile_command="${command}"

		#generate execution command
		helper_gen_exec_command "${test_exe}" "${MPCRUN_CONFIGURATION}"
		exec_command="${command}"

		#insert the rule
		common_insert_test "test_${test_name}" "${EXPECTED_EXIT_CODE}" "${compile_command} && ${exec_command} ${TEST_ARGS}"
	done
}

############## MULTIPLE CONFIGURATION LOADING #############
# For one file on test with specific options to mpcrun
# Parameters :
#    - $1 : Test name
#    - $2 : Source files
#    - $3 : mpcrun arguements (in addition to MPCRUN_ARGS)
#    - $4 ! number of resources (1 by default)
# Globals    :
#  - COMPILER : Define the compiler to use. If not defined, try to find the good one depending on first file language.
#  - CC : C compiler to use
#  - CXX : CXX compiler to use.
#  - F77 : Fotran compiler to use.
#  - MPC_TEST_EXE_PATH : To know where to put the output file
#  - CFLAGS
#  - LDFLAGS
#  - COMMON_FILES : Extra source files to add to compile with all tests
#  - EXPECTED_EXIT_CODE
#  - MPCRUN
#  - MPCRUN_ARGS
#  - MPC_AUTO_KILL_TIMEOUT
#  - MPCRUN_CONFIGURATION : Will be added to MPCRUN_ARGS, used to define -n/-p ....
specific_configuration()
{
	#calc test name by removing file extention
	test_name="$1"
	test_exe="${MPC_TEST_EXE_PATH}/${test_name}"

	#generate compile command
	helper_gen_compile_command "${test_exe}" "${2}" "${COMMON_FILES}"
	compile_command="${command}"

	#generate execution command
	helper_gen_exec_command "${test_exe}" "${3}"
	exec_command="${command}"

	#insert the rule
	common_insert_test "test_${test_name}" "${EXPECTED_EXIT_CODE}" "${compile_command} && ${exec_command}" "$4"
}


############## OPENMP CONFIGURATION LOADING #############
# Parameters :
#    - $1 : List of files, if empty, try to find by using *.cpp, *.c and *.C
#    - $2 : additionnal argument
# Globals    :
#  - COMPILER : Define the compiler to use. If not defined, try to find the good one depending on first file language.
#  - CC : C compiler to use
#  - CXX : CXX compiler to use.
#  - F77 : Fotran compiler to use.
#  - MPC_TEST_EXE_PATH : To know where to put the output file
#  - CFLAGS
#  - LDFLAGS
#  - COMMON_FILES : Extra source files to add to compile with all tests
#  - EXPECTED_EXIT_CODE
#  - MPCRUN
#  - MPCRUN_ARGS
#  - MPC_AUTO_KILL_TIMEOUT
#  - MPC_OPENMP_THREAD_LIST : list for OpenMP threads
#  - 
openmp_configuration()
{
	#if not files in params, try to find
	if [ -z "$2" ]; then
		files="$(helper_wildcard_test_list)"
	else
		files="$(helper_make_absolute_source_paths $2)"
	fi

	#check errors
	if [ -z "${files}" ]; then
		common_print_error "No source files found in ${PCVS_CSOURCE_DIR}"
		exit 1
	fi

	#loop on all tests
	for test_file in ${files}
	do

		#calc test name by removing file extention
		test_name=$(basename ${test_file} | sed -r -e 's/\.[a-zA-Z0-9+]+$//g')
    	test_exe="${MPC_TEST_EXE_PATH}/${test_name}_${1}"

		#generate compile command
		helper_gen_compile_command "${test_exe}" "${test_file}" "${COMMON_FILES}"


		#insert the rule
		common_insert_test "compile_${test_name}_${1}" "0" "$command"

		for openmp_thread in ${MPC_OPENMP_THREAD_LIST}
		do
			for thread in ${MPC_THREAD_LIST}
			do
				#gen options
				opts="--clean --autokill=${MPC_AUTO_KILL_TIMEOUT} -N=1 -n=1 -p=1 -net=tcp -m=${thread}"
				#gen command
				command="OMP_NUM_THREADS=$openmp_thread ${MPCRUN} ${opts} ${MPCRUN_ARGS} ${test_exe} ${COMMAND_OPTIONS}"
			
				#insert test
				common_insert_test "test_${test_name}_${1}_${openmp_thread}_${thread}" "${EXPECTED_EXIT_CODE}" "$command" "compile_${test_name}_${1}"
			done
		done
	done
}

################ SET OPENMP THREAD LIST
# set list of possible threads value with OpenMP tests
# No arguments
set_openmp_threads_list()
{
	if [ -z "$MPC_OPENMP_THREAD_LIST" ] 
	then
		#NCORES=`cat /proc/cpuinfo | egrep "core id|physical id" | tr -d '\n' | sed s/physical/\\\nphysical/g | grep -v "^$" | sort | uniq | wc -l`
		MPC_OPENMP_THREAD_LIST="`seq 1 ${CLUSTER_MAX_CORES_PER_NODE}`"
		#MPC_OPENMP_THREAD_LIST="1 ${MPC_OPENMP_THREAD_LIST}"
	fi
}

############## MULTIPLE CONFIGURATION LOADING #############
# Parameters :
#    - $* : List of files, if empty, try to find by using *.cpp, *.c and *.C
# Globals    :
#  - COMPILER : Define the compiler to use. If not defined, try to find the good one depending on first file language.
#  - CC : C compiler to use
#  - CXX : CXX compiler to use.
#  - F77 : Fotran compiler to use.
#  - MPC_TEST_EXE_PATH : To know where to put the output file
#  - CFLAGS
#  - LDFLAGS
#  - COMMON_FILES : Extra source files to add to compile with all tests
#  - EXPECTED_EXIT_CODE
#  - MPCRUN
#  - MPCRUN_ARGS
#  - MPC_AUTO_KILL_TIMEOUT
#  - MPC_NET_LIST
#  - MPC_THREAD_LIST
#  - MPC_PROC_NB_LIST
#  - MPC_TASK_NB_LIST
#  - TEST_ARGS
multiple_configuration()
{
	#if not files in params, try to find
	if [ -z "$*" ]; then
		files="$(helper_wildcard_test_list)"
	else
		files="$(helper_make_absolute_source_paths $*)"
	fi

	#check errors
	if [ -z "${files}" ]; then
		common_print_error "No source files found in ${PCVS_CSOURCE_DIR}"
		exit 1
	fi

	#loop on all tests
	for test_file in ${files}
	do
		#calc test name by removing file extention
		test_name=$(basename ${test_file} | sed -r -e 's/\.[a-zA-Z0-9+]+$//g')
		test_exe="${MPC_TEST_EXE_PATH}/${test_name}"

		#generate compile command
		helper_gen_compile_command "${test_exe}" "${test_file}" "${COMMON_FILES}"

		#insert the rule
		common_insert_test "compile_${test_name}" "0" "$command"

		#loop on all configurations
		helper_configuration_loop "test_${test_name}" "${test_exe} ${TEST_ARGS}" "compile_${test_name}"
	done
}

############## MAKEFILE CONFIGURATION LOADING #############
# create test rules from a Makefile
# - $@ : Extra Make arguments, provided by testSuite_config (like target, by example)
# - TESTLIST    :  List of test names to compile and execute
# - CFLAGS      :  Compilation flags to MPC
makefile_configuration()
{
	make clean -C ${PCVS_CSOURCE_DIR}/ 2> /dev/null >&2
	
	if test "$#" = "0";
	then
		test_name="$(basename ${PCVS_CSOURCE_DIR})"
	else
		test_name="$1"
		shift;
	fi
	#source a potential configuration file
	#duplicate from helper_functions.sh (helper_get_compiler not executed in case of Makefile)
	if test -n "${PCVS_COMPILER}"; then
		file=$PCVS_INTERNALS_DIR/configuration/compilers/${PCVS_COMPILER}.conf

		if test -f ${file}; then
			. ${file}
		fi
	fi
	common_insert_test "${test_name}" "0" "make -C ${PCVS_CSOURCE_DIR}/ $@ TS_CC=\"${TS_CC}\" TS_CXX=\"${TS_CXX}\" TS_F77=\"${TS_F77}\" TS_NV=\"${TS_NV}\" TS_CFLAGS=\"${TS_CFLAGS}\" TS_LDFLAGS=\"${TS_LDFLAGS}\""
}

####################### SUBDIR CASE #######################
# Loop over all subdirs to apply rules
# - $1 : specific list of subdirectories to manage
# SUBDIRS : all subdirectories to manage, must be relative to current
subdir_configuration()
{
	if [ -z "$1" ] ; then
		SUBDIRS=$(find "$PCVS_CSOURCE_DIR/" -maxdepth 1 -xtype d | sed -e '1d' -e 's#^\./##g')
		SUBDIRS=$(find "$PCVS_CSOURCE_DIR/" -maxdepth 1 -xtype d | sed -e "s@$PCVS_CSOURCE_DIR\/@@g")
	fi

	for subdir in ${SUBDIRS}
	do
		${PCVS_INTERNALS_DIR}/generation/gen_list_of_tests "${subdir}"
	done
}
