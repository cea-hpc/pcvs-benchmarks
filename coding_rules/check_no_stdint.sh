#!/bin/sh

#get arguments
MPC_SOURCE_DIR="$1"

#check args
if [ ! -d "${MPC_SOURCE_DIR}" ]; then
	echo "Missing parameter" 1>&2
	echo "Usage : $0 {path_to_mpc_source_dir}" 1>&2
	exit 1
fi

#check mpc subdirectory
if [ ! -d "${MPC_SOURCE_DIR}/mpcframework" ]; then
	echo "Cannot find mpc subdirectory in source directory $MPC_SOURCE_DIR"
	exit 1
fi

#search usage of stdint types in sources
#If in future their is too much trouble with this checking-approach, we can
#also redefine uint*_t in sctk_stdint.h if stdinh.is not present, but it
#may be harder to avoid definition conflicts on every platform.
res="`egrep --color -wRn '(u)?int[0-9]{1,2}_t' ${MPC_SOURCE_DIR}/mpcframework | grep -v 'sctk_basic/sctk_stdint.h'`"

if [ ! -z "$res" ]; then
	echo "TEST FAILURE, MPC sources use non C99 types from stdint.h" 1>&2
	echo "Please replace by sctk_stdint.h, and use types sctk_uint64_t" 1>&2
	echo "---------------------------------------------------------" 1>&2
	echo "$res" 1>&2
	echo "---------------------------------------------------------" 1>&2
	exit 1
else
	echo "No use of stdint.h ................................... OK"
fi

