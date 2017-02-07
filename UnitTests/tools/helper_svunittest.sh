#!/bin/sh

################### HELPER_FIND_SVUNITTEST ################
# Try to find the library svunit test, if not fallback to the lightweight inner version.
# Globals:
#   - SVUNITTEST_PREFIX
helper_find_svunittest()
{
	if [ ! -z "${SVUNITTEST_PREFIX}" ]; then
		SVUNITTEST_CFLAGS="-I${SVUNITTEST_PREFIX}/include"
		SVUNITTEST_LDFLAGS="${SVUNITTEST_PREFIX}/lib/libsvunittest.a"
		HAVE_SVUNITTEST="yes"
	elif pkg-config --exists svUnitTest > /dev/null 2>&1; then
		SVUNITTEST_CFLAGS="`pkg-config --cflags svUnitTest`"
		SVUNITTEST_LDFLAGS="`pkg-config --libs svUnitTest`"
		SVUNITTEST_PREFIX="`pkg-config --variable=prefix svUnitTest`"
		HAVE_SVUNITTEST="yes"
	else
		SVUNITTEST_CFLAGS="-I${MPC_TEST_SOURCE_DIR}/UnitTests/tools/svUnitTest_fake"
		SVUNITTEST_LDFLAGS=""
		HAVE_SVUNITTEST="no"
	fi
}

#################### HELPER_USE_SVUNITTEST ################
# Try to find the library svunit test, if not fallback to the lightweight inner version.
# Globals:
#   - SVUNITTEST_PREFIX
#   - CFLAGS
#   - LDFLAGS
#   - 
helper_use_svunittest()
{
	#find svut
	helper_find_svunittest

	#set cflags and ldflags
	CFLAGS="${CFLAGS} ${SVUNITTEST_CFLAGS}"
	LDFLAGS="${LDFLAGS} ${SVUNITTEST_LDFLAGS}"
}
