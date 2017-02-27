#!/bin/sh
############################################################################
#
# Run the ${MPITEST_TESTNAME} test
#
# Usage:  ${MPITEST_TESTNAME}.sh
#
# MPI operator test
#
############################################################################
echo ""
echo "*****  BEGIN ${MPITEST_TESTNAME} TEST  *****"
echo ""

${MPITEST_RM:-rm} -f $MPITEST_SCRATCH_FILE
${MPITEST_TOUCH:-touch} $MPITEST_SCRATCH_FILE

#
#  Check essential environment variables
#
if test -z "$MPITEST_IS_ENV_DEF" > ${MPITEST_DEVNULL:-/dev/null}
then
	echo "MPITEST environment variables are not defined, terminating test, ${MPITEST_FAIL:-FAILED}" >> ${MPITEST_SCRATCH_FILE:-scratch}
	${MPITEST_CAT:-cat} ${MPITEST_SCRATCH_FILE:-scratch}
	exit 4
fi
. $MPITEST_SHLIB

#
# Some pre processing(s) add here
#

#
# Get start time
#
GetStartTime

#
#  Run test
#

$MPITEST_BINDIR/startjob.$MPITEST_ARCH -appname node.${MPITEST_ARCH}${MPITEST_USERTAG}_bx $MPITEST_LOCAL_OPTIONS > ${MPITEST_SCRATCH_FILE:-scratch} 2>&1

#
# Get run info
#
GetRunInfo

#
# Capture application output
#
${MPITEST_CAT:-cat} ${MPITEST_SCRATCH_FILE:-scratch}

#
#  Report test results
#

echo ""

expectedPassCnt=1
actualPassCnt=0

if $MPITEST_GREP "$MPITEST_FAIL" ${MPITEST_SCRATCH_FILE:-scratch} > $MPITEST_DEVNULL
then
	echo "TEST_RESULT: F (found $MPITEST_FAIL) $runInfo"
else
	actualPassCnt=`$MPITEST_GREP -c "$MPITEST_PASS" ${MPITEST_SCRATCH_FILE:-scratch}`
	if test "$actualPassCnt" != "$expectedPassCnt" > $MPITEST_DEVNULL
	then
		echo "TEST_RESULT: F (wrong $MPITEST_PASS count, Expected: $expectedPassCnt, Actual: $actualPassCnt) $runInfo"
	else
		echo "TEST_RESULT: P $runInfo"
	fi
fi

echo ""
echo "*****  END ${MPITEST_TESTNAME} TEST  *****"
echo ""
