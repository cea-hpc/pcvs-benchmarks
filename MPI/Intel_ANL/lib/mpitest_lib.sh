##############################################################################
#
#  GetStartTime()
#
#  Records current system time.
#  Uses single call to date(1) to avoid boundry problems.  (Will not report
#     correct execution time if execution spans multiple years.)
#
##############################################################################
GetStartTime() {
	startTime=`$MPITEST_DATE '+%j+%H+%M+%S'`
}

##############################################################################
#
#  GetExecTime()
#
#  Sets $exec_time to the time that it took to run the test.
#  Uses the variable set by get_start_time().
#  Uses single call to date(1) to avoid boundry problems.  (Will not report
#     correct execution time if execution spans multiple years.)
#
##############################################################################
GetExecTime() {
	endTime=`$MPITEST_DATE '+%j+%H+%M+%S'`

	d1=`echo $startTime | awk -F+ ' { print $1 }'`
	h1=`echo $startTime | awk -F+ ' { print $2 }'`
	m1=`echo $startTime | awk -F+ ' { print $3 }'`
	s1=`echo $startTime | awk -F+ ' { print $4 }'`

	d2=`echo $endTime | awk -F+ ' { print $1 }'`
	h2=`echo $endTime | awk -F+ ' { print $2 }'`
	m2=`echo $endTime | awk -F+ ' { print $3 }'`
	s2=`echo $endTime | awk -F+ ' { print $4 }'`

	d=`${MPITEST_EXPR:-expr} \( $d2 - $d1 \) \* 86400`
	h=`${MPITEST_EXPR:-expr} \( $h2 - $h1 \) \* 3600`
	m=`${MPITEST_EXPR:-expr} \( $m2 - $m1 \) \* 60`

	execTime=`${MPITEST_EXPR:-expr} $d + $h + $m + \( $s2 - $s1 \)`
}

##############################################################################
#
#  GetRunInfo()
#
#  Creates a string, stored in variable $run_info, that includes the
#  following information about the test that was just run:
#  1)  Name of host on which the test was run on.
#  2)  Time that it took to run the test.
#  3)  Number of nodes that the test was run on.
#  4)  Name of the test.
#
##############################################################################

GetRunInfo() {
	GetExecTime
	runInfo="`$MPITEST_HOSTNAME` $execTime #"$MPITEST_NPROCS" $MPITEST_TESTNAME"
}
