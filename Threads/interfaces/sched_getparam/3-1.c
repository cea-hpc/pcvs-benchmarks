/* 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 2.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 * Test that sched_getparam() returns 0 on success
 */
#include <stdio.h>
#include <sched.h>
#include <errno.h>
#include "posixtest.h"

extern int errno;

int main(int argc, char **argv)
{	       

	struct sched_param param;
	int result = -1;

	param.sched_priority = -1;

	result = sched_getparam(0, &param);
	errno = 0;
	if(result == 0 &&
	   param.sched_priority != -1 &&
	   errno == 0) {
		printf("Test PASSED\n");
		return PTS_PASS;
	}

	if(errno != 0 ) {
		perror("Unexpected error");
		return PTS_FAIL;
	}

	if(result != 0) {
		printf("returned code is not zero.\n");
		return PTS_FAIL;
	} else {
		perror("Unresolved test error");
		return PTS_UNRESOLVED;	
	}        

	printf("This code should not be executed.\n");
        return PTS_UNRESOLVED;
}


