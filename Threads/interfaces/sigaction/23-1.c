/*
* Copyright (c) 2005, Bull S.A..  All rights reserved.
* Created by: Sebastien Decugis

* This program is free software; you can redistribute it and/or modify it
* under the terms of version 2 of the GNU General Public License as
* published by the Free Software Foundation.
*
* This program is distributed in the hope that it would be useful, but
* WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*
* You should have received a copy of the GNU General Public License along
* with this program; if not, write the Free Software Foundation, Inc., 59
* Temple Place - Suite 330, Boston MA 02111-1307, USA.


* This sample test aims to check the following assertions:
*
* If SA_NODEFER is not set in sa_flags, the caught signal is added to the 
* thread's signal mask during the handler execution.

* The steps are:
* -> register a signal handler for SIGABRT
* -> raise SIGABRT
* -> In handler, check for reentrance then raise SIGABRT again.

* The test fails if signal handler if reentered or signal is not pending when raised again.
*/


/* We are testing conformance to IEEE Std 1003.1, 2003 Edition */
#define _POSIX_C_SOURCE 200112L

/******************************************************************************/
/*************************** standard includes ********************************/
/******************************************************************************/
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <signal.h>
#include <errno.h>

/******************************************************************************/
/***************************   Test framework   *******************************/
/******************************************************************************/
#include "testfrmw.h"
#include "testfrmw.c" 
/* This header is responsible for defining the following macros:
 * UNRESOLVED(ret, descr);  
 *    where descr is a description of the error and ret is an int 
 *   (error code for example)
 * FAILED(descr);
 *    where descr is a short text saying why the test has failed.
 * PASSED();
 *    No parameter.
 * 
 * Both three macros shall terminate the calling process.
 * The testcase shall not terminate in any other maneer.
 * 
 * The other file defines the functions
 * void output_init()
 * void output(char * string, ...)
 * 
 * Those may be used to output information.
 */

/******************************************************************************/
/**************************** Configuration ***********************************/
/******************************************************************************/
#ifndef VERBOSE
#define VERBOSE 1
#endif

#define SIGNAL SIGABRT

/******************************************************************************/
/***************************    Test case   ***********************************/
/******************************************************************************/

int called = 0;

void handler( int sig )
{
	int ret;
	sigset_t pending;
	called++;

	if ( called == 2 )
	{
		FAILED( "Signal was not masked in signal handler" );
	}

	if ( called == 1 )
	{

		/* Raise the signal again. It should be masked */
		//ret = raise( SIGNAL );
		ret = pthread_kill(pthread_self(), SIGNAL );

		if ( ret != 0 )
		{
			UNRESOLVED( ret, "Failed to raise SIGABRT again" );
		}

		/* check the signal is pending */
		ret = sigpending( &pending );

		if ( ret != 0 )
		{
			UNRESOLVED( ret, "Failed to get pending signal set" );
		}

		ret = sigismember( &pending, SIGNAL );

		if ( ret != 1 )
		{
			FAILED( "signal is not pending" );
		}
	}

	called++;
	printf("Thread exiting..\n");
}

/* main function */
int main()
{
	int ret;

	struct sigaction sa;

	/* Initialize output */
	output_init();

	/* Set the signal handler */
	sa.sa_flags = 0;

	sa.sa_handler = handler;

	ret = sigemptyset( &sa.sa_mask );

	if ( ret != 0 )
	{
		UNRESOLVED( ret, "Failed to empty signal set" );
	}

	/* Install the signal handler for SIGABRT */
	ret = sigaction( SIGNAL, &sa, 0 );

	if ( ret != 0 )
	{
		UNRESOLVED( ret, "Failed to set signal handler" );
	}

	//ret = raise( SIGNAL );
	ret = pthread_kill(pthread_self(), SIGNAL );

	if ( ret != 0 )
	{
		UNRESOLVED( ret, "Failed to raise SIGABRT" );
	}
	int i = 0;
	while ( called != 4 ){
		if( i > 5 ) abort();
		i++;
		sched_yield();
		}

	/* Test passed */
#if VERBOSE > 0

	output( "Test passed\n" );

#endif

	PASSED;
}
