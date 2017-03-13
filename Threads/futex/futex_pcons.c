/*############################# MPC License ##############################*/
/*# Wed Nov 19 15:19:19 CET 2008                                         #*/
/*# Copyright or (C) or Copr. Commissariat a l'Energie Atomique          #*/
/*#                                                                      #*/
/*# IDDN.FR.001.230040.000.S.P.2007.000.10000                            #*/
/*# This file is part of the MPC Runtime.                                #*/
/*#                                                                      #*/
/*# This software is governed by the CeCILL-C license under French law   #*/
/*# and abiding by the rules of distribution of free software.  You can  #*/
/*# use, modify and/ or redistribute the software under the terms of     #*/
/*# the CeCILL-C license as circulated by CEA, CNRS and INRIA at the     #*/
/*# following URL http://www.cecill.info.                                #*/
/*#                                                                      #*/
/*# The fact that you are presently reading this means that you have     #*/
/*# had knowledge of the CeCILL-C license and that you accept its        #*/
/*# terms.                                                               #*/
/*#                                                                      #*/
/*# Authors:                                                             #*/
/*#   - CARRIBAULT Patrick patrick.carribault@cea.fr                     #*/
/*#   - PERACHE Marc marc.perache@cea.fr                                 #*/
/*#                                                                      #*/
/*########################################################################*/

#include <stdio.h>
#include <linux/futex.h>
#include <sys/time.h>
#include <sys/syscall.h>
#include <errno.h>
#include <stdlib.h>
#include <pthread.h>

int val = 0;

static int futex(void *addr1, int op, int val1, struct timespec *timeout, void *addr2, int val3)
{
	return syscall(SYS_futex, addr1, op, val1, timeout, addr2, val3);
}
	
unsigned long int  hits = 0;

void * producer( void * arg )
{
	printf("producer starting \n");

	int count = 0;

	while(hits < 300)
	{
		count=50;
		int i;
		for( i = 0 ; i < 3 ; i ++ )
		{
			sleep(1);
			printf(" %d", i);
			fflush(stdout);
		}
		

		//printf("\n");
		//printf("Procucer Trigger (over %d)\n", count);

		futex(&val, FUTEX_WAKE, count, NULL, NULL, 0);
	}
	
}

void * consumer( void * arg )
{
	int * v = (int *)arg;
	int id = *v;
	free( v );
	
	printf("Consumer starting consumer %d \n", id);

	
	while(hits < 300 )
	{
		int ret = futex(&val, FUTEX_WAIT, 0, NULL, NULL, 0);
	
		if( ret < 0 )
		{
			if( errno == EWOULDBLOCK )
			{
				printf("WB\n");
			}
			perror("WAIT");
			continue;
		}
	
		if( ret == 0 )
		{
			
			hits ++;
			

			printf("\tConsumer HIT inside %d\n", id);
				
			val = 0;
			
		}
		
	}
	
}

#define NUM 50


int main( int argc, char ** argv )
{
	
	pthread_t producers;
	
	pthread_t consumers[NUM];
	
	pthread_create( &producers, NULL, producer, NULL );

	int i;
	
	for( i = 0 ; i < NUM ; i++ )
	{
		printf("LAUNCH %d\n", i );
		int * v = malloc(sizeof(int) );
		*v = i;
		pthread_create( &consumers[i], NULL, consumer, (void *)v );
		
	}

	
	pthread_join( producers, NULL );

	futex(&val, FUTEX_WAKE, INT_MAX, NULL, NULL, 0);

	for( i = 0 ; i < NUM ; i++ )
	{
		pthread_join( consumers[i], NULL );
	}
	
	
	
	return 0;
}
