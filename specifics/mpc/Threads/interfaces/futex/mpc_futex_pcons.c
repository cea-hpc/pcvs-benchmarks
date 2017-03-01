#include <stdio.h>
#include <pthread.h>
#include <linux/futex.h>
#include <sys/time.h>
#include <sys/syscall.h>
#include <errno.h>
#include <stdlib.h>


int val = 0;

int sctk_futex(void *addr1, int op, int val1, 
               struct timespec *timeout, void *addr2, int val3);

void sctk_futex_context_init();
void sctk_futex_context_release(); 
      
static int futex(void *addr1, int op, int val1, struct timespec *timeout, void *addr2, int val3)
{
	return sctk_futex(addr1, op, val1, timeout, addr2, val3);
}
	
unsigned long int  hits = 0;

void * producer( void * arg )
{
	printf("producer starting \n");

	int count = 1;

	while(hits < 10000)
	{
		count*=2;
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

	
	while(hits < 10000 )
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
	
	/* In one leaves make sure that all the others too */
	futex(&val, FUTEX_WAKE, INT_MAX, NULL, NULL, 0);
	
}

#define NUM 500


int main( int argc, char ** argv )
{
	sctk_futex_context_init();
	
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


	for( i = 0 ; i < NUM ; i++ )
	{
		pthread_join( consumers[i], NULL );
	}
	
	
	sctk_futex_context_release();
	
	return 0;
}
