#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <aio.h>

#define FILE_SIZE 4294967296llu
#define REQ_COUNT 8192

int sctk_aio_lio_write_read()
{
	int fd = open("./foo3.dat", O_WRONLY | O_CREAT , S_IRUSR | S_IWUSR);

	if( fd < 0 )
	{
		perror("fd");
		return 1;
	}
	
	//ftruncate( fd, FILE_SIZE );

	unsigned char * buffer = malloc( FILE_SIZE * sizeof( char ) );
	
	if( !buffer )
	{
		perror("malloc");
		return 1;
	}
	
	unsigned long long int i;

	
	printf("Initializing buffer content ....");
	fflush( stdout );	
	for( i = 0 ; i < FILE_SIZE ; i++ )
	{
		buffer[i] = i % 256;
	}
	printf("DONE\n");
	
	/* Prepare a request wave */
	struct aiocb *  aiocb = calloc( REQ_COUNT, sizeof(struct aiocb ) );
	
	if( !aiocb )
	{
		perror("malloc");
		return 1;
	}
	
	struct aiocb * aiocblist[REQ_COUNT];

	
	for( i = 0 ; i < REQ_COUNT ; i++ )
	{
		aiocblist[i] = &aiocb[i];
	}

	
	ssize_t block_size = FILE_SIZE / REQ_COUNT;
	ssize_t remainder = FILE_SIZE % REQ_COUNT;
	
	printf("Initializing aiocbp ....");
	fflush( stdout );

	for( i = 0 ; i < REQ_COUNT ; i++ )
	{
		struct aiocb * current = &aiocb[i];
		
		current->aio_fildes = fd;
		current->aio_buf = buffer + block_size * i;
		current->aio_offset = block_size * i;
		current->aio_lio_opcode = LIO_WRITE;
		
		if( i == (REQ_COUNT - 1) )
		{
			current->aio_nbytes = block_size + remainder;
		}
		else
		{
			current->aio_nbytes = block_size;
		}
	
	}
	
	printf("DONE\n");
	
	printf("Starting parallel IO WRITE ....");
	fflush( stdout );
	
	sctk_aio_lio_listio( LIO_WAIT , aiocblist, REQ_COUNT, NULL );
	
	/* Now they all should be done */
	for( i = 0 ; i < REQ_COUNT ; i++ )
	{
		int status = sctk_aio_error( &aiocb[i] );
		
		if( status != 0 )
		{
			printf("Error at offset %llu\n", i );
			return 1;
		}
		
	}
	
	printf("DONE\n");

	close( fd );
	
	
	printf("Emptying buffer content ....");
	memset( buffer , 0 , FILE_SIZE );
	printf("DONE\n");
	

	fd = open("./foo3.dat", O_RDONLY | O_CREAT , S_IRUSR | S_IWUSR);

	if( fd < 0 )
	{
		perror("fd");
		return 1;
	}
	
	printf("Initializing aiocbp for READ....");
	fflush( stdout );
	
	for( i = 0 ; i < REQ_COUNT ; i++ )
	{
		struct aiocb * current = &aiocb[i];
		
		current->aio_fildes = fd;
		current->aio_buf = buffer + block_size * i;
		current->aio_offset = block_size * i;
		current->aio_lio_opcode = LIO_READ;
		
		if( i == (REQ_COUNT - 1) )
		{
			current->aio_nbytes = block_size + remainder;
		}
		else
		{
			current->aio_nbytes = block_size;
		}
	
	}
	
	printf("DONE\n");
	
	printf("Starting parallel IO READ ....");
	fflush( stdout );
	
	sctk_aio_lio_listio( LIO_WAIT , aiocblist, REQ_COUNT, NULL );
	
	/* Now they all should be done */
	for( i = 0 ; i < REQ_COUNT ; i++ )
	{
		int status = sctk_aio_error( &aiocb[i] );
		
		if( status != 0 )
		{
			printf("Error at offset %llu\n", i );
			return 1;
		}
		
	}

	printf("DONE\n");
	
	printf("Checking buffer content ....");
	fflush( stdout );	
	for( i = 0 ; i < FILE_SIZE ; i++ )
	{
		if( buffer[i] != (i % 256) )
		{
			printf("Error at offset %llu (%d instead of %llu)\n", i, buffer[i], i % 256 );
			return 1;
		}
	}
	printf("DONE\n");

	free( buffer );
	free( aiocb );
	
	unlink( "./foo3.dat" );




	return 0;
}



int main( int argc, char ** argv )
{
	if( sctk_aio_lio_write_read() )
		return 1;
	
	sctk_aio_threads_release();
	
	return 0;
}
