#include <aio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#define FILE_SIZE 134217728llu

int AIO_write_Test()
{
	int fd = open("./foo.dat", O_WRONLY | O_CREAT , S_IRUSR | S_IWUSR);

	if( fd < 0 )
	{
		perror("fd");
		return 1;
	}

	int * buffer = malloc( FILE_SIZE * sizeof( int ) );

	if( !buffer )
	{
		perror("malloc");
		return 1;
	}
	
	/* Fill in data */
	printf("Initializing data... ");
	fflush( stdout );
	unsigned long long int i;

	for( i = 0 ; i < FILE_SIZE ; i++ )
	{
		buffer[i] = i % 4096;
	}
	printf("DONE\n");

	/* Prepare AIO handle for WRITE */
	struct aiocb *  aiocb = calloc( 1, sizeof(struct aiocb ) );

	aiocb->aio_fildes = fd;

    aiocb->aio_buf = buffer;
    aiocb->aio_nbytes = FILE_SIZE * sizeof( int );
    aiocb->aio_offset = 0;
    aiocb->aio_lio_opcode = LIO_WRITE;

	int res = sctk_aio_write(aiocb);
	
	if( res != 0 )
	{
		perror("aio_write64");
		return 1;
	}
	
	
	// wait until the request has finished
	while(sctk_aio_error(aiocb) == EINPROGRESS)
	{
		//printf("WORKING\n");
	}

	res = sctk_aio_error(aiocb);
	ssize_t size = sctk_aio_return(aiocb);
	
	printf("Wrote %ld\n", size );
	
	if( res != 0 )
	{
		printf(" Error at aio_error() : %s\n", strerror (res));
		return 1;
	}
	
	close( fd );
	
	fd = open("./foo.dat", O_RDONLY | O_CREAT , S_IRUSR | S_IWUSR);

	if( fd < 0 )
	{
		perror("fd");
		return 1;
	}	

	/* Empty buffer */
	for( i = 0 ; i < FILE_SIZE ; i++ )
	{
		buffer[i] = 0;
	}

	/* Prepare AIO handle for READ */
	
	aiocb->aio_fildes = fd;

    aiocb->aio_buf = buffer;
    aiocb->aio_nbytes = FILE_SIZE * sizeof( int );
    aiocb->aio_offset = 0;
    aiocb->aio_lio_opcode = LIO_READ;

	res = sctk_aio_read(aiocb);

	if( res != 0 )
	{
		perror("aio_read");
		return 1;
	}
	

	// wait until the request has finished
	while(sctk_aio_error(aiocb) == EINPROGRESS)
	{
		//printf("WORKING\n");
	}

	res = sctk_aio_error(aiocb);
	size = sctk_aio_return(aiocb);
	
	printf("Read %ld\n", size );
	
	if( res != 0 )
	{
		printf(" Error at aio_error() : %s\n", strerror (res));
		return 1;
	}
	
	printf("Checking data... ");
	fflush( stdout );

	for( i = 0 ; i < FILE_SIZE ; i++ )
	{
		if( buffer[i] != (i % 4096) )
		{
			printf("Error at offset %llu ( exp %d got %d)\n", i , ((int)i % 4096), buffer[i]);
		}
	}
	printf("DONE\n");
	
	

	free( buffer );
	close( fd );
	
	unlink( "foo.dat" );
	
	return 0;
}



int main( int argc, char ** argv )
{
	return AIO_write_Test();
}

