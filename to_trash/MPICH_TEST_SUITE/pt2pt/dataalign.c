#include "test.h"
#include <stdio.h>
#include "mpi.h"

#define DEBUG

int main( int argc, char *argv[])
{
	struct a {	int	i;
			char	c;
		} s[10], s1[10];
	int j;
	int errs = 0, toterrs;
	int rank, size, tsize;
	MPI_Aint text;
	int blens[2]; 
	MPI_Aint disps[2];
	MPI_Datatype bases[2];
	MPI_Datatype str, con;
	MPI_Status status;
	MPI_Request r;

	MPI_Init( &argc, &argv );

	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );

	for( j = 0; j < 10; j ++ ) {
		s[j].i = j + rank;
		s[j].c = j + rank + 'a';
	}

	blens[0] = blens[1] = 1;
	disps[0] = 0; disps[1] = sizeof(int);
	bases[0] = MPI_INT; bases[1] = MPI_CHAR;
	MPI_Type_struct( 2, blens, disps, bases, &str );
	MPI_Type_commit( &str );
	MPI_Type_contiguous( 10, str, &con );
	MPI_Type_commit( &con );
	MPI_Type_size( con, &tsize );
	MPI_Type_extent( con, &text );

#ifdef DEBUG
	fprintf(stderr,"Size of MPI array is %d, extent is %d\n", tsize, text );
#endif

#ifdef DEBUG
        {
	void * p1, *p2;
	p1 = s;
	p2 = &(s[10].i);  /* This statement may fail on some systems */
	fprintf(stderr,"C array starts at %p and ends at %p for a length of %d\n",
		s, &(s[9].c), (char *)p2-(char *)p1 );
        }
#endif
	MPI_Type_extent( str, &text );
#ifdef DEBUG
	MPI_Type_size( str, &tsize );
	fprintf(stderr,"Size of MPI struct is %d, extent is %d\n", tsize, (int)text );
	fprintf(stderr,"Size of C struct is %d\n", sizeof(struct a) );
#endif
	if (text != sizeof(struct a)) {
	    fprintf(stderr, "Extent of struct a (%d) does not match sizeof (%d)\n",
		    (int)text, (int)sizeof(struct a) );
	    errs++;
	}

	MPI_Isend( s, 1, con, rank ^ 1, 0, MPI_COMM_WORLD,&r );
	MPI_Recv( s1, 1, con, rank ^ 1, 0, MPI_COMM_WORLD, &status );
	MPI_Wait(&r, MPI_STATUS_IGNORE);

	for( j = 0; j < 10; j++ ) {
#ifdef DEBUG	    
		fprintf(stderr,"%d Sent: %d %c, Got: %d %c\n", rank,
			s[j].i, s[j].c, s1[j].i, s1[j].c );
#endif
		if ( s1[j].i != j + status.MPI_SOURCE ) {
		    errs++;
		    fprintf(stderr, "Got s[%d].i = %d; expected %d\n", j, s1[j].i,
			    j + status.MPI_SOURCE );
		}
		if ( s1[j].c != 'a' + j + status.MPI_SOURCE ) {
		    errs++;
		    /* If the character is not a printing character, 
		       this can generate an file that diff, for example,
		       believes is a binary file */
		    if (isprint( (int)(s1[j].c) )) {
			fprintf(stderr, "Got s[%d].c = %c; expected %c\n", j, s1[j].c,
				j + status.MPI_SOURCE + 'a');
		    }
		    else {
			fprintf(stderr, "Got s[%d].c = %x; expected %c\n", j, (int)s1[j].c,
				j + status.MPI_SOURCE + 'a');
		    }
		}
	}

	MPI_Allreduce( &errs, &toterrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD );
	if (rank == 0) {
	    if (toterrs > 0) fprintf(stderr, "Found %d errors\n", toterrs );
	    else             fprintf(stderr, " No Errors\n" );
	}
	MPI_Type_free( &str );
	MPI_Type_free( &con );
	MPI_Finalize();
	return 0;
}
