#include <mpi.h>
#include <cstdlib>
#include <iostream>
#include <sys/syscall.h>

using namespace std;

#define gettid() syscall(SYS_gettid)
int privatize(gettid());

int main( int argc, char **argv )
{
	int rank, size;
	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );

	int myid = gettid();
	fprintf(stderr, "rank %d(%p) %d == %d ?\n", rank, &privatize, privatize, myid);
	
	if(myid != privatize)
	{
		cout << "PRIVATIZATION FAILURE\n" << endl;
		abort();
	}
	
	MPI_Barrier(MPI_COMM_WORLD);
	if(rank == 0)
		cout << "PRIVATIZATION SUCCESS\n" << endl;
	MPI_Finalize();
	return EXIT_SUCCESS;
}
