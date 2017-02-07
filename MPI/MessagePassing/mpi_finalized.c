#include <mpi.h>
#include <assert.h>
int main(int argc, char** argv){
	int flag; 
	MPI_Init(&argc,&argv);

	MPI_Finalized(&flag);
	assert(flag == 0);

	MPI_Finalize();

	MPI_Finalized(&flag);
        assert(flag == 1);

	return 0;
}
