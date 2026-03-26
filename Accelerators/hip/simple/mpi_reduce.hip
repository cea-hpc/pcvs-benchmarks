
#include <assert.h>
#include <hip/hip_runtime.h>
#include <mpi.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define safe_hip(u)                                                         \
	if (u != hipSuccess) {                                                    \
		fprintf(stderr, "Error %d: %s\n", __LINE__, hipGetErrorString(u));   \
		abort();                                                               \
	}

__global__
void square(int* in, int* out)
{
	(*out) = (*in) * (*in);
}

int main(int argc, char **argv)
{
	int rank = -1;
	int comm_size;

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	
	int *in, *out, *gin, *gout, check = 0, reduce = 0;

	in  = (int*) malloc(sizeof(int));
	out = (int*) malloc(sizeof(int));

	safe_hip(hipMalloc((void**)&gin, sizeof(int)));
	safe_hip(hipMalloc((void**)&gout, sizeof(int)));
	
	*in = rank;
	*out = -1;

	safe_hip(hipMemcpy(gin, in, sizeof(int), hipMemcpyHostToDevice));

	/* compute the square of the current rank */
	square<<<1, 1>>>(gin, gout);
	safe_hip(hipDeviceSynchronize());

	safe_hip(hipMemcpy(out, gout, sizeof(int), hipMemcpyDeviceToHost));

	safe_hip(hipFree(gin));
	safe_hip(hipFree(gout));

	/* sum all rank squares */
	MPI_Reduce(out, &reduce, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

	MPI_Finalize();
	
	int i;
	for (i = 0; i < comm_size; ++i)
	{
		check += (i * i);
	}

	if(!rank)
	{
		if(check != reduce)
		{
			fprintf(stderr, "Error: Expected %d and get %d\n", check, reduce);
			return 1;
		}
		else
		{
			fprintf(stderr, "Test Successfull !\n");
		}
	}
	return 0;
}
