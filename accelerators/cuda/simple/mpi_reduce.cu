#include <assert.h>
#include <cuda_runtime.h>
#include <mpi.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define safe_cudart(u)                                                         \
	if (u != cudaSuccess) {                                                    \
		fprintf(stderr, "Error %d: %s\n", __LINE__, cudaGetErrorString(u));   \
		abort();                                                               \
	}

int rank = -1;

	__global__
void square(int* in, int* out)
{
	(*out) = (*in) * (*in);
}

int main(int argc, char **argv)
{
	int comm_size;

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	
	int *in, *out, *gin, *gout, check = 0, reduce = 0;

	in  = (int*) malloc(sizeof(int));
	out = (int*) malloc(sizeof(int));

	safe_cudart(cudaMalloc((void**)&gin, sizeof(int)));
	safe_cudart(cudaMalloc((void**)&gout, sizeof(int)));
	
	*in = rank;
	*out = -1;

	safe_cudart(cudaMemcpy(gin, in, sizeof(int), cudaMemcpyHostToDevice));

	/* compute the square of the current rank */
	square<<<1, 1>>>(gin, gout);

	safe_cudart(cudaMemcpy(out, gout, sizeof(int), cudaMemcpyDeviceToHost));

	safe_cudart(cudaFree(gin));
	safe_cudart(cudaFree(gout));

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
