// This is the REAL "hello world" for CUDA!
// It takes the string "Hello ", prints it, then passes it to CUDA
// with an array of offsets. Then the offsets are added in parallel
// to produce the string "World!"
// By Ingemar Ragnemalm 2010

// nvcc hello-world.cu -L /usr/local/cuda/lib -lcudart -o hello-world

#include <stdio.h>
#include <cuda_runtime.h>
#include <mpi.h>
#include <assert.h>
#define safe_cudart(u)                                                         \
	if (u != cudaSuccess) {                                                    \
		fprintf(stderr, "Error %d: %s\n", __LINE__, cudaGetErrorString(u));   \
		abort();                                                               \
	}

const int N = 16; 
const int blocksize = 16; 

__global__ 
void hello(char *a, int *b) 
{
	a[threadIdx.x] += b[threadIdx.x];
}

int rank = -1;

int main(int argc, char ** argv)
{
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	char a[N] = "Hello \0\0\0\0\0\0";
	char final[N];
	int b[N] = {15, 10, 6, 0, -11, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

	char *ad;
	int *bd;
	const int csize = N*sizeof(char);
	const int isize = N*sizeof(int);

	sprintf(final, "%s", a);

	safe_cudart(cudaMalloc( (void**)&ad, csize ));
	safe_cudart(cudaMalloc( (void**)&bd, isize )); 
	safe_cudart(cudaMemcpy( ad, a, csize, cudaMemcpyHostToDevice )); 
	safe_cudart(cudaMemcpy( bd, b, isize, cudaMemcpyHostToDevice )); 
	
	dim3 dimBlock( blocksize, 1, 1);
	dim3 dimGrid( 10, 1, 1 );

	hello<<<dimGrid, dimBlock>>>(ad, bd);

	cudaError_t err = cudaGetLastError();
	if(err != cudaSuccess)
	{
		printf("KERNEL FAILED: %s\n", cudaGetErrorString(cudaGetLastError()));
		abort();
	}

	safe_cudart(cudaMemcpy( a, ad, csize, cudaMemcpyDeviceToHost )); 
	safe_cudart(cudaFree( ad ));
	safe_cudart(cudaFree( bd ));

	sprintf(final, "%s%s", final, a);
	fprintf(stderr, "Rank %2d: %s\n", rank, final);
	assert(strcmp(final, "Hello World!") == 0);
	MPI_Finalize();
	return EXIT_SUCCESS;
}
