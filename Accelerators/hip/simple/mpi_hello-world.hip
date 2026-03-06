// hipcc hello-world.cpp -L /usr/lib/x86_64-linux-gnu -lamdhip64 -o hello-world

#include <stdio.h>
#include <hip/hip_runtime.h>
#include <mpi.h>
#include <assert.h>
#define safe_hip(u)                                                        \
	if (u != hipSuccess) {                                                 \
		fprintf(stderr, "Error %d: %s\n", __LINE__, hipGetErrorString(u)); \
		abort();                                                           \
	}

const int N = 16; 
const int blocksize = 16; 

__global__ 
void hello(char *a, int *b) 
{
	a[threadIdx.x] += b[threadIdx.x];
}

int main(int argc, char ** argv)
{
	int rank = -1;

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

	safe_hip(hipMalloc( (void**)&ad, csize ));
	safe_hip(hipMalloc( (void**)&bd, isize )); 
	safe_hip(hipMemcpy( ad, a, csize, hipMemcpyHostToDevice )); 
	safe_hip(hipMemcpy( bd, b, isize, hipMemcpyHostToDevice )); 
	
	dim3 dimBlock( blocksize, 1, 1);
	dim3 dimGrid( 1, 1, 1 );

	hello<<<dimGrid, dimBlock>>>(ad, bd);
	safe_hip(hipDeviceSynchronize());

	hipError_t err = hipGetLastError();
	if(err != hipSuccess)
	{
		printf("KERNEL FAILED: %s\n", hipGetErrorString(hipGetLastError()));
		abort();
	}

	safe_hip(hipMemcpy( a, ad, csize, hipMemcpyDeviceToHost )); 
	safe_hip(hipFree( ad ));
	safe_hip(hipFree( bd ));

	sprintf(final, "%s%s", final, a);
	fprintf(stderr, "Rank %2d: %s\n", rank, final);
	assert(strcmp(final, "Hello World!") == 0);
	MPI_Finalize();
	return EXIT_SUCCESS;
}
