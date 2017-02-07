//#include <mpc.h>
#include <mpi.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

MPC_Datatype tabType[]={
	MPC_INTEGER1,MPC_INTEGER2,MPC_INTEGER4,MPC_INTEGER8,
	MPI_INTEGER1,MPI_INTEGER2,MPI_INTEGER4,MPI_INTEGER8,
	MPC_REAL4,MPC_REAL8,MPC_REAL16,
	MPI_REAL4,MPI_REAL8,MPI_REAL16
};

void check_type_size(){

	int typeSize = 0;
	MPI_Type_size(tabType[0], &typeSize);
	assert(typeSize == sizeof(int8_t));
	MPI_Type_size(tabType[1], &typeSize);
	assert(typeSize == sizeof(int16_t));
	MPI_Type_size(tabType[2], &typeSize);
	assert(typeSize == sizeof(int32_t));
	MPI_Type_size(tabType[3], &typeSize);
	assert(typeSize == sizeof(int64_t));
	MPI_Type_size(tabType[4], &typeSize);
	assert(typeSize == sizeof(int8_t));
	MPI_Type_size(tabType[5], &typeSize);
	assert(typeSize == sizeof(int16_t));
	MPI_Type_size(tabType[6], &typeSize);
	assert(typeSize == sizeof(int32_t));
	MPI_Type_size(tabType[7], &typeSize);
	assert(typeSize == sizeof(int64_t));
	MPI_Type_size(tabType[8], &typeSize);
	assert(typeSize == sizeof(float));
	MPI_Type_size(tabType[9], &typeSize);
	assert(typeSize == sizeof(double));
	MPI_Type_size(tabType[10], &typeSize);
	assert(typeSize == sizeof(long double));
	MPI_Type_size(tabType[11], &typeSize);
	assert(typeSize == sizeof(float));
	MPI_Type_size(tabType[12], &typeSize);
	assert(typeSize == sizeof(double));
	MPI_Type_size(tabType[13], &typeSize);
	assert(typeSize == sizeof(long double));
}
int main(int argc, char ** argv){

	const int NBELEM=16*2*4;
	const int NBTYPES=14;
	char buffer[NBELEM];
	int rank, i, j, typeSize=0;

	check_type_size();
	
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	for(i=0; i < NBTYPES; i++)
	{
		if(rank == 0)
		{
			for(j=0; j < NBELEM; j++)
				buffer[j] = 255;
			MPI_Send((void*)buffer, 4, tabType[i], 1, 1, MPI_COMM_WORLD);
		}
		else
		{
			printf("Checking type %d/14\t Type ID = %d\n", i, tabType[i]);
			memset(buffer,0, NBELEM);
			MPI_Recv((void*)buffer, 4, tabType[i], 0, 1, MPI_COMM_WORLD, NULL);
			MPI_Type_size(tabType[i], &typeSize);

			for(j=0; j < 4*typeSize; j++){
				assert(buffer[j] = 255);
			}
			assert(buffer[4*typeSize] == 0);
		}
	}
	MPI_Finalize();
	if(rank == 0) printf("\nALL PASSED !\n");
	return 0;
}
