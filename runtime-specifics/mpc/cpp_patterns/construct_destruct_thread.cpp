#include <iostream>
#include <mpi.h>
#define CRASH() ((void*)(*)()0x0)() 
int a;
int rank;

class A
{
	private:
		int i;
	public:
		A(size_t val) {a = 10; printf("CONSTRUCT(%d) %p\n", rank, (void*)this);}
		~A() {a = 20; printf("DESTRUCT(%d) %p\n", rank, (void*)this); }
};

A obj(20);

void* run(void* args)
{
	printf("I'm a thread(%d) -> %p\n", rank, &a);
}

int main(int argc, char *argv[])
{
	//A obj2(10);
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	printf("obj(%d) = %p\n", rank, (void*)(&obj));

	pthread_t t1, t2;
	pthread_create(&t1, NULL, run, NULL);
	pthread_create(&t2, NULL, run, NULL);
	pthread_join(t1, NULL);
	pthread_join(t2, NULL);
	//printf("obj2(%d) = %p\n", rank, (void*)(&obj2));
	MPI_Finalize();
	return 0;
}
