#include <omp.h>

int main(void){

	int total;
	#pragma omp parallel for reduction(max:total)
	for(int i =0; i < 10; i++){
		total += omp_get_thread_num();
	}
}
