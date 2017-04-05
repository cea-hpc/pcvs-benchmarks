
#ifdef USE_OPENMP
#include <omp.h>
#endif
#include <cstdio>


int main(int argc, char* argv[] )
{
    
    int tid = 0;
    
#pragma omp parallel default(shared), private(tid)
    {
        tid = omp_get_thread_num();

#pragma omp barrier
        printf("My threadid=%d\n",tid);
    }
    
    return 0;
}
