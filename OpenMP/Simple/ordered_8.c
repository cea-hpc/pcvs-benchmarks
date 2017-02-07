#include <mpc.h>
#include <omp.h>
#include <stdlib.h>

long cnt_static;
long cnt_static_chunked;
long cnt_dynamic;
long cnt_dynamic_chunked;
long cnt_guided;
long cnt_guided_chunked;
long cnt_auto;
long cnt_runtime;

void check_static (long val, char * sched)
{
    //fprintf(stderr, "iteration number %d\n", val);
    if (cnt_static++ != val)
    {
        abort ();
    }
}

void check_static_chunked (long val, char * sched)
{
    if (cnt_static_chunked++ != val)
    {
        abort ();
    }
}

void check_dynamic (long val, char * sched)
{
    if (cnt_dynamic++ != val)
    {
        abort ();
    }
}

void check_dynamic_chunked (long val, char * sched)
{
    if (cnt_dynamic_chunked++ != val)
    {
        abort ();
    }
}

void check_guided (long val, char * sched)
{
    if (cnt_guided++ != val)
    {
        abort ();
    }
}

void check_guided_chunked (long val, char * sched)
{
    if (cnt_guided_chunked++ != val)
    {
        abort ();
    }
}

void check_auto (long val, char * sched)
{
    if (cnt_auto++ != val)
    {
        abort ();
    }
}

void check_runtime (long val, char * sched)
{
    if (cnt_runtime++ != val)
    {
        abort ();
    }
}

int main(void)
{
    long j;
    int N = 5000;
    long size = 2147483647;
    long init = size - N;
    int cs;

    for(cs = 1; cs < N+1; cs++)
    {
    //fprintf(stderr, "\nOrdered Schedule static\n#################################################\n");
    cnt_static = init;
    #pragma omp parallel for schedule(static) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_static (j, "static");
        }
    }
   
    //fprintf(stderr, "\nOrdered Schedule static chunked(%d)\n#################################################\n", cs);
    cnt_static_chunked = init;
    #pragma omp parallel for schedule(static, cs) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_static_chunked (j, "static chunked");
        }
    }
     
    //fprintf(stderr, "\nOrdered Schedule dynamic\n#################################################\n");
    cnt_dynamic = init;
    #pragma omp parallel for schedule(dynamic) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_dynamic (j, "dynamic");
        }
    }
    
    //fprintf(stderr, "\nOrdered Schedule dynamic chunked(%d)\n#################################################\n", cs);
    cnt_dynamic_chunked = init;
    #pragma omp parallel for schedule(dynamic, cs) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_dynamic_chunked (j, "dynamic chunked");
        }
    }
    
    //fprintf(stderr, "\nOrdered Schedule guided\n#################################################\n");
    cnt_guided = init;
    #pragma omp parallel for schedule(guided) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_guided (j, "guided");
        }
    }
    
    //fprintf(stderr, "\nOrdered Schedule guided chunked(%d)\n#################################################\n", cs);
    cnt_guided_chunked = init;
    #pragma omp parallel for schedule(guided, cs) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_guided_chunked (j, "guided chunked");
        }
    }
    /*    
    fprintf(stderr, "\nOrdered Schedule auto\n#################################################\n");
    cnt_auto = init;
    #pragma omp parallel for schedule(auto) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_auto (j, "auto");
        }
    }
    
    fprintf(stderr, "\nOrdered Schedule runtime\n#################################################\n");
    cnt_runtime = init;
    #pragma omp parallel for schedule(runtime) ordered
    for (j = init; j < size; j++)
    {
        #pragma omp ordered
        {
            check_runtime (j, "runtime");
        }
    }
    */
    }
    return EXIT_SUCCESS;
}
