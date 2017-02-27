<ompts:test>
<ompts:testdescription>Test which checks the omp task directive. The idea of the tests is to generate a set of tasks in a single region. We let pause the tasks generated so that other threads get sheduled to the newly opened tasks.</ompts:testdescription>
<ompts:ompversion>3.0</ompts:ompversion>
<ompts:directive>omp task final</ompts:directive>
<ompts:dependences>omp single</ompts:dependences>
<ompts:testcode>
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"
#include "omp_my_sleep.h"


int <ompts:testcode:functionname>omp_task_final</ompts:testcode:functionname>(FILE * logFile){
    <ompts:orphan:vars>
    int tids[NUM_TASKS];
    int includedtids[NUM_TASKS];
    int i;
    </ompts:orphan:vars>
    int error;
#pragma omp parallel 
{
#pragma omp single
    {
        for (i = 0; i < NUM_TASKS; i++) {
            <ompts:orphan>
            /* First we have to store the value of the loop index in a new variable
             * which will be private for each task because otherwise it will be overwritten
             * if the execution of the task takes longer than the time which is needed to 
             * enter the next step of the loop!
             */
            int myi;
            myi = i;

            #pragma omp task <ompts:check>final(i>=10)</ompts:check>
            {
                tids[myi] = omp_get_thread_num();
                /*we generate included tasks for final tasks*/
                if(myi >= 10){
                    int included = myi;
                    #pragma omp task
                    {
                        my_sleep (SLEEPTIME);

                        includedtids[included] = omp_get_thread_num();
                    } /* end of omp included task of the final task */

                    my_sleep (SLEEPTIME);

                } /* end of if it is a final task*/

            } /* end of omp task */
            </ompts:orphan>
        } /* end of for */
    } /* end of single */
} /*end of parallel */

/* Now we check if more than one thread executed the final task and its included task. */
    for (i = 10; i < NUM_TASKS; i++) {
        if (tids[i] != includedtids[i])
            error++;
    }
    return (error==0);
} /* end of check_paralel_for_private */
</ompts:testcode>
</ompts:test>
