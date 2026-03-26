/****************************************************************************
*                                                                           *
*             OpenMP MicroBenchmark Suite - Version 4.0                     *
*                                                                           *
*                            produced by                                    *
*                                                                           *
*                             Mark Bull                                     *
*                                                                           *
*                                at                                         *
*                                                                           *
*                   EPCC, University of Edinburgh                           *
*                                                                           *
*                    email: m.bull@epcc.ed.ac.uk                            *
*                                                                           *
*                                                                           *
*      This version copyright (c) The University of Edinburgh, 2023.        *
*                                                                           *
*                                                                           *
*  Licensed under the Apache License, Version 2.0 (the "License");          *
*  you may not use this file except in compliance with the License.         *
*  You may obtain a copy of the License at                                  *
*                                                                           *
*      http://www.apache.org/licenses/LICENSE-2.0                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <omp.h>

#include "common.h"
#include "syncbench.h"

omp_lock_t lock1;
omp_lock_t lock2;
omp_lock_t lock3;

int main(int argc, char **argv) {
    extern char type[120];

    init(argc, argv);

    omp_init_lock(&lock1);
    omp_init_lock_with_hint(&lock2, omp_lock_hint_contended);
    omp_init_lock_with_hint(&lock3, omp_lock_hint_uncontended);

    /* GENERATE REFERENCE TIME */
    reference("reference time 1", &refer);

    /* TEST PARALLEL REGION */
    if((strcmp("PARALLEL",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("PARALLEL", &testpr);
  }
    /* TEST FOR */
    if((strcmp("FOR",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("FOR", &testfor);
  }
    /* TEST PARALLEL FOR */
    if((strcmp("PARALLEL_FOR",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("PARALLEL FOR", &testpfor);
  }
    /* TEST BARRIER */
    if((strcmp("BARRIER",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("BARRIER", &testbar);
  }
    /* TEST BARRIER LATE ARRIVER*/
    if((strcmp("BARRIER_VAR",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("BARRIER_VAR", &testbarvar);
}
    /* TEST SINGLE */
    if((strcmp("SINGLE",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("SINGLE", &testsing);
    }
    /* TEST  CRITICAL*/
    if((strcmp("CRITICAL",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("CRITICAL", &testcrit);
    }
    /* TEST  LOCK/UNLOCK CONTENDED*/
    if((strcmp("LOCK_CONTENDED",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("LOCK_CONTENDED", &testlock);
    }
    /* TEST  LOCK/UNLOCK CONTENDED WITH HINT*/
    if((strcmp("LOCK_CONTENDED_HINT",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("LOCK_CONTENDED_HINT", &testlockhint);
    }
    /* TEST  LOCK/UNLOCK UNCONTENDED*/
    if((strcmp("LOCK_UNCONTENDED",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("LOCK_UNCONTENDED", &testlockuncontended);
    }
    /* TEST  LOCK/UNLOCK UNCONTENDED WITH HINT*/
    if((strcmp("LOCK_UNCONTENDED_HINT",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("LOCK_UNCONTENDED_HINT", &testlockuncontendedhint);
    }
    /* TEST ORDERED SECTION */
    if((strcmp("ORDERED",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("ORDERED", &testorder);
    }

    /* GENERATE NEW REFERENCE TIME */
    if((strcmp("ATOMIC",type)==0)||(strcmp("ATOMIC_SEQCST",type)==0)||(strcmp("ALL",type)==0)){
    reference("reference time 2", &referatom);
    }

    /* TEST ATOMIC */
    if((strcmp("ATOMIC",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("ATOMIC", &testatom);
    }
    /* TEST ATOMIC SEQCST */
    if((strcmp("ATOMIC_SEQCST",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("ATOMIC_SEQCST", &testatomseqcst);
    }

    /* GENERATE NEW REFERENCE TIME */
    if((strcmp("REDUCTION",type)==0)||(strcmp("ALL",type)==0)){
    reference("reference time 3", &referred);
    }

    /* TEST REDUCTION (1 var)  */
    if((strcmp("REDUCTION",type)==0)||(strcmp("ALL",type)==0)){
    benchmark("REDUCTION", &testred);
    }

    finalise();
    return EXIT_SUCCESS;

}

void refer() {
    int j;
    for (j = 0; j < innerreps; j++) {
	delay(delaylength);
    }
}

void referatom(){
    int j;
    double aaaa = 0.0;
    double epsilon = 1.0e-15;
    double b, c;
    b = 1.0;
    c = (1.0 + epsilon);
    for (j = 0; j < innerreps; j++) {
	aaaa += b;
	b *= c;
    }
    if (aaaa < 0.0)
	printf("%f\n", aaaa);
}

void referred() {
    int j;
    int aaaa = 0;
    for (j = 0; j < innerreps; j++) {
	delay(delaylength);
	aaaa += 1;
    }
}

void testpr() {
    int j;
    for (j = 0; j < innerreps; j++) {
#pragma omp parallel
	{
	    delay(delaylength);
	}
    }
}

void testfor() {
    int i, j;
#pragma omp parallel private(j)
    {
	for (j = 0; j < innerreps; j++) {
#pragma omp for
	    for (i = 0; i < nthreads; i++) {
		delay(delaylength);
	    }
	}
    }
}

void testpfor() {
    int i, j;
    for (j = 0; j < innerreps; j++) {
#pragma omp parallel for
	for (i = 0; i < nthreads; i++) {
	    delay(delaylength);
	}
    }
}
void testbar() {
    int j;
#pragma omp parallel private(j)
    {
	for (j = 0; j < innerreps; j++) {
	    delay(delaylength);
#pragma omp barrier
	}
    }
}

void testbarvar() {
    int j;
#pragma omp parallel private(j)
    {
    int thread_num = omp_get_thread_num();
	for (j = 0; j < innerreps; j++) {
    if(thread_num==(j%nthreads))
	    delay(delaylength);
#pragma omp barrier
	}
    }
}

void testsing() {
    int j;
#pragma omp parallel private(j)
    {
	for (j = 0; j < innerreps; j++) {
#pragma omp single
	    delay(delaylength);
	}
    }
}

void testcrit() {
    int j;
#pragma omp parallel private(j)
    {
	for (j = 0; j < innerreps / nthreads; j++) {
#pragma omp critical
	    {
		delay(delaylength);
	    }
	}
    }
}

void testlock() {
    int j;
#pragma omp parallel private(j)
    {
	for (j = 0; j < innerreps / nthreads; j++) {
            omp_set_lock(&lock1);
	    delay(delaylength);
	    omp_unset_lock(&lock1);
	}
    }
}
void testlockhint() {
    int j;

#pragma omp parallel private(j)
    {
	for (j = 0; j < innerreps / nthreads; j++) {
	    omp_set_lock(&lock2);
	    delay(delaylength);
	    omp_unset_lock(&lock2);
	}
    }
}

void testlockuncontended() {
    int j;

#pragma omp parallel private(j)
    {
#pragma omp master
      {
	for (j = 0; j < innerreps; j++) {
	    omp_set_lock(&lock1);
	    delay(delaylength);
	    omp_unset_lock(&lock1);
	}
      }
    }
}

void testlockuncontendedhint() {
    int j;

#pragma omp parallel private(j)
    {
#pragma omp master
      {
	for (j = 0; j < innerreps; j++) {
	    omp_set_lock(&lock3);
	    delay(delaylength);
	    omp_unset_lock(&lock3);
	}
      }
    }
}

void testorder() {
    int j;
#pragma omp parallel for ordered schedule (static,1)
    for (j = 0; j < (int)innerreps; j++) {
#pragma omp ordered
	delay(delaylength);
    }
}

void testatom() {
    int j;
    double aaaa = 0.0;
    double epsilon = 1.0e-15;
    double b,c;
    b = 1.0;
    c = (1.0 + epsilon);
#pragma omp parallel private(j) firstprivate(b)
    {
	int ub = innerreps / nthreads; //avoid division on every iteration 
    	for (j = 0; j < ub; j++) {
#pragma omp atomic
	    aaaa += b;
	    b *= c;
	}
    }
    if (aaaa < 0.0)
	printf("%f\n", aaaa);
}

void testatomseqcst() {
    int j;
    double aaaa = 0.0;
    double epsilon = 1.0e-15;
    double b,c;
    b = 1.0;
    c = (1.0 + epsilon);
#pragma omp parallel private(j) firstprivate(b)
    {
        int ub = innerreps / nthreads; //avoid division on every iteration
	for (j = 0; j < ub; j++) {
#pragma omp atomic seq_cst
	    aaaa += b;
	    b *= c;
	}
    }
    if (aaaa < 0.0)
	printf("%f\n", aaaa);
}

void testred() {
    int j;
    int aaaa = 0;
    for (j = 0; j < innerreps; j++) {
#pragma omp parallel reduction(+:aaaa)
	{
	    delay(delaylength);
	    aaaa += 1;
	}
    }
}
