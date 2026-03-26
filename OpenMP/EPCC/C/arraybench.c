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
#include <math.h>
#include <omp.h>
#include <string.h>
#include "common.h"
#include "arraybench.h"

double btest[IDA];
double atest[IDA];

#pragma omp threadprivate (btest)

int main(int argc, char **argv) {

    init(argc, argv);

    char testName[32];
    extern char type[120];

    /* GENERATE REFERENCE TIME */
    reference("reference time 1", &refer);

    /* TEST  PRIVATE */
    if((strcmp("PRIVATE",type)==0)||(strcmp("ALL",type)==0)){
      sprintf(testName, "PRIVATE %d", IDA);
      benchmark(testName, &testprivnew);
    }

    /* TEST  FIRSTPRIVATE */
    if((strcmp("FIRSTPRIVATE",type)==0)||(strcmp("ALL",type)==0)){
      sprintf(testName, "FIRSTPRIVATE %d", IDA);
      benchmark(testName, &testfirstprivnew);
    }

    /* TEST  COPYPRIVATE */
    if((strcmp("COPYPRIVATE",type)==0)||(strcmp("ALL",type)==0)){
      sprintf(testName, "COPYPRIVATE %d", IDA);
      benchmark(testName, &testcopyprivnew);
    }

    /* TEST  THREADPRIVATE - COPYIN */
    if((strcmp("COPYIN",type)==0)||(strcmp("ALL",type)==0)){
      sprintf(testName, "COPYIN %d", IDA);
      benchmark(testName, &testthrprivnew);
    }

    finalise();
    return EXIT_SUCCESS;

}

void refer() {
    int j;
    double a[1];
    for (j = 0; j < innerreps; j++) {
	array_delay(delaylength, a);
    }
}

void testprivnew() {
    int j;
    for (j = 0; j < innerreps; j++) {
#pragma omp parallel private(atest)
	{
	    array_delay(delaylength, atest);
	}
    }
}

void testfirstprivnew() {
    int j;
    for (j = 0; j < innerreps; j++) {
#pragma omp parallel firstprivate(atest)
	{
	    array_delay(delaylength, atest);
	}
    }
}


void testcopyprivnew()
{
    int j;
    for (j=0; j<innerreps; j++) {
#pragma omp parallel private(atest)
	{
#pragma omp single copyprivate(atest)
		{
	    	array_delay(delaylength, atest);
		}
    	}
    }
}


void testthrprivnew() {
    int j;
    for (j = 0; j < innerreps; j++) {
#pragma omp parallel copyin(btest)
	{
	    array_delay(delaylength, btest);
	}
    }

}
