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
#include <omp.h>
#include <string.h>
#include "common.h"
#include "taskbench.h"

#define DEPTH 6


int main(int argc, char **argv) {
  extern char type[120];
  init(argc, argv);


    /* GENERATE REFERENCE TIME */

    reference("reference time 1", &refer);

    /* TEST PARALLEL TASK GENERATION */

    if((strcmp("PARALLEL_TASK",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("PARALLEL TASK", &testParallelTaskGeneration);
    }

    if((strcmp("PARALLEL_TASK_DEPS",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("PARALLEL TASK DEPS", &testParallelTaskGenerationWithDeps);
    }

    if((strcmp("MASTER_TASK_DEPS",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("MASTER TASK DEPS", &testMasterTaskGenerationWithDeps);
    }

    if((strcmp("MASTER_TASK",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("MASTER TASK", &testMasterTaskGeneration);
    }

    if((strcmp("MASTER_TASK_BUSY_SLAVES",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("MASTER TASK BUSY SLAVES", &testMasterTaskGenerationWithBusySlaves);
    }

    if((strcmp("CONDITIONAL_TASK",type)==0)||(strcmp("ALL",type)==0))
    {
          benchmark("CONDITIONAL TASK", &testConditionalTaskGeneration);
    }

    if((strcmp("MASTER_TASK",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("MASTER TASK", &testMasterTaskGeneration);
    }

    if((strcmp("TASK_WAIT",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("TASK WAIT", &testTaskWait);
    }

    if((strcmp("TASK_BARRIER",type)==0)||(strcmp("ALL",type)==0))
    {
          benchmark("TASK BARRIER", &testTaskBarrier);
    }

    if((strcmp("NESTED_TASK",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("NESTED TASK", &testNestedTaskGeneration);
    }

    if((strcmp("NESTED_MASTER_TASK",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("NESTED MASTER TASK", &testNestedMasterTaskGeneration);
    }

    /* GENERATE THE SECOND REFERENCE TIME */
    if((strcmp("BRANCH_TASK_TREE",type)==0)||(strcmp("LEAF_TASK_TREE",type)==0)||(strcmp("ALL",type)==0))
    {
      reference("reference time 2", &refer2);
    }

    if((strcmp("BRANCH_TASK_TREE",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("BRANCH TASK TREE", &testBranchTaskGeneration);
    }

    if((strcmp("LEAF_TASK_TREE",type)==0)||(strcmp("ALL",type)==0))
    {
      benchmark("LEAF TASK TREE", &testLeafTaskGeneration);
    }

    finalise();
    return EXIT_SUCCESS;



}

/* Calculate the reference time. */
void refer() {
    int j;
    for (j = 0; j < innerreps; j++) {
	delay(delaylength);
    }

}

/* Calculate the second reference time. */
void refer2() {
    int j;
    for (j = 0; j < (innerreps >> DEPTH) * (1 << DEPTH); j++) {
	delay(delaylength);
    }
    printf("ref time delays = %d\n",j); 

}

/* Test parallel task generation overhead */
void testParallelTaskGeneration() {
    int j;

#pragma omp parallel private( j )
    {
	for ( j = 0; j < innerreps; j ++ ) {
#pragma omp task
	    {
		delay( delaylength );

	    } // task
	}; // for j
    } // parallel

}
void testParallelTaskGenerationWithDeps() {
     int j, id;

     int *a;

     a = (int *) malloc (omp_get_max_threads() * sizeof(int));
#pragma omp parallel private( j, id )
     {

     id = omp_get_thread_num();

     for ( j = 0; j < innerreps; j ++ ) {
#pragma omp task depend(inout:a[id])
         {
         delay( delaylength );

         } // task
     }; // for j
   }; // parallel

}
void testMasterTaskGeneration() {
    int j;
#pragma omp parallel private(j)
    {
#pragma omp master
	{
	    for (j = 0; j < innerreps * nthreads; j++) {
#pragma omp task
		{
		    delay(delaylength);

		}

	    } /* End for j */
	} /* End master */
    } /* End parallel */

}
void testMasterTaskGenerationWithDeps() {
     int j, id;

     int *a;

     a = (int *) malloc (omp_get_max_threads() * sizeof(int));

#pragma omp parallel private(j, id)
     {
#pragma omp master
     {
         for (j = 0; j < innerreps * nthreads; j++) {

         id = j%nthreads;

#pragma omp task depend(inout: a[id])
         {
             delay(delaylength);

         }

         } /* End for j */
     } /* End master */
     } /* End parallel */

}
void testMasterTaskGenerationWithBusySlaves() {
    int j;
#pragma omp parallel private( j )
    {
	int thread_num = omp_get_thread_num();
	for (j = 0; j < innerreps; j ++ ) {

	    if ( thread_num == 0 ) {
#pragma omp task
		{
		    delay( delaylength );
		} // task

	    } else {
		delay( delaylength );

	    }; // if
	}; // for j
    } // parallel
}

void testConditionalTaskGeneration() {
    int j;
#pragma omp parallel private(j)
    {
	for (j = 0; j < innerreps; j++) {
#pragma omp task if(returnfalse())
	    {
		delay( delaylength );
	    }
	}
    }
}

void testNestedTaskGeneration() {
    int i,j;
#pragma omp parallel private( i, j )
    {
  for ( j = 0; j < innerreps / nthreads; j ++ ) {
#pragma omp task private( i )
	    {
		for ( i = 0; i < nthreads; i ++ ) {
#pragma omp task untied
		    {
			delay( delaylength );

		    } // task
		}; // for i

		// wait for inner tasks to complete
#pragma omp taskwait

	    } // task
	}; // for j
    } // parallel
}




/* Measure overhead of nested tasks (master thread constructs outer tasks) */
void testNestedMasterTaskGeneration() {
    int i, j;
#pragma omp parallel private( i, j )
    {
#pragma omp master
	{
	    for ( j = 0; j < innerreps; j ++ ) {
#pragma omp task private( i )
		{
		    for ( i = 0; i < nthreads; i ++ ) {
#pragma omp task
			{
			    delay( delaylength );

			} // task
		    }; // for i

		    // wait for inner tasks to complete
#pragma omp taskwait

		} // task
	    }; // for j
	} // master
    } // parallel
}


void testTaskWait() {
    int j;
#pragma omp parallel private( j )
    {
	for ( j = 0; j < innerreps; j ++ ) {
#pragma omp task
	    {
		delay( delaylength );

	    } // task
#pragma omp taskwait

	}; // for j
    } // parallel
}

void testTaskBarrier() {
    int j;
#pragma omp parallel private( j )
    {
	for ( j = 0; j < innerreps; j ++ ) {
#pragma omp task
	    {
		delay( delaylength );

	    } // task
#pragma omp barrier

	}; // for j
    } // parallel
}

    int n; 

void testBranchTaskGeneration() {
    int j;

#pragma omp parallel private(j)
    {
	for (j = 0; j < (innerreps >> DEPTH); j++) {
#pragma omp task
	    {
		branchTaskTree(DEPTH);
		delay(delaylength);
	    }

	}
    }
}

void branchTaskTree(int tree_level) {
    if ( tree_level > 0 ) {
#pragma omp task
	{
	    branchTaskTree(tree_level - 1);
	    branchTaskTree(tree_level - 1);
	    delay(delaylength);
	}
    }
}


void testLeafTaskGeneration() {
    int j;
#pragma omp parallel private(j)
    {
	for (j = 0; j < (innerreps >> DEPTH); j++) {
	    leafTaskTree(DEPTH);
	}
    }
}

void leafTaskTree(int tree_level) {
    if ( tree_level == 0 ) {
	delay(delaylength);
    } else {
#pragma omp task
	{
	    leafTaskTree(tree_level - 1);
	    leafTaskTree(tree_level - 1);
	}
    }
}
