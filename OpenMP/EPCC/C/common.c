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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <omp.h>


#include <sys/types.h>

#include <unistd.h>

#include "common.h"


#define CONF95 1.96

int nthreads = -1;           // Number of OpenMP threads
int delaylength = -1;        // The number of iterations to delay for
int outerreps = -1;          // Outer repetitions
double delaytime = -1.0;     // Length of time to delay for in microseconds
double targettesttime = 0.0; // The length of time in microseconds that the test
                             // should run for.
unsigned long innerreps; // Inner repetitions
double *times;           // Array of doubles storing the benchmark times in microseconds
char type[120]="ALL";
double referencetime;    // The average reference time in microseconds to perform
			 // outerreps runs
double referencesd;      // The standard deviation in the reference time in
			 // microseconds for outerreps runs.
double referencemed;     // The median reference time in microseconds to perform
			 // outerreps runs
double testtime;         // The average test time in microseconds for
			 // outerreps runs
double testsd;		 // The standard deviation in the test time in
			 // microseconds for outerreps runs.
double testmed;          // The median test time in microseconds for
			 // outerreps runs

void dofile(char *filename);/* Read a file, parse, render back, etc. */
void usage(char *argv[]) {
    printf("Usage: %s \n"
	   "\t--measureonly <selected measurement> (runs all by default)\n"
	   "\t--outer-repetitions <outer-repetitions> (default %d)\n"
	   "\t--test-time <target-test-time> (default %0.2f microseconds)\n"
	   "\t--delay-time <delay-time> (default %0.4f microseconds)\n"
	   "\t--delay-length <delay-length> "
	   "(default auto-generated based on processor speed)\n",
	   argv[0],
	   DEFAULT_OUTER_REPS, DEFAULT_TEST_TARGET_TIME, DEFAULT_DELAY_TIME);
}

void parse_args(int argc, char *argv[]) {
    // Parse the parameters
    int arg;
    for (arg = 1; arg < argc; arg++) {
	if (strcmp(argv[arg], "--delay-time") == 0.0) {
	    delaytime = atof(argv[++arg]);
	    if (delaytime == 0.0) {
		printf("Invalid float:--delay-time: %s\n", argv[arg]);
		usage(argv);
		exit(EXIT_FAILURE);
	    }

	} else if (strcmp(argv[arg], "--outer-repetitions") == 0) {
	    outerreps = atoi(argv[++arg]);
	    if (outerreps == 0) {
		printf("Invalid integer:--outer-repetitions: %s\n", argv[arg]);
		usage(argv);
		exit(EXIT_FAILURE);
	    }

	} else if (strcmp(argv[arg], "--test-time") == 0) {
	    targettesttime = atof(argv[++arg]);
	    if (targettesttime == 0) {
		printf("Invalid integer:--test-time: %s\n", argv[arg]);
		usage(argv);
		exit(EXIT_FAILURE);
	    }

	} else if (strcmp(argv[arg], "-h") == 0) {
	    usage(argv);
	    exit(EXIT_SUCCESS);

        } else if (strcmp(argv[arg], "--measureonly")==0) {
	    strcpy(type, argv[++arg]);
	} else {
	    printf("Invalid parameters: %s\n", argv[arg]);
	    usage(argv);
	    exit(EXIT_FAILURE);
	}
    }
}

int getdelaylengthfromtime(double delaytime) {
    int i, reps;
    double lapsedtime, starttime; // seconds

    reps = 1000;
    lapsedtime = 0.0;

    delaytime = delaytime/1.0E6; // convert from microseconds to seconds

    // Note: delaytime is local to this function and thus the conversion
    // does not propagate to the main code.

    // Here we want to use the delaytime in microseconds to find the
    // delaylength in iterations. We start with delaylength=0 and
    // increase until we get a large enough delaytime, return delaylength
    // in iterations.

    delaylength = 0;
    delay(delaylength);

    while (lapsedtime < delaytime) {
	delaylength = delaylength * 1.1 + 1;
	starttime = getclock();
	for (i = 0; i < reps; i++) {
	    delay(delaylength);
	}
	lapsedtime = (getclock() - starttime) / (double) reps;
    }
    return delaylength;

}

unsigned long getinnerreps(void (*test)(void)) {
    innerreps = 10L;  // some initial value
    double time = 0.0;

    while (time < targettesttime) {
	double start  = getclock();
	test();
	time = (getclock() - start) * 1.0e6;
	innerreps *=2;

	// Test to stop code if compiler is optimising reference time expressions away
	if (innerreps > (targettesttime*1.0e15)) {
	    printf("Compiler has optimised reference loop away, STOP! \n");
	    printf("Try recompiling with lower optimisation level \n");
	    exit(1);
	}
    }
    return innerreps;
}

void printheader(char *name) {
    printf("\n");
    printf("--------------------------------------------------------\n");
    printf("Computing %s time using %lu reps\n", name, innerreps);
}

void swap(double *p, double *q)
{
    double buf;
    buf = *p;
    *p = *q;
    *q = buf;
    return;
}

void quickSort(double *a, int low, int high)
{
    int i = low;
    int j = high;
    double key = a[low];
    if (low >= high)  
    {
        return ;
    }
    while (low < high)
    {
        while (low < high && key <= a[high])
        {
            --high;  
        }
        if (key > a[high])
        {
            swap(&a[low], &a[high]);
            ++low;
        }
        while (low < high && key >= a[low])
        {
            ++low;  
        }
        if (key < a[low])
        {
            swap(&a[low], &a[high]);
            --high;
        }
    }
    quickSort(a, i, low-1);  
    quickSort(a, low+1, j); 
}

void stats(char *name, double *mtp, double *sdp, double *medp) {

    double meantime, totaltime, sumsq, mintime, maxtime, sd, cutoff, median, cvl, cvh;
    int mid, t1, t2;

    int i, nr;

    mintime = 1.0e10;
    maxtime = 0.;
    totaltime = 0.;

    // calculate min max and mean times
    for (i = 0; i < outerreps; i++) {
	mintime = (mintime < times[i]) ? mintime : times[i];
	maxtime = (maxtime > times[i]) ? maxtime : times[i];
	totaltime += times[i];
    }

    meantime = totaltime / outerreps;

    // calculate standard deviation
    sumsq = 0.;

    for (i = 0; i < outerreps; i++) {
	sumsq += (times[i] - meantime) * (times[i] - meantime);
    }
    sd = sqrt(sumsq / (outerreps - 1));

    // calaculate number of outliers (more than 3 sigma from mean) 
    cutoff = 3.0 * sd;

    nr = 0;

    for (i = 0; i < outerreps; i++) {
	if (fabs(times[i] - meantime) > cutoff)
	    nr++;
    }

    // calculate median 
    quickSort(times,0,outerreps-1);
   
    if (outerreps%2 == 1) {
       median = times[outerreps/2]; 
    }
    else { 
       median = 0.5 * (times[outerreps/2 -1] + times[outerreps/2]); 
    }

//    for (i = 0; i < outerreps; i++) {
//        printf(" time %d = %f\n", i, times[i]); 
//    }
//    printf(" median = %f\n", median); 

    printf("\n");
    printf("Sample_size       Mean       Median     Min        Max        StdDev     Outliers\n");
    printf(" %d               %f   %f   %f   %f   %f   %d\n",
	   outerreps, meantime, median, mintime, maxtime, sd, nr);

    *mtp = meantime;
    *sdp = sd;
    *medp = median;


}

void printfooter(char *name, double testtime, double testsd, double testmed, 
		 double referencetime, double referencesd, double referencemed) {
    printf("%s time         = %f microseconds +/- %f\n",
	   name, testtime, CONF95*testsd);
    printf("%s overhead     = %f microseconds +/- %f\n",
	   name, testtime-referencetime, CONF95*(testsd+referencesd));
    printf("%s median_ovrhd = %f microseconds \n",
	   name, testmed-referencemed);

}

void printreferencefooter(char *name, double referencetime, double referencesd, double referencemed) {
    printf("%s mean time    = %f microseconds +/- %f\n",
	   name, referencetime, CONF95 * referencesd);
    printf("%s median time  = %f microseconds\n",
	   name, referencemed);
}

void init(int argc, char **argv)
{
#pragma omp parallel
    {
#pragma omp master
	{
	    nthreads = omp_get_num_threads();
	}

    }

    parse_args(argc, argv);

    if (outerreps == -1) {
	outerreps = DEFAULT_OUTER_REPS;
    }
    if (targettesttime == 0.0) {
	targettesttime = DEFAULT_TEST_TARGET_TIME;
    }
    if (delaytime == -1.0) {
	delaytime = DEFAULT_DELAY_TIME;
    }
    delaylength = getdelaylengthfromtime(delaytime); 

    times = malloc((outerreps) * sizeof(double));

    printf("Running OpenMP benchmark version 4.0\n"
	   "\t%d thread(s)\n"
	   "\t%d outer repetitions\n"
	   "\t%0.2f test time (microseconds)\n"
	   "\t%d delay length (iterations) \n"
	   "\t%f delay time (microseconds)\n",
	   nthreads,
	   outerreps, targettesttime,
	   delaylength, delaytime);
}

void finalise(void) {
    free(times);

}

void initreference(char *name) {
    printheader(name);

}

/* Calculate the reference time. */
void reference(char *name, void (*refer)(void)) {
    int k;
    double start;

    // Calculate the required number of innerreps
    innerreps = getinnerreps(refer);

    initreference(name);

    // ignore timing for first time through 
    refer();

    // do outerreps timed reference measurements 
    for (k = 0; k < outerreps; k++) {
	start = getclock();
	refer();
	times[k] = (getclock() - start) * 1.0e6 / (double) innerreps;
    }

    finalisereference(name);

}

void finalisereference(char *name) {
    stats(name, &referencetime, &referencesd, &referencemed);
    printreferencefooter(name, referencetime, referencesd, referencemed);

}

void inittest(char *name) {
    printheader(name);

}

void finalisetest(char *name) {
    stats(name, &testtime, &testsd, &testmed);
    printfooter(name, testtime, testsd, testmed, referencetime, referencesd, referencemed);

}

/* Function to run a microbenchmark test*/
void benchmark(char *name, void (*test)(void))
{
    int k;
    double start;

    // Calculate the required number of innerreps
    innerreps = getinnerreps(test);

    inittest(name);

    // ignore timing for first time through 
    test();

    // do outerreps timed tests 
    for (k=0; k<outerreps; k++) {
	start = getclock();
	test();
	times[k] = (getclock() - start) * 1.0e6 / (double) innerreps;
    }

    finalisetest(name);

}

void delay(int delaylength) {

    int i;
    float a = 0.;

    for (i = 0; i < delaylength; i++)
	a += i;
    if (a < 0)
	printf("%f \n", a);

}

void array_delay(int delaylength, double a[1]) {

    int i;
    a[0] = 1.0;
    for (i = 0; i < delaylength; i++)
	a[0] += i;
    if (a[0] < 0)
	printf("%f \n", a[0]);

}

double getclock() {
    double time;
    // Returns a value in seconds of the time elapsed from some arbitrary,
    // but consistent point.
    double omp_get_wtime(void);
    time = omp_get_wtime();
    return time;
}

int returnfalse() {
    return 0;

}


