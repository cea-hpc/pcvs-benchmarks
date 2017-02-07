
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"


int test_omp_parallel_for_reduction(FILE * logFile){
	
    int sum;
	int known_sum;
	double dsum;
	double dknown_sum;
	double dt=0.5;				/* base of geometric row for + and - test*/
	double rounding_error= 1.E-9;
#define DOUBLE_DIGITS 20		/* dt^DOUBLE_DIGITS */
	int diff;
	double ddiff;
	int product;
	int known_product;
#define MAX_FACTOR 10
#define KNOWN_PRODUCT 3628800	/* 10! */
	int logic_and;
	int logic_or;
	int bit_and;
	int bit_or;
	int exclusiv_bit_or;
	int logics[LOOPCOUNT];
	int i;
	double dpt;
	int result;
    

    sum =0;
    dsum=0;
	dt = 1./3.;
    result = 0;
    product = 1;
	logic_and=1;
	logic_or=0;
	bit_and=1;
	bit_or=0;
	exclusiv_bit_or=0;

	known_sum = (LOOPCOUNT*(LOOPCOUNT+1))/2;

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(+:sum)
	for (i=1;i<=LOOPCOUNT;i++)
	{
		sum=sum+i;
	}

	if(known_sum!=sum)
	{
		result++;
		fprintf(logFile,"Error in sum with integers: Result was %d instead of %d\n",sum,known_sum); 
	}

	diff = (LOOPCOUNT*(LOOPCOUNT+1))/2;
#pragma omp parallel for schedule(dynamic,1) private(i) reduction(-:diff)
	for (i=1;i<=LOOPCOUNT;++i)
	{
		diff=diff-i;
	}

	if(diff != 0)
	{
		result++;
		fprintf(logFile,"Error in difference with integers: Result was %d instead of 0.\n",diff);
	}

	/* Tests for doubles */
	dsum=0;
	dpt=1;
	for (i=0;i<DOUBLE_DIGITS;++i)
	{
		dpt*=dt;
	}
	dknown_sum = (1-dpt)/(1-dt);
#pragma omp parallel for schedule(dynamic,1) private(i) reduction(+:dsum)
	for (i=0;i<DOUBLE_DIGITS;++i)
	{
		dsum += pow(dt,i);
	}

	if( fabs(dsum-dknown_sum) > rounding_error )
	{
		result++; 
		fprintf(logFile,"Error in sum with doubles: Result was %f instead of %f (Difference: %E)\n",dsum,dknown_sum, dsum-dknown_sum);
	}

	dpt=1;

	for (i=0;i<DOUBLE_DIGITS;++i)
	{
		dpt*=dt;
	}
	fprintf(logFile,"\n");
	ddiff = (1-dpt)/(1-dt);
#pragma omp parallel for schedule(dynamic,1) private(i) reduction(-:ddiff)
	for (i=0;i<DOUBLE_DIGITS;++i)
	{
		ddiff -= pow(dt,i);
	}
	if( fabs(ddiff) > rounding_error)
	{
		result++;
		fprintf(logFile,"Error in Difference with doubles: Result was %E instead of 0.0\n",ddiff);
	}

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(*:product)
	for(i=1;i<=MAX_FACTOR;i++)
	{
		product *= i;
	}

	known_product = KNOWN_PRODUCT;
	if(known_product != product)
	{
		result++;
		fprintf(logFile,"Error in Product with integers: Result was %d instead of %d\n\n",product,known_product);
	}

	for(i=0;i<LOOPCOUNT;i++)
	{
		logics[i]=1;
	}

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(&&:logic_and)
	for(i=0;i<LOOPCOUNT;++i)
	{
		logic_and = (logic_and && logics[i]);
	}
	if(!logic_and)
	{
		result++;
		fprintf(logFile,"Error in logic AND part 1.\n");
	}

	logic_and = 1;
	logics[LOOPCOUNT/2]=0;

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(&&:logic_and)
	for(i=0;i<LOOPCOUNT;++i)
	{
		logic_and = logic_and && logics[i];
	}
	if(logic_and)
	{
		result++;
		fprintf(logFile,"Error in logic AND part 2.\n");
	}

	for(i=0;i<LOOPCOUNT;i++)
	{
		logics[i]=0;
	}

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(||:logic_or)
	for(i=0;i<LOOPCOUNT;++i)
	{
		logic_or = logic_or || logics[i];
	}
	if(logic_or)
	{
		result++;
		fprintf(logFile,"Error in logic OR part 1.\n");
	}
	logic_or = 0;
	logics[LOOPCOUNT/2]=1;

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(||:logic_or)
	for(i=0;i<LOOPCOUNT;++i)
	{
		logic_or = logic_or || logics[i];
	}
	if(!logic_or)
	{
		result++;
		fprintf(logFile,"Error in logic OR part 2.\n");
	}


	for(i=0;i<LOOPCOUNT;++i)
	{
		logics[i]=1;
	}

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(&:bit_and)
	for(i=0;i<LOOPCOUNT;++i)
	{
		bit_and = (bit_and & logics[i]);
	}
	if(!bit_and)
	{
		result++;
		fprintf(logFile,"Error in BIT AND part 1.\n");
	}

	bit_and = 1;
	logics[LOOPCOUNT/2]=0;

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(&:bit_and)
	for(i=0;i<LOOPCOUNT;++i)
	{
		bit_and = bit_and & logics[i];
	}
	if(bit_and)
	{
		result++;
		fprintf(logFile,"Error in BIT AND part 2.\n");
	}

	for(i=0;i<LOOPCOUNT;i++)
	{
		logics[i]=0;
	}

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(|:bit_or)
	for(i=0;i<LOOPCOUNT;++i)
	{
		bit_or = bit_or | logics[i];
	}
	if(bit_or)
	{
		result++;
		fprintf(logFile,"Error in BIT OR part 1\n");
	}
	bit_or = 0;
	logics[LOOPCOUNT/2]=1;

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(|:bit_or)
	for(i=0;i<LOOPCOUNT;++i)
	{
		bit_or = bit_or | logics[i];
	}
	if(!bit_or)
	{
		result++;
		fprintf(logFile,"Error in BIT OR part 2\n");
	}

	for(i=0;i<LOOPCOUNT;i++)
	{
		logics[i]=0;
	}

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(^:exclusiv_bit_or)
	for(i=0;i<LOOPCOUNT;++i)
	{
		exclusiv_bit_or = exclusiv_bit_or ^ logics[i];
	}
	if(exclusiv_bit_or)
	{
		result++;
		fprintf(logFile,"Error in EXCLUSIV BIT OR part 1\n");
	}

	exclusiv_bit_or = 0;
	logics[LOOPCOUNT/2]=1;

#pragma omp parallel for schedule(dynamic,1) private(i) reduction(^:exclusiv_bit_or)
	for(i=0;i<LOOPCOUNT;++i)
	{
		exclusiv_bit_or = exclusiv_bit_or ^ logics[i];
	}
	if(!exclusiv_bit_or)
	{
		result++;
		fprintf(logFile,"Error in EXCLUSIV BIT OR part 2\n");
	}

	/*printf("\nResult:%d\n",result);*/
	return (result==0);
}
int main()
{
	int i;			/* Loop index */
	int result;		/* return value of the program */
	int failed=0; 		/* Number of failed tests */
	int success=0;		/* number of succeeded tests */


	printf("######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	printf("## Repetitions: %3d                       ####\n",REPETITIONS);
	printf("## Loop Count : %6d                    ####\n",LOOPCOUNT);
	printf("##############################################\n");
	printf("Testing omp parallel for reduction\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp parallel for reduction\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_parallel_for_reduction out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_parallel_for_reduction(stdout)){
			fprintf(stdout,"Test successful.\n");
			success++;
		}
		else {
			fprintf(stdout,"Error: Test failed.\n");
			printf("Error: Test failed.\n");
			failed++;
		}
	}

    if(failed==0){
		fprintf(stdout,"\nDirective worked without errors.\n");
		printf("Directive worked without errors.\n");
		result=0;
	}
	else{
		fprintf(stdout,"\nDirective failed the test %i times out of %i. %i were successful\n",failed,REPETITIONS,success);
		printf("Directive failed the test %i times out of %i.\n%i test(s) were successful\n",failed,REPETITIONS,success);
		result = (int) (((double) failed / (double) REPETITIONS ) * 100 );
	}
	printf ("Result: %i\n", result);
	return result;
}
