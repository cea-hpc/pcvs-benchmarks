//----------------------------------*-C++-*----------------------------------//
// Copyright 2009 Lawrence Livermore National Security, LLC
// All rights reserved.
//---------------------------------------------------------------------------//

// This work performed under the auspices of the U.S. Department of Energy by
// Lawrence Livermore National Laboratory under Contract DE-AC52-07NA27344

//  DISCLAIMER
//  This work was prepared as an account of work sponsored by an agency of the
//  United States Government. Neither the United States Government nor the
//  Lawrence Livermore National Security, LLC, nor any of their employees,
//  makes any warranty, express or implied, including the warranties of
//  merchantability and fitness for a particular purpose, or assumes any
//  legal liability or responsibility for the accuracy, completeness, or
//  usefulness of any information, apparatus, product, or process disclosed,
//  or represents that its use would not infringe privately owned rights.

#include <mpi.h>
#include <iostream>

#include "Nonblocking_Gather.hh"
#include "Nonblocking_Sync.hh"
#include "ASSERT.hh"

#ifdef USE_OPENMP
#include <omp.h>
#endif

#include <cstdio>

int Nonblocking_tests_debug = 0;

using namespace std;

// unsigned long esent;
// unsigned long erec;

void contrivedAdvance(int myid, int numprocs)
{

    int tid = 0;
    int numOmpThreads = 1;
    
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
    numOmpThreads = omp_get_num_threads();
#endif
    
    // The total number we expect from the gather.
    unsigned long long expected_total_base = 
        100*numOmpThreads*(numprocs)*(numprocs-1)/2+numprocs*(numOmpThreads)*(numOmpThreads-1)/2;
    
    // Check the sum a few times
    unsigned long long jmax = 3;
    for( unsigned long long j=1; j<=jmax; ++j)
    {
        // Simulate between time-step synchronization.
        // Needed to make sure different iterations don't interact.
        // If barriers aren't your style, you need to change the
        // tags for each Nonblocking_Sync and Nonblocking_Gather in your code.
// #pragma omp barrier
#pragma omp master
        {
            MPI_Barrier( MPI_COMM_WORLD );
        }
         
        // Start nonblocking communications
        IMC_namespace::Nonblocking_Sync nbsync( 1, MPI_COMM_WORLD );
        IMC_namespace::Nonblocking_Gather nbgather( 2, 1, MPI_COMM_WORLD );

        // Set up counts to collect
        unsigned long long my_value = 100*myid+tid;
        unsigned long long my_sum = my_value;
        const unsigned long long expected_total = expected_total_base;

        if( Nonblocking_tests_debug ) printf("MPI rank %d, tid: %d, my_value= %llu\n ",myid, tid, my_value);
        if(myid == 0 && tid == 0)
        {
            printf("Starting sum %llu of %llu, expected sum = %llu\n",j, jmax, expected_total);
        }

       // This barrier is just to make sure the above print statement is good.
#pragma omp master
        {
            MPI_Barrier( MPI_COMM_WORLD );
        }
        int iter=0;
        bool localDoneAccumulating=false;

        //
        // first accumulate the thread data so the master thread can do the MPI communication
        //
        nbgather.accumulate_local( &my_sum );

        if( Nonblocking_tests_debug ) printf("  MPI task %d, tid: %d, after OMP sum, my_sum=%llu\n",myid, tid, my_sum);
        
        //
        // root rank (0) repeatedly calls accumulate until it's sum equals the expected total
        //
#pragma omp master
        {
            if( myid == nbsync.root_rank() )
            {
                while( my_sum != expected_total )
                {
                    nbgather.accumulate( &my_sum );

                    iter++;
                    if( Nonblocking_tests_debug ) printf("  MPI task %d, tid: %d, iter: %d, current my_sum=%llu\n",myid, tid, iter, my_sum);
                }

                nbsync.send_finished();
            }
            else
            {
                //
                // all other ranks repeatedly call accumulate_and_send until the get a finished message
                // from the sync object.
                while( !nbsync.finished() )
                {
                    nbgather.accumulate_and_send( &my_sum );
                    iter++;
                    if( Nonblocking_tests_debug )printf("  MPI task %d, tid: %d, iter: %d\n",myid, tid, iter);
                }

            }
        }
        
// #pragma omp barrier
        //
        // all threads zero out the shared gather buffer here
        //
        nbgather.reset_shared_buffer();

#pragma omp master
        {
            MPI_Barrier( MPI_COMM_WORLD );
        }
         
// #pragma omp barrier

        if( Nonblocking_tests_debug ) printf(" ITERATION %llu FINISHED\n", j);
        
    }//end of simple iteration loop

#pragma omp master
   {    
       MPI_Barrier( MPI_COMM_WORLD );
       if( myid == 0 )
       {
           printf("PASS\n");
       }
   }

   if( Nonblocking_tests_debug ) printf("MPI task %d , tid %d Exiting ContrivedAdvance().\n", myid, tid);

}

//! \brief Test both Nonblocking_Gather and Nonblocking_Sync
//!
//!  We initiate both a nonblocking gather and sync.  We gather all
//!  the values of the processor ID's.  When we've counted all them,
//!  we send the done message.
//!
int main(int argc,char *argv[])
{
   // Start MPI.
   int myid, numprocs;
   int required, provided;

// set this to desired value 
   required = MPI_THREAD_FUNNELED;
// set this to minimum possible value
   provided = MPI_THREAD_SINGLE;


   MPI_Init_thread(&argc, &argv, required, &provided );
 
   MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
   MPI_Comm_rank(MPI_COMM_WORLD,&myid);

   if (myid == 0) 
       printf ("MPI_THREAD level provided  = %d \n", provided );
   
   int tid=0;
#pragma omp parallel default(shared) private(tid)
   {

#ifdef USE_OPENMP
       tid = omp_get_thread_num();
#endif

       contrivedAdvance(myid, numprocs);
   }

   printf("Calling MPI_Finalize in main.\n");   

   MPI_Finalize();

   return 0;

}

