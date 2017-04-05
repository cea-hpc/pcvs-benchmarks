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

#include "Nonblocking_Sync.hh"
#include "ASSERT.hh"
#include <cstdio>
#include <unistd.h>

#ifdef USE_OPENMP
#include <omp.h>
#endif


using namespace std;

void contrivedAdvance(int myid, int numprocs)
{
    int tid= 0;
    
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif
    
   // Check the sum a few times
   unsigned long long jmax = 3;
   for( unsigned long long j=1; j<=jmax; ++j)
   {
         // Simulate between time-step synchronization.
         // Needed to make sure different iterations don't interact.
         // If barriers aren't your style, you need to change the
         // tags for each Nonblocking_Sync and Nonblocking_Gather in your code.
#pragma omp master
         {
             MPI_Barrier( MPI_COMM_WORLD );
         }
         
         // Start nonblocking communications
         IMC_namespace::Nonblocking_Sync nbsync( 69+(int)j, MPI_COMM_WORLD );

#pragma omp master
         {
             if( myid == nbsync.root_rank() )
             {
                 
                 printf("iteration %llu, mpi task %d , tid %d sleeping for %d seconds.\n", j, myid, tid, (tid+1));
                 sleep((tid+1));
                 printf("iteration %llu, mpi task %d , tid %d WAKE UP!\n", j, myid, tid);
                 //
                 // only root rank calls send_finished()
                 nbsync.send_finished();
             }
             else 
             {
                 //
                 // non-root ranks poll on finished() until root rank says so
                 while( !nbsync.finished() )
                 {
//                 printf("   iteration %d, mpi task %d , tid %d waiting on master rank to finish.\n",j,myid,tid);
                 }
             }
         }//end of omp master section

#pragma omp master
         {
             MPI_Barrier( MPI_COMM_WORLD );
             printf(" MPI rank %d, tid: %d, Iteration %llu finished.\n\n\n", myid, tid, j);
         }
         
   }//end of simple iteration loop

#pragma omp master
   {    
       MPI_Barrier( MPI_COMM_WORLD );
       if( myid == 0 )
       {
           printf("PASS\n");
       }
   }
#pragma omp barrier
   
   printf("MPI task %d , tid %d Exiting ContrivedAdvance().\n", myid, tid);

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

#ifdef USE_OPENMP
   MPI_Init_thread(&argc, &argv, required, &provided );
#else
   MPI_Init(&argc, &argv);
#endif

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

       printf(" Calling contrivedAdvance on thread %d for mpi task %d out of a total %d MPI tasks\n",tid, myid, numprocs);
       contrivedAdvance(myid, numprocs);
   }

   if (myid == 0) 
       printf(" ContrivedAdvance finished, exiting test.\n");

   MPI_Finalize();

   return 0;

}

