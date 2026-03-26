
#include "ULLreduce.hh"
#include "mpi.h"
#include <iostream>
#include <limits>

int main( int argc, char *argv[] )
{
   // Start MPI.
   int myid, numprocs;
   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
   MPI_Comm_rank(MPI_COMM_WORLD,&myid);

   // Need scope so that destructor of ULLreduce is called before MPI_Finalize() in
   // order to free operations.
   {
      IMC_namespace::ULLreduce ullred;

      // Compute value to sum that will push sum into something that only unsigned long long can handle.
      const unsigned long long base =std::numeric_limits<unsigned long long>::max() / numprocs - 2*numprocs;
      const unsigned long long my_value = base + myid;

      // expected sum - should need unsigned long long type
      unsigned long long total = base * numprocs;
      for(int i=0; i<numprocs; ++i)
      {
         total += i;
      }

      unsigned long long expected_max = base + (numprocs-1);

      if( myid == 0 )
      {
         std::cout << "Maximum unsigned long long: "
            << std::numeric_limits<unsigned long long>::max() << '\n';
         std::cout << "Expected sum:               " << total << '\n';
         std::cout << "Difference:                 "
            << (std::numeric_limits<unsigned long long>::max()  - total) 
            << '\n';
         std::cout << "Expected maximum:           " << expected_max << '\n';
      }

      unsigned long long p = my_value;
      unsigned long long sum = 0;
      unsigned long long max = 0;

      // Do an all reduce so that the functions are called more times on each processor.
      MPI_Allreduce( &p, &sum, 1, ullred.getType(), ullred.getSum(), MPI_COMM_WORLD );
      MPI_Allreduce( &p, &max, 1, ullred.getType(), ullred.getMax(), MPI_COMM_WORLD );


      if( myid == 0 )
      {
         std::cout << "Computed sum:               " << sum << '\n';
         std::cout << "Computed maximum:           " << max << '\n';
      }

      bool pass = true;

      if(sum != total)
      {
         std::cerr << "FAIL: Sum not correct on processor " << myid 
            << ": " << sum << std::endl;
         pass = false;
      }

      if ( max != expected_max )
      {
         std::cerr << "FAIL: Maximum not correct on processor " << myid 
            << ": " << max << std::endl;
         pass = false;
      }

      MPI_Barrier( MPI_COMM_WORLD );

      if( !pass )
      {
         MPI_Abort( MPI_COMM_WORLD, -1 );
      }

      MPI_Barrier( MPI_COMM_WORLD );

      if(myid == 0)
      {
         std::cout << "PASS!\n";
      }

      //ULLreduce destructor called.
   }

   MPI_Finalize();

   return 0;

}
