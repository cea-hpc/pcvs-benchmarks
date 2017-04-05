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

#include "printGlobalInfo.hh"

#include <mpi.h>
#include <iostream>

#include <vector>
#include <string>

int main(int argc,char *argv[])
{
   // Start MPI.
   int myid, numprocs;
   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
   MPI_Comm_rank(MPI_COMM_WORLD,&myid);

   // Test unsigned long long  version of the tallies
   std::vector<unsigned long long> tallies;
   std::vector<std::string> names;

   tallies.push_back(0);
   names.push_back("all zeroes");

   tallies.push_back(1);
   names.push_back("all ones");

   tallies.push_back(1000);
   names.push_back("all 1000");

   tallies.push_back(myid);
   names.push_back("myid");

   if( myid < numprocs/3 )
   {
      tallies.push_back( 5 );
   }
   else if ( myid < 2*numprocs/3 )
   {
      tallies.push_back( 6 );
   }
   else
   {
      tallies.push_back( 7 );
   }
   names.push_back("Avg about 6");

//    IMC_namespace::printGlobalInfo(names, tallies, 0, *ullred, MPI_COMM_WORLD, false);
//    IMC_namespace::printGlobalInfo(names, tallies, 0, *ullred, MPI_COMM_WORLD, true);

   IMC_namespace::printGlobalInfo(names, tallies, 0, false);
   IMC_namespace::printGlobalInfo(names, tallies, 0, true);

//    delete ullred;

   // Test double version of the tallies

   std::vector<double> dtallies;

   dtallies.push_back(0.0);

   dtallies.push_back(1.0);

   dtallies.push_back(1000.0);

   dtallies.push_back(myid);

   if( myid < numprocs/3 )
   {
      dtallies.push_back( 5.0 );
   }
   else if ( myid < 2*numprocs/3 )
   {
      dtallies.push_back( 6.0 );
   }
   else
   {
      dtallies.push_back( 7.0 );
   }

//    IMC_namespace::printGlobalInfo(names, dtallies, 0,  MPI_COMM_WORLD, false);
//    IMC_namespace::printGlobalInfo(names, dtallies, 0, MPI_COMM_WORLD, true);
   IMC_namespace::printGlobalInfo(names, dtallies, 0,  false);
   IMC_namespace::printGlobalInfo(names, dtallies, 0, true);

   MPI_Finalize();

   return 0;

}
