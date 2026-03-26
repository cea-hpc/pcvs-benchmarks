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

#include "LinePartition.hh"

#include <iostream>
#include <cstdlib>

namespace IMC_namespace
{

   namespace
   {

      void checkInput( const unsigned int N_zones_global,
                       const double min_global,
                       const double max_global,
                       const unsigned int N_procs,
                       const unsigned int my_id)
      {

         if( max_global <= min_global )
         {
            std::cerr << "LinePartition: max_global <= min_global ( " 
               << max_global << " <= " 
               << min_global << " ) is not allowed." << std::endl;
            std::exit(-1);
         }

         if( N_zones_global == 0)
         {
            std::cerr << "LinePartition: N_zones_global = " << N_zones_global 
               << " must be greater than 0. " << std::endl;
            std::exit(-1);
         }

         if( N_procs == 0)
         {
            std::cerr << "LinePartition: N_procs = " << N_procs 
               << " must be greater than 0. " << std::endl;
            std::exit(-1);
         }

         if( my_id >= N_procs )
         {
            std::cerr << "LinePartition: my_id must be less than N_procs ( my_id = " << my_id 
               << ", N_procs = " << N_procs << " )." << std::endl;
            std::exit(-1);
         }

         if( N_zones_global < N_procs )
         {
            std::cerr << "LinePartition: Must have one zone in each direction on each processor.\n"
               << "  N_zones_global = " << N_zones_global 
               << ", N_procs = " << N_procs 
               << std::endl;
            std::exit(-1);
         }

      }

   }

   void LinePartition( const unsigned int N_zones_global,
                       const double min_global,
                       const double max_global,
                       const unsigned int N_procs,
                       const unsigned int my_id,
                       unsigned int & N_zones_local,
                       double & min_local,
                       double & max_local )
   {

      checkInput( N_zones_global, min_global, max_global, N_procs, my_id );

      N_zones_local = N_zones_global / N_procs;

      // The remainder of zones.  These will be evenly distributed between
      // the first several processors.
      unsigned int N_zones_extra = N_zones_global % N_procs;

      unsigned int start_zone = my_id * N_zones_local;
      if( my_id < N_zones_extra )
         start_zone += my_id;
      else
         start_zone += N_zones_extra;

      // Evenly distribute extra zones.
      if( my_id < N_zones_extra )
         N_zones_local += 1;

      // This is one past the zones of this processor, just like STL containers' use of end().
      unsigned int end_zone = start_zone + N_zones_local;

      double dx = (max_global - min_global)/N_zones_global;

      min_local = min_global + start_zone*dx;
      max_local = min_global + end_zone*dx;

   }

}
