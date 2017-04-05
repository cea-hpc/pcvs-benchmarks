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

#include "BoxPartition.hh"
#include "LinePartition.hh"

#include <iostream>
#include <cstdlib>

namespace IMC_namespace
{

   void BoxPartition( const unsigned int N_x_zones_global,
                      const double x_min_global,
                      const double x_max_global,
                      const unsigned int N_y_zones_global,
                      const double y_min_global,
                      const double y_max_global,
                      const unsigned int N_x_procs,
                      const unsigned int N_y_procs,
                      const unsigned int my_id,
                      unsigned int & N_x_zones_local,
                      double & x_min_local,
                      double & x_max_local,
                      unsigned int & N_y_zones_local,
                      double & y_min_local,
                      double & y_max_local )
   {

      const unsigned int N_procs = N_x_procs*N_y_procs;

      if( my_id >= N_procs )
      {
         std::cerr << "BoxPartition: my_id must be less than N_x_procs*N_y_procs ("
            << my_id << " >= " 
            << N_x_procs << " * " 
            << N_y_procs << ")." << std::endl;
         std::exit(-1);
      }

      // Find coordinates of this processor
      const unsigned int proc_x = my_id % N_x_procs;
      const unsigned int proc_y = my_id / N_x_procs;

      LinePartition( N_x_zones_global, x_min_global, x_max_global,
                     N_x_procs, proc_x,
                     N_x_zones_local, x_min_local, x_max_local);

      LinePartition( N_y_zones_global, y_min_global, y_max_global,
                     N_y_procs, proc_y,
                     N_y_zones_local, y_min_local, y_max_local);


   }

}
