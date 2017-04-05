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

#include <iostream>
#include <iomanip>
#include <limits>
#include <cmath>
#include <algorithm>

using namespace std;

bool equal( double a, double b )
{

   const double eps = 50.0*numeric_limits<double>::epsilon();
   //return (std::fabs(a - b) <= eps); 
   return (std::fabs(a - b) <= eps*std::max(std::fabs(a), std::fabs(b))); 

}

namespace
{
   unsigned int errors;
}

#define CHECK(a) if (!(a)) { std::cout << "FAILED: " << #a << " at line " << __LINE__ << std::endl; ++errors; throw("Error"); } 

int main()
{

   cout << setiosflags(ios::scientific);
   cout << setprecision(5);

   errors = 0;

   const double x_min_global = 1.0;
   const double x_max_global = 3.0;
   const unsigned int N_x_zones_global = 11;
   double global_dx = (x_max_global - x_min_global)/N_x_zones_global;

   const double y_min_global = -2.0;
   const double y_max_global = -1.0;
   const unsigned int N_y_zones_global = 7;
   double global_dy = (y_max_global - y_min_global)/N_y_zones_global;

   const unsigned int total_zones_global = N_x_zones_global * N_y_zones_global;

   // Check each possible number of decompositions
   for(unsigned int N_x_proc = 1; N_x_proc <= N_x_zones_global; ++N_x_proc)
   {
      
      cout << "N_x_proc = " << N_x_proc << ", dx = " << global_dx << endl;

      for(unsigned int N_y_proc = 1; N_y_proc <= N_y_zones_global; ++N_y_proc)
      {

         cout << "  N_y_proc = " << N_y_proc << ", dy = " << global_dy << endl;

         unsigned int my_id = 0;
         unsigned int total_zones = 0;

         double last_y_max = y_min_global;
         // Check each processor for each decomposition
         for(unsigned int py = 0; py<N_y_proc; ++py)
         {

            unsigned int N_y_zones_local;
            double y_min_local;
            double y_max_local;

            double last_x_max = x_min_global;

            for(unsigned int px = 0; px<N_x_proc; ++px)
            {

               unsigned int N_x_zones_local;
               double x_min_local;
               double x_max_local;

               IMC_namespace::BoxPartition( N_x_zones_global, x_min_global, x_max_global,
                                            N_y_zones_global, y_min_global, y_max_global,
                                            N_x_proc, N_y_proc, my_id,
                                            N_x_zones_local, x_min_local, x_max_local,
                                            N_y_zones_local, y_min_local, y_max_local );

               double local_dx = (x_max_local -x_min_local)/N_x_zones_local;
               double local_dy = (y_max_local -y_min_local)/N_y_zones_local;

               cout << "    " 
                  << my_id << ' '
                  << px << ' '
                  << py << ' '
                  << N_x_zones_local << ' '
                  << N_y_zones_local << ' '
                  << local_dx << ' '
                  << local_dy << ' '
                  << x_min_local << ' '
                  << x_max_local << ' '
                  << y_min_local << ' '
                  << y_max_local << ' '
                  << endl;

               // Make sure spacing is the same in the global and local
               CHECK( equal( global_dx, local_dx ) );
               CHECK( equal( global_dy, local_dy ) );

               // Make sure last processor ended where this one starts
               CHECK( equal( last_x_max, x_min_local ) );
               CHECK( equal( last_y_max, y_min_local ) );

               last_x_max = x_max_local;

               ++my_id;

               total_zones += N_y_zones_local*N_x_zones_local;

            }

            CHECK( equal( last_x_max, x_max_global ) );

            last_y_max = y_max_local;

         }

         // Make sure the last processor boundary lines up with the global boundary.
         CHECK( equal( last_y_max, y_max_global ) );

         CHECK( total_zones == total_zones_global );

         if( errors != 0)
            return 1;

      }

   }

   if( errors != 0 )
      cout << "\n\nFAILED:  " << errors << " tests did not pass.\n\n" << endl;
   else
      cout << "\n\nPASSED\n\n" << endl;

   return errors;

}
