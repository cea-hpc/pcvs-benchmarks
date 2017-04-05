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
#include <iomanip>
#include <limits>
#include <cmath>
#include <algorithm>
#include <cstdlib>

using namespace std;

bool equal( double a, double b )
{

   const double eps = 30.0*numeric_limits<double>::epsilon();
   return (std::fabs(a - b) < eps*std::max(std::fabs(a), std::fabs(b))); 

}

namespace
{
   unsigned int errors;
}

#define CHECK(a) if (!(a)) { std::cout << "FAILED: " << #a << " at line " << __LINE__ << std::endl; ++errors; } 

int main()
{

   cout << setiosflags(ios::scientific);
   cout << setprecision(15);

   errors = 0;

   const double min_global = 1.0;
   const double max_global = 3.0;

   const unsigned int N_zones_global = 13;
   double global_dx = (max_global -min_global)/N_zones_global;

   // Check each possible number of decompositions
   for(unsigned int N_proc = 1; N_proc <= N_zones_global; ++N_proc)
   {

      cout << "N_proc = " << N_proc << ", dx = " << global_dx << endl;

      double last_max = min_global;

      // Check each processor for each decomposition
      for(unsigned int p = 0; p<N_proc; ++p)
      {

         unsigned int N_zones_local;
         double min_local;
         double max_local;

         IMC_namespace::LinePartition( N_zones_global, min_global, max_global,
                                       N_proc, p,
                                       N_zones_local, min_local, max_local );

         double local_dx = (max_local -min_local)/N_zones_local;

         cout << "   " 
            << p << ' '
            << N_zones_local << ' '
            << min_local << ' '
            << max_local << ' '
            << local_dx << ' '
            << endl;

         // Make sure spacing is the same in the global and local
         CHECK( equal( global_dx, local_dx ) );
         // Make sure last processor ended where this one starts
         CHECK( equal( last_max, min_local ) );
         last_max = max_local;

      }

      // Make sure the last processor boundary lines up with the global boundary.
      CHECK( equal( last_max, max_global ) );

   }

   if( errors != 0 )
      cout << "\n\nFAILED:  " << errors << " tests did not pass.\n\n" << endl;
   else
      cout << "\n\nPASSED\n\n" << endl;

   return errors;

}
