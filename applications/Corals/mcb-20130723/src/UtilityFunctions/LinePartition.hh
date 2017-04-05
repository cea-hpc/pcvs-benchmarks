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

#ifndef LINEPARTITION_HH
#define LINEPARTITION_HH

namespace IMC_namespace
{

   void LinePartition( const unsigned int N_zones_global,
                       const double min_global,
                       const double max_global,
                       const unsigned int N_procs,
                       const unsigned int my_id,
                       unsigned int & N_zones_local,
                       double & min_local,
                       double & max_local );

}

#endif // LINEPARTITION_HH
