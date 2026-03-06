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

#ifndef PRINTGLOBALINFO_HH
#define PRINTGLOBALINFO_HH

#ifdef USE_MPI
#include <mpi.h>

#endif

#include <vector>
#include <string>

namespace IMC_namespace
{

   void printGlobalInfo( std::vector< std::string >& tally_names,
                         std::vector< unsigned long long >& tallies,
                         int root,
                         bool print_all_processors = false );



   void printGlobalInfoPrintf( std::vector< std::string >& tally_names,
                               std::vector< unsigned long long >& tallies,
                               int root,
                               bool print_all_processors = false );


   void printGlobalInfo( std::vector< std::string >& tally_names,
                         std::vector< double >& tallies,
                         int root,
                         bool print_all_processors = false );

   void printGlobalInfoPrintf( std::vector< std::string >& tally_names,
                               std::vector< double >& tallies,
                               int root,
                               bool print_all_processors = false );

}

#endif // PRINTGLOBALINFO_HH
