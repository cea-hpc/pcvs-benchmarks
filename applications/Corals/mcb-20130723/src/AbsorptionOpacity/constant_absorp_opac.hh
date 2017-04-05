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

#ifndef __constant_absorp_opac_hh__
#define __constant_absorp_opac_hh__

#include "ASSERT.hh"
#include "absorption_opacity_base.hh"
#include "rng.hh"

namespace IMC_namespace
{

class constant_absorp_opac : public absorption_opacity_base
{
  public:
    explicit constant_absorp_opac( double sigma_in );

    double Planck_opacity() const;
   
    double absorption_opacity() const;

  private:
    double sigma;    //    the constant opacity
    
//    don't want these used
    constant_absorp_opac();
    constant_absorp_opac( const constant_absorp_opac& opacity_in );
    constant_absorp_opac& operator=( const constant_absorp_opac& );
};

}    //    namespace IMC_namespace

#endif


