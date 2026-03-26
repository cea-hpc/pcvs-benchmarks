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

#ifndef __MCAnswer_hh__
#define __MCAnswer_hh__

//===========================================================================//
//
// class MCAnswer
//
//===========================================================================//

#include <iostream>
#include <fstream>
#include <iomanip>
#include <vector>
#include <string>
#include <cmath>

namespace IMC_namespace
{

class MCAnswer
{
  public:

    MCAnswer( unsigned int nXTerms, unsigned int nYTerms,
              double D, double sigmaAbsorb,
              double sourceStrength,
              double xRoot, double yRoot, 
              double omega, double phi,
              bool perfectlyLoadBalanced,
              double xMax, double yMax );
    
    double Answer( double x, double y, double t );
    
  private:
    
    double calculateXExpansionCoef( unsigned int n ) const;
    double calculateYExpansionCoef( unsigned int m ) const;
    double timeExponArgument( unsigned int n, unsigned int m ) const;
    double temporalIntegral( double t, 
                             unsigned int n, unsigned int m ) const;
    double loadBalancedTemporalIntegral( double t ) const;
    double loadImbalancedAnswer( double x, double y, double t );
    double loadBalancedAnswer( double t );
    
    void fillXExpansionCoefs();
    void fillYExpansionCoefs();
    void fillXFunctionVectors( double x );
    void fillYFunctionVectors( double y );
   
//    number of terms in Fourier series in x and y directions
    unsigned int nXTerms, nYTerms;
    
//    parameters characterizing the material properties
    double D, sigmaAbsorb;
    
//    parameters characterizing the spatial and temporal shape of the
//    source
    double sourceStrength;
    double xRoot, yRoot;
    double yRoot2, xRoot6, yRoot6;
    double omega, phi;

//    When spatial dependence is removed from source to eliminate load
//    imbalance, don't calculate Fourier series of source
    const bool mPerfectlyLoadBalanced;

//    vars used in calculating Fourier expansion of spatial part of source
    double xNorm, yNorm;
    std::vector<double> xExpansionCoefs, yExpansionCoefs;
//    These vars hold values of the normalized expansion functions at some
//    given value of x and y
    std::vector<double> xFunctionVector, yFunctionVector;

//    parameters characterizing the mesh
    double xMax, yMax;
    double xBC, yBC;
    
//    don't want these called, so make them private
    MCAnswer();
    MCAnswer( const MCAnswer& );
    MCAnswer& operator=( const MCAnswer& );
};
    
//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif  // __MCAnswer_hh__
