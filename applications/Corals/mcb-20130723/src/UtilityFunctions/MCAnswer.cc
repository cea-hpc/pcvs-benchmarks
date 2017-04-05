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

#include "ASSERT.hh"
#include "MCAnswer.hh"

using namespace std;

using namespace IMC_namespace;

const double pi = 3.1415926535897931;
const double pi2 = pi*pi;

//---------------------------------------------------------------------------//

MCAnswer::
MCAnswer( unsigned int nXTerms_in, unsigned int nYTerms_in,
          double D_in, double sigmaAbsorb_in,
          double sourceStrength_in,
          double xRoot_in, double yRoot_in, 
          double omega_in, double phi_in,
          bool perfectlyLoadBalanced,
          double xMax_in, double yMax_in )
  : nXTerms( nXTerms_in ),
    nYTerms( nYTerms_in ),
    D( D_in ),
    sigmaAbsorb( sigmaAbsorb_in ),
    sourceStrength( sourceStrength_in ),
    xRoot( xRoot_in ),
    yRoot( yRoot_in ),
    omega( omega_in ),
    phi( phi_in ),
    mPerfectlyLoadBalanced( perfectlyLoadBalanced ),
    xExpansionCoefs( nXTerms ),
    yExpansionCoefs( nYTerms ),
    xFunctionVector( nXTerms ),
    yFunctionVector( nYTerms ),
    xMax( xMax_in ),
    yMax( yMax_in ),
    xBC( xMax + 2.0*D ),
    yBC( yMax )
{ 
    yRoot2 = yRoot*yRoot;
    xRoot6 = xRoot*xRoot*xRoot*xRoot*xRoot*xRoot;
    yRoot6 = yRoot2*yRoot2*yRoot2;

    xNorm = sqrt( 2.0/xBC );
    yNorm = sqrt( 2.0/yBC );

  //    since shape of source does not change, we can get the Fourier
  //    coefs of the X and y parts once
    if( !mPerfectlyLoadBalanced )
    {
        fillXExpansionCoefs();
        fillYExpansionCoefs();
    }
}

//---------------------------------------------------------------------------//

double MCAnswer::
calculateXExpansionCoef( unsigned int n ) const
{
    ASSERT( !mPerfectlyLoadBalanced );

    double xBC2 = xBC*xBC;
    double xBC4 = xBC2*xBC2;
    
    double term = (2.0*xBC*(12.0*(1 + 2.0*n)*pi*xRoot*xBC*(1920.0*xBC4 - 
                                                           32.0*pi2*xRoot*xRoot*pow(xBC + 2.0*n*xBC,2.0))*
                            cos(((1 + 2.0*n)*pi*xRoot)/(2.*xBC)) + 
                            4608.0*xBC4*(pi2*pow(xRoot + 2.0*n*xRoot,2.0) - 10*xBC2)*
                            sin(((1 + 2.0*n)*pi*xRoot)/(2.*xBC))))/pow(pi + 2.0*n*pi,7.0);


  return xNorm * term / xRoot6;
}

//---------------------------------------------------------------------------//

double MCAnswer::
calculateYExpansionCoef( unsigned int m ) const
{
    ASSERT( !mPerfectlyLoadBalanced );

    if( m == 0 )
        return -16.0 * yRoot/ ( 35.0 * sqrt(yBC) );
    
    double yBC2 = yBC*yBC;
    double m2 = m*m;
    double pi2 = pi*pi;
    double yBC4 = yBC*yBC*yBC*yBC;
    
    double term = (-48.0*yBC4*(m*pi*yRoot*(m2*pi2*yRoot2 - 15.0*yBC2)*
                               cos((m*pi*yRoot)/yBC) + 3.0*yBC*
                               (-2.0*m2*pi2*yRoot2 + 5.0*yBC2)*
                               sin((m*pi*yRoot)/yBC)))/(pow(m*pi,7.0));
    
    
    return yNorm * term / yRoot6;
}

//---------------------------------------------------------------------------//
void MCAnswer::
fillXExpansionCoefs()
{
    ASSERT( !mPerfectlyLoadBalanced );

    for( unsigned int n = 0; n < nXTerms; ++n )
    {
        ASSERT( n < xExpansionCoefs.size() );
        xExpansionCoefs[n] = calculateXExpansionCoef(n);
    }
}

//---------------------------------------------------------------------------//
  
void MCAnswer::
fillYExpansionCoefs()
{
    ASSERT( !mPerfectlyLoadBalanced );

    for( unsigned int m = 0; m < nYTerms; ++m )
    {
        ASSERT( m < yExpansionCoefs.size() );
        yExpansionCoefs[m] = calculateYExpansionCoef(m);
    }
}

//---------------------------------------------------------------------------//

//    Expansion in x is in phiX(x,n) = sqrt(2/xBC) cos[ (2n+1) pi/2 x/xBC ]
//    xBC = xMax + 2.0*D
//    d/dx phiX(0,n) = 0
//    phiX(xBC,n) = 0

void MCAnswer::
fillXFunctionVectors( double x )
{
    ASSERT( !mPerfectlyLoadBalanced );
    ASSERT( x >= 0.0 );

//    use NR 3rd Ed, 5.4.6, p 219 to get values of cos without calling trig
//    functions repeatedly
    double delta = pi*x/xBC;
    double sinTerm = sin( 0.5 * delta );
    double alpha = 2.0*sinTerm*sinTerm;
    double beta = sin( delta );
    double theta0 = 0.5*pi*x/xBC;

    //    first value
    double cosTheta = cos( theta0 );
    double sinTheta = sin( theta0 );
    xFunctionVector[0] = xNorm*cosTheta;

    //    get the rest by recurrence relation
    
    for( unsigned int n = 1; n < nXTerms; ++n )
    {
        double newCosTheta = cosTheta - (alpha*cosTheta + beta*sinTheta );
        double newSinTheta = sinTheta - (alpha*sinTheta - beta*cosTheta );

        xFunctionVector[n] = xNorm*newCosTheta;

        cosTheta = newCosTheta;
        sinTheta = newSinTheta;
    }
}

//---------------------------------------------------------------------------//
  
//    Expansion in y is in phiY(y,m) = sqrt(2/yBC) cos[ m pi y/yBC ]
//    yBC = yMax
//    d/dy phiY(0,m) = 0
//    d/dy phiY(yBC,m) = 0
//    For m = 0, value is constant and normalized:
//    phiY(y,m=0) = 1.0/yBC

void MCAnswer::
fillYFunctionVectors( double y )
{
    ASSERT( !mPerfectlyLoadBalanced );
    ASSERT( y >= 0.0 );

//    use NR 3rd Ed, 5.4.6, p 219 to get values of cos without calling trig
//    functions repeatedly
    double delta = pi*y/yBC;
    double sinTerm = sin( 0.5 * delta );
    double alpha = 2.0*sinTerm*sinTerm;
    double beta = sin( delta );
    double theta0 = 0.0;

//    m = 0 is a special case
    yFunctionVector[0] = 1.0/sqrt(yBC);


//    get the rest by recurrence relation
//    first value
    double cosTheta = cos( theta0 );
    double sinTheta = sin( theta0 );

    for( unsigned int m = 1; m < nYTerms; ++m )
    {
        double newCosTheta = cosTheta - (alpha*cosTheta + beta*sinTheta );
        double newSinTheta = sinTheta - (alpha*sinTheta - beta*cosTheta );

        yFunctionVector[m] = yNorm*newCosTheta;

        cosTheta = newCosTheta;
        sinTheta = newSinTheta;
    }
}

//---------------------------------------------------------------------------//

double MCAnswer::
timeExponArgument( unsigned int n, unsigned int m ) const
{
    double nTerm = (2.0*n + 1.0)*pi/2.0 / xBC;

    double mTerm = m*pi / yBC;
    
    return D*( nTerm*nTerm + mTerm*mTerm ) + sigmaAbsorb;
}

//---------------------------------------------------------------------------//
  
//    integral[T(tau) exp(-Kmn(t - tau)), {tau, 0, t)}
//    where T(t) = 0.5 * ( 1 + sin( omega *t + phi ) )

double MCAnswer::
temporalIntegral( double t, unsigned int n, unsigned int m ) const
{
    ASSERT( !mPerfectlyLoadBalanced );

    double Kmn = timeExponArgument( n, m );
    
    double Kmn2 = Kmn*Kmn;
    double omega2 = omega*omega;
    double expmk = exp( -Kmn * t );
    
    double term1 = (0.5 - 0.5*expmk)/Kmn;
    
    double term2a = omega * cos(phi) * expmk;
    double term2b = -omega*cos(phi + omega*t);
    double term2c = -Kmn*sin(phi) * expmk;
    double term2d = Kmn*sin(phi + omega*t);
    double denom = Kmn2 + omega2;
    
    double term2 = (term2a + term2b + term2c + term2d)/denom;
    
    double val = term1 + 0.5*term2;
    
    ASSERT( val >= 0.0 );
    return val;
}

//---------------------------------------------------------------------------//

//    integral[T(tau) exp(-sigmaAbsorb (t - tau)), {tau, 0, t)}
//    where T(t) = 0.5 * ( 1 + sin( omega *t + phi ) )

double MCAnswer::
loadBalancedTemporalIntegral( double t ) const
{
    ASSERT( mPerfectlyLoadBalanced );
    ASSERT( sigmaAbsorb > 0.0 );

    double sigmaAbsorb2 = sigmaAbsorb*sigmaAbsorb;
    double omega2 = omega*omega;
    double expmk = exp( -sigmaAbsorb * t );
    
    double term1 = (0.5 - 0.5*expmk)/sigmaAbsorb;
    
    double term2a = omega * cos(phi) * expmk;
    double term2b = -omega*cos(phi + omega*t);
    double term2c = -sigmaAbsorb*sin(phi) * expmk;
    double term2d = sigmaAbsorb*sin(phi + omega*t);
    double denom = sigmaAbsorb2 + omega2;
    
    double term2 = (term2a + term2b + term2c + term2d)/denom;
    
    double val = term1 + 0.5*term2;
    
    ASSERT( val >= 0.0 );
    return val;
}

//---------------------------------------------------------------------------//

double MCAnswer::
loadImbalancedAnswer( double x, double y, double t )
{
    ASSERT( !mPerfectlyLoadBalanced );
    ASSERT( !mPerfectlyLoadBalanced );

//    first fill vectors of the spatial expansion functions (cos in x and y)
    fillXFunctionVectors( x );
    fillYFunctionVectors( y );


//    now add up values of the double sum
    double sum = 0.0;
    for( unsigned int n = 0; n < nXTerms; ++n )
    {
        double xTerm = xExpansionCoefs[n]*xFunctionVector[n];

        for( unsigned int m = 0; m < nYTerms; ++m )
        {
          double yTerm = yExpansionCoefs[m]*yFunctionVector[m];
          double tTerm = temporalIntegral(t,n,m);

          sum += xTerm * yTerm * tTerm;
          
        }
    }

    if( sum < 0.0 )
        sum = 0.0;

    const double min = -1.0e-8;
    if( sum < min )
    {
      cout << "x = " << x << endl;
      cout << "y = " << y << endl;
      cout << "t = " << t << endl;
      cout << "sum = " << sum << endl;
    }

    ASSERT( sum >= min );

    return sourceStrength*sum;
}

//---------------------------------------------------------------------------//

double MCAnswer::
loadBalancedAnswer( double t )
{
    return sourceStrength * loadBalancedTemporalIntegral( t );
}

//---------------------------------------------------------------------------//

double MCAnswer::
Answer( double x, double y, double t )
{
    if( mPerfectlyLoadBalanced )
        return loadBalancedAnswer( t );
    else
        return loadImbalancedAnswer( x, y, t );
}
