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

#ifndef __zcf_hh__
#define __zcf_hh__

//    A class defining zone centered scalar fields on the Mesh_type.
//    NOTE - now, data is of length number_of_zones, so only real cells
//    have data.

#include <cmath>
#include <iostream>
#include <vector>

#include <boost/type_traits.hpp>

#include "ASSERT.hh"

#include <cstdio>
#include <algorithm>
#ifdef USE_OPENMP
#include "OpenMP_ReductionFunctors.hh"
#endif

  
namespace IMC_namespace
{

#ifdef USE_OPENMP
// Proxy for routing calls to the correct threadSum function, depending on
// the type of T:
template<typename Mesh_type, 
         typename data_type, 
         bool is_fundamental_type = boost::is_fundamental<data_type>::value>
struct zcfThreadReduction_proxy;
#endif

template<class Mesh_type, class data_type>
class zcf
{
  public:
  
    explicit zcf( const Mesh_type& Mesh_in );

    const data_type& operator[] ( unsigned int zone ) const;
    data_type& operator[] ( unsigned int zone );

    unsigned int size() const;

//    for assignment of all members of field to a scalar
    zcf& operator=( data_type x );

//    do MPI_Allreduce over replicas in mesh replicated version. The argument
//    scratch is scratch data for the MPI call, useful if we are doing many
//    consecutive calls to the function
    void replicaSum();
    void replicaSum( zcf& scratch );
    void replicaMax();
    void replicaMax( zcf& scratch );

#ifdef USE_OPENMP
    // corresponding sum/max in multithreaded environment
    void threadSum()        
        {
            zcfThreadReduction_proxy<Mesh_type, data_type>::perform_threadSum(*this);
        }

    void threadMax()
        {
            zcfThreadReduction_proxy<Mesh_type, data_type>::perform_threadMax(*this);
        }
#endif
    
  private:
#ifdef USE_OPENMP
    template<typename, typename, bool>
    friend struct zcfThreadReduction_proxy;
#endif
    std::vector<data_type> data;

    const Mesh_type& Mesh;    //    the mesh this zcf is defined on

#ifdef USE_OPENMP
    std::vector<data_type>& mThreadReductionBuffer();    
#endif

//    make these private so they can't be used
    zcf();
    zcf( const zcf& );
    zcf& operator=( const zcf& rhs );
};    

#ifdef USE_OPENMP
// Specialize zcfThreadReduction_proxy to call the correct member of fcf depending
// on the value of is_fundamental_type.
template<typename Mesh_type, 
         typename data_type, 
         bool is_fundamental_type>
struct zcfThreadReduction_proxy
{
    static void perform_threadSum(zcf<Mesh_type, data_type>& obj)
        {

            //
            // one thread zeros out the reduction buffer.  Since fcf may have a data_type
            // without a 'true' zero element, we use a unary_operator functor to hide this fact.
            // Zero_functor::operator() return 0 for data_types that have a zero and trips
            // an error condition if not
            //
            std::vector<data_type> &theThreadReductionBuffer = obj.mThreadReductionBuffer();
            std::vector<data_type> &theData = obj.data;

#pragma omp single
            {
                if( theThreadReductionBuffer.size() != theData.size() )
                    theThreadReductionBuffer.resize(theData.size());
                std::transform(theThreadReductionBuffer.begin(),theThreadReductionBuffer.end(),
                               theThreadReductionBuffer.begin(),Zero_functor<data_type>());
            }//end of single block

            //
            // in a thread-safe fashion, sum all thread's theData vector into the 
            // theThreadReductionBuffer.  Again, since data_type can be a type where '+' is 
            // not defined we use the functor trick mentioned above to get this to work.
            //
            for(size_t i = 0 ; i < theData.size() ; i++)
            {
#pragma omp atomic
                theThreadReductionBuffer[i] += theData[i];
            }
    
            //wait until all threads accumulate into the buffer
#pragma omp barrier

            // copy the thread accmulation back into theData
            std::copy(theThreadReductionBuffer.begin(), theThreadReductionBuffer.end(),theData.begin());
#pragma omp barrier
        }

    static void perform_threadMax(zcf<Mesh_type, data_type>& obj)
        {
            // grr... max cannot be used in atomic statement in C/C++
    
            //
            // one thread zeros out the reduction buffer.  Since fcf may have a data_type
            // without a 'true' zero element, we use a unary_operator functor to hide this fact.
            // Zero_functor::operator() return 0 for data_types that have a zero and trips
            // an error condition if not
            //
            std::vector<data_type> &theThreadReductionBuffer = obj.mThreadReductionBuffer();
            std::vector<data_type> &theData = obj.data;
#pragma omp single
            {
                if( theThreadReductionBuffer.size() != theData.size() )
                    theThreadReductionBuffer.resize(theData.size());
                std::transform(theThreadReductionBuffer.begin(),theThreadReductionBuffer.end(),
                               theThreadReductionBuffer.begin(),Zero_functor<data_type>());
            }//end of single block

            //
            // in a thread-safe fashion, sum all thread's theData vector into the 
            // theThreadReductionBuffer.  Again, since data_type can be a type where '+' is 
            // not defined we use the functor trick mentioned above to get this to work.
            //
#pragma omp critical (threadSumReduction)
            {
                std::transform(theData.begin(), theData.end(), theThreadReductionBuffer.begin(),
                               theThreadReductionBuffer.begin(),Max_functor<data_type>());
            } //end of critical block

            //wait until all threads accumulate into the buffer
#pragma omp barrier

            // copy the thread accmulation back into theData
            std::copy(theThreadReductionBuffer.begin(), theThreadReductionBuffer.end(),theData.begin());
#pragma omp barrier
    
        }
};

template<typename Mesh_type, 
         typename data_type> 
struct zcfThreadReduction_proxy<Mesh_type, data_type, false>
{
    static void perform_threadSum(zcf<Mesh_type, data_type>& obj)
        {
            //
            // one thread zeros out the reduction buffer.  Since fcf may have a data_type
            // without a 'true' zero element, we use a unary_operator functor to hide this fact.
            // Zero_functor::operator() return 0 for data_types that have a zero and trips
            // an error condition if not
            //
            std::vector<data_type> &theThreadReductionBuffer = obj.mThreadReductionBuffer();
            std::vector<data_type> &theData = obj.data;

#pragma omp single
            {
                if( theThreadReductionBuffer.size() != theData.size() )
                    theThreadReductionBuffer.resize(theData.size());
                std::transform(theThreadReductionBuffer.begin(),theThreadReductionBuffer.end(),
                               theThreadReductionBuffer.begin(),Zero_functor<data_type>());
            }//end of single block

            //
            // in a thread-safe fashion, sum all thread's theData vector into the 
            // theThreadReductionBuffer.  Again, since data_type can be a type where '+' is 
            // not defined we use the functor trick mentioned above to get this to work.
            //
#pragma omp critical (threadSumReduction)
            {
                std::transform(theData.begin(), theData.end(), theThreadReductionBuffer.begin(),
                               theThreadReductionBuffer.begin(),Sum_functor<data_type>());
            } //end of critical block

            //wait until all threads accumulate into the buffer
#pragma omp barrier

            // copy the thread accmulation back into theData
            std::copy(theThreadReductionBuffer.begin(), theThreadReductionBuffer.end(),theData.begin());
#pragma omp barrier
    
        }
    static void perform_threadMax(zcf<Mesh_type, data_type>& obj)
        {
            // grr... max cannot be used in atomic statement in C/C++
    
            //
            // one thread zeros out the reduction buffer.  Since fcf may have a data_type
            // without a 'true' zero element, we use a unary_operator functor to hide this fact.
            // Zero_functor::operator() return 0 for data_types that have a zero and trips
            // an error condition if not
            //
            std::vector<data_type> &theThreadReductionBuffer = obj.mThreadReductionBuffer();
            std::vector<data_type> &theData = obj.data;
#pragma omp single
            {
                if( theThreadReductionBuffer.size() != theData.size() )
                    theThreadReductionBuffer.resize(theData.size());
                std::transform(theThreadReductionBuffer.begin(),theThreadReductionBuffer.end(),
                               theThreadReductionBuffer.begin(),Zero_functor<data_type>());
            }//end of single block

            //
            // in a thread-safe fashion, sum all thread's theData vector into the 
            // theThreadReductionBuffer.  Again, since data_type can be a type where '+' is 
            // not defined we use the functor trick mentioned above to get this to work.
            //
#pragma omp critical (threadSumReduction)
            {
                std::transform(theData.begin(), theData.end(), theThreadReductionBuffer.begin(),
                               theThreadReductionBuffer.begin(),Max_functor<data_type>());
            } //end of critical block

            //wait until all threads accumulate into the buffer
#pragma omp barrier

            // copy the thread accmulation back into theData
            std::copy(theThreadReductionBuffer.begin(), theThreadReductionBuffer.end(),theData.begin());
#pragma omp barrier
    
        }
};
#endif  // USE_OPENMP


//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>
inline zcf<Mesh_type, data_type>::
zcf( const Mesh_type& Mesh_in)
    :  data( Mesh_in.N_zones()+1 ),
       Mesh( Mesh_in )
{
#ifdef USE_OPENMP
    std::vector<data_type> &accumBuffer =  mThreadReductionBuffer();
#endif
//     accumBuffer.resize(5);
}

//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>                
inline const data_type& zcf<Mesh_type, data_type>::
operator[] ( unsigned int zone ) const
{
    ASSERT( zone < data.size() );
    
    return data[zone];
}
    
//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>                
inline data_type& zcf<Mesh_type, data_type>::
operator[] ( unsigned int zone )
{
    ASSERT( zone < data.size() );
    
    return data[zone];
}
        
//---------------------------------------------------------------------------//
    
template<class Mesh_type, class data_type>
inline unsigned int zcf<Mesh_type, data_type>::
size() const
{
    return static_cast<unsigned int>(data.size());
}    

//---------------------------------------------------------------------------//

}    //   namespace IMC_namespace

#endif    //    end of zcf.hh







