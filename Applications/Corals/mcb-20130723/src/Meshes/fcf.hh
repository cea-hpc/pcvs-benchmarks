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

#ifndef __fcf_hh__
#define __fcf_hh__

//    A class defining face centered scalar fields on the Mesh_type.

#include <cmath>
#include <iostream>
#include <vector>

#include <boost/type_traits.hpp>

#include <cstdio>
#include <algorithm>
#include "OpenMP_ReductionFunctors.hh"

#include "ASSERT.hh"  

namespace IMC_namespace
{

// Proxy for routing calls to the correct threadSum function, depending on
// the type of T:
template<typename Mesh_type, 
         typename data_type, 
         bool is_fundamental_type = boost::is_fundamental<data_type>::value>
struct fcfThreadReduction_proxy;

template<class Mesh_type, class data_type>
class fcf
{
  public:
    explicit fcf( const Mesh_type& Mesh_in );

    const data_type& operator[] ( unsigned int face ) const;
    data_type& operator[] ( unsigned int face );

    unsigned int size() const;

//    for iterating over data
    typedef typename std::vector<data_type>::iterator iterator;
    typename std::vector<data_type>::iterator begin() { return data.begin(); }
    typename std::vector<data_type>::iterator end() { return data.end(); }

//    for assignment of all members of field to a scalar
    fcf& operator=( data_type x );

//    do MPI_Allreduce over replicas in mesh replicated version.
    void replicaSum();
    void replicaMax();

    // corresponding sum/max in multithreaded environment
    void threadSum()
        {
            fcfThreadReduction_proxy<Mesh_type, data_type>::perform_threadSum(*this);
        }
    
    void threadMax()
        {
            fcfThreadReduction_proxy<Mesh_type, data_type>::perform_threadMax(*this);
        }
    
  private:

    template<typename, typename, bool>
    friend struct fcfThreadReduction_proxy;

    std::vector<data_type> data;

    const Mesh_type& Mesh;    //    the mesh this fcf is defined on

    std::vector<data_type>& mThreadReductionBuffer();    

//    make these private so they can't be used
    fcf();
    fcf( const fcf& );
    fcf& operator=( const fcf& rhs );

};    

// Specialize fcfThreadReduction_proxy to call the correct member of fcf depending
// on the value of is_fundamental_type.
template<typename Mesh_type, 
         typename data_type, 
         bool is_fundamental_type>
struct fcfThreadReduction_proxy
{
    static void perform_threadSum(fcf<Mesh_type, data_type>& obj)
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

            for( size_t i = 0;i < theData.size(); i++)
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

    static void perform_threadMax(fcf<Mesh_type, data_type>& obj)
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
struct fcfThreadReduction_proxy<Mesh_type, data_type, false>
{
    static void perform_threadSum(fcf<Mesh_type, data_type>& obj)
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

    static void perform_threadMax(fcf<Mesh_type, data_type>& obj)
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

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>
inline fcf<Mesh_type, data_type>::
fcf( const Mesh_type& Mesh_in)
    : data( Mesh_in.N_faces()+1 ),
      Mesh( Mesh_in )
{
    std::vector<data_type> &accumBuffer =  mThreadReductionBuffer();
}

//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>                
inline const data_type& fcf<Mesh_type, data_type>::
operator[] ( unsigned int face ) const
{
    ASSERT( face < data.size() );
    
    return data[face];
}
    
//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>                
inline data_type& fcf<Mesh_type, data_type>::
operator[] ( unsigned int face )
{
    ASSERT( face < data.size() );
    
    return data[face];
}
    
//---------------------------------------------------------------------------//
    
template<class Mesh_type, class data_type>
inline unsigned int fcf<Mesh_type, data_type>::
size() const
{
    return data.size();
}    


//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif    //    end of fcf.hh







