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

//---------------------------------------------------------------------------//

//    If a class is templated on data_type, and we need to do an MPI
//    operation, it needs access to an associated MPI type. For example, if a
//    zcf is templated on double, it needs to pass MPI_DOUBLE as an arg to
//    All_reduce. This traits class provides that mapping.
#ifndef __MPI_TYPE__
#define __MPI_TYPE__

#include <mpi.h>
#include <iostream>


namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    general case has to provide functions for compilation of all zcf types
//    even if we don't call reduce on them

template<typename data_type>
class MPI_type
{
  public:
    MPI_Datatype get_MPI_type()
    { 
        std::cout << "using an unknown type for template parameter " << std::endl;
        std::cout << "in MPI_type.hh" << std::endl;
        std::cout << "calling exit in MPI_type::get_MPI_type!" << std::endl;
        MPI_Abort(MPI_COMM_WORLD, -1);
        return MPI_INT; 
    }  
  
    MPI_Op get_MPI_SUM()
    { 
        std::cout << "using an unknown type for template parameter " << std::endl;
        std::cout << "in MPI_type.hh" << std::endl;
        std::cout << "calling exit in MPI_type::get_MPI_SUM!" << std::endl;
        MPI_Abort(MPI_COMM_WORLD, -1);
        return MPI_SUM; 
    }

    MPI_Op get_MPI_MAX()
    { 
        std::cout << "using an unknown type for template parameter " << std::endl;
        std::cout << "in MPI_type.hh" << std::endl;
        std::cout << "calling exit in MPI_type::get_MPI_SUM!" << std::endl;
        MPI_Abort(MPI_COMM_WORLD, -1);
        return MPI_SUM; 
    }
};

//---------------------------------------------------------------------------//

template<>
class MPI_type<double>
{
  public:
    MPI_Datatype get_MPI_type() { return MPI_DOUBLE; }
    MPI_Op get_MPI_SUM() { return MPI_SUM; }
    MPI_Op get_MPI_MAX() { return MPI_MAX; }
};

//---------------------------------------------------------------------------//

template<>
class MPI_type<int>
{
  public:
    MPI_Datatype get_MPI_type() { return MPI_INTEGER; }
    MPI_Op get_MPI_SUM() { return MPI_SUM; }
    MPI_Op get_MPI_MAX() { return MPI_MAX; }
};

//---------------------------------------------------------------------------//

template<>
class MPI_type<unsigned int>
{
  public:
    MPI_Datatype get_MPI_type() { return MPI_UNSIGNED; }
    MPI_Op get_MPI_SUM() { return MPI_SUM; }
    MPI_Op get_MPI_MAX() { return MPI_MAX; }
};

//---------------------------------------------------------------------------//

template<>
class MPI_type<long>
{
  public:
    MPI_Datatype get_MPI_type() { return MPI_LONG; }
    MPI_Op get_MPI_SUM() { return MPI_SUM; }
    MPI_Op get_MPI_MAX() { return MPI_MAX; }
};

//---------------------------------------------------------------------------//

template<>
class MPI_type<unsigned long>
{
  public:
    MPI_Datatype get_MPI_type() { return MPI_UNSIGNED_LONG; }
    MPI_Op get_MPI_SUM() { return MPI_SUM; }
    MPI_Op get_MPI_MAX() { return MPI_MAX; }
};

//---------------------------------------------------------------------------//

template<>
class MPI_type<long long>
{
  public:
    MPI_Datatype get_MPI_type() { return MPI_LONG_LONG; }
    MPI_Op get_MPI_SUM() { return MPI_SUM; }
    MPI_Op get_MPI_MAX() { return MPI_MAX; }
};

//---------------------------------------------------------------------------//

template<>
class MPI_type<unsigned long long>
{
  public:
    MPI_Datatype get_MPI_type() { return MPI_UNSIGNED_LONG_LONG; }
    MPI_Op get_MPI_SUM() { return MPI_SUM; }
    MPI_Op get_MPI_MAX() { return MPI_MAX; }
};

//---------------------------------------------------------------------------//

//    generic function to add vectors together

template<typename GenericVectorType>
void genericVectorSum( void* inVoid,
                       void* inoutVoid,
                       int *len,
                       MPI_Datatype* datatype )
{
// Cast the void* to our data type.
    GenericVectorType* in = static_cast<GenericVectorType*>(inVoid);
    GenericVectorType* inout = static_cast<GenericVectorType*>(inoutVoid);
    
    for (int i = 0; i < *len; ++i)
    {
        *inout = (*in + *inout);
        ++in;
        ++inout;
    }
}

}    //    end namespace IMC_namespace

//    Here add types that are specific to different implimentations. For
//    example, Alegra ull stuff can go here.

//---------------------------------------------------------------------------//

//    need to specialize for Vector3d classes
//    here, stand-alone mesh version for Vector3d_namespace::Vector3d

#include "Vector3d.hh"

namespace IMC_namespace
{

template<>
class MPI_type<Vector3d_namespace::Vector3d>
{
  public:

    MPI_type()
    {
        MPI_Type_contiguous( 3, MPI_DOUBLE, &vector3dType );
        MPI_Type_commit( &vector3dType );

        MPI_Op_create( genericVectorSum<Vector3d_namespace::Vector3d>, 
                       true, 
                       &vector3dSum );
    }

    ~MPI_type()
    {
        MPI_Type_free( &vector3dType );
        MPI_Op_free( &vector3dSum );
    }

    MPI_Datatype get_MPI_type() { return vector3dType; }

    MPI_Op get_MPI_SUM() { return vector3dSum; }

    MPI_Op get_MPI_MAX()
    { 
        std::cout << "can't do MAX for Vector3d type " << std::endl;
        std::cout << "in MPI_type.hh" << std::endl;
        std::cout << "calling exit in MPI_type::get_MPI_MAX!" << std::endl;
        MPI_Abort(MPI_COMM_WORLD, -1);
        return MPI_MAX; 
    }

  private:
    MPI_Datatype vector3dType;
    MPI_Op vector3dSum;
};

}    //    end IMC_namespace




#endif
