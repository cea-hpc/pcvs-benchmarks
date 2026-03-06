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

//    If a class is templated on data_type, and we need to do an OpenMP
//    reduction operation, it needs an operator(). This traits class provides that mapping.
#ifndef __OPENMP_FUNCTORS__
#define __OPENMP_FUNCTORS__

#include <iostream>
#include <algorithm>
#include "Vector3d.hh"

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    general case has to provide functions for compilation of all zcf types
//    even if we don't call reduce on them

template<typename arg_t>
struct Zero_functor
{
	arg_t operator() (arg_t a)
	{
		std::cout<<"Zero_functor default case forbidden."<<std::endl;
		exit(1);	
    return arg_t();
	}
};

//---------------------------------------------------------------------------//

template<>
struct Zero_functor<double>
{
	double operator() (double a)
        {
            return 0.0;
        }
};


//---------------------------------------------------------------------------//

template<>
struct Zero_functor<int>
{
    int operator() (int a)
        {
            return 0;
        }
};

//---------------------------------------------------------------------------//

template<>
struct Zero_functor<unsigned int>
{
    unsigned int operator() (unsigned int a)
        {
            return 0;
        }
};
    
//---------------------------------------------------------------------------//

template<>
struct Zero_functor<long>
{
	long operator() (long a)
	{
      return 0;
	}
};

//---------------------------------------------------------------------------//
 
template<>
struct Zero_functor<unsigned long>
{
	unsigned long operator() (unsigned long a)
	{
		return 0;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Zero_functor<long long>
{
	long long operator() (long long a)
	{
		return 0;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Zero_functor<unsigned long long>
{
	unsigned long long operator() (unsigned long long a)
	{
		return 0;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Zero_functor<Vector3d_namespace::Vector3d>
{
	Vector3d_namespace::Vector3d operator() (Vector3d_namespace::Vector3d a)
	{
      return Vector3d_namespace::Vector3d(0.0,0.0,0.0);
      
	}
};

//---------------------------------------------------------------------------//

//    general case has to provide functions for compilation of all zcf types
//    even if we don't call reduce on them

template<typename arg_t>
struct Sum_functor
{
	arg_t operator() (arg_t a, arg_t b)
	{
		std::cout<<"default case forbidden."<<std::endl;
		exit(1);	
    return arg_t();
	}
};

//---------------------------------------------------------------------------//

template<>
struct Sum_functor<double>
{
	double operator() (double a, double b)
	{
		return a+b;
	}
};


//---------------------------------------------------------------------------//

template<>
struct Sum_functor<int>
{
	int operator() (int a, int b)
	{
		return a+b;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Sum_functor<unsigned int>
{
	unsigned int operator() (unsigned int a, unsigned int b)
	{
		return a+b;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Sum_functor<long>
{
	long operator() (long a, long b)
	{
		return a+b;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Sum_functor<unsigned long>
{
	unsigned long operator() (unsigned long a, unsigned long b)
	{
		return a+b;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Sum_functor<long long>
{
	long long operator() (long long a, long long b)
	{
		return a+b;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Sum_functor<unsigned long long>
{
	unsigned long long operator() (unsigned long long a, unsigned long long b)
	{
		return a+b;
	}
};

//---------------------------------------------------------------------------//

template<>
struct Sum_functor<Vector3d_namespace::Vector3d>
{
	Vector3d_namespace::Vector3d operator() (Vector3d_namespace::Vector3d a, Vector3d_namespace::Vector3d b)
	{
		return a+b;
	}
};


//---------------------------------------------------------------------------//

//    general case has to provide functions for compilation of all zcf types
//    even if we don't call reduce on them

template<typename arg_t>
struct Max_functor
{
	arg_t operator() (arg_t a, arg_t b)
	{
		std::cout<<"Max_functor default case forbidden."<<std::endl;
		exit(1);	
    return arg_t();
	}
};

//---------------------------------------------------------------------------//

template<>
struct Max_functor<double>
{
	double operator() (double a, double b)
	{
      return std::max(a,b);
	}
};


//---------------------------------------------------------------------------//

template<>
struct Max_functor<int>
{
	int operator() (int a, int b)
	{
      return std::max(a,b);
	}
};

//---------------------------------------------------------------------------//

template<>
struct Max_functor<unsigned int>
{
	unsigned int operator() (unsigned int a, unsigned int b)
	{
      return std::max(a,b);
	}
};

//---------------------------------------------------------------------------//

template<>
struct Max_functor<long>
{
	long operator() (long a, long b)
	{
      return std::max(a,b);
	}
};

//---------------------------------------------------------------------------//
 
template<>
struct Max_functor<unsigned long>
{
	unsigned long operator() (unsigned long a, unsigned long b)
	{
		return std::max(a,b);
	}
};

//---------------------------------------------------------------------------//

template<>
struct Max_functor<long long>
{
	long long operator() (long long a, long long b)
	{
		return std::max(a,b);
	}
};

//---------------------------------------------------------------------------//

template<>
struct Max_functor<unsigned long long>
{
	unsigned long long operator() (unsigned long long a, unsigned long long b)
	{
		return std::max(a,b);
	}
};

//---------------------------------------------------------------------------//

template<>
struct Max_functor<Vector3d_namespace::Vector3d>
{
	Vector3d_namespace::Vector3d operator() (Vector3d_namespace::Vector3d a, Vector3d_namespace::Vector3d b)
	{
      std::cout<<"Max_functor Vector3d case forbidden."<<std::endl;
      exit(1);
      return Vector3d_namespace::Vector3d(0.0,0.0,0.0);
	}
    
};



}    //    end IMC_namespace
#endif



