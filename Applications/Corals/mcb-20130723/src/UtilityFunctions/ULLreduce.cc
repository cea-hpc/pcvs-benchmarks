
#include "ULLreduce.hh"
#include "ASSERT.hh"
#include <iostream>
#include <limits>

namespace IMC_namespace
{

   ////////////////////////////////////////////////////////////////////////

   //! A sizeof operator for MPI Datatypes.
   size_t mpi_sizeof( MPI_Datatype * type )
   {
      int mpi_typesize;
      MPI_Type_size(*type, &mpi_typesize);
      return mpi_typesize;
   }

   ////////////////////////////////////////////////////////////////////////

   //! The MPI operation for computing the sum.
   void ull_sum( void* invec,
                 void* inoutvec,
                 int *len,
                 MPI_Datatype* datatype)
   {

      ASSERT( mpi_sizeof(datatype) == sizeof(unsigned long long) );

      // Cast the void* to our data type.
      unsigned long long* in = static_cast<unsigned long long*>(invec);
      unsigned long long* inout = static_cast<unsigned long long*>(inoutvec);

      for( int i=0; i< *len; ++i)
      {
         *inout += *in;
         ++inout;
         ++in;
      }
   }

   ////////////////////////////////////////////////////////////////////////

   //! The MPI operation for finding the maximum.
   void ull_max( void* invec,
                 void* inoutvec,
                 int *len,
                 MPI_Datatype* datatype)
   {

      ASSERT( mpi_sizeof(datatype) == sizeof(unsigned long long) );

      // Cast the void* to our data type.
      unsigned long long* in = static_cast<unsigned long long*>(invec);
      unsigned long long* inout = static_cast<unsigned long long*>(inoutvec);

      for( int i=0; i< *len; ++i)
      {
         if( *inout < *in )
         {
            *inout = *in;
         }
         ++inout;
         ++in;
      }
   }

   ////////////////////////////////////////////////////////////////////////

   //! Set MPI types and operations for unsigned long longs, trying
   //! to use native operations first, if possible.
   ULLreduce::ULLreduce()
      : custom_ops(false)
   {
#ifdef MPI_UNSIGNED_LONG_LONG
      // If there is an MPI_UNSIGNED_LONG_LONG, then we're OK, and we'll just use that.
      type = MPI_UNSIGNED_LONG_LONG;
      sum = MPI_SUM;
      max = MPI_MAX;
#else
      if( sizeof(unsigned long long) == sizeof(unsigned long) )
      {
         // If unsinged long's and unsinged long long's are really the same,
         // then use them.  This will be the case on most 64 bit machines.
         type = MPI_UNSIGNED_LONG;
         sum = MPI_SUM;
         max = MPI_MAX;
      }
      else if( sizeof(unsigned long long) == sizeof(double) )
      {
         // double should be the same size as unsigned long long, so use that with our
         // own special functions.
         custom_ops = true;
         type = MPI_DOUBLE;
         MPI_Op_create( ull_sum, true, &sum );
         MPI_Op_create( ull_max, true, &max );
      }
      else
      {
         // We could have more options to go through the whole list of data types, trying to find one
         // that worked.  First time this error is called, I suspect we'll add something.
         std::cerr << "ERROR(ULLreduce):  Cannot find a type the same length as an unsigned long long.\n";
         MPI_Abort(MPI_COMM_WORLD, -1);
      }
#endif

      // Whatever we chose above, make sure MPI and the compiler agree on the size
      // of the data, otherwise things will be broken.
      if( mpi_sizeof(&type) != sizeof(unsigned long long) )
      {
         std::cerr << "ERROR(ULLreduce):  MPI_Datatype " 
            << type 
            << " is not the same size as an unsigned long long.\n";
         std::cerr << "                   sizeof(MPI_Datatype) = " 
            << mpi_sizeof(&type) 
            << " != sizeof(signed long long) = "
            << sizeof(unsigned long long)
            << ".\n";
         MPI_Abort(MPI_COMM_WORLD, -1);
      }

   }
   ////////////////////////////////////////////////////////////////////////

   //! Free the custom operations, if we have them.
   ULLreduce::~ULLreduce()
   {
      if(custom_ops)
      {
         MPI_Op_free( &sum );
         MPI_Op_free( &max );
      }
   }
}

