#ifndef ULLREDUCE_HH
#define ULLREDUCE_HH

#include "mpi.h"

namespace IMC_namespace
{
   //! \brief Hides the ugliness of supporting 64-bit unsigned long long
   //! operations on all machines.
   //!
   //! NOTE: This only works on homogeneous clusters, if we need to resort to tricks.
   class ULLreduce
   {
      public:

         //! Set the operations up.  This can be expensive, so you should keep each instance
         //! around as long as possible.
         ULLreduce();
         //! Free custom operations, if we have them.
         ~ULLreduce();

         //! Access to the data type to pass to MPI_Functions
         const MPI_Datatype& getType() const { return type; }
         //! The Sum operator for MPI
         const MPI_Op& getSum() const { return sum; }
         //! The Max operator for MPI
         const MPI_Op& getMax() const { return max; }

      private:

         MPI_Datatype type;
         MPI_Op sum;
         MPI_Op max;

         //! If we register a custom operation, this is true.
         bool custom_ops;
   };
}

#endif // ULLREDUCE_HH
