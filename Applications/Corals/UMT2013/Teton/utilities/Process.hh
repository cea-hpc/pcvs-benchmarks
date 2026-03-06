//---------------------------------------------------------------------------
// Copyright (c) 2001 Regents of the University of California. 
// All rights reserved.
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// For full text see LICENSE.TXT
//-----------------------------------------------------------------------
// \author ???
// Modified by Susan Hazlett.  Merged MPIHelper with Process (MPIHelper
// only existed in a sub branch of main while working on communications.)
//---------------------------------------------------------------------------

// The Process "class" is a set of static methods that give you information 
// about the running process and communication status.  

#ifndef PROCESS_HH
#define PROCESS_HH

#include "mpi.h"
#include <string>
#include <vector>
#include "utilities/DBC.hh"

class Process
{
public:

   //! \return The total number of processes associated with MPI_COMM_WORLD
   static int  getNumberOfProcs()
   {
      if (numProcs == 0)
      {
         if (Process::mpiInitialized())
            MPI_Comm_size(MPI_COMM_WORLD, &numProcs);
         else
            numProcs = 1;
      }
      return numProcs;
   }

   static int  getNumberOfProcs(const MPI_Comm& comm)
   {
      if (numProcs == 0) 
      {
         if (Process::mpiInitialized())
            MPI_Comm_size(comm, &numProcs);
         else
            numProcs = 1;
      }
      return numProcs;
   }

   //! \return True if MPI is initialized
   static int  mpiInitialized()
   {
      if (isInitialized == -1)
      {
         MPI_Initialized(&isInitialized);
      }
      return isInitialized;
   }

   //! \return The rank of this process based on an arbitrary communicator.
   static int  getRank(const MPI_Comm& comm=MPI_COMM_WORLD)
   {
      // if we're not parallel, I'm always proc 0.
      // no preprocessor guards needed - done in mpiInitialized.
      int rank = 0;
      if (mpiInitialized())
      {
         MPI_Comm_rank(comm, &rank);
      }
      return rank;
   }

 
   static int getNextTag(const MPI_Comm& comm=MPI_COMM_WORLD);

   //! \return True if this is a parallel job.  Checks to see if procs > 1 and
   //! mpiInitialized.
   static int isParallel() 
   { 
      if (isparallel == -1)
         isparallel = mpiInitialized() && (getNumberOfProcs() > 1);
      return isparallel; 
   }

   //! Terminate all processes with prejudice
   static void haltAll (const char* msg);
   static void haltAll (const std::string& msg);

private:
   Process();
   ~Process();
   Process& operator=(const Process&);
   Process(const Process&);

   static int numProcs;
   static int isInitialized;
   static int isparallel;

}; // end class Process

// -------------------------------------------------------
// MPI_DEBUG_MSG (msg)
//
// Prints out rank based messages and flushes buffer if
// MPIDBG is defined.
// -------------------------------------------------------
#ifdef MPIDBG
#define MPI_DEBUG_MSG(msg) \
std::cout << Process::getRank() << ":" << msg << std::endl; \
std::cout.flush()
#else
#define MPI_DEBUG_MSG(msg) ;
#endif

#endif // COMMUNICATION_PROCESS_HH
