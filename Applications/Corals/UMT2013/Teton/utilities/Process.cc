//---------------------------------------------------------------------------
//
// Process.cc
//
//---------------------------------------------------------------------------

#include "utilities/Process.hh"

//#define MPIDBG 1

extern "C"
{
#include "mpi.h"
}
#include <cstdlib>
#include <iostream>

// static members
int Process::numProcs = 0;
int Process::isInitialized = -1;
int Process::isparallel = -1;


int Process::getNextTag(const MPI_Comm& comm) {
  static int nextID=0;
  int maxID=-1;
  
  MPI_Allreduce(&nextID,&maxID,1,MPI_INT,MPI_MAX,comm);
  
  nextID=maxID+1;
  
  return nextID;
} 

//----------------------------------------------------------------------------
void Process::haltAll(const char* msg) {
   std::cout << msg << std::endl;
   std::cout.flush();
   std::cerr.flush();  
   if (Process::mpiInitialized())
   {
      MPI_Abort(MPI_COMM_WORLD, 1);
   } 
   abort();
} // end Process::haltAll
//----------------------------------------------------------------------------


//----------------------------------------------------------------------------
void Process::haltAll(const std::string& msg) {
   Process::haltAll(msg.c_str());
} // end Process::haltAll
//----------------------------------------------------------------------------

