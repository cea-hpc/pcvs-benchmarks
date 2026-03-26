//------------------------------*-C++-*----------------------------------
// Copyright 2001 The Regents of the University of California. 
// All rights reserved.
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// For full text see LICENSE.TXT
//-----------------------------------------------------------------------

// Currently maintained by Jeff Johnson.


#include "DomainNeighborMap.hh"
#include <map>
#include <iostream>
#include "mpi.h"
using namespace std;

namespace Communication
{


// ----------------------------------------------------------------------
template <typename Element>
DomainNeighborMap<Element>::
DomainNeighborMap():
   mSendMap(),
   mReceiveMap(),
   mSendProcIDs(),
   mReceiveProcIDs()
{
} // end constructor
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
DomainNeighborMap<Element>::
DomainNeighborMap(const map<size_t, vector<Element> >& sendMap,
                  const map<size_t, vector<Element> >& receiveMap):
   mSendMap(),
   mReceiveMap(),
   mSendProcIDs(),
   mReceiveProcIDs()
{
   // Assign the send and receive maps, performing all necessary checks.
   assignSendElements(sendMap);
   assignReceiveElements(receiveMap);
} // end constructor
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
DomainNeighborMap<Element>::
DomainNeighborMap (const DomainNeighborMap& map):
   mSendMap(map.mSendMap),
   mReceiveMap(map.mReceiveMap),
   mSendProcIDs(map.mSendProcIDs),
   mReceiveProcIDs(map.mReceiveProcIDs)
{
} // end copy constructor
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
DomainNeighborMap<Element>&
DomainNeighborMap<Element>::
operator=(const DomainNeighborMap& map)
{
   if (this != &map) 
   {
      mSendMap = map.mSendMap;
      mReceiveMap = map.mReceiveMap;
      mSendProcIDs = map.mSendProcIDs;
      mReceiveProcIDs = map.mReceiveProcIDs;

   } // end if
   return *this;
} // end assignment operator
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
DomainNeighborMap<Element>::
~DomainNeighborMap<Element>()
{
} // end destructor
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
void
DomainNeighborMap<Element>::
assignSendElements(size_t processID,
                   const vector<Element>& elements)
{

   // Make sure that the element list contains no duplicates.

   // Add the elements to the mapping.
   mSendProcIDs.insert(processID);
   mSendMap[processID] = elements;

} // end assignSendElements
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
void
DomainNeighborMap<Element>::
assignSendElements(const map<size_t, vector<Element> >& sendMap)
{

   // Replace the current mapping.
   mSendMap = sendMap;

   // Make a list of process IDs for the given map.
   mSendProcIDs.clear();
   for (typename map<size_t, vector<Element> >::const_iterator 
        i = mSendMap.begin(); i != mSendMap.end(); ++i)
   {
      mSendProcIDs.insert(i->first);
   } // end for

} // end assignSendElements
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
void
DomainNeighborMap<Element>::
assignReceiveElements(size_t processID,
                      const std::vector<Element>& elements)
{

   // Do the mapping.
   mReceiveProcIDs.insert(processID);
   mReceiveMap[processID] = elements;

} // end assignReceiveElements
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
void
DomainNeighborMap<Element>::
assignReceiveElements(const map<size_t, vector<Element> >& receiveMap)
{

   // Replace the current mapping.
   mReceiveMap = receiveMap;

   // Make a list of process IDs for the given map.
   mReceiveProcIDs.clear();
   for (typename map<size_t, vector<Element> >::const_iterator 
        i = mReceiveMap.begin(); i != mReceiveMap.end(); ++i)
   {
      mReceiveProcIDs.insert(i->first);
   } // end for

} // end assignSendElements
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
bool
DomainNeighborMap<Element>::
sendsTo(size_t processID) const
{
   return (mSendProcIDs.find(processID) != mSendProcIDs.end());
} // end sendsTo
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
bool
DomainNeighborMap<Element>::
receivesFrom(size_t processID) const
{
   return (mReceiveProcIDs.find(processID) != mReceiveProcIDs.end());
} // end receivesFrom
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ProcessIterator
DomainNeighborMap<Element>::
sendProcessBegin() const
{
   return mSendProcIDs.begin();
} // end sendProcessBegin
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ProcessIterator
DomainNeighborMap<Element>::
sendProcessEnd() const
{
   return mSendProcIDs.end();
} // end sendProcessEnd
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
size_t
DomainNeighborMap<Element>::
numberOfSendProcesses() const
{
   return mSendProcIDs.size();
} // end numberOfSendProcesses
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ProcessIterator
DomainNeighborMap<Element>::
receiveProcessBegin() const
{
   return mReceiveProcIDs.begin();
} // end receiveProcessBegin
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ProcessIterator
DomainNeighborMap<Element>::
receiveProcessEnd() const
{
   return mReceiveProcIDs.end();
} // end receiveProcessEnd
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
size_t
DomainNeighborMap<Element>::
numberOfReceiveProcesses() const
{
   return mReceiveProcIDs.size();
} // end numberOfReceiveProcesses
// ----------------------------------------------------------------------

// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ElementIterator
DomainNeighborMap<Element>::
sendElementBegin(size_t processID) const
{
   return mSendMap.find(processID)->second.begin();
} // end sendElementBegin
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ElementIterator
DomainNeighborMap<Element>::
sendElementEnd(size_t processID) const
{
   return mSendMap.find(processID)->second.end();
} // end sendElementEnd
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
size_t
DomainNeighborMap<Element>::
numberOfSendElements(size_t processID) const
{
   return mSendMap.find(processID)->second.size();
} // end numberOfSendElements
// ----------------------------------------------------------------------



// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ElementIterator
DomainNeighborMap<Element>::
receiveElementBegin(size_t processID) const
{
   return mReceiveMap.find(processID)->second.begin();
} // end receiveElementBegin
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
typename DomainNeighborMap<Element>::ElementIterator
DomainNeighborMap<Element>::
receiveElementEnd(size_t processID) const
{
   return mReceiveMap.find(processID)->second.end();
} // end receiveElementEnd
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
size_t
DomainNeighborMap<Element>::
numberOfReceiveElements(size_t processID) const
{
   return mReceiveMap.find(processID)->second.size();
} // end numberOfReceiveElements
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
vector<size_t>
DomainNeighborMap<Element>::
sendProcesses() const
{
   return vector<size_t>(sendProcessBegin(), sendProcessEnd());
} // end sendProcesses
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
vector<size_t>
DomainNeighborMap<Element>::
receiveProcesses() const
{
   return vector<size_t>(receiveProcessBegin(), receiveProcessEnd());
} // end receiveProcesses
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
map<size_t, vector<Element> >
DomainNeighborMap<Element>::
sendMap() const
{
   return mSendMap;
} // end sendMap
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
map<size_t, vector<Element> >&
DomainNeighborMap<Element>::
getSendMap()
{
   return mSendMap;
} // end sendMap
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
map<size_t, vector<Element> >
DomainNeighborMap<Element>::
receiveMap() const
{
   return mReceiveMap;
} // end receiveMap
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
map<size_t, vector<Element> >&
DomainNeighborMap<Element>::
getReceiveMap()
{
   return mReceiveMap;
} // end receiveMap
// ----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
void
DomainNeighborMap<Element>::
clear() 
{
   mSendMap.clear();
   mReceiveMap.clear();
   mSendProcIDs.clear();
   mReceiveProcIDs.clear();
} // end clear
// ----------------------------------------------------------------------


//-----------------------------------------------------------------------
template <typename Element>
bool
DomainNeighborMap<Element>::
operator==(const DomainNeighborMap<Element>& rhs) const
{
   return ((mSendMap == rhs.mSendMap) &&
         (mReceiveMap == rhs.mReceiveMap) &&
         (mSendProcIDs == rhs.mSendProcIDs) &&
         (mReceiveProcIDs == rhs.mReceiveProcIDs));
} // end operator==
//-----------------------------------------------------------------------


//-----------------------------------------------------------------------
template <typename Element>
bool
DomainNeighborMap<Element>::
operator!=(const DomainNeighborMap<Element>& rhs) const
{
   return !(*this == rhs);
} // end operator!=
//-----------------------------------------------------------------------


#if 0
//-----------------------------------------------------------------------
template <typename Element>
bool
DomainNeighborMap<Element>::
isValid(const typename MeshTypeStruct<Element>::MeshType *space) const
{

   typedef typename Element::MeshType MeshType;
   //MeshType

   // I'm an optimist.  I assume validity until proven wrong.
   bool valid = true;
#ifdef USE_MPI
   if (Process::isParallel())
   {
      // Okay, okay, okay... we need to verify that the send and receive 
      // info in this object corresponds to the send and receive info 
      // in the objects on neighboring domains corresponding to this object.
      // This means that if I have to transmit a certain set of elements
      // to a given processor, then that processor should be expecting that 
      // set of elements.

      // We'll need an MPI communicator and an MPI tag.  
      MPI_Comm comm = MPI_COMM_WORLD;
      int tag = 0;

      // Make sure the numbers of send/receive elements are consistent.
      vector<shared_ptr<MessageBuffer> > sendSizeBuffers(numberOfSendProcesses());
      size_t i = 0;
      for (ProcessIterator p = sendProcessBegin(); p != sendProcessEnd(); ++p)
      {
         // There should be a positive number of send elements.
         ASSERT(numberOfSendElements(*p) > 0);
         sendSizeBuffers[i].reset(new MessageBuffer());
         sendSizeBuffers[i]->pack(numberOfSendElements(*p));
         ++i;
      } // end for
      vector<shared_ptr<MessageBuffer> > receiveSizeBuffers(numberOfReceiveProcesses());
      for (i = 0; i < numberOfReceiveProcesses(); ++i)
      {
         receiveSizeBuffers[i].reset(new MessageBuffer(sizeof(size_t)));
      } // end for

      if (space->getDomainManager().getMasterProcessID() == Process::getRank()){
         Communicate::exchange(sendSizeBuffers, receiveSizeBuffers, 
               *this, tag, comm);
      }
      // Update any domain group slaves in the domain replicated case.
      space->getDomainManager().updateDomainGroup(receiveSizeBuffers, tag, comm);

      i = 0;
      for (ProcessIterator p = receiveProcessBegin(); p != receiveProcessEnd(); ++p)
      {
         size_t size;
         receiveSizeBuffers[i]->unpack(size);
         // There should be a positive number of receive elements.
         ASSERT(numberOfReceiveElements(*p) > 0);
         if (size != numberOfReceiveElements(*p))
         {
std::cout << "Number of receive elements for process " << *p << " is wrong: "
   << size << " != " << numberOfReceiveElements(*p) << "\n";
            valid = false;
         } // end if
         ++i;
      } // end for

      // If we are still valid, do the rest of the validity check.
      if (valid)
      {
         // Fill the send buffers in with the send structure data.
         vector<shared_ptr<MessageBuffer> > sendBuffers(numberOfSendProcesses());
         size_t i = 0;
         for (ProcessIterator p = sendProcessBegin(); p != sendProcessEnd(); ++p)
         {
            sendBuffers[i].reset(new MessageBuffer());
            for (size_t j = 0; j < mSendMap.find(*p)->second.size(); ++j)
            {
               const Element e = (mSendMap.find(*p)->second)[j];
               sendBuffers[i]->pack(e.getMesh().getGlobalID(e));
            } // end for
            ++i;
         } // end for

         // Presize the receive buffers.
         vector<shared_ptr<MessageBuffer> > receiveBuffers(numberOfReceiveProcesses());
         i = 0;
         for (ProcessIterator p = receiveProcessBegin(); p != receiveProcessEnd(); ++p)
         {
            receiveBuffers[i].reset(new MessageBuffer(sizeof(Element) * numberOfReceiveElements(*p)));
            ++i;
         } // end for


         // Perform the exchange.
         if (space->getDomainManager().getMasterProcessID() == Process::getRank()){
            Communicate::exchange(sendBuffers, receiveBuffers, *this, tag, comm);
         }
         // Update any domain group slaves in the domain replicated case.
         space->getDomainManager().updateDomainGroup(receiveBuffers, tag, comm);


         // Now make sure that the data are consistent.
         i = 0;
         for (ProcessIterator p = receiveProcessBegin(); 
               p != receiveProcessEnd(); ++p)
         {
            for (size_t j = 0; j < mReceiveMap.find(*p)->second.size(); ++j)
            {
               size_t id;
               receiveBuffers[i]->unpack(id);
               const Element e = (mReceiveMap.find(*p)->second)[j];
               if (id != (e.getMesh().getGlobalID(e)))
               {
                  valid = false;
std::cout << "ID: " << id << " != " << e.getMesh().getGlobalID(e) << std::endl;
                  break;
               } // end if
            } // end for
            ++i;
         } // end for

      } // end if

   } // end if
#endif
   return valid;
} // end isValid
//-----------------------------------------------------------------------
#endif

//-----------------------------------------------------------------------
template <>
bool
DomainNeighborMap<int>::
isValid(const MeshTypeStruct<int>::MeshType *) const
{
   // This method is only here for completeness right now.
   return true;
} // end isValid
//-----------------------------------------------------------------------


// ----------------------------------------------------------------------
template <typename Element>
std::ostream&
operator<<(std::ostream& out, const DomainNeighborMap<Element>& dm) 
{
    int myRank;
    MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
    
   typedef DomainNeighborMap<Element> DNM;
   for (typename DNM::ProcessIterator p = dm.sendProcessBegin(); 
        p != dm.sendProcessEnd(); ++p)
   {
      out << "Process " << myRank << " -> Process " << *p << ": ";
      for (typename DNM::ElementIterator e = dm.sendElementBegin(*p);
           e != dm.sendElementEnd(*p); ++e)
      {
         out << e->getMesh().getGlobalID(*e) << ' '; 
      } // end for
      out << endl;
   } // end for

   for (typename DNM::ProcessIterator p = dm.receiveProcessBegin(); 
        p != dm.receiveProcessEnd(); ++p)
   {
      out << "Process " << myRank << " <- Process " << *p << ": ";
      for (typename DNM::ElementIterator e = dm.receiveElementBegin(*p);
           e != dm.receiveElementEnd(*p); ++e)
      {
         out << e->getMesh().getGlobalID(*e) << ' '; 
      } // end for
      out << endl;
   } // end for

   return out;
} // end operator<<
// ----------------------------------------------------------------------


} // end namespace Communication
//-----------------------------------------------------------------------
//                              end of <basename>.cc
//-----------------------------------------------------------------------

