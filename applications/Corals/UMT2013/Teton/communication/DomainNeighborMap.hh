//------------------------------*-C++-*----------------------------------
// Copyright (c) 1997-2002 Regents of the University of California. 
// All rights reserved.
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// For full text see LICENSE.TXT
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// DomainNeighborMap
//-----------------------------------------------------------------------

#ifndef COMMUNICATION_DOMAINNEIGHBORMAP_HH
#define COMMUNICATION_DOMAINNEIGHBORMAP_HH

#include <set>
#include <vector>
#include <map>

namespace Communication
{

    typedef std::vector<size_t> ProcIDList; 


// Apologies for this ugliness.  DomainNeighborMap<Element> can be 
// instantiated with type int, in which case there is no type MeshType
// nested in Element.  This is required for the isValid member function, 
// which now takes a pointer to the mesh over which this DomainNeighborMap
// is defined.  Therefore, I use this struct to get the true type in the 
// "real" cases, and the type is void in the int case. -TJA aug.31.06


    template <typename Element>
    struct MeshTypeStruct {
        typedef typename Element::MeshType MeshType;
    };

    template<>
    struct MeshTypeStruct<int> {
        typedef void MeshType;
    };

//! \class DomainNeighborMap
//! This class represents a set of neighbors for this process.  More 
//! specifically, this process has a list of neighboring processes, and a list
//! of send elements on those processes that correspond to receive elements
//! on this process (as well as receive elements on those processes that 
//! correspond to send elements on this process).  This data structure 
//! represents the mapping of a neighboring process ID to its constituent 
//! elements.
    template <typename Element>
    class DomainNeighborMap
    {
    public:
        // Typedef
        typedef typename std::set<size_t>::const_iterator  ProcessIterator;
        typedef typename std::vector<Element>::const_iterator ElementIterator;

        //! Build an empty domain neighbor map.
        DomainNeighborMap();

        //! Build a domain neighbor map using maps relating processor IDs to 
        //! elements.  A send map and a receive map are needed.
        //! \param sendMap The map relating send procs -> send elements.
        //! \param receiveMap The map relating receive procs -> receive elements.
        DomainNeighborMap(const std::map<size_t, std::vector<Element> >& sendMap,
                          const std::map<size_t, std::vector<Element> >& receiveMap);

        //! Copy constructor.
        DomainNeighborMap(const DomainNeighborMap& map);

        //! Assignment operator.
        DomainNeighborMap& operator=(const DomainNeighborMap& map);

        //! Destructor.
        ~DomainNeighborMap();

        //! Associate a list of send elements with a given process ID.
        //! This list of elements will REPLACE the former list of send elements 
        //! associated with the given process ID.
        //! \param processID The given process ID (MPI rank).
        //! \param elements The list of send elements for the given process ID.
        void assignSendElements(size_t processID, 
                                const std::vector<Element>& elements);

        //! Define the mapping of send processes to send elements using a given map.
        //! This mapping will replace the existing mapping within the data structure.
        //! \param sendMap The given map of send processes -> send elements.
        void assignSendElements(const std::map<size_t, std::vector<Element> >& sendMap);

        //! Associate a list of receive elements with a given process ID.
        //! This list of elements will REPLACE the former list of receive elements 
        //! associated with the given process ID.
        //! \param processID The given process ID (MPI rank).
        //! \param elements The list of receive elements for the given process ID.
        void assignReceiveElements(size_t processID, 
                                   const std::vector<Element>& elements);

        //! Define the mapping of receive processes to receive elements using a given map.
        //! This mapping will replace the existing mapping within the data structure.
        //! \param receiveMap The given map of receive processes -> receive elements.
        void assignReceiveElements(const std::map<size_t, std::vector<Element> >& receiveMap);

        //! Returns true iff this process sends data to the given process (rank).
        //! \param processID The given process ID (MPI rank).
        bool sendsTo(size_t processID) const;

        //! Returns true iff this process receives data from the given process (rank).
        //! \param processID The given process ID (MPI rank).
        bool receivesFrom(size_t processID) const;

        //! Returns an iterator to the beginning of the list of processors to 
        //! which this process sends data.
        ProcessIterator sendProcessBegin() const;

        //! Returns an iterator to the end of the list of processors to 
        //! which this process sends data.
        ProcessIterator sendProcessEnd() const;

        //! Returns the number of processes to which this one sends data.
        size_t numberOfSendProcesses() const;

        //! Constructs and returns a vector containing the process IDs (MPI ranks) 
        //! of the send processes for this map.
        std::vector<size_t> sendProcesses() const;

        //! Returns an iterator to the beginning of the list of processors from 
        //! which this process receives data.
        ProcessIterator receiveProcessBegin() const;

        //! Returns an iterator to the end of the list of processors from 
        //! which this process receives data.
        ProcessIterator receiveProcessEnd() const;

        //! Returns the number of processes from which this one receives data.
        size_t numberOfReceiveProcesses() const;

        //! Constructs and returns a vector containing the process IDs (MPI ranks) 
        //! of the receive processes for this map.
        std::vector<size_t> receiveProcesses() const;

        //! Returns an iterator to the beginning of the list of elements to which 
        //! we send data (on the process with the given ID/rank).
        //! \param processID The ID of the given process receiving data from this one.
        ElementIterator sendElementBegin(size_t processID) const;

        //! Returns an iterator to the end of the list of elements to which 
        //! we send data (on the process with the given ID/rank).
        //! \param processID The ID of the given process receiving data from this one.
        ElementIterator sendElementEnd(size_t processID) const;

        //! Returns the number of send elements associated with the given process ID.
        //! \param processID The ID of the given process receive data from this one.
        size_t numberOfSendElements(size_t processID) const;

        //! Returns an iterator to the beginning of the list of elements from which 
        //! we receive data (on the process with the given ID/rank).
        //! \param processID The ID of the given process sending data to this one.
        ElementIterator receiveElementBegin(size_t processID) const;

        //! Returns an iterator to the end of the list of elements from which 
        //! we receive data (on the process with the given ID/rank).
        //! \param processID The ID of the given process sending data to this one.
        ElementIterator receiveElementEnd(size_t processID) const;

        //! Returns the number of send elements associated with the given process ID.
        //! \param processID The ID of the given process receive data from this one.
        size_t numberOfReceiveElements(size_t processID) const;

        //! Constructs and returns a data structure mapping send process IDs to 
        //! send elements.
        std::map<size_t, std::vector<Element> > sendMap() const;

        std::map<size_t, std::vector<Element> >& getSendMap();

        //! Constructs and returns a data structure mapping receive process IDs to 
        //! receive elements.
        std::map<size_t, std::vector<Element> > receiveMap() const;

        std::map<size_t, std::vector<Element> >& getReceiveMap();

       //! Empty the domain neighbor map.
        void clear(); 

        //! Returns true iff this domain neighbor map is identical to rhs.
        bool operator==(const DomainNeighborMap& rhs) const;

        //! Not-equals operator
        bool operator!=(const DomainNeighborMap& rhs) const;

        //! Returns true if this DomainNeighborMap is valid (consistent across
        //! domain boundaries), false otherwise.  This method is expensive and 
        //! should only be used for debugging.
        bool isValid(const typename MeshTypeStruct<Element>::MeshType *space) const;

    private:

        // The implementation of the domain neighbor map is hidden for 
        // encapsulation.
        std::map<size_t, std::vector<Element> > mSendMap, mReceiveMap;
        std::set<size_t> mSendProcIDs, mReceiveProcIDs;
    };


//! Output operator.
    template <typename Element>
    std::ostream& operator<<(std::ostream& out, 
                             const DomainNeighborMap<Element>& dm);


} // end namespace Communication


// The rest of the stuff below is for template specialization of the 
// DomainNeighborMap class on the CommAgentID-related stuff.  It's used 
// to check the mesh communication.

namespace Communication
{

    template <>
    bool DomainNeighborMap<int>::isValid(const MeshTypeStruct<int>::MeshType *) const;

} // end namespace Communication

#endif             // __COMMUNICATION_DOMAINNEIGHBORMAP_HH__

//-----------------------------------------------------------------------
//                end of communication/DomainNeighborMap.hh
//-----------------------------------------------------------------------


