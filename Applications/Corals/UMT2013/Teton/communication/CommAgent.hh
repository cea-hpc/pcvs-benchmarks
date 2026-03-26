//------------------------------*-C++-*----------------------------------
// Copyright (c) 1997-2002 Regents of the University of California. 
// All rights reserved.
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// For full text see LICENSE.TXT
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
//! \class CommAgent CommAgent.hh
//
// This class uses the Process class to get MPI information and 
// to do basic MPI tasks (getting the communicator, splitting it,
// and getting the number of total tasks.)  All other MPI functionality
// is located in Process or Communicate.
//-----------------------------------------------------------------------

#ifndef __Communication_CommAgent_hh__
#define __Communication_CommAgent_hh__

#ifdef USE_MPI
#include "mpi.h"
#endif

#include <vector>
      
#include "DomainNeighborMap.hh"

// Forward declare some classes.
namespace Geometry
{
    class ZoneBase;
    class NodeBase;
    class FaceBase;
    class EdgeBase;
    class SideBase;
    class CornerBase;
}


//! \namespace Communication
namespace Communication
{

//!  Communication agent class.
//!  
//!  This class provides the interface for the communication domain
//!  neighbor maps for elements within the mesh.
    class CommAgent
    {
    public:
        typedef Geometry::ZoneBase             ZoneHandle;
        typedef Geometry::NodeBase             NodeHandle;
        typedef Geometry::FaceBase             FaceHandle;
        typedef Geometry::EdgeBase             EdgeHandle;
        typedef Geometry::SideBase             SideHandle;
        typedef Geometry::CornerBase           CornerHandle;

        typedef DomainNeighborMap<ZoneHandle>   ZoneMap;
        typedef DomainNeighborMap<NodeHandle>   NodeMap;
        typedef DomainNeighborMap<FaceHandle>   FaceMap;
        typedef DomainNeighborMap<EdgeHandle>   EdgeMap;
        typedef DomainNeighborMap<SideHandle>   SideMap;
        typedef DomainNeighborMap<CornerHandle> CornerMap;

        //! Creates a communication agent with valid processor id (rank), number of
        //! processors and communicator.
        CommAgent();
   
        //! Destroys the communication agent, freeing the MPI communicator.
        ~CommAgent();
   


        // ----------------------------------------------------------------------
        // public interface
        // ----------------------------------------------------------------------
        //! \return The CommAgent's processor ID for its given communicator.
        int                       getRank() const;

        //! \return The total number of processors in this run.
        int                       getNumberOfProcessors() const;

        //! \treturn The tag number for space communication
//    int                       getTag() const;

        //! \return The Communicator for this agent.
//    const Communicator&       getComm() const;
//    Communicator&             getComm();

        //! Clears the member data of this agent.  All domain neighbor maps are
        //! cleared;  the communicator is freed (using an underlying MPI_Comm_free
        //! call) and mProcID, mNumProcs are set to zero.
        void                      clear();
       

        // ----------------------------------------------------------------------
        //                           Mutator methods
        // ----------------------------------------------------------------------

        //! \param info - the DomainNeighborMap from which to obtain and change this
        //! agent's map data.
//    void                      setZoneNeighborMap(const ZoneMap& info);

        //! \param info - the DomainNeighborMap from which to obtain and change this
        //! agent's map data.
//    void                      setNodeNeighborMap(const NodeMap& info);

        //! \param info - the DomainNeighborMap from which to obtain and change this
        //! agent's map data.
        void                      setFaceNeighborMap(const FaceMap& info);

        //! \param info - the DomainNeighborMap from which to obtain and change this
        //! agent's map data.
//    void                      setEdgeNeighborMap(const EdgeMap& info);

        //! \param info - the DomainNeighborMap from which to obtain and change this
        //! agent's map data.
//    void                      setSideNeighborMap(const SideMap& info);

        //! \param info - the DomainNeighborMap from which to obtain and change this
        //! agent's map data.
//    void                      setCornerNeighborMap(const CornerMap& info);

        //! Update the communicator for this CommAgent to reflect the communication 
        //! topology for its domain neighbor maps.  This must be called on all 
        //! processes or you will experience deadlock!
//    void                      updateComm();

        //! This static function takes a vector of CommAgents and updates all 
        //! their communicators as efficiently as possible.
//    static void               updateComms(std::vector<CommAgent<Space>*>& commAgents);

        // ----------------------------------------------------------------------
        //                        Accessor Methods
        // ----------------------------------------------------------------------

        //! \return The Zone element communication mapping for this agent.
//    const ZoneMap&  getZoneNeighborMap() const;

        //! \return The Node element communication mapping for this agent.
//    const NodeMap&  getNodeNeighborMap() const;

        //! \return The Face element communication mapping for this agent.
        const FaceMap&  getFaceNeighborMap() const;
        FaceMap&  getFaceNeighborMap();

        //! \return The Edge element communication mapping for this agent.
//    const EdgeMap&  getEdgeNeighborMap() const;

        //! \return The Side element communication mapping for this agent.
//    const SideMap&  getSideNeighborMap() const;

        //! \return The Corner element communication mapping for this agent.
//    const CornerMap&  getCornerNeighborMap() const;

        //! Returns a unique list of neighbor processes with which this 
        //! CommAgent interacts.
//    std::vector<int> findNeighbors() const;

    private:

        // ----------------------------------------------------------------------
        //                        Data Members
        // ----------------------------------------------------------------------
//    Communicator* mComm;
        int      mTag;

        //! The zone mapping of all send and receive neighbors
//    ZoneMap* mZoneNeighborMap;
        //! The node mapping of all send and receive neighbors
//    NodeMap* mNodeNeighborMap;
        //! The edge mapping of all send and receive neighbors
//    EdgeMap* mEdgeNeighborMap;
        //! The face mapping of all send and receive neighbors
        FaceMap* mFaceNeighborMap;
        //! The side mapping of all send and receive neighbors
//    SideMap* mSideNeighborMap;
        //! The corner mapping of all send and receive neighbors
//    CornerMap* mCornerNeighborMap;

        //! Not implemented.
        CommAgent( const CommAgent& );

        //! Disabled.
        CommAgent& operator=( const CommAgent& );
    };

    // ----------------------------------------------------------------------
    //                        inline member functions
    // ----------------------------------------------------------------------
    inline int                       
    CommAgent::getRank() const
    {
        int myRank;
        MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
        return myRank;
    }
    
    inline int                       
    CommAgent::getNumberOfProcessors() const
    {
        int numProcs;
        MPI_Comm_size(MPI_COMM_WORLD,&numProcs);
        return numProcs;
    }

}  // namespace Communication


#endif                          // __Communication_CommAgent_hh__


//-----------------------------------------------------------------------
//                              end of communication/CommAgent.hh
//-----------------------------------------------------------------------
