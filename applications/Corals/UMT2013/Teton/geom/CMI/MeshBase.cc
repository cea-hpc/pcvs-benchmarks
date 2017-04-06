#include "MeshBase.hh"
#include <iostream>
#include <iomanip>
#include <sstream>
#include "DBC.hh"

namespace Geometry
{
    
// static member
const MeshBase* MeshBase::mMeshPtr = NULL;


MeshBase::MeshBase(const char* meshFile)
        :mCommAgent(),
         mOwnedNodeCache(),
         mOwnedZoneCache(),
         mOwnedFaceCache(),
         mOwnedEdgeCache(),
         mOwnedCornerCache(),
         mOwnedSideCache(),
         mZonesCorners(),
         mZonesSides(),
         mCornersFaces(),
         mFacesCorners()
{
    mMeshPtr = this;

    int myRank=0;
        
#if USE_MPI
    MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
#endif

    Init_CMG_Problem_From_File(meshFile);
        
    InitializeAllStorage ( );

    Create_KC_Mesh ( );
        
    CheckAllConnectivityInfo ("After creating KC mesh.\n");
        
    CalcAllFaceCentroids ( );
    CalcAllZoneCentroids ( );
    CalculateAllZoneVolumes ( 0 );
    CheckAllConnectivityInfo ("After centroid and volume calcs.\n");

    buildNodeCache();
    buildZoneCache();
    buildFaceCache();
    buildEdgeCache();
    buildCornerCache();
    buildSideCache();
        
    updateFaceClassification();

    buildConnectivity();
        
    buildCommAgent();
    updateCommFaceClassification();
}
    
MeshBase::~MeshBase()
{        
    FreeAllStorage ( );
}

//-------------------------------------------------------------------
//                internal cache-creation functions
//-------------------------------------------------------------------
void
MeshBase::buildNodeCache()
{        
    mOwnedNodeCache.resize( Number_Of_Owned_Nodes() );
        
    for(NodePtr theNodePtr = Owned_Node_Begin();
        theNodePtr != Owned_Node_End();
        theNodePtr = Next_Owned_Node() )
    {
        size_t id = theNodePtr->nNodeLID;
        ASSERT( (id>=0)&&(id<mOwnedNodeCache.size()) );

        mOwnedNodeCache[id] = NodeBase(theNodePtr);
    }
}

void
MeshBase::buildZoneCache()
{
    mOwnedZoneCache.resize( Number_Of_Owned_Zones() );
        
    for(ZonePtr theZonePtr = Owned_Zone_Begin();
        theZonePtr != Owned_Zone_End();
        theZonePtr = Next_Owned_Zone() )
    {
        size_t id = theZonePtr->nZoneLID;
        ASSERT( (id>=0) && (id < mOwnedZoneCache.size()) );
            
        mOwnedZoneCache[id] = ZoneBase(theZonePtr);
    }
}

void
MeshBase::buildFaceCache()
{
    mOwnedFaceCache.resize( Number_Of_Owned_Faces() );
        
    for(FacePtr theFacePtr = Owned_Face_Begin();
        theFacePtr != Owned_Face_End();
        theFacePtr = Next_Owned_Face())
    {
        size_t id = theFacePtr->nFaceLID;
        ASSERT( (id>=0) && (id < mOwnedFaceCache.size()) );
        mOwnedFaceCache[id] = FaceBase(theFacePtr);
    }
}

void
MeshBase::buildEdgeCache()
{
    mOwnedEdgeCache.resize( Number_Of_Owned_Edges() );
        
    for(EdgePtr theEdgePtr = Owned_Edge_Begin();
        theEdgePtr != Owned_Edge_End();
        theEdgePtr = Next_Owned_Edge())
    {
        size_t id = theEdgePtr->nEdgeLID;
        ASSERT( (id>=0) && (id < mOwnedEdgeCache.size()) );
            
        mOwnedEdgeCache[id] = EdgeBase(theEdgePtr);
    }
}

void
MeshBase::buildCornerCache()
{
    mOwnedCornerCache.resize( Number_Of_Owned_Corners() );
        
    for(CornerPtr theCornerPtr = Owned_Corner_Begin();
        theCornerPtr != Owned_Corner_End();
        theCornerPtr = Next_Owned_Corner())
    {
        size_t id = theCornerPtr->nCornerLID;
        ASSERT( (id>=0) && (id<mOwnedCornerCache.size()) );
            
        mOwnedCornerCache[id] = CornerBase(theCornerPtr);
    }
}

void
MeshBase::buildSideCache()
{
    mOwnedSideCache.resize( Number_Of_Owned_Sides() );
        
    for(SidePtr theSidePtr = Owned_Side_Begin();
        theSidePtr != Owned_Side_End();
        theSidePtr = Next_Owned_Side())
    {
        size_t id = theSidePtr->nSideLID;
        ASSERT( (id >= 0) && (id < mOwnedSideCache.size()) );
            
        mOwnedSideCache[id] = SideBase(theSidePtr);
    }
}

void
MeshBase::buildConnectivity()
{
    for(std::vector<FaceBase>::iterator fIt = mOwnedFaceCache.begin();
        fIt != mOwnedFaceCache.end();
        ++fIt)
    {
        fIt->updateLocalConnectivity();
    }    
    
    for(std::vector<SideBase>::iterator sIt = mOwnedSideCache.begin();
        sIt != mOwnedSideCache.end();
        ++sIt)
    {
        sIt->updateLocalConnectivity();
    }        

    for(std::vector<CornerBase>::iterator cIt = mOwnedCornerCache.begin();
        cIt != mOwnedCornerCache.end();
        ++cIt)
    {
        cIt->updateLocalConnectivity();
    }        


    // build zones corners map and zones sides
    for(std::vector<ZoneBase>::const_iterator zIt = mOwnedZoneCache.begin();
        zIt != mOwnedZoneCache.end(); ++zIt)
    {
        long zID = zIt->getLocalID();
        std::vector<CornerBase> theZonesCorners;

        for(CornerPtr theCornerPtr = Zones_Corner_Begin( zIt->getZonePtr() );
            theCornerPtr != Zones_Corner_End( zIt->getZonePtr() );
            theCornerPtr = Zones_Next_Corner( ) )
        {
            theZonesCorners.push_back( mOwnedCornerCache[theCornerPtr->nCornerLID] );
        }
        mZonesCorners[zID] = theZonesCorners;

        std::vector<SideBase> theZonesSides;

        for(SidePtr theSidePtr = Zones_Side_Begin( zIt->getZonePtr() );
            theSidePtr != Zones_Side_End( zIt->getZonePtr() );
            theSidePtr = Zones_Next_Side( ) )
        {
            theZonesSides.push_back( mOwnedSideCache[theSidePtr->nSideLID] );
        }
        mZonesSides[zID] = theZonesSides;
    }//end of loop over owned zones
        
    // build corners faces map
    for(std::vector<CornerBase>::const_iterator cIt = mOwnedCornerCache.begin();
        cIt != mOwnedCornerCache.end();
        ++cIt)
    {
        long cID = cIt->getLocalID();
        std::vector<FaceBase> theCornersFaces;
        for(FacePtr theFacePtr = Corners_Face_Begin( cIt->getCornerPtr() );
            theFacePtr != Corners_Face_End( cIt->getCornerPtr() );
            theFacePtr = Corners_Next_Face( ))
        {
            theCornersFaces.push_back( mOwnedFaceCache[theFacePtr->nFaceLID] );
        }
        mCornersFaces[cID] = theCornersFaces;
    }//end of loop over owned corners

    // build faces corners map
    for(std::vector<FaceBase>::const_iterator fIt = mOwnedFaceCache.begin();
        fIt != mOwnedFaceCache.end();
        ++fIt)
    {
        long fID = fIt->getLocalID();
        std::vector<CornerBase> theFacesCorners;
        for(CornerPtr theCornerPtr = Faces_Corner_Begin( fIt->getFacePtr() );
            theCornerPtr != Faces_Corner_End( fIt->getFacePtr() );
            theCornerPtr = Faces_Next_Corner( ))
        {
            theFacesCorners.push_back( mOwnedCornerCache[theCornerPtr->nCornerLID] );
        }
        mFacesCorners[fID] = theFacesCorners;
    }//end of loop over owned corners

    // build faces side map
    for(std::vector<FaceBase>::const_iterator fIt = mOwnedFaceCache.begin();
        fIt != mOwnedFaceCache.end();
        ++fIt)
    {
        long fID = fIt->getLocalID();
        std::vector<SideBase> theFacesSides;
        for(SidePtr theSidePtr = Faces_Side_Begin( fIt->getFacePtr() );
            theSidePtr != Faces_Side_End( fIt->getFacePtr() );
            theSidePtr = Faces_Next_Side( ))
        {
            theFacesSides.push_back( mOwnedSideCache[theSidePtr->nSideLID] );
        }
        mFacesSides[fID] = theFacesSides;
    }//end of loop over owned corners

    // build zone faces map
    for(std::vector<ZoneBase>::const_iterator zIt = mOwnedZoneCache.begin();
        zIt != mOwnedZoneCache.end(); ++zIt)
    {
        long zID = zIt->getLocalID();
        std::vector<FaceBase> theZonesFaces;

        for(FacePtr theFacePtr = Zones_Face_Begin( zIt->getZonePtr() );
            theFacePtr != Zones_Face_End( zIt->getZonePtr() );
            theFacePtr = Zones_Next_Face( ) )
        {
            theZonesFaces.push_back( mOwnedFaceCache[theFacePtr->nFaceLID] );
        }
        mZonesFaces[zID] = theZonesFaces;

    }//end of loop over owned zones

}
    
void
MeshBase::updateFaceClassification()
{
    for( std::vector<FaceBase>::iterator fIt = mOwnedFaceCache.begin();
         fIt != mOwnedFaceCache.end();
         ++fIt)
    {
        fIt->updateClassification();
    }//end of loop over face cache
}
    
void
MeshBase::updateCommFaceClassification()
{
    std::map<size_t,std::vector<FaceBase> >& theSendMap=mCommAgent.getFaceNeighborMap().getSendMap();
    std::map<size_t,std::vector<FaceBase> >& theReceiveMap=mCommAgent.getFaceNeighborMap().getReceiveMap();
        
    for(std::map<size_t,std::vector<FaceBase> >::iterator mapIt = theSendMap.begin();
        mapIt != theSendMap.end() ; ++mapIt)
    {
            
        for(std::vector<FaceBase>::iterator fIt = mapIt->second.begin();
            fIt != mapIt->second.end();
            ++fIt)
        {
            fIt->setClassification(FaceBase::SEND);
        }    
    }

    for(std::map<size_t,std::vector<FaceBase> >::iterator mapIt = theReceiveMap.begin();
        mapIt != theReceiveMap.end() ; ++mapIt)
    {
            
        for(std::vector<FaceBase>::iterator fIt = mapIt->second.begin();
            fIt != mapIt->second.end();
            ++fIt)
        {
            fIt->setClassification(FaceBase::RECEIVE);
        }    
    }
}
    
void
MeshBase::buildCommAgent()
{
    std::map<size_t,std::vector<FaceBase> > theSendMap,theReceiveMap;

    int nSendNbrs = Number_Of_Send_Neighbors( );
    int nRecvNbrs = Number_Of_Receive_Neighbors( );
    int* theSendNbrs = Send_Neighbor_IDs();
    int* theRecvNbrs = Receive_Neighbor_IDs();
        
    for(int i=0; i<nSendNbrs;i++)
    {
        int theSendProcNum = theSendNbrs[i];
        std::vector<FaceBase>& theSendFaces = theSendMap[theSendProcNum];

        for( FacePtr theFacePtr = Send_Face_Begin(theSendProcNum);
             theFacePtr != Send_Face_End(theSendProcNum);
             theFacePtr = Next_Send_Face(theSendProcNum) )
        {
            FaceBase thisFace(theFacePtr);
            std::vector<FaceBase>::iterator fIt = std::find(mOwnedFaceCache.begin(),mOwnedFaceCache.end(),thisFace);
            ASSERT( fIt != mOwnedFaceCache.end() );
            fIt->setClassification(FaceBase::SEND);
            theSendFaces.push_back( *fIt );
        }
    }
        
    for(int i=0; i<nRecvNbrs;i++)
    {
        int theRecvProcNum = theRecvNbrs[i];
        std::vector<FaceBase>& theRecvFaces = theReceiveMap[theRecvProcNum];

        for( FacePtr theFacePtr = Receive_Face_Begin(theRecvProcNum);
             theFacePtr != Receive_Face_End(theRecvProcNum);
             theFacePtr = Next_Receive_Face(theRecvProcNum) )
        {
            FaceBase thisFace(theFacePtr);
            std::vector<FaceBase>::iterator fIt = std::find(mOwnedFaceCache.begin(),mOwnedFaceCache.end(),thisFace);
            ASSERT( fIt != mOwnedFaceCache.end() );
            fIt->setClassification(FaceBase::RECEIVE);
            theRecvFaces.push_back( *fIt );
        }
    }
        
    //
    // at this point we have send and receive maps as they are defined from CMG.  However, this is 
    // insufficient for our purposes since Kull expects every receive face to have a corresponding
    // send face and vice versa.  We may get around this by merging the send map into the receive map
    // and the receive map into the send map.  
    //
   
    for(std::map<size_t,std::vector<FaceBase> >::const_iterator sendMapIt = theSendMap.begin();
        sendMapIt != theSendMap.end();++sendMapIt)
    {
        int domainID = sendMapIt->first;
        const std::vector<FaceBase>& theSendFaces = sendMapIt->second;
        if( theReceiveMap[domainID].size() == 0 )
        {
            theReceiveMap[domainID] = theSendFaces;
            for(std::vector<FaceBase>::iterator rit = theReceiveMap[domainID].begin();
                rit != theReceiveMap[domainID].end() ; ++rit)
            {
                rit->setClassification(FaceBase::RECEIVE);
            }
        }
    }
        
    for( std::map<size_t,std::vector<FaceBase> >::const_iterator recvMapIt = theReceiveMap.begin();
         recvMapIt != theReceiveMap.end(); ++recvMapIt)
    {
        int domainID = recvMapIt->first;
        const std::vector<FaceBase>& theRecvFaces = recvMapIt->second;
        if( theSendMap[domainID].size() == 0 )
        {
            theSendMap[domainID] = theRecvFaces;
            for(std::vector<FaceBase>::iterator sit = theSendMap[domainID].begin();
                sit != theSendMap[domainID].end(); ++sit)
            {
                sit->setClassification(FaceBase::SEND);
            }
        }
    }
        
    Communication::DomainNeighborMap<FaceBase> theFaceNeighborMap(theSendMap,theReceiveMap);
    mCommAgent.setFaceNeighborMap( theFaceNeighborMap );
}//end of buildCommAgent
    
} // end namespace Geometry
