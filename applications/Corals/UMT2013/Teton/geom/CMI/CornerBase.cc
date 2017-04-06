#include "MeshBase.hh"
#include <algorithm>
#include <cassert>

namespace Geometry
{
    CornerBase::
    CornerBase(CornerPtr theCornerPtr)
            :mCornerPtr(theCornerPtr),mID(Local_CornerID_From_Ptr(mCornerPtr)),mCornersZone(0),mCornersNode(0)
    {
    }

    void
    CornerBase::updateLocalConnectivity()
    {
        ZoneBase thisCornersZone( Corners_Zone( mCornerPtr ) );
        ZoneIterator zoneIt = std::find(MeshBase::mMeshPtr->ownedZoneBegin(),MeshBase::mMeshPtr->ownedZoneEnd(),thisCornersZone);
        assert( zoneIt != MeshBase::mMeshPtr->ownedZoneEnd() );
        mCornersZone = const_cast<ZoneBase*>(&(*zoneIt)); 

        NodeBase thisCornersNode( Corners_Node( mCornerPtr ) );
        NodeIterator nodeIt = std::find(MeshBase::mMeshPtr->ownedNodeBegin(),MeshBase::mMeshPtr->ownedNodeEnd(),thisCornersNode);
        assert( nodeIt != MeshBase::mMeshPtr->ownedNodeEnd() );
        mCornersNode = const_cast<NodeBase*>(&(*nodeIt)); 
    }
    
    ZoneBase
    CornerBase::getZone() const
    {
        return *mCornersZone;
    }
    
    
    NodeBase
    CornerBase::getNode() const
    {
        return *mCornersNode;
    }

    CornerBase::FaceIterator 
    CornerBase::faceBegin() const
    {
        return MeshBase::mMeshPtr->cornersFacesBegin(mID);
    }
    
    CornerBase::FaceIterator 
    CornerBase::faceEnd() const
    {
        return MeshBase::mMeshPtr->cornersFacesEnd(mID);
    }


    
}//end namespace Geometry
