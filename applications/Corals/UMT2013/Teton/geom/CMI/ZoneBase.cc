#include "MeshBase.hh"
#include "C2K-KC_Geom.h"

namespace Geometry
{
    ZoneBase::
    ZoneBase(ZonePtr theZonePtr)
            :mZonePtr(theZonePtr),mID(Local_ZoneID_From_Ptr(mZonePtr))
    {
    }
    
    ZoneBase::CornerIterator 
    ZoneBase::cornerBegin() const
    {
        return MeshBase::mMeshPtr->zonesCornersBegin(mID);
    }
    
    ZoneBase::CornerIterator 
    ZoneBase::cornerEnd() const
    {
        return MeshBase::mMeshPtr->zonesCornersEnd(mID);
    }

    ZoneBase::SideIterator 
    ZoneBase::sideBegin() const
    {
        return MeshBase::mMeshPtr->zonesSidesBegin(mID);
    }
    
    ZoneBase::SideIterator 
    ZoneBase::sideEnd() const
    {
        return MeshBase::mMeshPtr->zonesSidesEnd(mID);
    }

    ZoneBase::FaceIterator 
    ZoneBase::faceBegin() const
    {        
        return MeshBase::mMeshPtr->zonesFacesBegin(mID);
    }

    ZoneBase::FaceIterator 
    ZoneBase::faceEnd() const
    {        
        return MeshBase::mMeshPtr->zonesFacesEnd(mID);
    }
        
    bool 
    ZoneBase::isSend() const
    {
        return false;
    }
    
    bool 
    ZoneBase::isReceive() const
    {
        return false;
    }   
 
    double
    ZoneBase::getVolume() const
    {
        return CalcZoneVol(mZonePtr);
    }

} // end namespace Geometry
