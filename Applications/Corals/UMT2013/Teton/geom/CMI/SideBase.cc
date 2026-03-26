#include "MeshBase.hh"
#include <algorithm>
#include <iostream>
#include <cassert>

namespace Geometry
{ 
    SideBase::
    SideBase(SidePtr theSidePtr)
            :mSidePtr(theSidePtr),mID(Local_SideID_From_Ptr(theSidePtr)),mSidesZone(0),mSidesFace(0),
             mSidesRightCorner(0),mSidesLeftCorner(0),mSidesOppositeSide(0)
    {
    }
    
    SideBase::
    ~SideBase()
    {
    }
    
    SideBase& 
    SideBase::getDummyExteriorSide()
    {
        static SideBase theDummyExteriorSide;
        
        static bool firstCall= true;

        if( firstCall )
        {
            // use default constructor for the connectivity data members so that each
            // of their local IDs = -1
            static ZoneBase theSidesZone;
            static FaceBase theSidesFace;
            static CornerBase theSidesRightCorner;
            static CornerBase theSidesLeftCorner;
            static SideBase theSidesOppositeSide;
            static NodeBase theSidesOppositeSideLeftCornerNode;

            theDummyExteriorSide.mSidesZone = &theSidesZone;
            theDummyExteriorSide.mSidesFace = &theSidesFace;
            theDummyExteriorSide.mSidesRightCorner = &theSidesRightCorner;
            theDummyExteriorSide.mSidesLeftCorner = &theSidesLeftCorner;
            theDummyExteriorSide.mSidesLeftCorner->setNode(&theSidesOppositeSideLeftCornerNode);
            theDummyExteriorSide.mSidesOppositeSide = &theSidesOppositeSide;
            firstCall=false;
        }
        
        return theDummyExteriorSide;
    }
    
    void
    SideBase::updateLocalConnectivity()
    {
        ZoneIterator zoneIt = MeshBase::mMeshPtr->ownedZoneBegin()+ (Sides_Zone( mSidePtr))->nZoneLID;
        assert( zoneIt != MeshBase::mMeshPtr->ownedZoneEnd() );
        mSidesZone = const_cast<ZoneBase*>(&(*zoneIt)); 

        FaceIterator faceIt = MeshBase::mMeshPtr->ownedFaceBegin() + (Sides_Face( mSidePtr ))->nFaceLID;
        assert( faceIt != MeshBase::mMeshPtr->ownedFaceEnd() );
        mSidesFace = const_cast<FaceBase*>(&(*faceIt));
        
        CornerIterator cornerIt = MeshBase::mMeshPtr->ownedCornerBegin() + (Sides_Right_Corner( mSidePtr ))->nCornerLID;
        assert( cornerIt != MeshBase::mMeshPtr->ownedCornerEnd() );
        mSidesRightCorner = const_cast<CornerBase*>(&(*cornerIt));
        
        cornerIt = MeshBase::mMeshPtr->ownedCornerBegin() + (Sides_Left_Corner( mSidePtr ))->nCornerLID;
        assert( cornerIt != MeshBase::mMeshPtr->ownedCornerEnd() );
        mSidesLeftCorner = const_cast<CornerBase*>(&(*cornerIt));
        
        if( Sides_Opposite_Side( mSidePtr ) == NULL )
        {
            FB_States theSidesFB_State =  mSidePtr->pFace->FType;
            
            if( (theSidesFB_State == RECEIVE_FACE) || (theSidesFB_State == SEND_FACE) || 
                (theSidesFB_State == PROBLEM_BOUNDARY_FACE) )
            {
                mSidesOppositeSide = &(getDummyExteriorSide());
            }
            else
            {
                std::cout<<" SideBase::updateLocalConnectivity ERROR, opposite side not set."<<std::endl;
                mSidesOppositeSide = NULL;
            }
            
        }
        else
        {
            SideIterator sideIt = MeshBase::mMeshPtr->ownedSideBegin() + (Sides_Opposite_Side( mSidePtr ))->nSideLID;
            if( sideIt != MeshBase::mMeshPtr->ownedSideEnd() )
            {    
                mSidesOppositeSide = const_cast<SideBase*>(&(*sideIt));
            }
            else
            {
                mSidesOppositeSide = NULL;
            }
        }
        
    }
    
    ZoneBase
    SideBase::getZone() const
    {
        return *mSidesZone;
    }
    
    FaceBase
    SideBase::getFace() const
    {
        return *mSidesFace;
    }
    
    CornerBase
    SideBase::getRightCorner() const
    {
        return *mSidesRightCorner;
    }
    
    CornerBase
    SideBase::getLeftCorner() const
    {
        return *mSidesLeftCorner;
    }

    SideBase
    SideBase::getOppositeSide() const
    {
        return *mSidesOppositeSide;
    }

}//end namespace Geometry
