#include "MeshBase.hh"
#include <algorithm>
#include <iostream>

namespace Geometry
{
     
    FaceBase::
    FaceBase(FacePtr theFacePtr)
            :mFacePtr(theFacePtr),mID( Local_FaceID_From_Ptr(mFacePtr) ),mClassification(NONE),mFacesOppositeFace(0)
    {
    }
    
    FaceBase::
    ~FaceBase()
    {
    }
    
    void 
    FaceBase::updateLocalConnectivity()
    {
        if( Faces_Opposite_Face( mFacePtr ) == NULL )
        {
            if( mFacePtr->FType == RECEIVE_FACE )
            {
                mFacesOppositeFace = &(getDummySendFace());
            }
            else if( mFacePtr->FType == SEND_FACE )
            {
                mFacesOppositeFace = &(getDummyReceiveFace());
            }
            else
            {
                mFacesOppositeFace = NULL;
            }
            
        }
        else
        {   
            FaceBase thisFacesOppositeFace(mFacePtr);
            
            FaceIterator faceIt = std::find(MeshBase::mMeshPtr->ownedFaceBegin(),
                                            MeshBase::mMeshPtr->ownedFaceEnd(),
                                            thisFacesOppositeFace);

            if( faceIt != MeshBase::mMeshPtr->ownedFaceEnd() )
            {
                mFacesOppositeFace = const_cast<FaceBase*>(&(*faceIt));
            }
            else
            {
                mFacesOppositeFace = NULL;
            }
            
        }       
    }
    
    void 
    FaceBase::
    updateClassification()
    {
        int myRank=0;
#ifdef USE_MPI
        MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
#endif

        if( mFacePtr->FType == INTERIOR_FACE )
        {
            mClassification = INTERNAL;
        }
        else if( mFacePtr->FType == PROBLEM_BOUNDARY_FACE )
        {
            mClassification = EXTERNAL_SURFACE;
        }
        else if( mFacePtr->FType == SEND_FACE )
        {
            mClassification = SEND;
        }
        else if( mFacePtr->FType == RECEIVE_FACE )
        {
            mClassification = RECEIVE;
        }
        
        else
        {
            std::cout<<"proc "<<myRank<<"  FaceBase::mClassification NOT SET for lid="<<getLocalID()<<", gid="<<getGlobalID()<<std::endl;
        }
        
    }//end of updateClassification
    
    void 
    FaceBase::
    setClassification(CF_TYPE theClassification)
    {
        mClassification = theClassification;
        if( theClassification == SEND)
        {
            mFacesOppositeFace = &(getDummyReceiveFace());
        }
        else if( theClassification == RECEIVE )
        {
            mFacesOppositeFace = &(getDummySendFace());
        }
        
    }//end of updateClassification
    

    bool
    FaceBase::
    isSend() const
    {
        return ( (mClassification == SEND) || (mClassification == RECEIVE) );
    }

    bool
    FaceBase::
    isReceive() const
    {
        return ( (mClassification == SEND) || (mClassification == RECEIVE) );
    }

    bool
    FaceBase::
    isInternal() const
    {
        return (mClassification == INTERNAL);
    }

    bool 
    FaceBase::
    isExternalSurface() const
    {
        return (mClassification == EXTERNAL_SURFACE);
    }

    FaceBase::CornerIterator 
    FaceBase::cornerBegin() const
    {
        return MeshBase::mMeshPtr->facesCornersBegin(mID);
    }
    
    FaceBase::CornerIterator 
    FaceBase::cornerEnd() const
    {
        return MeshBase::mMeshPtr->facesCornersEnd(mID);
    }
    
   FaceBase::SideIterator FaceBase::sideBegin() const
   {
       return MeshBase::mMeshPtr->facesSidesBegin(mID);
   }

   FaceBase::SideIterator FaceBase::sideEnd() const
   {       
       return MeshBase::mMeshPtr->facesSidesEnd(mID);
   }

    FaceBase 
    FaceBase::
    getOppositeFace() const
    {
        return *mFacesOppositeFace;
    }
    
}//end namespace Geometry
