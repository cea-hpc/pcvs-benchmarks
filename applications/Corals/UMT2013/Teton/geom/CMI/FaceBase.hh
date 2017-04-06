#ifndef __FACE_BASE__
#define __FACE_BASE__
#include <string>

namespace Geometry
{
    class MeshBase;
    
    class FaceBase
    {
    public:

        // enum for classification
        enum CF_TYPE {NONE=0, INTERNAL=1, EXTERNAL_SURFACE=2, SEND=3, RECEIVE=4};

        typedef MeshBase                                  MeshType;
        typedef std::vector<CornerBase>::const_iterator   CornerIterator;
        typedef std::vector<SideBase>::const_iterator     SideIterator;
        typedef std::vector<FaceBase>::const_iterator     FaceIterator;

        FaceBase();
        FaceBase( FacePtr theFacePtr);
        ~FaceBase();
    
        std::vector<double> getPosition()const;
    
        long getLocalID() const;
        long getGlobalID() const;
        
        FacePtr getFacePtr() const;
        
        CornerIterator cornerBegin() const;
        CornerIterator cornerEnd() const;
        
        SideIterator sideBegin() const;
        SideIterator sideEnd() const;
        
        FaceBase getOppositeFace() const;
        
        bool isSend() const;
        bool isReceive() const;
        bool isInternal() const;
        bool isExternalSurface() const;
        
        std::string getTag() const;
        
        bool operator==(const FaceBase &rhs) const;
        
        void updateLocalConnectivity();
        void updateClassification();
        
        void setClassification(CF_TYPE theClassification);
        CF_TYPE getClassification() const;
        
        FaceBase& getDummySendFace();
        FaceBase& getDummyReceiveFace();        

    private:
        FacePtr mFacePtr;
        long mID;
        CF_TYPE mClassification;
        
        FaceBase* mFacesOppositeFace;

        friend class MeshBase;

    };

    inline 
    FaceBase::
    FaceBase()
            :mFacePtr(0),mID(-1),mClassification(NONE),mFacesOppositeFace(0)
    {}
    
    inline 
    std::vector<double>
    FaceBase::getPosition() const
    {
        std::vector<double> thePosition(3);
        thePosition[0] = mFacePtr->Centroid.x;
        thePosition[1] = mFacePtr->Centroid.y;
        thePosition[2] = mFacePtr->Centroid.z;
    
        return thePosition;
    }
    
    inline 
    long
    FaceBase::getLocalID() const
    {
        return mID;
    }

    inline 
    long
    FaceBase::getGlobalID() const
    {
        return Global_FaceID_From_Ptr(mFacePtr);
    }

    inline 
    FacePtr 
    FaceBase::getFacePtr() const
    {
        return mFacePtr;
    }
    
    inline 
    std::string 
    FaceBase::getTag() const
    {
        return std::string( Get_Boundary_Face_Tag(mFacePtr) );
    }
    
    inline bool 
    FaceBase::operator==(const FaceBase &rhs) const
    {
        return (mID == rhs.mID);
    }

    inline 
    FaceBase::CF_TYPE 
    FaceBase::getClassification() const
    {
        return mClassification;
    }
    

    inline
    FaceBase& 
    FaceBase::getDummySendFace()
    {
        static FaceBase mDummySendFace;

        mDummySendFace.mClassification = SEND;
        return mDummySendFace;
    }
    
    inline
    FaceBase& 
    FaceBase::getDummyReceiveFace()
    {
        static FaceBase mDummyReceiveFace;

        mDummyReceiveFace.mClassification = RECEIVE;
        
        return mDummyReceiveFace;
    }
    

} // end namespace Geometry
#endif
