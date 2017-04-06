#ifndef __ZONE_BASE__
#define __ZONE_BASE__

namespace Geometry
{
    class MeshBase;
    class FaceBase;
    class SideBase;
    class CornerBase;
    
    class ZoneBase
    {
    public:

        typedef std::vector<FaceBase>::const_iterator     FaceIterator;
        typedef std::vector<SideBase>::const_iterator     SideIterator;
        typedef std::vector<CornerBase>::const_iterator   CornerIterator;

        ZoneBase();
        ZoneBase( ZonePtr theZonePtr);
        ~ZoneBase();
    
        std::vector<double> getPosition()const;
    
        CornerIterator cornerBegin() const;
        CornerIterator cornerEnd() const;
        
        SideIterator sideBegin() const;
        SideIterator sideEnd() const;
        
        FaceIterator faceBegin() const;
        FaceIterator faceEnd() const;
        
        long getLocalID() const;
        
        double getVolume() const;
        
        bool isSend() const;
        bool isReceive() const;
        
        ZonePtr getZonePtr() const;
        
        bool operator==(const ZoneBase &rhs) const;
    private:
        ZonePtr mZonePtr;
        long mID;
    
        friend class MeshBase;
   };

    inline 
    ZoneBase::
    ZoneBase()
            :mZonePtr(0),mID(-1)
    {}
    
    inline 
    ZoneBase::
    ~ZoneBase()
    {}
    
    inline 
    std::vector<double>
    ZoneBase::getPosition() const
    {
        std::vector<double> thePosition(3);
        thePosition[0] = mZonePtr->Centroid.x;
        thePosition[1] = mZonePtr->Centroid.y;
        thePosition[2] = mZonePtr->Centroid.z;
    
        return thePosition;
    }
    

    inline long
    ZoneBase::getLocalID() const
    {
        return mID;
    }

    inline ZonePtr 
    ZoneBase::getZonePtr() const
    {
        return mZonePtr;
    }
    
    inline bool 
    ZoneBase::operator==(const ZoneBase &rhs) const
    {
        return (mID == rhs.mID);
    }

} // end namespace Geometry
#endif
