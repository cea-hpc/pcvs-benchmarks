#ifndef __SIDE_BASE__
#define __SIDE_BASE__

namespace Geometry
{
    class MeshBase;
    class ZoneBase;
    class FaceBase;
    class CornerBase;
    
    class SideBase
    {
    public:
        typedef std::vector<ZoneBase>::const_iterator     ZoneIterator;
        typedef std::vector<FaceBase>::const_iterator     FaceIterator;
        typedef std::vector<SideBase>::const_iterator     SideIterator;
        typedef std::vector<CornerBase>::const_iterator   CornerIterator;
        SideBase();
        SideBase( SidePtr theSidePtr);
        ~SideBase();
    
        ZoneBase getZone() const;
        FaceBase getFace() const;        
        CornerBase getRightCorner() const;
        CornerBase getLeftCorner() const;
        SideBase getOppositeSide() const;

        long getLocalID() const;

        bool operator==(const SideBase &rhs) const;

        void updateLocalConnectivity();
        
        SideBase& getDummyExteriorSide();
        
    private:
        SidePtr     mSidePtr;
        long        mID;
        
        ZoneBase*   mSidesZone;
        FaceBase*   mSidesFace;
        CornerBase* mSidesRightCorner;
        CornerBase* mSidesLeftCorner;
        SideBase*   mSidesOppositeSide;

        friend class MeshBase;
    };

    inline 
    SideBase::
    SideBase()
            :mSidePtr(0),mID(-1),mSidesZone(0),mSidesFace(0),
             mSidesRightCorner(0),mSidesLeftCorner(0),mSidesOppositeSide(0)
    {}
    
    inline
    long
    SideBase::getLocalID() const
    {
        return mID;
    }
    inline
    bool 
    SideBase::operator==(const SideBase &rhs) const
    {
        return (mID == rhs.mID);
    }
    
} // end namespace Geometry
#endif
