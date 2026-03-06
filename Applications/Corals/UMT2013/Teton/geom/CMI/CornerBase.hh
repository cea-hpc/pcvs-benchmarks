#ifndef __CORNER_BASE__
#define __CORNER_BASE__


namespace Geometry
{
    class MeshBase;
    class FaceBase;
    class SideBase;
    class ZoneBase;
    
    class CornerBase
    {
    public:

        typedef std::vector<FaceBase>::const_iterator     FaceIterator;
        typedef std::vector<SideBase>::const_iterator     SideIterator;
        typedef std::vector<NodeBase>::const_iterator     NodeIterator;
        typedef std::vector<ZoneBase>::const_iterator     ZoneIterator;

        CornerBase();
        CornerBase( CornerPtr theCornerPtr);
        ~CornerBase();    

        ZoneBase getZone() const;
        NodeBase getNode() const;
        
        void setNode( NodeBase* theCornerNode);
        
        long getLocalID() const;

        FaceIterator faceBegin() const;
        FaceIterator faceEnd() const;
        
        CornerPtr getCornerPtr() const;

        bool operator==(const CornerBase &rhs) const;

        void updateLocalConnectivity();

    private:
        CornerPtr mCornerPtr;
        long mID;
        
        ZoneBase* mCornersZone;
        NodeBase* mCornersNode;
        
        friend class MeshBase;
    };

    inline 
    CornerBase::
    CornerBase()
            :mCornerPtr(0),mID(-1),mCornersZone(0),mCornersNode(0)
    {}
    
    inline 
    CornerBase::
    ~CornerBase()
    {}
    
    inline
    long
    CornerBase::getLocalID() const
    {
        return mID;
    }
    inline
    CornerPtr
    CornerBase::getCornerPtr() const
    {
        return mCornerPtr;
    }

    inline
    void 
    CornerBase::setNode( NodeBase* theCornerNode)
    {
        mCornersNode = theCornerNode;
    }

    inline
    bool 
    CornerBase::operator==(const CornerBase &rhs) const
    {
        return (mID == rhs.mID);
    }
    

} // end namespace Geometry
#endif
