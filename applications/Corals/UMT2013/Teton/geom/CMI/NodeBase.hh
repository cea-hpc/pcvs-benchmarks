#ifndef __NODE_BASE__
#define __NODE_BASE__


namespace Geometry
{
    class MeshBase;
    
    class NodeBase
    {
    public:
        NodeBase();
        NodeBase( NodePtr theNodePtr);
        ~NodeBase();    
    
        std::vector<double> getPosition()const;
    
        long getLocalID() const;
        long getGlobalID() const;
        
        bool operator==(const NodeBase &rhs) const;
        
    private:
        NodePtr mNodePtr;
        long mID;
        
        friend class MeshBase;
    };

    inline 
    NodeBase::
    NodeBase()
            :mNodePtr(0),mID(-1)
    {}
    
    inline 
    NodeBase::
    NodeBase(NodePtr theNodePtr)
            :mNodePtr(theNodePtr),mID(Local_NodeID_From_Ptr(mNodePtr))
    {}
    
    inline 
    NodeBase::
    ~NodeBase()
    {}
    
    inline 
    std::vector<double>
    NodeBase::getPosition() const
    {
        std::vector<double> thePosition(3);
        thePosition[0] = mNodePtr->Loc3D.x;
        thePosition[1] = mNodePtr->Loc3D.y;
        thePosition[2] = mNodePtr->Loc3D.z;
    
        return thePosition;
    }

    inline
    long
    NodeBase::getLocalID() const
    {
        return mID;
    }
    
    inline
    long
    NodeBase::getGlobalID() const
    {
        return Global_NodeID_From_Ptr(mNodePtr);
    }

    inline bool 
    NodeBase::operator==(const NodeBase &rhs) const
    {
        return (mID == rhs.mID);
    }
    
    
} // end namespace Geometry
#endif
