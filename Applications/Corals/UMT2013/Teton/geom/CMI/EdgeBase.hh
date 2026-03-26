#ifndef __EDGE_BASE__
#define __EDGE_BASE__


namespace Geometry
{
    class MeshBase;
    class CornerBase;
    
    class EdgeBase
    {
    public:
        typedef std::vector<CornerBase>::const_iterator     CornerIterator;

        EdgeBase();
        EdgeBase( EdgePtr theEdgePtr);
        ~EdgeBase();    
    
    private:
        EdgePtr mEdgePtr;
        long mID;

        friend class MeshBase;
        
    };

    inline 
    EdgeBase::
    EdgeBase()
            :mEdgePtr(0),mID(-1)
    {}
    
    inline 
    EdgeBase::
    EdgeBase(EdgePtr theEdgePtr)
            :mEdgePtr(theEdgePtr),mID(Local_EdgeID_From_Ptr(theEdgePtr))
    {}
    
    inline 
    EdgeBase::
    ~EdgeBase()
    {}
    

} // end namespace Geometry
#endif
