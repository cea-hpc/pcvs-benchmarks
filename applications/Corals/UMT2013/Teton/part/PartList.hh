#ifndef PARTLIST_HH
#define PARTLIST_HH
#include "part/Part.hh"

#ifdef USE_MPI
#include "mpi.h"
#endif

#include <map>

template <typename Mesh>
class PartList
{
private:
    typedef std::vector<Part<Mesh> >                   StorageType;

public:
   //------------------------------------------------------------------
   //                         Type Definitions
   //------------------------------------------------------------------

    typedef Mesh                                       MeshType;
    typedef Part<Mesh>                                 PartType;   
    typedef typename PartType::RegionType              RegionType;

    // STL container types.
    typedef PartType                                   value_type;
    typedef typename StorageType::iterator             iterator;
    typedef typename StorageType::const_iterator       const_iterator;
    typedef iterator                                   Iterator;
    typedef const_iterator                             ConstIterator;

    PartList(const Mesh& m);
    
    void addPart( const PartType& thePart) 
        {
            const_iterator theIt = std::find(mParts.begin(),mParts.end(),thePart);
            if( theIt == mParts.end() )
                mParts.push_back(thePart);
        }
    
    // Return an iterator pointing to the first part.
    Iterator                begin();

    // Return an iterator pointing one past the last part.
    Iterator                end();

    const MeshType&         getMesh() const;

private:
    const MeshType*   mMeshPtr;
    
    // List of parts.
    StorageType                   mParts;
};

//------------------------------------------------------------------
//                         implementation
//------------------------------------------------------------------
template<typename Mesh>
PartList<Mesh>::PartList(const Mesh& m)
        :mMeshPtr(&m)
{}

template<typename Mesh>
inline 
typename PartList<Mesh>::Iterator                
PartList<Mesh>::begin()
{
    return mParts.begin();
}

template<typename Mesh>
inline 
typename PartList<Mesh>::Iterator                
PartList<Mesh>::end()
{
    return mParts.end();
}
    
template<typename Mesh>
inline
const Mesh&         
PartList<Mesh>::getMesh() const
{
    return *mMeshPtr;
}




#endif
