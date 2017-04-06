//----------------------------------------------------------------------------

// MeshBase.hh

//! This class adheres to the Mesh class specification in version 1.1 of 
//! the KULL Common Mesh Interface.  Please see that documentation for a
//! description of the interface.
//----------------------------------------------------------------------------

#ifndef GEOMETRY_MESHBASE_HH
#define GEOMETRY_MESHBASE_HH
#include <algorithm>
#include <cassert>

extern "C"
{

#include "C2K-Storage.h"
#include "C2K-Lists.h"

#include "C2K-CMG.h"
#include "C2K-KC_Geom.h"
#include "C2K-KC_Check.h"
#include "C2K-KC_Create.h"
#include "C2K-KC_SubDivide.h"

#include "C2K-KC_API.h"
}

// Pull in std::vector and std::map
#include <vector>
#include <map>

#include "geom/CMI/SideBase.hh"
#include "geom/CMI/FaceBase.hh"
#include "geom/CMI/ZoneBase.hh"
#include "geom/CMI/NodeBase.hh"
#include "geom/CMI/CornerBase.hh"
#include "geom/CMI/EdgeBase.hh"
#include "geom/CMI/ZoneBase.hh"

#include "communication/CommAgent.hh"

namespace Geometry
{
    
class MeshBase
{
public:

    //-------------------------------------------------------------------
    //                         Type Definitions
    //-------------------------------------------------------------------
    typedef double     ScalarType;
    typedef std::vector<double> VectorType;
        
    typedef NodeBase   NodeHandle;
    typedef CornerBase CornerHandle;
    typedef EdgeBase   EdgeHandle;
    typedef FaceBase   FaceHandle;
    typedef SideBase   SideHandle;
    typedef ZoneBase   ZoneHandle;

    // a typedef so that derived types know their based type
    // Element iterators.
    typedef std::vector<NodeBase>::const_iterator     NodeIterator;
    typedef std::vector<ZoneBase>::const_iterator     ZoneIterator;
    typedef std::vector<FaceBase>::const_iterator     FaceIterator;
    typedef std::vector<EdgeBase>::const_iterator     EdgeIterator;
    typedef std::vector<SideBase>::const_iterator     SideIterator;
    typedef std::vector<CornerBase>::const_iterator   CornerIterator;

    typedef Communication::CommAgent         CommAgentType;

    //-------------------------------------------------------------------
    //                Constructors, Destructors, Etc.
    //-------------------------------------------------------------------
    MeshBase(const char* meshFile);

    ~MeshBase();

    template<typename ElementType>
    size_t getLocalID(const ElementType& element) const;

    template<typename ElementType>
    size_t getGlobalID(const ElementType& element) const;

    size_t getNumberOfOwnedNodes() const;
    size_t getNumberOfNodes() const;
    NodeIterator ownedNodeBegin() const;
    NodeIterator ownedNodeEnd() const;
    NodeIterator nodeBegin() const;
    NodeIterator nodeEnd() const;

    size_t getNumberOfInternalSides() const;
    SideIterator ownedSideBegin() const;
    SideIterator ownedSideEnd() const;
        
    size_t getNumberOfOwnedZones() const;
    ZoneIterator ownedZoneBegin() const;
    ZoneIterator ownedZoneEnd() const;
    size_t getNumberOfInternalZones() const;
    ZoneIterator internalZoneBegin() const;
    ZoneIterator internalZoneEnd() const;
    ZoneIterator zoneBegin() const;
    ZoneIterator zoneEnd() const;
    const std::vector<ZoneBase>& ownedZones() const { return mOwnedZoneCache; }
        
    size_t getNumberOfCorners() const;
    size_t getNumberOfOwnedCorners() const;
    CornerIterator ownedCornerBegin() const;
    CornerIterator ownedCornerEnd() const;
    CornerIterator cornerBegin() const;
    CornerIterator cornerEnd() const;
        
    size_t getNumberOfOwnedFaces() const;
    size_t getNumberOfFaces() const;
    FaceIterator ownedFaceBegin() const;
    FaceIterator ownedFaceEnd() const;
    FaceIterator faceBegin() const;
    FaceIterator faceEnd() const;
        
    //-------------------------------------------------------------------
    //                         Connectivity Methods
    //-------------------------------------------------------------------

    CornerIterator zonesCornersBegin(long zID) const;
    CornerIterator zonesCornersEnd(long zID) const;
        
    SideIterator zonesSidesBegin(long zID) const;
    SideIterator zonesSidesEnd(long zID) const;

    FaceIterator zonesFacesBegin(long zID) const;
    FaceIterator zonesFacesEnd(long zID) const;

    FaceIterator cornersFacesBegin(long cID) const;
    FaceIterator cornersFacesEnd(long cID) const;

    CornerIterator facesCornersBegin(long fID) const;
    CornerIterator facesCornersEnd(long fID) const;

    SideIterator facesSidesBegin(long fID) const;
    SideIterator facesSidesEnd(long fID) const;

    //-------------------------------------------------------------------
    //                         Communication interface Methods
    //-------------------------------------------------------------------
    const CommAgentType& getCommAgent() const;

    // The mesh contains no surface faces, so isExternal() always returns false.
    // However, for hack purposes we'll ask the face if it is an external surface
    bool isSurface(const FaceHandle& element) const { return element.isExternalSurface(); }

    // The mesh contains no external elements, so isExternal() always returns false.
    bool isExternal( const FaceHandle& theFace ) const { return false; }
        
    // The mesh contains no external elements, so isInternal() always returns true.
    bool isInternal( const FaceHandle& theFace ) const { return true; }
        
        
    // Const reference to the mesh.
    static const MeshBase* mMeshPtr;

private:

    //-------------------------------------------------------------------
    //                         Disabled Methods
    //-------------------------------------------------------------------

    // No copy constructor.
    MeshBase(const MeshBase& mesh);

    // No assignment operator.
    MeshBase& operator=(const MeshBase& mesh);

    //-------------------------------------------------------------------
    //                         Internal methods
    //-------------------------------------------------------------------
    void buildNodeCache();
    void buildZoneCache();
    void buildFaceCache();
    void buildEdgeCache();
    void buildCornerCache();
    void buildSideCache();

    void buildConnectivity();
        
    void updateFaceClassification();
    void updateCommFaceClassification();

    void buildCommAgent();
        
protected:
    // Communication Agent
    CommAgentType mCommAgent;

    //-------------------------------------------------------------------
    //                         Cached Data
    //-------------------------------------------------------------------
    std::vector<NodeBase>   mOwnedNodeCache;
    std::vector<ZoneBase>   mOwnedZoneCache;
    std::vector<FaceBase>   mOwnedFaceCache;
    std::vector<EdgeBase>   mOwnedEdgeCache;
    std::vector<CornerBase> mOwnedCornerCache;
    std::vector<SideBase>   mOwnedSideCache;          

    std::map<long,std::vector<CornerBase> > mZonesCorners;
    std::map<long,std::vector<FaceBase> >   mZonesFaces;
    std::map<long,std::vector<SideBase> >   mZonesSides;
    std::map<long,std::vector<FaceBase> >   mCornersFaces;
    std::map<long,std::vector<CornerBase> > mFacesCorners;
    std::map<long,std::vector<SideBase> >   mFacesSides;
        
}; // end class MeshBase
    
}// end namespace Geometry

//----------------------------------------------------------------------------
//                            Inlined Methods
//----------------------------------------------------------------------------

namespace Geometry
{
    
template<typename ElementType>
inline size_t 
MeshBase::getLocalID(const ElementType& element) const
{
    return element.getLocalID();
}    

template<typename ElementType>
inline size_t 
MeshBase::getGlobalID(const ElementType& element) const
{
    return element.getGlobalID();
}
    

//-------------------------------------------------------------------
//                Node related functions
//-------------------------------------------------------------------
inline size_t
MeshBase::getNumberOfOwnedNodes() const
{
    return mOwnedNodeCache.size();
}

inline size_t
MeshBase::getNumberOfNodes() const
{
    return mOwnedNodeCache.size();
}

inline MeshBase::NodeIterator
MeshBase::ownedNodeBegin() const
{ 
    return mOwnedNodeCache.begin(); 
}

inline MeshBase::NodeIterator
MeshBase::ownedNodeEnd() const
{ 
    return mOwnedNodeCache.end(); 
}

inline MeshBase::NodeIterator
MeshBase::nodeBegin() const
{ 
    return mOwnedNodeCache.begin(); 
}

inline MeshBase::NodeIterator
MeshBase::nodeEnd() const
{ 
    return mOwnedNodeCache.end(); 
}

//-------------------------------------------------------------------
//                Side related functions
//-------------------------------------------------------------------
inline size_t
MeshBase::getNumberOfInternalSides() const
{
    return mOwnedSideCache.size();
}
    
inline MeshBase::SideIterator
MeshBase::ownedSideBegin() const
{ 
    return mOwnedSideCache.begin(); 
}

inline MeshBase::SideIterator
MeshBase::ownedSideEnd() const
{ 
    return mOwnedSideCache.end(); 
}

//-------------------------------------------------------------------
//                Zone related functions
//-------------------------------------------------------------------
inline size_t
MeshBase::getNumberOfOwnedZones() const
{ 
    return mOwnedZoneCache.size(); 
}

inline MeshBase::ZoneIterator
MeshBase::ownedZoneBegin() const
{ 
    return mOwnedZoneCache.begin(); 
}

inline MeshBase::ZoneIterator
MeshBase::ownedZoneEnd() const
{ 
    return mOwnedZoneCache.end(); 
}

inline size_t
MeshBase::getNumberOfInternalZones() const
{ 
    return mOwnedZoneCache.size(); 
}

inline MeshBase::ZoneIterator
MeshBase::internalZoneBegin() const
{ 
    return mOwnedZoneCache.begin(); 
}

inline MeshBase::ZoneIterator
MeshBase::internalZoneEnd() const
{ 
    return mOwnedZoneCache.end(); 
}

inline MeshBase::ZoneIterator
MeshBase::zoneBegin() const
{ 
    return mOwnedZoneCache.begin(); 
}

inline MeshBase::ZoneIterator
MeshBase::zoneEnd() const
{ 
    return mOwnedZoneCache.end(); 
}

//-------------------------------------------------------------------
//                Corner related functions
//-------------------------------------------------------------------
inline size_t
MeshBase::getNumberOfOwnedCorners() const
{ 
    return mOwnedCornerCache.size(); 
}

inline size_t
MeshBase::getNumberOfCorners() const
{ 
    return mOwnedCornerCache.size(); 
}

inline MeshBase::CornerIterator
MeshBase::ownedCornerBegin() const
{ 
    return mOwnedCornerCache.begin(); 
}

inline MeshBase::CornerIterator
MeshBase::ownedCornerEnd() const
{ 
    return mOwnedCornerCache.end(); 
}

inline MeshBase::CornerIterator
MeshBase::cornerBegin() const
{ 
    return mOwnedCornerCache.begin(); 
}

inline MeshBase::CornerIterator
MeshBase::cornerEnd() const
{ 
    return mOwnedCornerCache.end(); 
}

//-------------------------------------------------------------------
//                Face related functions
//-------------------------------------------------------------------
inline size_t 
MeshBase::getNumberOfOwnedFaces() const
{
    return mOwnedFaceCache.size();
}
    
inline size_t 
MeshBase::getNumberOfFaces() const
{
    return mOwnedFaceCache.size();
}

inline MeshBase::FaceIterator
MeshBase::ownedFaceBegin() const
{ 
    return mOwnedFaceCache.begin(); 
}

inline MeshBase::FaceIterator
MeshBase::ownedFaceEnd() const
{ 
    return mOwnedFaceCache.end(); 
}

inline MeshBase::FaceIterator
MeshBase::faceBegin() const
{ 
    return mOwnedFaceCache.begin(); 
}

inline MeshBase::FaceIterator
MeshBase::faceEnd() const
{ 
    return mOwnedFaceCache.end(); 
}

//-------------------------------------------------------------------
//                         Connectivity Methods
//-------------------------------------------------------------------

inline MeshBase::CornerIterator 
MeshBase::zonesCornersBegin(long zID) const
{
    return const_cast<MeshBase*>(this)->mZonesCorners[zID].begin();
}
    
inline MeshBase::CornerIterator 
MeshBase::zonesCornersEnd(long zID) const
{
    return const_cast<MeshBase*>(this)->mZonesCorners[zID].end();
}
        
inline MeshBase::SideIterator 
MeshBase::zonesSidesBegin(long zID) const
{
    return const_cast<MeshBase*>(this)->mZonesSides[zID].begin();
}

inline MeshBase::SideIterator 
MeshBase::zonesSidesEnd(long zID) const
{
    return const_cast<MeshBase*>(this)->mZonesSides[zID].end();
}

inline MeshBase::FaceIterator 
MeshBase::zonesFacesBegin(long zID) const
{   
    return const_cast<MeshBase*>(this)->mZonesFaces[zID].begin();
}

inline MeshBase::FaceIterator 
MeshBase::zonesFacesEnd(long zID) const
{
    return const_cast<MeshBase*>(this)->mZonesFaces[zID].end();
}

inline MeshBase::FaceIterator 
MeshBase::cornersFacesBegin(long cID) const
{
    return const_cast<MeshBase*>(this)->mCornersFaces[cID].begin();
}
        
inline MeshBase::FaceIterator 
MeshBase::cornersFacesEnd(long cID) const
{
    return const_cast<MeshBase*>(this)->mCornersFaces[cID].end();
}
    
inline MeshBase::CornerIterator 
MeshBase::facesCornersBegin(long fID) const
{
    return const_cast<MeshBase*>(this)->mFacesCorners[fID].begin();
}

inline MeshBase::CornerIterator 
MeshBase::facesCornersEnd(long fID) const
{
    return const_cast<MeshBase*>(this)->mFacesCorners[fID].end();
}
inline MeshBase::SideIterator MeshBase::facesSidesBegin(long fID) const
{
    return const_cast<MeshBase*>(this)->mFacesSides[fID].begin();
}

inline MeshBase::SideIterator MeshBase::facesSidesEnd(long fID) const
{
    return const_cast<MeshBase*>(this)->mFacesSides[fID].end();
}


//-------------------------------------------------------------------
//                         Communication interface Methods
//-------------------------------------------------------------------
inline const MeshBase::CommAgentType& 
MeshBase::getCommAgent() const
{
    return mCommAgent;
}
    

} // end namespace Geometry

#endif
