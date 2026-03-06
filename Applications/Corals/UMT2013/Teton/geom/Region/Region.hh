#ifndef GEOMETRY_REGION_HH
#define GEOMETRY_REGION_HH

#include <vector>


namespace Geometry
{
    
    class ZoneBase;
    
    template <typename Mesh>
    class Region
    {
    public:
        //-------------------------------------------------------------------
        //                         Type Definitions
        //-------------------------------------------------------------------
        typedef Mesh                                             MeshType;
        typedef ZoneBase                                         ZoneHandle;
        typedef typename std::vector<ZoneHandle>::const_iterator ZoneIterator;

        //! Constructor that produces an empty Region upon a given mesh.
        explicit Region(const MeshType& mesh);

        size_t getNumberOfInternalZones() const{ return mInternalZones.size();}
        ZoneIterator internalZoneBegin() const{ return mInternalZones.begin();}
        ZoneIterator internalZoneEnd() const{ return mInternalZones.end();}

        size_t getNumberOfZones() const{ return mInternalZones.size();}
        ZoneIterator zoneBegin() const{ return mInternalZones.begin();}
        ZoneIterator zoneEnd() const{ return mInternalZones.end();}

        size_t getNumberOfOwnedZones() const{ return mInternalZones.size();}
        ZoneIterator ownedZoneBegin() const{ return mInternalZones.begin();}
        ZoneIterator ownedZoneEnd() const{ return mInternalZones.end();}
        
    
        size_t getLocalID(const ZoneHandle& element) const { return element.getLocalID();};
        
        
                
        //-------------------------------------------------------------------
        //                      Other Accessors
        //-------------------------------------------------------------------
    
        const MeshType&             getMesh() const { return mMesh; }
    
    private:
    
        //-------------------------------------------------------------------
        //                         Disabled Methods
        //-------------------------------------------------------------------

        // No default constructor.
        Region();

        // No copy constructor.
        Region(const Region& Region);

        // No assignment operator.
        Region& operator=(const Region& Region);

        // Reference to a mesh.
        const MeshType&                        mMesh;
    
        // Zones.
        std::vector<ZoneHandle>                 mInternalZones;
    };

} //end namespace Geometry


#endif
