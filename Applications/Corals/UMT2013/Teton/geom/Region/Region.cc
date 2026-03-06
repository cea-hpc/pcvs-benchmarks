#include "Region.hh"

namespace Geometry
{
    
    template <typename Mesh>
    Region<Mesh>::
    Region(const typename Region<Mesh>::MeshType& mesh):
            mMesh(mesh)
    {
        for (typename Region<Mesh>::MeshType::ZoneIterator it = mesh.ownedZoneBegin();
             it != mesh.ownedZoneEnd();++it)
        {
            // Toss out any receive zones we find; add the rest.
            ZoneHandle zone = *it;
            if( ! zone.isReceive() )
            {
                mInternalZones.push_back(zone);
            }
        } // end for
    
    }
    
}//end namespace Geometry
