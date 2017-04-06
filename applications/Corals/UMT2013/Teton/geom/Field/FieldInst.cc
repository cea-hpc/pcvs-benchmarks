#include "geom/CMI/MeshBase.hh"
#include "Field.cc"

namespace Geometry 
{
    template class Field<MeshBase,FaceBase,int>;
    template class Field<MeshBase,ZoneBase,double>;
    template class Field<Region<MeshBase>,ZoneBase,double>;
    
}//end namespace Geometry
