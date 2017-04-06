#ifndef __SWIG_Region_
#define __SWIG_Region_

%include "geom/CMI/swig/MeshBase.sw"

%{
#include "geom/Region/Region.hh"
%}

namespace Geometry
{
   template <typename Mesh>
   class Region
   {
      public:
      typedef Mesh MeshType;
      explicit Region(const MeshType& mesh);
   };
}

#endif
