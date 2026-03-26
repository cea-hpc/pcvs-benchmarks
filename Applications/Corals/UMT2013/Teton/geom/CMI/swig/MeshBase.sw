#ifndef __SWIG_MeshBase
#define __SWIG_MeshBase

%{
#include "geom/CMI/MeshBase.hh"
%}

namespace Geometry
{
   class MeshBase
   {
      public:
      MeshBase(const char* meshFile);

      size_t getNumberOfOwnedNodes() const;
      size_t getNumberOfOwnedZones() const;
      const std::vector<ZoneBase>& ownedZones() const { return mOwnedZoneCache; }
   };
}

#endif
