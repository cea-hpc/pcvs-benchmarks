#ifndef __SWIG_PartList_
#define __SWIG_PartList_

%include "geom/CMI/swig/MeshBase.sw"
%include "part/swig/Part.sw"

%{
#include "part/PartList.hh"
%}

template <typename Mesh>
class PartList
{
   public:
   typedef Part<Mesh>   PartType;

   PartList(const Mesh& m);
   void addPart( const PartType& thePart);
};
#endif
