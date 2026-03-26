#ifndef __SWIG_Part_
#define __SWIG_Part

%include "geom/Region/swig/Region.sw"
%include "part/swig/Material.sw"

%{
#include "part/Part.hh"
%}
   
enum eosType { CONSTANT, MARSHAK };

template <typename Mesh>
class Part
{
   public:

   typedef Geometry::Region<Mesh>   RegionType;
   Part(RegionType& region, Material& material, eosType theEosType=CONSTANT, 
         double initialDensity=1.0, double initialTemp=1.0);
};

#endif
