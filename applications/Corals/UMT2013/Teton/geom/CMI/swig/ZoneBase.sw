#ifndef __SWIG_ZoneBase
#define __SWIG_ZoneBase

%{
#include "geom/CMI/ZoneBase.hh"
%}

namespace Geometry
{
   class ZoneBase
   {
      public:
      ZoneBase();

      std::vector<double> getPosition()const;
      
   };
}

#endif
