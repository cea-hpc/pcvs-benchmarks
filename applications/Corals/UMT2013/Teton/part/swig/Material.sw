#ifndef __SWIG_Material_
#define __SWIG_Material_

%include "part/swig/OpacityBase.sw"

%{
#include "part/Material.hh"
%}

class Material
{
	public:
	Material(OpacityBase& opacity);
};

#endif
