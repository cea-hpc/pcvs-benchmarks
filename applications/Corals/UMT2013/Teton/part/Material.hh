#ifndef MATERIAL_HH
#define MATERIAL_HH

#include "OpacityBase.hh"

class Material
{
public:
   
    Material(OpacityBase& opacity) 
            :mOpacity(opacity)
        {};
    
    
    inline const OpacityBase& getOpacity(void)  const
        { return mOpacity; }
protected:

    //------------------------------------------------------------
    //                   Disabled methods.
    //------------------------------------------------------------

    // Default constructor.
    Material();

    // Copy constructor.
    Material(const Material& material);

    // Assignment operator.
    Material&
    operator=(const Material& material);

    //------------------------------------------------------------
    //                   Data members.
    //------------------------------------------------------------

    OpacityBase&            mOpacity;

}; // end of class Material

#endif
