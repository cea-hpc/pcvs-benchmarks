#ifndef GEOMETRY_FIELD_HH
#define GEOMETRY_FIELD_HH
#include "geom/CMI/MeshBase.hh"
#include "geom/Region/Region.hh"
#include <vector>

namespace Geometry 
{

    template <typename Space, typename Centering, typename Value>
    class Field
    {
    private:
        typedef std::vector<Value>                        StorageType;
        typedef typename StorageType::reference           Reference;
        typedef typename StorageType::const_reference     ConstReference;
        
    public:
        //--------------------------------------------------------------------
        //          Constructors, Destructor, Assignment Operators 
        //--------------------------------------------------------------------
        
        //! Construct a field and with the given associated space and name.
        
        //! This constructor initializes all field values to the default value of 
        //! the given data type.
        //! \param space the space upon which this field takes its values.
        Field(const Space& space);

        Field(const Space& space, const Value& aValue);
        
        ~Field();
        
        //! Geometric mapping operator.
        //! \param e a geometric element
        //! \returns a reference to the value of the field corresponding to e.
        Reference            operator[](const Centering& e);

        //! Const geometric mapping operator.
        //! \param e a geometric element
        //! \returns a const reference to the value of the field corresponding to e.
        ConstReference       operator[](const Centering& e) const;

        //! Return the number of values in this field.
        size_t               size() const;

        bool operator==(const Field &rhs) const;
        
    private:
        std::vector<Value> mDataVector;
    
    };
    

} // end of namespace Geometry


#endif
