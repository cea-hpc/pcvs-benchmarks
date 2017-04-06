#include "Field.hh"

namespace Geometry 
{
    template<typename Space, typename Centering, typename Value> 
    Field<Space,Centering,Value>::~Field()
    {
    }

    template<typename Space, typename Centering, typename Value>
    size_t
    Field<Space,Centering,Value>::size() const
    {        
        return mDataVector.size();
    }
    
    template<typename Space, typename Centering, typename Value>
    bool
    Field<Space,Centering,Value>::operator==(const Field& rhs) const
    {
        return (mDataVector == rhs.mDataVector);
    }
    

//------------------------------------------------------------------------
//               template specialization <MeshBase,FaceBase,int>
//------------------------------------------------------------------------
    template<> 
    Field<MeshBase,FaceBase,int>::Field(const MeshBase& theMesh)
            :mDataVector(theMesh.getNumberOfFaces() )
    {
    }

    template<> 
    Field<MeshBase,FaceBase,int>::Field(const MeshBase& theMesh, const int& aValue)
            :mDataVector(theMesh.getNumberOfFaces() )
    {
        std::fill(mDataVector.begin(),mDataVector.end(), aValue);
    }
    
    
    template<>
    Field<MeshBase,FaceBase,int>::Reference
    Field<MeshBase,FaceBase,int>::operator[](const FaceBase& e)
    {
        return mDataVector[e.getLocalID()];
    }
    
    template<>
    Field<MeshBase,FaceBase,int>::ConstReference
    Field<MeshBase,FaceBase,int>::operator[](const FaceBase& e) const
    {        
        return mDataVector[e.getLocalID()];
    }
    
//------------------------------------------------------------------------
//               template specialization <Region<MeshBase>,ZoneBase,double>
//------------------------------------------------------------------------
    template<> 
    Field<Region<MeshBase>,ZoneBase,double>::Field(const Region<MeshBase>& theRegion)
            :mDataVector(theRegion.getNumberOfZones() )
    {
    }

    template<> 
    Field<Region<MeshBase>,ZoneBase,double>::Field(const Region<MeshBase>& theRegion, const double& aValue)
            :mDataVector(theRegion.getNumberOfZones() )

    {
        std::fill(mDataVector.begin(),mDataVector.end(), aValue);
    }

    template<>
    Field<Region<MeshBase>,ZoneBase,double>::Reference
    Field<Region<MeshBase>,ZoneBase,double>::operator[](const ZoneBase& e)
    {
        return mDataVector[e.getLocalID()];
    }
    
    template<>
    Field<Region<MeshBase>,ZoneBase,double>::ConstReference
    Field<Region<MeshBase>,ZoneBase,double>::operator[](const ZoneBase& e) const
    {        
        return mDataVector[e.getLocalID()];
    }
    
}//end namespace Geometry 
