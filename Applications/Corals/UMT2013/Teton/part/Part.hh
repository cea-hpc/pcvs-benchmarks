#ifndef PART_HH
#define PART_HH

#include "geom/Region/Region.hh"
#include "geom/Field/Field.hh"
#include "Material.hh"

namespace Geometry
{
    class ZoneBase;
}

enum eosType { CONSTANT, MARSHAK };

template <typename Mesh>
class Part
{
public:
    
    // Spaces.
    typedef Mesh                                                MeshType;
    typedef Geometry::Region<Mesh>                              RegionType;

    // Mesh elements.
    typedef Geometry::ZoneBase                                  ZoneHandle;

    // Field types.
    typedef Geometry::Field<RegionType, ZoneHandle, double>     ZonalScalarFieldType;
    typedef Geometry::Field<RegionType, ZoneHandle, double>     ZCSF;

    //-------------------------------------------------------------------
    //                      Constructor, Destructor.
    //-------------------------------------------------------------------

    Part(RegionType& region, Material& material, eosType theEosType=CONSTANT, 
         double initialDensity=1.0, double initialTemp=1.0);
    
    const RegionType&          getRegion() const { return *mRegionPtr;}

    void         setPressure();
    void         setSpecificEnergy();
    void         setTemperature();
    void         setSoundSpeed();
    void         setSpecificHeatCV();
    void         setGamma();

    ZCSF&  getPressure() { return mPressure; }
    ZCSF&  getSpecificEnergy() { return mSpecificEnergy; }
    ZCSF&  getTemperature() { return mTemperature; }
    ZCSF&  getSoundSpeed() { return mSoundSpeed; }
    ZCSF&  getSpecificHeatCV() { return mSpecificHeatCV; }
    ZCSF&  getGamma() { return mGamma; }
    ZCSF&  getMassDensity() { return mMassDensity; }
    ZCSF&  getVolumeFraction() { return mVolumeFraction; }
    ZCSF&  getElectronTemperature() { return mElectronTemperature; }
    ZCSF&  getRadiationTemperature() { return mRadiationTemperature; }
    
    const Material&            getMaterial() const{ return *mMaterialPtr;}
    
    void setEOSType( eosType newType) { mEOSType = newType; }
    eosType getEOSType( )const { return mEOSType; }
            
    bool operator==(const Part& rhs) const;
    
    void updateThermodynamicState(const std::vector<double> &matTemp, 
                                  const std::vector<double> &radTemp);

private:
    RegionType*                                mRegionPtr;
    Material*                                  mMaterialPtr;

    ZCSF                                       mPressure;
    ZCSF                                       mSpecificEnergy;
    ZCSF                                       mTemperature;
    ZCSF                                       mSoundSpeed;
    ZCSF                                       mSpecificHeatCV;
    ZCSF                                       mGamma;
    ZCSF                                       mMassDensity;
    ZCSF                                       mVolumeFraction;
    ZCSF                                       mElectronTemperature;
    ZCSF                                       mRadiationTemperature;
    eosType                                    mEOSType;

    void initializeThermodynamicState(double initDensity, double initTemp);
};

#endif
