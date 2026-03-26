#include "Part.hh"
#include <cmath>

template<typename Mesh>
Part<Mesh>::Part(RegionType& region, Material& material, eosType theEosType,
                 double initialDensity, double initialTemp)
:
        mRegionPtr(&region),
        mMaterialPtr(&material),
        mPressure(region,0.0),
        mSpecificEnergy(region,0.05),
        mTemperature(region,1.0),
        mSoundSpeed(region,0.0),
        mSpecificHeatCV(region,0.05),
        mGamma(region,0.05),
        mMassDensity(region,1.0),
        mVolumeFraction(region,1.0),
        mElectronTemperature(region,1.0),
        mRadiationTemperature(region,1.0),
        mEOSType(theEosType)
{
    initializeThermodynamicState(initialDensity, initialTemp);
}


template<typename Mesh>
bool 
Part<Mesh>::operator==(const Part& rhs) const
{
    return ( (mPressure == rhs.mPressure) &&
             (mSpecificEnergy == rhs.mSpecificEnergy) &&
             (mTemperature == rhs.mTemperature) &&
             (mSoundSpeed == rhs.mSoundSpeed) &&
             (mSpecificHeatCV == rhs.mSpecificHeatCV) &&
             (mGamma == rhs.mGamma) &&
             (mMassDensity == rhs.mMassDensity) &&
             (mVolumeFraction == rhs.mVolumeFraction) &&
             (mElectronTemperature == rhs.mElectronTemperature) &&
             (mRadiationTemperature == rhs.mRadiationTemperature)
             );
}

    

template<typename Mesh>
void  
Part<Mesh>::setPressure() 
{
    if( mEOSType == CONSTANT )
    {
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mPressure[*zIt] = 0.0;
        }
    }
    else if( mEOSType == MARSHAK ) 
    {
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mPressure[*zIt] = 0.0;
        }
    }
}


template<typename Mesh>
void  
Part<Mesh>::setSpecificEnergy() 
{
    if( mEOSType == CONSTANT )
    {
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mSpecificEnergy[*zIt] = 0.05;
        }
    }
    else if( mEOSType == MARSHAK ) 
    {
        double mCvParameter = 5.488066013988917e-02;
        
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            double T = mTemperature[*zIt];
            mSpecificEnergy[*zIt] = 0.25 * mCvParameter * T * T * T * T;
        }
    }
}


template<typename Mesh>
void  
Part<Mesh>::setTemperature() 
{
    if( mEOSType == CONSTANT )
    {
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mTemperature[*zIt] = 1.0;
        }
    }
    else if( mEOSType == MARSHAK ) 
    {
        double mCvParameter = 5.488066013988917e-02;
        
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            double x = 4.0 * mSpecificEnergy[*zIt]/mCvParameter;
            mTemperature[*zIt] = std::pow(x,0.25);
        }
    }
}


template<typename Mesh>
void  
Part<Mesh>::setSoundSpeed() 
{
    if( mEOSType == CONSTANT )
    {
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mSoundSpeed[*zIt] = 0.0;
        }
    }
    else if( mEOSType == MARSHAK ) 
    {
        
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mSoundSpeed[*zIt] = 0.0;
        }
    }
}


template<typename Mesh>
void  
Part<Mesh>::setSpecificHeatCV() 
{
    if( mEOSType == CONSTANT )
    {
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mSpecificHeatCV[*zIt] = 0.05;
        }
    }
    else if( mEOSType == MARSHAK ) 
    {
        double mCvParameter = 5.488066013988917e-02;
        
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            double T = mElectronTemperature[*zIt];
            mSpecificHeatCV[*zIt] = mCvParameter * T * T * T;
        }
    }

}
            
template<typename Mesh>
void  
Part<Mesh>::setGamma() 
{
    if( mEOSType == CONSTANT )
    {
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mGamma[*zIt] = 0.05;
        }
    }
    else if( mEOSType == MARSHAK ) 
    {        
        for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
            zIt != mRegionPtr->internalZoneEnd(); ++zIt)
        {
            mGamma[*zIt] = 1.0;       
        }
    }
}


template<typename Mesh>
void 
Part<Mesh>::initializeThermodynamicState(double initDensity, double initTemp)
{
    
    for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
        zIt != mRegionPtr->internalZoneEnd(); ++zIt)
    {
        mTemperature[*zIt] = initTemp;
        mRadiationTemperature[*zIt] = initTemp;
        mElectronTemperature[*zIt] = initTemp;
        mMassDensity[*zIt] = initDensity;
        mVolumeFraction[*zIt] = 1.0;
    }
    setPressure();
    setSpecificEnergy();
    setSoundSpeed();
    setSpecificHeatCV();
    setGamma();
}


template<typename Mesh>
void 
Part<Mesh>::updateThermodynamicState(const std::vector<double> &matTemp, 
                                     const std::vector<double> &radTemp)
{
    std::vector<double>::const_iterator matTempIter = matTemp.begin();
    std::vector<double>::const_iterator radTempIter = radTemp.begin();
    
    // we assume electron temperature == material temperature

    for(typename RegionType::ZoneIterator zIt = mRegionPtr->internalZoneBegin(); 
        zIt != mRegionPtr->internalZoneEnd(); ++zIt,++matTempIter,++radTempIter)
    {
        mTemperature[*zIt] = *matTempIter;
        mRadiationTemperature[*zIt] = *radTempIter;
        mElectronTemperature[*zIt] = *matTempIter;
        
    }
    
    setPressure();
    setSpecificEnergy();
    setSoundSpeed();
    setSpecificHeatCV();
    setGamma();

}

