#include "OpacityBase.hh"

OpacityBase::OpacityBase()
{}

void
OpacityBase::getAbsorption(std::vector<double>& opacity,
                           const std::vector<double>& density, 
                           const std::vector<double>& electronTemperature) const
{
    std::fill(opacity.begin(),opacity.end(),1.0);
}

void 
OpacityBase::getScattering(std::vector<double>& opacity,
                           const std::vector<double>& density, 
                           const std::vector<double>& electronTemperature,
                           const std::vector<double>& radiationTemperature) const
{
    std::fill(opacity.begin(),opacity.end(),0.0);
}
