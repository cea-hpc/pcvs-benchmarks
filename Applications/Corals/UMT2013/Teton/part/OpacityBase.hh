#ifndef OPACITY_BASE_HH
#define OPACITY_BASE_HH

#include <vector>

class OpacityBase
{
public:
    OpacityBase();
    
    void getAbsorption(std::vector<double>& opacity,
                       const std::vector<double>& density, 
                       const std::vector<double>& electronTemperature) const;
    
    void getScattering(std::vector<double>& opacity,
                       const std::vector<double>& density, 
                       const std::vector<double>& electronTemperature,
                       const std::vector<double>& radiationTemperature) const;

};//end of class OpacityBase


#endif
