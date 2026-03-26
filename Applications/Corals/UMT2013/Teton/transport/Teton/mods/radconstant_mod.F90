! Radiation Constant Module:  commonly used radiation constants.

module radconstant_mod

use kind_mod

private

  real(adqt), parameter, public :: &
                      speed_light   = 299.792458_adqt, &
                      rad_constant  = 0.0137215980748942_adqt, &
                      rad_constant2 = 0.002112985235141618_adqt, &
                      electronMass  = 511.0041_adqt

end module radconstant_mod

