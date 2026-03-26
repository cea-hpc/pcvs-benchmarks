! Constant Module:  various commonly used constants.

module constant_mod

use kind_mod

private

  real(adqt), parameter, public :: &
                      zero = 0.0_adqt, &
                      sixth = 1.0_adqt/6.0_adqt, &
                      fourth = 0.25_adqt, &
                      third = 1.0_adqt/3.0_adqt, &
                      half = 0.5_adqt, &
                      one = 1.0_adqt, &
                      two = 2.0_adqt, &
                      three = 3.0_adqt, &
                      pi = 3.14159265358979323846264338327950_adqt, &
                      four = 4.0_adqt, &
                      five = 5.0_adqt, &
                      six = 6.0_adqt, &
                      seven = 7.0_adqt, &
                      eight = 8.0_adqt, &
                      nine = 9.0_adqt, &
                      ten = 10.0_adqt, &
                      twenty = 20.0_adqt, &
                      sixty = 60.0_adqt, &
                      hundred = 100.0_adqt

  real(adqt), parameter, public :: &
                      adqtTiny = tiny(0.0_adqt), &
                      adqtEpsilon = epsilon(0.0_adqt), &
                      longTiny = tiny(0.0_long), &
                      longEpsilon = epsilon(0.0_long)

end module constant_mod
