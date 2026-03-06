! Kind Module:  define the kind required for the requested precision;
!               `adqt' is the default "adequate" precision.
!   short:  real kind for 6 digit accuracy
!    long:  real kind for 15 digit accuracy
!   quadp:  real kind for 33 digit accuracy
!    adqt:  default ("adequate") precision

module kind_mod

private

  integer, parameter, public :: short = selected_real_kind( 6,  37)
  integer, parameter, public ::  long = selected_real_kind(13, 307)
  integer, parameter, public :: quadp = selected_real_kind(33,4931)
  integer, parameter, public ::  adqt = selected_real_kind(13, 307)

end module kind_mod
