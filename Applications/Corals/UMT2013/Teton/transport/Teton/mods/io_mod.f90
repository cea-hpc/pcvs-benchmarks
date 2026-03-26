# 1 "mods/io_mod.F90"
# 1 "<interne>"
# 1 "<ligne-de-commande>"
# 1 "mods/io_mod.F90"
! I/O Module:  various input/output units.

module io_mod

private

  integer, parameter, public :: nopac = 4, &
                                nin   = 5, &
                                nout  = 6, &
                                neout = 8, &
                                nbout = 9

end module io_mod
