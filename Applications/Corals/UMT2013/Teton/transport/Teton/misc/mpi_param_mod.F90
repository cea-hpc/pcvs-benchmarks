!=======================================================================
!                        Version 1: 02/99, MRZ
!-----------------------------------------------------------------------
! MPI_param
!   This class wraps the MPI parameters obtained from the system-
! dependent include file so that it can be used by both free- and
! fixed-format Fortran90.
!-----------------------------------------------------------------------
! v1.0: Original implementation
!=======================================================================

module mpi_param_mod
                                                                                                     
  use kind_mod

  public

#ifdef MPI
! MPI is enabled: include the system-dependent MPI include file

#include <mpif.h>

#else
! MPI is disabled: required variable declarations to satisfy remaining
! stub routines

  integer, parameter, public :: MPI_COMM_WORLD=0
  integer, parameter, public :: MPI_STATUS_SIZE=1

#endif

end module mpi_param_mod
