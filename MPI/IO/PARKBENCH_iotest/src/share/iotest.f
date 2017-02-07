!     $Id: iotest.f,v 1.8 1997/11/18 13:24:40 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     iotest
!     Release 1.0
!
!     Description:
!     Top level Fortran 90 module to be used in all IOT programs
!
!     Interface:
!     use iotest
!
!     External components:
!     iot_defs, iot_string, iot_key, iot_file, iot_err
!     iot_err, iot_types, iot_rand
!     mpif.h
!-------------------------------------------------------------------

      module iotest

      use iot_defs
      use iot_string
      use iot_key
      use iot_file
      use iot_err
      use iot_types
      use iot_rand

      contains

      subroutine iot_init(ierr)
      integer:: ierr
      IOT_STAT_EMPTY = huge(real(0,8))
      do i = 1, IOT_NUM_KEYS
         ios(i)%inuse = .false.
      end do
      ierr = IOT_SUCCESS
      return
      end subroutine

      end module iotest

!-------------------------------------------------------------------
