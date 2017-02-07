!     $Id: iot_rand.f,v 1.1 1997/11/06 16:02:34 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     Pseudo random function
!     Release 1.0
!
!     Description:
!     Portable random generator 
!
!     Interface:
!     use iot_rand
!
!     External components:
!     none
!-------------------------------------------------------------------

      module iot_rand

      contains

!-----------------------------------------------------------------------
!     iotrand
!
!     Portable random generator 
!      Not a good generator -- but sufficient for this purpose.
!      Returns a double
!-----------------------------------------------------------------------

      function iotrand(seed) result (ran)

      implicit none

      integer, intent(in) :: seed

      integer, save :: jran = -1
      integer, parameter :: im = 214326, ia = 3613, ic = 45289
      real(kind=8), parameter :: invim = 1.0/im
      real(kind=8) :: ran

      if ((jran < 0).or.(seed < 0)) jran = abs(seed)

      jran = modulo(jran*ia + ic,im)
      ran = real(jran*invim,8)
      
      end function iotrand

!-----------------------------------------------------------------------

      end module iot_rand




