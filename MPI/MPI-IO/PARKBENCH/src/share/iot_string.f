!     $Id: iot_string.f,v 1.4 1997/09/08 12:33:59 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     iot_string
!     Release 1.0
!
!     Description:
!     Fortran 90 module containing string type and functions
!
!     Interface:
!     use iot_string
!
!     External components:
!     iot_defs
!-------------------------------------------------------------------

      module iot_string
      use iot_defs

      intrinsic len

!     string structure
      type string
         logical :: inuse
         integer :: length
         character(len=IOT_MAX_STRING_LEN),pointer :: s
      end type

      interface assignment(=)
      module procedure char_from_string, string_from_char
      end interface
      interface len
      module procedure len_string
      end interface
!      interface str
!      module procedure str_string
!      end interface
      interface operator(==)
      module procedure eq_string_string, eq_string_char
      module procedure eq_char_string
      end interface

      contains

!     define len intrinsic for derived type string
      function len_string(a) result (res)
      implicit none
      integer :: res
      type(string) :: a
      res = a%length
      return
      end function len_string

!     define str function to return character array for derived type string
!      function str_string(a) result (res)
!      implicit none
!      character(len=IOT_MAX_STRING_LEN),pointer :: res
!      type(string),target :: a
!      res => a%s
!      return
!      end function str_string

      subroutine string_from_char(a, b)
      implicit none
      character(len=*),intent(in) :: b
      type(string),intent(out) :: a
      integer n
      n = len_trim(b)
      if (len(a%s) < n) n = len(a%s)
      a%length = n
      a%s = b(1:a%length)
      end subroutine

      subroutine char_from_string(a, b)
      implicit none
      intrinsic len
      character(len=*),intent(out) :: a
      type(string),intent(in) :: b
      integer n
      n = b%length
      if (len(a) < n) n = len(a)
      a(1:n) = b%s(1:n)
      end subroutine

!     define .eq. intrinsic for derived type string
      function eq_string_string(a, b) result (res)
      implicit none
      logical :: res
      type(string),intent(in) :: a, b
      if (a%s(1:a%length) == b%s(1:b%length)) then
         res = .true.
      else
         res = .false.
      end if
      return
      end function

      function eq_string_char(a, b) result (res)
      implicit none
      logical :: res
      type(string),intent(in) :: a
      character(len=*),intent(in) :: b
      if (a%s(1:a%length) == b) then
         res = .true.
      else
         res = .false.
      end if
      return
      end function

      function eq_char_string(a, b) result (res)
      implicit none
      logical :: res
      type(string),intent(in) :: b
      character(len=*),intent(in) :: a
      if (b%s(1:b%length) == a) then
         res = .true.
      else
         res = .false.
      end if
      return
      end function

!     allocate string memory
      subroutine allocate_string(a, ierr)
      implicit none
      type(string),intent(inout) :: a
      integer :: ierr
      ierr = IOT_SUCCESS
      if (a%inuse) then
!        already in use
         ierr = IOT_ERR_ALLOC
         return
      end if
      allocate(a%s, stat=ierr)
      if (ierr /= 0) then
         ierr = IOT_ERR_ALLOC
         return
      end if
      a%length = 0
      a%inuse = .true.
      end subroutine

!     deallocate string memory
      subroutine deallocate_string(a, ierr)
      implicit none
      type(string),intent(inout) :: a
      integer :: ierr
      ierr = IOT_SUCCESS
      if (.not. a%inuse) then
!        not in use
         ierr = IOT_ERR_ALLOC
         return
      end if
      deallocate(a%s, stat=ierr)
      if (ierr /= 0) then
         ierr = IOT_ERR_ALLOC
         return
      end if
      a%length = 0
      a%inuse = .false.
      end subroutine

      end module iot_string

