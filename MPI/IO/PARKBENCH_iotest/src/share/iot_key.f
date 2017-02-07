!     $Id: iot_key.f,v 1.5 1997/08/22 15:49:35 oliver Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     iot_key
!     Release 1.0
!
!     Description:
!     Fortran 90 module containing global key, value string structure
!
!     Interface:
!     use iot_key
!
!     External components:
!     iot_defs, iot_string
!-------------------------------------------------------------------

      module iot_key

      use iot_defs
      use iot_string

!     array of input key,value pairs
      type(string),dimension(IOT_NUM_KEYS) :: ios

      real(kind=8) :: IOT_STAT_EMPTY

!     arrays to hold statistics data, type and keyword info
      real(kind=8),dimension(:),allocatable :: sval
      integer,dimension(:),allocatable :: skey
      integer,dimension(:),allocatable :: stype
      integer :: sidx
      integer :: maxstatsize

!     character array and integer to hold process name for run
      character(len=MPI_MAX_PROCESSOR_NAME) :: nodename
      integer :: lnodename

!     character arrays and integers to hold time stamps for run
      character(len=IOT_MAX_DATE_AND_TIME_LEN) :: starttime, finishtime
      integer :: lstarttime, lfinishtime

      end module iot_key
