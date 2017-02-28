!     $Id: iot_defs.f,v 1.27 1998/01/21 18:13:57 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     iot_defs
!     Release 1.0
!
!     Description:
!     Fortran 90 module containing constant definitions for IOT
!     and debug **variable**
!
!     Interface:
!     use iot_defs
!
!     External components:
!     None
!-------------------------------------------------------------------

      module iot_defs
      
!     include MPI definitions
      include "mpif.h"

!     add some MPI-2 defs not yet in PALLAS file
!      integer,parameter :: MPI_OFFSET_KIND = 8
!      integer,parameter :: MPI_ADDRESS_KIND = 4
!      integer,parameter :: MPI_DATATYPE_NULL = 0 ! is this right?
!      integer,parameter :: MPI_TYPE_NULL = 0 ! is this right?

!     missing from new Pallas MPI
!      double precision MPI_WTIME, MPI_WTICK
!      external MPI_WTIME, MPI_WTICK

!     general constants

!     scaling for user input file values filesize and blocksize
      integer,parameter :: IOT_FILESIZE_UNIT = 1000000
      integer,parameter :: IOT_BLOCKSIZE_UNIT = 1000000
!     maximum line length for line in user input file
      integer,parameter :: IOT_MAX_LINE_LEN = 256
!     maximum length of strings
      integer,parameter :: IOT_MAX_STRING_LEN = 256
!     maximum buffer size for broadcasting test parameters
      integer,parameter :: IOT_MAX_BUF_LEN = 10000
!     Fortran unit number for user test parameter file
      integer,parameter :: IOT_USERFILE_UNIT = 12
!     Fortran unit number for output statistics file
      integer,parameter :: IOT_STATFILE_UNIT = 13
!     Fortran unit number for Fortran I/O data file
      integer,parameter :: IOT_DATAFILE_UNIT = 14
!     Fortran unit number for program error messages
      integer,parameter :: fout = 6
!     keyword field width in output file
      integer,parameter :: IOT_OFKEY_LEN = 20
!     value/timing field width in output file
      integer,parameter :: IOT_OFVAL_LEN = 20
!     length of character variable for format strings
      integer,parameter :: IOT_FORMAT_LEN = 50
!     number of decimal places in value/timing in output file
      integer,parameter :: IOT_OFVAL_DP = 8
!     maximum length of string for F90 date and time function
      integer,parameter :: IOT_MAX_DATE_AND_TIME_LEN = 25
!     random seed for iot generator
      integer,parameter :: IOT_RSEED = 79
!     accuracy for self checking
      real(kind=8),parameter :: IOT_ACCURACY = 1.0e-5
!     kind type for 8 byte, double precision reals
      integer,parameter :: DBL = SELECTED_REAL_KIND(15,307)
!     kind type for 8 byte, integers
      integer,parameter :: IDBL = SELECTED_INT_KIND(15)

!     data types for output information
      integer,parameter :: IOT_REAL = 1
      integer,parameter :: IOT_INTEGER = 2
      integer,parameter :: IOT_CHARACTER = 3

!     keyword identifiers
      integer,parameter :: IOT_NUM_KEYS = 33
      integer,parameter :: IOT_FIRST_HINT = 4
      integer,parameter :: IOT_LAST_HINT = 6
      integer,parameter :: IOT_KEY_CLASSNAME = 1
      integer,parameter :: IOT_KEY_TESTNAME  = 2
      integer,parameter :: IOT_KEY_FILENAME  = 3
      integer,parameter :: IOT_KEY_ACCESS_STYLE = 4
      integer,parameter :: IOT_KEY_STRIPING_FACTOR = 5
      integer,parameter :: IOT_KEY_STRIPING_UNIT = 6
      integer,parameter :: IOT_KEY_NON_BLOCKING = 7
      integer,parameter :: IOT_KEY_COLLECTIVE = 8
      integer,parameter :: IOT_KEY_NUMFILESIZE = 9
      integer,parameter :: IOT_KEY_FILESIZE = 10
      integer,parameter :: IOT_KEY_NUMBLOCKSIZE = 11
      integer,parameter :: IOT_KEY_BLOCKSIZE = 12
      integer,parameter :: IOT_KEY_PREALLOCATE  = 13
      integer,parameter :: IOT_KEY_TIMINGSFILENAME  = 14
      integer,parameter :: IOT_KEY_NUMSIZES  = 15
      integer,parameter :: IOT_KEY_XSIZE  = 16
      integer,parameter :: IOT_KEY_YSIZE  = 17
      integer,parameter :: IOT_KEY_ZSIZE  = 18
      integer,parameter :: IOT_KEY_NUMPROCGRIDS  = 19
      integer,parameter :: IOT_KEY_XPROC  = 20
      integer,parameter :: IOT_KEY_YPROC  = 21
      integer,parameter :: IOT_KEY_ZPROC  = 22
      integer,parameter :: IOT_KEY_REPEAT  = 23
      integer,parameter :: IOT_KEY_TASKLOOPS  = 24
      integer,parameter :: IOT_KEY_NUMUPDATE = 25
      integer,parameter :: IOT_KEY_UPDATE = 26
      integer,parameter :: IOT_KEY_BLOCKMIN = 27
      integer,parameter :: IOT_KEY_BLOCKMAX = 28
      integer,parameter :: IOT_KEY_NUMRUNS = 29
      integer,parameter :: IOT_KEY_OUTERLOOP = 30
      integer,parameter :: IOT_KEY_INNERLOOP = 31
      integer,parameter :: IOT_KEY_DELETE = 32
      integer,parameter :: IOT_KEY_DEBUG = 33

!     keyword strings
      character(len=*),parameter,dimension(IOT_NUM_KEYS) :: iokey = (/ &
       "classname           ", &
       "testname            ", &
       "filename            ", &
       "access_style        ", &
       "striping_factor     ", &
       "striping_unit       ", &
       "non_blocking        ", &
       "collective          ", &
       "numfilesize         ", &
       "filesize            ", &
       "numblocksize        ", &
       "blocksize           ", &
       "preallocate         ", &
       "timingsfilename     ", &
       "numsizes            ", &
       "xsize               ", &
       "ysize               ", &
       "zsize               ", &
       "numprocgrids        ", &
       "xproc               ", &
       "yproc               ", &
       "zproc               ", &
       "repeat              ", &
       "taskloops           ", &
       "numupdate           ", &
       "update_frac         ", &
       "blockmin            ", &
       "blockmax            ", &
       "numruns             ", &
       "outerloop           ", &
       "innerloop           ", &
       "delete              ", &
       "debug               " &
       /)

!     test program identifiers
      integer,parameter :: IOT_NUM_TESTS = 11
      integer,parameter :: IOT_TEST_SINGLE = 1
      integer,parameter :: IOT_TEST_MULTIPLE = 2
      integer,parameter :: IOT_TEST_SINGLEI = 3
      integer,parameter :: IOT_TEST_MULTIPLEI = 4
      integer,parameter :: IOT_TEST_MATRIX2D = 5
      integer,parameter :: IOT_TEST_MATRIX3D = 6
      integer,parameter :: IOT_TEST_NONSEQ = 7
      integer,parameter :: IOT_TEST_SHAREDFP = 8
      integer,parameter :: IOT_TEST_GATHERSCAT2D = 9
      integer,parameter :: IOT_TEST_TRANSPOSE = 10
      integer,parameter :: IOT_TEST_SINUNIX = 11

!     test program strings
      character(len=*),parameter,dimension(IOT_NUM_TESTS) :: &
        iotestname = (/ &
       "single       ", &
       "multiple     ", &
       "singleI      ", &
       "multipleI    ", &
       "matrix2D     ", &
       "matrix3D     ", &
       "nonseq       ", &
       "sharedfp     ", &
       "gatherscat2D ", &
       "transpose    ", &
       "sinunix      " &
       /)

!     shared output statistic keywords
      integer,parameter :: IOT_NUM_SHAREDKEYS = 6
      integer,parameter :: IOT_STAT_PROCNAME = 1 
      integer,parameter :: IOT_STAT_START = 2
      integer,parameter :: IOT_STAT_FINISH = 3
      integer,parameter :: IOT_STAT_WTICK = 4
      integer,parameter :: IOT_STAT_NPROCS = 5
      integer,parameter :: IOT_STAT_TIMESTAMP = 6

!     shared keyword strings 
      character(len=*),parameter,dimension(IOT_NUM_SHAREDKEYS) :: &
        sharedkey = (/ &
       "nodename  ", &
       "start     ", &
       "finish    ", &
       "ampi_wtick", &
       "nprocs    ", &
       "timestamp " &
       /)

!     error values
      integer,parameter :: IOT_SUCCESS = 0
      integer,parameter :: IOT_ERR_TEST = 1
      integer,parameter :: IOT_ERR_ALLOC = 2
      integer,parameter :: IOT_ERR_EOF = 3
      integer,parameter :: IOT_ERR_KEY = 4
      integer,parameter :: IOT_ERR_MISSKEY = 5
      integer,parameter :: IOT_ERR_MPI = 6

!     debug and delete variables for developers' use
      logical :: debug = .false.
      logical :: delete = .true.

      end module iot_defs




