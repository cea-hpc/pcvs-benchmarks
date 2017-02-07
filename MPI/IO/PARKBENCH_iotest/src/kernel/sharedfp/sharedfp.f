!     $Id: sharedfp.f,v 1.9 1998/02/16 16:42:00 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     sharedfp
!     Release 1.0
!
!     Description:
!     sharedfp IO test subroutines
!
!     Interface:
!     call iot_wrap_sharedfp(comm, node, nprocs, datafilename, info,
!                          statfile, ierr)
!
!     Arguments:
!     integer :: comm, node, nprocs, info, statfile, ierr
!     character(len=IOT_MAX_STRING_LEN) :: datafilename
!
!     External components:
!     
!     Detail of test:
!
!-----------------------------------------------------------------------
!     individual test subroutines:
!
!     iot_run_sharedfp()  :
!        Subroutine to perform sharedfp test run.
!        Parameters specific to test.
!     iot_check_sharedfp() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_sharedfp() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_sharedfp

!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 15
      integer,parameter :: IOT_STAT_FILESIZE = 1 
      integer,parameter :: IOT_STAT_BLOCKMIN = 2
      integer,parameter :: IOT_STAT_BLOCKMAX = 3
      integer,parameter :: IOT_STAT_PALLOC = 4 
      integer,parameter :: IOT_STAT_PRE = 5
      integer,parameter :: IOT_STAT_POST = 6 
      integer,parameter :: IOT_STAT_FOPEN = 7
      integer,parameter :: IOT_STAT_FCLOSE = 8
      integer,parameter :: IOT_STAT_FVIEW = 9
      integer,parameter :: IOT_STAT_NITER = 10 
      integer,parameter :: IOT_STAT_BSUM = 11
      integer,parameter :: IOT_STAT_W = 12
      integer,parameter :: IOT_STAT_B = 13
      integer,parameter :: IOT_ERR = 14
      integer,parameter :: IOT_STAT_TOTAL = 15

!     keyword strings 
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize     ", &
       "blockmin     ", &
       "blockmax     ", &
       "palloc_time  ", &
       "pre_time     ", &
       "post_time    ", &
       "fopen_time   ", &
       "fclose_time  ", &
       "fview_time   ", &
       "niter        ", &
       "bsum         ", &
       "w            ", &
       "b            ", &
       "error        ", &
       "total_time   " &
       /)

      end module iostat_sharedfp


!-----------------------------------------------------------------------
!     iot_run_sharedfp
!
!     individual test code
!        The parameter list varies depending on the individual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_sharedfp.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!
!    Low-level write test.     
!    Sharedfp individual file, MPI_COMM_SELF   
!    write_explicit.                         
!    Run on only one processor.              
!-----------------------------------------------------------------------

      subroutine iot_run_sharedfp(comm, node, nprocs, &
              datafilename, info, filesize, &
              blockmin, blockmax, palloc, collective, ierr)

      use iotest
      use iostat_sharedfp
      implicit none

!     include "VT.inc" 

!    standard MPI junk
      integer :: comm, node, nprocs, info, ierr
      character(len=*) :: datafilename
      character(len=*),parameter :: datarep = "native"
      integer, parameter :: etype = MPI_DOUBLE_PRECISION
      integer :: etypesize
      integer, dimension(MPI_STATUS_SIZE) :: status
      integer datafilepointer, dmode
      integer(kind=MPI_OFFSET_KIND), parameter :: offset_zero = 0
 
!    timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tinit
      real(kind=DBL), dimension(:), allocatable :: writetime

!    stats variable
      real(kind=DBL), dimension(:), allocatable :: bsize 

!    test variables
      integer(kind=IDBL) :: filesize, filedsize
      integer(kind=IDBL) :: blockmin, blockmax, bdminsize, bdmaxsize
      integer(kind=IDBL) :: bddiffsize, bdsize, bsum
      logical :: palloc, collective
      real(kind=DBL), dimension(:), allocatable :: wbuf
      integer :: j, niter, bisize
      real(kind=DBL) :: rand




!    time
      tinit = MPI_Wtime()
      told = tinit

!    change from bytes to etype units
      call mpi_type_size(etype, etypesize, ierr)
      bdmaxsize = blockmax/etypesize
      bdminsize = blockmin/etypesize
      bddiffsize = bdmaxsize - bdminsize
      filedsize = filesize/etypesize
      niter = 2*filesize/(blockmin+blockmax)/nprocs
      if (bdminsize == 0) then
         bdminsize = 1
      endif

      allocate (writetime(niter))
      allocate (bsize(niter))
      allocate (wbuf(blockmax))



!   open MPI-I/O data file
      call MPI_file_delete(datafilename, MPI_INFO_NULL, ierr)
      call mpi_barrier(comm, ierr)
      dmode = MPI_MODE_CREATE + MPI_MODE_RDWR
      tpre = MPI_Wtime()
      call MPI_file_open(comm, datafilename, dmode, &
                   info, datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call prtMPIerr(ierr)
         write (fout,*) "Error opening data file for writing", &
           datafilename, "node (MPI) ",node
         ierr = IOT_ERR_MPI
         deallocate (writetime,wbuf,bsize)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FOPEN, tdiff, IOT_REAL, ierr)

!    preallocation
      if (palloc) then
         tnew = MPI_Wtime()
         tpre = tnew - told
         told = tnew
         call MPI_File_preallocate(datafilepointer, filesize, ierr)
         if (ierr /= MPI_SUCCESS) then
            call iotMPIerr(node, ierr, "Error preallocating")
            deallocate (writetime,wbuf,bsize)
            return
         end if
         tnew = MPI_Wtime()
         tdiff = tnew - told
         call save_stat(IOT_STAT_PALLOC, tdiff, IOT_REAL, ierr)
         told = tnew - tpre
      endif

!    Default Fileview
      tpre = MPI_Wtime()
      call MPI_File_Set_View(datafilepointer, offset_zero, &
           etype, etype, datarep, info, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr,  "Error in file_set_view")
         deallocate (writetime,wbuf,bsize)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)

!    Independent random seeds for each process**
      rand = iotrand(-(node + 1)*IOT_RSEED)

!    Create the buffer 
      do j=1,bdmaxsize
         wbuf(j) = node + iotrand(IOT_RSEED)
      enddo

      
      call mpi_barrier(comm, ierr)

!    collective or non-collective versions
      bsum = 0
      if (collective) then
         do j = 1 , niter
            bdsize = bdminsize + int((bddiffsize)*iotrand(IOT_RSEED),IDBL) 
            bsize(j) = bdsize
            bisize = int(bdsize,4)
            bsum = bsum + bdsize
            call MPI_file_write_ordered(datafilepointer, & 
              wbuf(1), bisize, etype, status, ierr)
            tnew = MPI_Wtime()
            writetime(j) = tnew - told
            told = tnew
         enddo
      else
         do j = 1 , niter
            bdsize = bdminsize + int((bddiffsize)*iotrand(IOT_RSEED),IDBL) 
            bsize(j) = bdsize
            bisize = int(bdsize,4)
            bsum = bsum + bdsize
            call MPI_file_write_shared(datafilepointer, & 
              wbuf(1), bisize, etype, status, ierr)
            tnew = MPI_Wtime()
            writetime(j) = tnew - told
            told = tnew
         enddo
      endif

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error writing data")
         return
      end if


      call save_stat(IOT_STAT_NITER, real(niter,DBL), IOT_INTEGER, ierr)
      bsum = bsum * etypesize
      call save_stat(IOT_STAT_BSUM, real(bsum,DBL), IOT_INTEGER, ierr)


      tpre = MPI_Wtime()
      call MPI_file_close(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error closing data file")
         deallocate (writetime,wbuf,bsize)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FCLOSE, tdiff, IOT_REAL, ierr)


      do j=1,niter
         call save_stat(IOT_STAT_B, real(bsize(j) * etypesize,DBL), &
           IOT_INTEGER, ierr)
         call save_stat(IOT_STAT_W, writetime(j), IOT_REAL, ierr)
      enddo


      deallocate (writetime)
      deallocate (bsize)
      deallocate (wbuf)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_POST, tdiff, IOT_REAL, ierr)
      tdiff = tnew - tinit
      call save_stat(IOT_STAT_TOTAL, tdiff, IOT_REAL, ierr)

      end subroutine iot_run_sharedfp


!-----------------------------------------------------------------------
!     iot_wrap_sharedfp
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_kernel.
!        This subroutine calls iot_check_sharedfp and iot_test_sharedfp
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_sharedfp(comm, node, nprocs, &
          datafilename, info, statfile, ierr)
      use iotest
      use iostat_sharedfp
      implicit none

      integer comm, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename
      integer :: num_blocksizes
      integer(kind=IDBL) :: filesize, blmin, blmax
      integer :: i, j, k
      integer,parameter :: MAX_NUM_PARAMS = 50
      integer(kind=IDBL),dimension(MAX_NUM_PARAMS) :: blockmin, blockmax
      logical :: palloc, collective
      integer :: statsize




      call time_stamp(node, statfile, ierr)

      call iot_check_sharedfp(filesize, num_blocksizes, blockmin, &
        blockmax, palloc, collective, ierr)
      if (ierr /= IOT_SUCCESS) return

!    write input keys to file      
      call write_keys(comm, node, nprocs, statfile, &
        datafilename, info, statkey, ierr)

!    repeat over block sizes
      do k = 1, num_blocksizes
         blmin = blockmin(k)
         blmax = blockmax(k)

!       allocate storage for timing information
!       maximum number of statistics to record
!            -- need a margin for error
         statsize = 8*filesize/(blmin + blmax) + 40
         call alloc_stat(statsize, ierr)
         if (ierr /= IOT_SUCCESS) then
            write(fout,*) "ERROR: failed to allocate memory for stats"
            call mpi_abort(comm, ierr, ierr)
         end if


!       save input parameters
         call save_stat(IOT_STAT_FILESIZE,  &
           real(filesize,DBL), IOT_INTEGER, ierr)
         call save_stat(IOT_STAT_BLOCKMIN, &
           real(blmin,DBL), IOT_INTEGER, ierr)
         call save_stat(IOT_STAT_BLOCKMAX, &
           real(blmax,DBL), IOT_INTEGER, ierr)

!       sharedfp kernel test
         call iot_run_sharedfp(comm, node, nprocs, datafilename, &
           info, filesize, blmin, blmax, palloc, collective, ierr)
         if (ierr /= IOT_SUCCESS) then
            write (fout,*) "ERROR: node ",node," Error in sharedfp test"
         end if

!       write statistics information to file
         call write_stat(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)

!       free storage for timing information
         call free_stat(ierr)
                 
      end do                    ! block size loop


      call time_stamp(node, statfile, ierr)


      end subroutine iot_wrap_sharedfp

!-----------------------------------------------------------------------
!     iot_check_sharedfp
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_sharedfp.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!
!     Return filesize and blocksize in byte units.
!
!-----------------------------------------------------------------------

      subroutine iot_check_sharedfp(filesize, num_blocksizes, blockmin, &
        blockmax, palloc, collective, ierr)
      use iotest
      implicit none


      integer :: num_blocksizes,ierr
      real(kind=DBL) ::  realfilesize
      integer(kind=IDBL) ::  filesize
      integer(kind=IDBL),dimension(*) ::  blockmin, blockmax
      real(kind=DBL),dimension(:),allocatable :: num
      logical :: palloc, collective
      integer :: i, err


      ierr = IOT_SUCCESS


!    filesize (required)
      if (len(ios(IOT_KEY_FILESIZE)) /= 0) then
         read (ios(IOT_KEY_FILESIZE)%s, *) realfilesize
         filesize = realfilesize*IOT_FILESIZE_UNIT
         if (filesize < 8) then
!           filesize < size of one double
            write(fout,*) 'Error: filesize too small.'
         endif
      else
         write(fout,*) 'Error: missing filesize value.'
         ierr = IOT_ERR_MISSKEY
         return
      endif


!    blocksizes (required)

      if (len(ios(IOT_KEY_NUMBLOCKSIZE)) /= 0) then
      read (ios(IOT_KEY_NUMBLOCKSIZE)%s, *) num_blocksizes
         if (len(ios(IOT_KEY_BLOCKMIN)) /= 0) then
            allocate(num(num_blocksizes))
            read (ios(IOT_KEY_BLOCKMIN)%s, *) (num(i),i=1,num_blocksizes)
            do i = 1, num_blocksizes
               blockmin(i) = num(i)*IOT_BLOCKSIZE_UNIT
               if (blockmin(i) < 0) then
!             blockmin < 0
                  write(fout,*) 'Error: blockmin too small.'
               endif
               if (blockmin(i) > filesize) then
!             blockmin > filesize
                  write(fout,*) 'Error: blockmin larger than', & 
                  ' filesize.'
               endif
            end do
            deallocate(num)
         else
            write(fout,*) 'Error: missing blockmin values.'
            ierr = IOT_ERR_MISSKEY
            return
         endif
         if (len(ios(IOT_KEY_BLOCKMAX)) /= 0) then
            allocate(num(num_blocksizes))
            read (ios(IOT_KEY_BLOCKMAX)%s, *) (num(i),i=1,num_blocksizes)
            do i = 1, num_blocksizes
               blockmax(i) = num(i)*IOT_BLOCKSIZE_UNIT
               if (blockmax(i) < 8) then
!             blockmax < size of one double
                  write(fout,*) 'Error: blockmax too small.'
               endif
               if (blockmax(i) > filesize) then
!             blockmax > filesize
                  write(fout,*) 'Error: blockmax larger than', & 
                  ' filesize.'
               endif
               if (blockmax(i) < blockmin(i)) then
!             blockmax < blockmin 
                  write(fout,*) 'Error: blockmax smaller than', & 
                  ' blockmin.'
               endif
            end do
            deallocate(num)
         else
            write(fout,*) 'Error: missing blockmax values.'
            ierr = IOT_ERR_MISSKEY
            return
         endif
      else
         write(fout,*) 'Error: missing numblocksizes value.'
         ierr = IOT_ERR_MISSKEY
         return
      endif


!    preallocation (optional)
      palloc = .true.
      if (len(ios(IOT_KEY_PREALLOCATE)) /= 0) then
         read (ios(IOT_KEY_PREALLOCATE)%s, *) palloc
      endif

      if (len(ios(IOT_KEY_DEBUG)) /= 0) then
         read (unit=ios(IOT_KEY_DEBUG)%s, fmt=*, iostat=err) debug
         if (err /= 0) return
      else
         debug = .false.
      end if

      if (len(ios(IOT_KEY_COLLECTIVE)) /= 0) then
         read (unit=ios(IOT_KEY_COLLECTIVE)%s, fmt=*, iostat=err) collective
         if (err /= 0) return
      else
      collective = .false.
      endif

      end subroutine iot_check_sharedfp









