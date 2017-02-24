!     $Id: sinunix.f,v 1.1 1998/01/21 18:06:37 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     sinunix
!     Release 1.0
!
!     Description:
!     sinunix IO test subroutines
!
!     Interface:
!     call iot_wrap_sinunix(comm, node, nprocs, datafilename, info,
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
!     iot_run_sinunix()  :
!        Subroutine to perform sinunix test run.
!        Parameters specific to test.
!     iot_wrap_sinunix() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!     iot_check_sinunix() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_sinunix
!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 15
      integer,parameter :: IOT_STAT_FILESIZE = 1 
      integer,parameter :: IOT_STAT_BLOCKSIZE = 2
      integer,parameter :: IOT_STAT_NITER = 3 
      integer,parameter :: IOT_STAT_PALLOC = 4
      integer,parameter :: IOT_STAT_PRE = 5
      integer,parameter :: IOT_STAT_POST = 6 
      integer,parameter :: IOT_STAT_SYNC = 7 
      integer,parameter :: IOT_STAT_FOPEN = 8
      integer,parameter :: IOT_STAT_FCLOSE = 9
      integer,parameter :: IOT_STAT_FVIEW = 10
      integer,parameter :: IOT_STAT_W = 11
      integer,parameter :: IOT_STAT_R = 12
      integer,parameter :: IOT_STAT_CHECK = 13
      integer,parameter :: IOT_ERR = 14
      integer,parameter :: IOT_STAT_TOTAL = 15

!     keyword strings 
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize   ", &
       "blocksize  ", &
       "niter      ", &
       "palloc_time", &
       "pre_time   ", &
       "post_time  ", &
       "sync_time  ", &
       "fopen_time ", &
       "fclose_time", &
       "fview_time ", &
       "w          ", &
       "r          ", &
       "sumcheck   ", &
       "error      ", &
       "total_time " &
       /)

      end module iostat_sinunix


!-----------------------------------------------------------------------
!     iot_run_sinunix
!
!     individual test code
!        The parameter list varies depending on the individual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_sinunix.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!
!    Low-level write test.     
!    Sinunix individual file, MPI_COMM_SELF   
!    write_explicit.                         
!    Run on only one processor.              
!-----------------------------------------------------------------------

      subroutine iot_run_sinunix(comm, node, nprocs, &
           datafilename, info, &
           datasz, blocksz, palloc, ierr)
      use iotest
      use iostat_sinunix
      implicit none

!     include "VT.inc" 

!    standard MPI junk
      integer :: comm, node, nprocs, info, ierr
      character(len=*) :: datafilename
      integer, parameter :: etype = MPI_DOUBLE_PRECISION
      integer :: etypesize

!    timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tinit
      real(kind=DBL), dimension(:), allocatable :: writetime, readtime

!    test variables
      integer(kind=IDBL) :: datasz, blocksz
      logical :: palloc
      integer :: bsize, niter
      real(kind=DBL), dimension(:), allocatable :: wbuf, rbuf
      integer :: j
      real(kind=DBL) :: sumcheck
      integer :: ierra, ierrb, ierrc, ierrd




!    time
      tinit = MPI_Wtime()
      told = tinit

!    change from bytes to etype units
      call mpi_type_size(etype, etypesize, ierr)
      bsize = blocksz/etypesize
      niter = datasz/blocksz
      call save_stat(IOT_STAT_NITER, real(niter,8), IOT_INTEGER, ierr)


      allocate (writetime(niter),stat=ierra)
      allocate (readtime(niter),stat=ierrb)
      allocate (wbuf(bsize),stat=ierrc)
      allocate (rbuf(bsize),stat=ierrd)
      if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0) &
        .or.(ierrd /= 0)) then
         ierr = IOT_ERR_ALLOC
         write(fout,*) 'Error: sinunix allocation.'
         call mpi_abort(comm, ierr, ierr)
         stop
      end if




!    open Standard Fortran I/O data file
      tpre = MPI_Wtime()
      open(unit=IOT_DATAFILE_UNIT, file=datafilename, &
        status="replace", form="unformatted", iostat=ierr)
      if (ierr /= 0) then
         write (fout,*) "ERROR: Error opening data file ", &
         datafilename
         call mpi_abort(comm, ierr, ierr)
         deallocate (writetime, readtime, wbuf, rbuf)
         stop
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FOPEN, tdiff, IOT_REAL, ierr)


!    initialize randomly
      wbuf(1) = iotrand(IOT_RSEED)
      do j=1,bsize
         wbuf(j) = node + iotrand(IOT_RSEED)
      enddo


      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)


      do j=1,niter
         write(unit=IOT_DATAFILE_UNIT) wbuf(1:bsize)
         tnew = MPI_Wtime()
         writetime(j) = tnew - told
         told = tnew
      enddo

      close(unit=IOT_DATAFILE_UNIT)

      sumcheck = 0.0
      told = MPI_Wtime()



!    read from disk using standard Fortran I/O
      open(unit=IOT_DATAFILE_UNIT, file=datafilename, status="old", &
        form="unformatted", iostat=ierr)
        if (ierr /= 0) then
           write (fout,*) "ERROR: Error opening data file ", &
           datafilename
           call mpi_abort(comm, ierr, ierr)
           stop
        end if


      do j=1,niter
         read(unit=IOT_DATAFILE_UNIT) rbuf(1:bsize)
         tnew = MPI_Wtime()
         readtime(j) = tnew - told
         told = tnew
!       Check and touch data.
         sumcheck = sumcheck + rbuf(bsize)
      enddo


      tpre = MPI_Wtime()
      close(unit=IOT_DATAFILE_UNIT)
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FCLOSE, tdiff, IOT_REAL, ierr)

!    Checking
      sumcheck = sumcheck - niter*wbuf(bsize)
      call save_stat(IOT_STAT_CHECK, sumcheck, IOT_REAL, ierr)
!    Another check
      sumcheck = 0.0
      do j=1,bsize
         if (wbuf(j) - rbuf(j) > IOT_ACCURACY) then
            sumcheck = sumcheck + 1.0
         endif
      enddo
      call save_stat(IOT_STAT_CHECK, sumcheck, IOT_REAL, ierr)


      do j=1,niter
         call save_stat(IOT_STAT_W, writetime(j), IOT_REAL, ierr)
      enddo
      do j=1,niter
         call save_stat(IOT_STAT_R, readtime(j), IOT_REAL, ierr)
      enddo


      deallocate (writetime, readtime, wbuf, rbuf)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_POST, tdiff, IOT_REAL, ierr)
      tdiff = tnew - tinit
      call save_stat(IOT_STAT_TOTAL, tdiff, IOT_REAL, ierr)

!    Improve error handling
      ierr = IOT_SUCCESS 

      end subroutine iot_run_sinunix


!-----------------------------------------------------------------------
!     iot_wrap_sinunix
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_sinunix.
!        This subroutine calls iot_check_sinunix and iot_test_sinunix
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_sinunix(comm, node, nprocs, &
          datafilename, info, statfile, ierr)
      use iotest
      use iostat_sinunix
      implicit none

      integer comm, comm_self, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename
!     n: data file size; b: block size; In bytes.
      integer :: numruns
      integer(kind=IDBL) :: n, b
      integer :: k
      integer,parameter :: MAX_NUM_PARAMS = 50
      integer(kind=IDBL),dimension(MAX_NUM_PARAMS) :: filesz, blocksz
      logical :: palloc
      integer :: statsize



!    A separate communicator for this test  
      comm_self = MPI_COMM_SELF
!    Run sinunix test on node 0 
      if (node == 0) then
         call time_stamp(node, statfile, ierr)

         call iot_check_sinunix(numruns, filesz, blocksz, palloc, ierr)
         if (ierr /= IOT_SUCCESS)  return
         
!       write input keys to file      
         call write_keys(comm_self, node, 1, statfile, &
           datafilename, info, statkey, ierr)

!       repeat over number of runs (filesize,blocksize pairs)
         do k = 1, numruns
            n = filesz(k)
            b = blocksz(k)

!          maximum number of statistics to record
            statsize = 2*n/b + 40
            call alloc_stat(statsize, ierr)
            if (ierr /= IOT_SUCCESS) then
               write(fout,*) "ERROR: failed to allocate memory for stats"
               call mpi_abort(comm, ierr, ierr)
            end if

!          save input parameters
            call save_stat(IOT_STAT_FILESIZE,  &
              real(n,DBL), IOT_INTEGER, ierr)
            call save_stat(IOT_STAT_BLOCKSIZE, &
              real(b,DBL), IOT_INTEGER, ierr)
                  
!          sinunix low-level test
            call iot_run_sinunix(comm_self, node, 1, &
              datafilename, info, n, b, palloc, ierr)
            if (ierr /= IOT_SUCCESS) then
               write (fout,*) "ERROR: Error within sinunix test"
            end if

!          write statistics information to file
            call write_stat(comm_self, node, 1, statfile, &
              datafilename, info, statkey, ierr)
                 
            if (debug) then
               call flush(IOT_STATFILE_UNIT)
            end if

!          free storage for timing information
            call free_stat(ierr)
         end do                 
         
         call time_stamp(node, statfile, ierr)
      else
!       success for all other nodes
         ierr = IOT_SUCCESS
      end if

      end subroutine iot_wrap_sinunix 

!-----------------------------------------------------------------------
!     iot_check_sinunix
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_sinunix.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!
!     Return filesize and blocksize in byte units.
!
!-----------------------------------------------------------------------

      subroutine iot_check_sinunix(numruns, filesz, blocksz, palloc, &
        ierr)
      use iotest
      implicit none

      integer :: numruns, ierr
      integer(kind=IDBL),dimension(*) :: filesz, blocksz
      real(kind=DBL),dimension(:),allocatable :: num
      logical :: palloc
      integer :: i,err

      ierr = IOT_SUCCESS


!    filesize/blocksize (required)
      if (len(ios(IOT_KEY_NUMRUNS)) /= 0) then
         read (ios(IOT_KEY_NUMRUNS)%s, *) numruns         
         if ( (len(ios(IOT_KEY_FILESIZE)) /= 0).and. &
           (len(ios(IOT_KEY_BLOCKSIZE)) /= 0) ) then
            allocate(num(numruns))
            read (ios(IOT_KEY_FILESIZE)%s, *) (num(i),i=1,numruns)
            do i = 1, numruns
               filesz(i) = num(i)*IOT_FILESIZE_UNIT
               if (filesz(i) < 8) then
!                 filesize < size of one double
                  write(fout,*) 'Error: filesize too small.'
               endif
            end do
            read (ios(IOT_KEY_BLOCKSIZE)%s, *) (num(i),i=1,numruns)
            do i = 1, numruns
               blocksz(i) = num(i)*IOT_BLOCKSIZE_UNIT
               if (blocksz(i) < 8) then
!             blocksize < size of one double
                  write(fout,*) 'Error: blocksize too small.'
               endif
               if (blocksz(i) > filesz(i)) then
!             blocksize > filesize
                  write(fout,*) 'Error: blocksize larger than', & 
                  ' filesize.'
               endif
            end do
            deallocate(num)
         else
            write(fout,*) 'Error: missing filesize/blocksize values.'
            ierr = IOT_ERR_MISSKEY
            return
         endif
      else
         write(fout,*) 'Error: missing numruns value.'
         ierr = IOT_ERR_MISSKEY
         return
      endif


!    preallocation (optional)
      palloc = .true.
      if (len(ios(IOT_KEY_PREALLOCATE)) /= 0) then
         read (ios(IOT_KEY_PREALLOCATE)%s, *) palloc
      endif

!    debug (optional)
      if (len(ios(IOT_KEY_DEBUG)) /= 0) then
         read (unit=ios(IOT_KEY_DEBUG)%s, fmt=*, iostat=err) debug
         if (err /= 0) return
      else
         debug = .false.
      end if

      end subroutine iot_check_sinunix





