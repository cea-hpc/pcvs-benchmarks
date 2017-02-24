!     $Id: multiple.f,v 1.8 1998/01/21 18:11:14 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     multiple
!     Release 1.0
!
!     Description:
!     multiple IO test subroutines
!
!     Interface:
!     call iot_wrap_multiple(comm, node, nprocs, datafilename, info,
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
!     iot_run_multiple()  :
!        Subroutine to perform multiple test run.
!        Parameters specific to test.
!     iot_check_multiple() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_multiple() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_multiple

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

      end module iostat_multiple


!-----------------------------------------------------------------------
!     iot_run_multiple
!
!     individual test code
!        The parameter list varies depending on the individual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_multiple.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!
!    Low-level write test.     
!    Multiple individual file, MPI_COMM_SELF   
!    write_explicit.                         
!    Run on only one processor.              
!-----------------------------------------------------------------------

      subroutine iot_run_multiple(comm, node, nprocs, &
           datafilename, info, &
           datasz, blocksz, palloc, collective, ierr)
      use iotest
      use iostat_multiple
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
      integer(kind=MPI_OFFSET_KIND) offset_niter, offset_proc, offset      
      integer(kind=MPI_OFFSET_KIND), parameter :: offset_zero = 0
 
!    timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tinit
      real(kind=DBL), dimension(:), allocatable :: writetime, readtime

!    test variables
      integer(kind=IDBL) :: datasz, blocksz
      logical :: palloc, collective
      integer :: bsize, niter
      real(kind=DBL), dimension(:), allocatable :: wbuf, rbuf
      integer :: j
      real(kind=DBL) :: sumcheck
      integer :: ierra, ierrb, ierrc, ierrd



!    time
      tinit = MPI_Wtime()
      told =tinit

!    change from bytes to etype units
      call mpi_type_size(etype, etypesize, ierr)
      bsize = blocksz/etypesize
      niter = datasz/blocksz/nprocs
      call save_stat(IOT_STAT_NITER, real(niter,DBL), IOT_INTEGER, ierr)
      offset_proc = node*bsize

      allocate (writetime(niter),stat=ierra)
      allocate (readtime(niter),stat=ierrb)
      allocate (wbuf(bsize),stat=ierrc)
      allocate (rbuf(bsize),stat=ierrd)
      if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0) &
        .or.(ierrd /= 0)) then
         ierr = IOT_ERR_ALLOC
         write(fout,*) 'Error: multiple allocation.'
         call mpi_abort(comm, ierr, ierr)
         stop
      end if


!   open MPI-I/O data file
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
         deallocate (writetime, readtime, wbuf, rbuf)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FOPEN, tdiff, IOT_REAL, ierr)

!    preallocation
      if (palloc) then
         tnew = MPI_Wtime()
         tpre = tnew - told
         told = tnew
         call MPI_File_preallocate(datafilepointer, datasz, ierr)
         if (ierr /= MPI_SUCCESS) then
            call iotMPIerr(node, ierr, "Error preallocating")
            deallocate (writetime, readtime, wbuf, rbuf)
            return
         end if
         tnew = MPI_Wtime()
         tdiff = tnew - told
         call save_stat(IOT_STAT_PALLOC, tdiff, IOT_REAL, ierr)
         told = tnew - tpre
      endif

!    Fileview is the default
      tpre = MPI_Wtime()
      call MPI_File_Set_View(datafilepointer, offset_zero, &
           etype, etype, datarep, info, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr,  "Error in file_set_view")
         deallocate (writetime, readtime, wbuf, rbuf)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)

!    initialize random
      wbuf(1) = iotrand(IOT_RSEED)
      do j=1,bsize
         wbuf(j) = node + iotrand(IOT_RSEED)
      enddo
      offset_niter = offset_zero


      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)
      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()


!    collective or non-collective versions
      if (collective) then
         do j=1,niter
            offset = offset_niter + offset_proc
            
            call MPI_file_write_at_all(datafilepointer, offset, & 
              wbuf(1), bsize, etype, status, ierr)
            tnew = MPI_Wtime()
            writetime(j) = tnew - told
            told = tnew
            offset_niter = offset_niter + nprocs*bsize
         enddo
      else
         do j=1,niter
            offset = offset_niter + offset_proc
            
            call MPI_file_write_at(datafilepointer, offset, & 
              wbuf(1), bsize, etype, status, ierr)
            tnew = MPI_Wtime()
            writetime(j) = tnew - told
            told = tnew
            offset_niter = offset_niter + nprocs*bsize
         enddo
      endif

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error writing data")
         return
      end if

      call MPI_file_sync(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in MPI_file_sync")
         deallocate (writetime, readtime, wbuf, rbuf)
         return
      end if
      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_SYNC, tdiff, IOT_REAL, ierr)
!    May add something here to try and flush the cache 

      sumcheck = 0.0
      offset_niter = offset_zero
      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()

!    collective or non-collective versions
      if (collective) then
         do j=1,niter
            offset = offset_niter + offset_proc
            call MPI_file_read_at_all(datafilepointer, offset, & 
              rbuf(1), bsize, etype, status, ierr)
            tnew = MPI_Wtime()
            readtime(j) = tnew - told
            told = tnew
!       Touch data by checking.
            sumcheck = sumcheck + rbuf(bsize)
            offset_niter = offset_niter + nprocs*bsize
         enddo
      else
         do j=1,niter
            offset = offset_niter + offset_proc
            call MPI_file_read_at(datafilepointer, offset, & 
              rbuf(1), bsize, etype, status, ierr)
            tnew = MPI_Wtime()
            readtime(j) = tnew - told
            told = tnew
!       Touch data by checking.
            sumcheck = sumcheck + rbuf(bsize)
            offset_niter = offset_niter + nprocs*bsize
         enddo
      endif

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error reading data")
         deallocate (writetime, readtime, wbuf, rbuf)
         return
      end if

      tpre = MPI_Wtime()
      call MPI_file_close(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error closing data file")
         deallocate (writetime, readtime, wbuf, rbuf)
         return
      end if
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

      end subroutine iot_run_multiple


!-----------------------------------------------------------------------
!     iot_wrap_multiple
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_multiple.
!        This subroutine calls iot_check_multiple and iot_test_multiple
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_multiple(comm, node, nprocs, &
          datafilename, info, statfile, ierr)
      use iotest
      use iostat_multiple
      implicit none

      integer comm, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename
!     n: data file size; b: block size; In bytes.
      integer :: numruns
      integer(kind=IDBL) :: n, b
      integer :: k
      integer,parameter :: MAX_NUM_PARAMS = 100
      integer(kind=IDBL),dimension(MAX_NUM_PARAMS) :: filesz, blocksz
      logical :: palloc, collective
      integer :: statsize



      call time_stamp(node, statfile, ierr)

      call iot_check_multiple(nprocs, numruns, filesz, &
        blocksz, palloc, collective, ierr)
      if (ierr /= IOT_SUCCESS) return

!    write input keys to file      
      call write_keys(comm, node, nprocs, statfile, &
        datafilename, info, statkey, ierr)

!    repeat over filesize,blocksize pairs
      do k = 1, numruns
         n = filesz(k)
         b = blocksz(k)

!       maximum number of statistics to record
         statsize = 2*n/b + 40
         call alloc_stat(statsize, ierr)
         if (ierr /= IOT_SUCCESS) then
            write(fout,*) "ERROR: failed to allocate memory for stats"
            call mpi_abort(comm, ierr, ierr)
         end if

!       save input parameters
         call save_stat(IOT_STAT_FILESIZE,  &
           real(n,DBL), IOT_INTEGER, ierr)
         call save_stat(IOT_STAT_BLOCKSIZE, &
           real(b,DBL), IOT_INTEGER, ierr)

!       multiple low-level test
         call iot_run_multiple(comm, node, nprocs, datafilename, &
            info, n, b, palloc, collective, ierr)
         if (ierr /= IOT_SUCCESS) then
            write (fout,*) "ERROR: node ",node," Error in multiple test"
         end if

!       write statistics information to file
         call write_stat(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)
                  
         if (debug) then
            call flush(IOT_STATFILE_UNIT)
         end if

!       free storage for timing information
         call free_stat(ierr)
                
      end do 

!    finalize/delete statistics structure
!      call finalize_stat(node, statfile, ierr)
!       write time stamp
      call time_stamp(node, statfile, ierr)

      end subroutine iot_wrap_multiple

!-----------------------------------------------------------------------
!     iot_check_multiple
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_multiple.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!
!     Return filesize and blocksize in byte units.
!
!-----------------------------------------------------------------------



      subroutine iot_check_multiple(nprocs, numruns, &
        filesz, blocksz, palloc, collective, ierr)
      use iotest
      implicit none

      integer :: nprocs, numruns, ierr
      integer(kind=IDBL),dimension(*) :: filesz, blocksz
      real(kind=DBL),dimension(:),allocatable :: num
      logical :: palloc, collective
      integer :: i, err

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
               if (filesz(i) < 8*nprocs) then
!                 filesize < size of one double per proc
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


!    collective (optional)
      collective = .false.
      if (len(ios(IOT_KEY_COLLECTIVE)) /= 0) then
         read (ios(IOT_KEY_COLLECTIVE)%s, *) collective
      endif


!    preallocation (optional)
      palloc = .true.
      if (len(ios(IOT_KEY_PREALLOCATE)) /= 0) then
         read (ios(IOT_KEY_PREALLOCATE)%s, *) palloc
      endif

 !    delete (optional)
      if (len(ios(IOT_KEY_DELETE)) /= 0) then
         read (unit=ios(IOT_KEY_DELETE)%s, fmt=*, iostat=err) delete
         if (err /= 0) return
      else
         delete = .true.
      end if

 !    debug (optional)
      if (len(ios(IOT_KEY_DEBUG)) /= 0) then
         read (unit=ios(IOT_KEY_DEBUG)%s, fmt=*, iostat=err) debug
         if (err /= 0) return
      else
         debug = .false.
      end if

      end subroutine iot_check_multiple









