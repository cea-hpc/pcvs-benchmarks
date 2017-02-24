!     $Id: multipleI.f,v 1.10 1998/02/10 15:41:56 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     multipleI
!     Release 1.0
!
!     Description:
!     multiple IO test subroutines
!
!     Interface:
!     call iot_wrap_multipleI(comm, node, nprocs, datafilename, info,
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
!     iot_run_multipleI()  :
!        Subroutine to perform multipleI test run.
!        Parameters specific to test.
!     iot_check_multipleI() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_multipleI() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_multipleI

!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 18
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
      integer,parameter :: IOT_STAT_TASK = 11
      integer,parameter :: IOT_STAT_IW = 12
      integer,parameter :: IOT_STAT_IR = 13
      integer,parameter :: IOT_STAT_IWTASK = 14
      integer,parameter :: IOT_STAT_IRTASK = 15
      integer,parameter :: IOT_STAT_CHECK = 16
      integer,parameter :: IOT_ERR = 17
      integer,parameter :: IOT_STAT_TOTAL = 18

!     keyword strings 
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize    ", &
       "blocksize   ", &
       "niter       ", &
       "palloc_time ", &
       "pre_time    ", &
       "post_time   ", &
       "sync_time   ", &
       "fopen_time  ", &
       "fclose_time ", &
       "fview_time  ", &
       "t           ", &
       "wI          ", &
       "rI          ", &
       "wtI         ", &
       "rtI         ", &
       "sumcheck    ", &
       "error       ", &
       "total_time  " &
       /)

      end module iostat_multipleI


!-----------------------------------------------------------------------
!     iot_run_multipleI
!
!     individual test code
!        The parameter list varies depending on the individual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_multipleI.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!
!    Low-level write test.     
!    MultipleI individual file, MPI_COMM_SELF   
!    write_explicit.                         
!    Run on only one processor.              
!-----------------------------------------------------------------------


     subroutine iot_run_multipleI(comm, node, nprocs, &
           datafilename, info, &
           datasz, blocksz, collective, taskloops, repeat,&
           outerloop, innerloop, palloc, ierr)
      use iotest
      use iostat_multipleI
      use matrix_util
      implicit none

!     include "VT.inc" 

!    standard MPI junk
      integer :: comm, node, nprocs, info, ierr
      character(len=*) :: datafilename
      character(len=*),parameter :: datarep = "native"
      integer, parameter :: etype = MPI_DOUBLE_PRECISION
      integer :: etypesize
      integer :: request
      integer, dimension(MPI_STATUS_SIZE) :: status
      integer datafilepointer, dmode
      integer(kind=MPI_OFFSET_KIND) offset_niter, offset_proc, offset      
      integer(kind=MPI_OFFSET_KIND), parameter :: offset_zero = 0
 
!    timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tcomp, tinit
      real(kind=DBL), dimension(:), allocatable :: writetime, readtime
      real(kind=DBL), dimension(:), allocatable :: wtasktime, rtasktime

!    test variables
      integer(kind=IDBL) :: datasz, blocksz
      integer :: repeat, taskloops, outerloop, innerloop
      logical :: palloc, collective
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
      niter = datasz/blocksz/nprocs
      call save_stat(IOT_STAT_NITER, real(niter,DBL), IOT_INTEGER, ierr)
      offset_proc = node*bsize

      allocate (writetime(niter),stat=ierra)
      allocate (readtime(niter),stat=ierrb)
      allocate (wtasktime(niter),stat=ierrc)
      allocate (rtasktime(niter),stat=ierrd)
      if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0) &
        .or.(ierrd /= 0)) then
         ierr = IOT_ERR_ALLOC
         write(fout,*) 'Error: multipleI allocation.'
         call mpi_abort(comm, ierr, ierr)
      end if
      allocate (wbuf(bsize),stat=ierra)
      allocate (rbuf(bsize),stat=ierrb)
      if ((ierra /= 0).or.(ierrb /= 0)) then
         ierr = IOT_ERR_ALLOC
         write(fout,*) 'Error: multipleI buffer allocation.'
         call mpi_abort(comm, ierr, ierr)
      end if


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
         deallocate (writetime, readtime, wtasktime, rtasktime, wbuf, rbuf)
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
            deallocate (writetime, readtime, wtasktime, rtasktime, wbuf, rbuf)
            return
         end if
         tnew = MPI_Wtime()
         tdiff = tnew - told
         call save_stat(IOT_STAT_PALLOC, tdiff, IOT_REAL, ierr)
         told = tnew - tpre
      endif

      tpre = MPI_Wtime()
      call MPI_File_Set_View(datafilepointer, offset_zero, &
           etype, etype, datarep, info, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr,  "Error in file_set_view")
         deallocate (writetime, readtime, wtasktime, rtasktime, wbuf, rbuf)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)


      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)


!------------------------------
!   Time the extra computation 
!     in isolation, as a check.
!------------------------------

!    initialise extra computation
      call comp_task(repeat,innerloop,outerloop,1)


      told = MPI_Wtime()
      do j=1, taskloops
!       the extra computation
         call comp_task(repeat,innerloop,outerloop,0)
         tnew = MPI_Wtime()
         wtasktime(j) = tnew - told
         told = tnew
      enddo
      do j=1, taskloops
         call save_stat(IOT_STAT_TASK, wtasktime(j), IOT_REAL, ierr)
      enddo

!    reinitialize random generator
      wbuf(1) = iotrand(-IOT_RSEED)
      do j=1,bsize
         wbuf(j) = node + iotrand(IOT_RSEED)
      enddo


!------------------------------
!   Now do the asynchronous part
!------------------------------

      offset_niter = offset_zero

!    reset the time
      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()

      if (collective) then
         do j=1,niter
            offset = offset_niter + offset_proc
            call MPI_file_write_at_all_begin(datafilepointer, offset, & 
              wbuf(1), bsize, etype, ierr)
            tpre = MPI_Wtime()
            call comp_task(repeat,innerloop,outerloop,0)
            tcomp = MPI_Wtime() - tpre
            wtasktime(j) = tcomp
            call MPI_file_write_at_all_end(datafilepointer, & 
              wbuf(1), status, ierr)
            tnew = MPI_Wtime()
            writetime(j) = tnew - told - tcomp
            told = tnew
            offset_niter = offset_niter + nprocs*bsize
         enddo
      else
         do j=1,niter
            offset = offset_niter + offset_proc
            call MPI_file_Iwrite_at(datafilepointer, offset, & 
              wbuf(1), bsize, etype, request, ierr)
            tpre = MPI_Wtime()
            call comp_task(repeat,innerloop,outerloop,0)
            tcomp = MPI_Wtime() - tpre
            wtasktime(j) = tcomp
            call MPI_Wait(request, status, ierr)
            tnew = MPI_Wtime()
            writetime(j) = tnew - told - tcomp
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
         deallocate (writetime, readtime, wtasktime, rtasktime, wbuf, rbuf)
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



      if (collective) then
         do j=1,niter
            offset = offset_niter + offset_proc
            call MPI_file_read_at_all_begin(datafilepointer, offset, & 
              rbuf(1), bsize, etype, ierr)
            tpre = MPI_Wtime()
            call comp_task(repeat,innerloop,outerloop,0)
            tcomp = MPI_Wtime() - tpre
            rtasktime(j) = tcomp 
            call MPI_file_read_at_all_end(datafilepointer, & 
              wbuf(1), status, ierr)
            tnew = MPI_Wtime()
            readtime(j) = tnew - told - tcomp
            told = tnew

!       Touch data by checking.
            sumcheck = sumcheck + rbuf(bsize) - wbuf(bsize)
            offset_niter = offset_niter + nprocs*bsize
         enddo
      else
         do j=1,niter
            offset = offset_niter + offset_proc
            call MPI_file_Iread_at(datafilepointer, offset, & 
              rbuf(1), bsize, etype, request, ierr)
            tpre = MPI_Wtime()
            call comp_task(repeat,innerloop,outerloop,0)
            tcomp = MPI_Wtime() - tpre
            rtasktime(j) = tcomp 
            call MPI_Wait(request, status, ierr)
            tnew = MPI_Wtime()
            readtime(j) = tnew - told - tcomp
            told = tnew

!       Touch data by checking.
            sumcheck = sumcheck + rbuf(bsize) - wbuf(bsize)
            offset_niter = offset_niter + nprocs*bsize
         enddo
      endif

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error reading data")
         deallocate (writetime, readtime, wtasktime, rtasktime, wbuf, rbuf)
         return
      end if


      tpre = MPI_Wtime()
      call MPI_file_close(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error closing data file")
         deallocate (writetime, readtime, wtasktime, rtasktime, wbuf, rbuf)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FCLOSE, tdiff, IOT_REAL, ierr)


!    Checking
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
         call save_stat(IOT_STAT_IWTASK, wtasktime(j), IOT_REAL, ierr)
      enddo
      do j=1,niter
         call save_stat(IOT_STAT_IW, writetime(j), IOT_REAL, ierr)
      enddo
      do j=1,niter
         call save_stat(IOT_STAT_IRTASK, rtasktime(j), IOT_REAL, ierr)
      enddo
      do j=1,niter
         call save_stat(IOT_STAT_IR, readtime(j), IOT_REAL, ierr)
      enddo


      deallocate (writetime, readtime, wtasktime, rtasktime, wbuf, rbuf)
!    deallocate task arrays
      call comp_task(repeat,innerloop,outerloop,2)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_POST, tdiff, IOT_REAL, ierr)
      tdiff = tnew - tinit
      call save_stat(IOT_STAT_TOTAL, tdiff, IOT_REAL, ierr)

      end subroutine iot_run_multipleI


!-----------------------------------------------------------------------
!     iot_wrap_multipleI
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_multipleI.
!        This subroutine calls iot_check_multipleI and iot_test_multipleI
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_multipleI(comm, node, nprocs, &
          datafilename, info, statfile, ierr)
      use iotest
      use iostat_multipleI
      implicit none

      integer comm, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename
!     n: data file size; b: block size; In bytes.
      integer :: numruns
      integer(kind=IDBL) :: n, b
      integer :: k
      integer,parameter :: MAX_NUM_PARAMS = 100
      integer(kind=IDBL),dimension(MAX_NUM_PARAMS) :: filesz, blocksz
      integer :: repeat, taskloops, outerloop, innerloop
      logical :: palloc, collective
      integer :: statsize



      call time_stamp(node, statfile, ierr)

      call iot_check_multipleI(nprocs, numruns, filesz, &
        blocksz, collective, taskloops, repeat, &
        outerloop, innerloop, palloc, ierr)
      if (ierr /= IOT_SUCCESS) return

!    write input keys to file      
      call write_keys(comm, node, nprocs, statfile, &
        datafilename, info, statkey, ierr)

!    repeat over filesize,blocksize pairs
      do k = 1, numruns
         n = filesz(k)
         b = blocksz(k)

!       maximum number of statistics to record
         statsize = 4*n/b + 50
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

!       multipleI low-level test
         call iot_run_multipleI(comm, node, nprocs, &
           datafilename, info, n, b, collective, taskloops, repeat, &
           outerloop, innerloop, palloc, ierr)
         if (ierr /= IOT_SUCCESS) then
            write (fout,*) "ERROR: node ",node," Error in multipleI test"
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

      call time_stamp(node, statfile, ierr)

      end subroutine iot_wrap_multipleI

!-----------------------------------------------------------------------
!     iot_check_multipleI
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_multipleI.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!
!     Return filesize and blocksize in byte units.
!
!-----------------------------------------------------------------------


      subroutine iot_check_multipleI(nprocs, numruns, &
        filesz, blocksz, collective, taskloops, repeat, &
        outerloop, innerloop, palloc, ierr)
      use iotest
      implicit none

      integer :: nprocs, numruns, ierr
      integer(kind=IDBL),dimension(*) :: filesz, blocksz
      real(kind=DBL),dimension(:),allocatable :: num
      logical :: palloc, collective
      integer :: i, err
      real(kind=DBL) :: temp
      integer :: taskloops, repeat, outerloop, innerloop

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


!    preallocation (optional)
      palloc = .true.
      if (len(ios(IOT_KEY_PREALLOCATE)) /= 0) then
         read (ios(IOT_KEY_PREALLOCATE)%s, *) palloc
      endif


!    collective (optional)
      collective = .false.
      if (len(ios(IOT_KEY_COLLECTIVE)) /= 0) then
         read (ios(IOT_KEY_COLLECTIVE)%s, *) collective
      endif


!    taskloops (optional)
      if (len(ios(IOT_KEY_TASKLOOPS)) /= 0) then
         read (ios(IOT_KEY_TASKLOOPS)%s, *) taskloops
         if (taskloops < 0) then
            write(fout,*) 'Error: negative taskloops' 
         endif
      else
         taskloops = 0
      endif

!     get repeat count (required)
      if (len(ios(IOT_KEY_REPEAT)) /= 0) then
         read (unit=ios(IOT_KEY_REPEAT)%s, fmt=*, iostat=err) repeat
         if (err /= 0) return
      else
         repeat = 1
      end if

!    outerloop (temporary)
      if (len(ios(IOT_KEY_OUTERLOOP)) /= 0) then
         read (ios(IOT_KEY_OUTERLOOP)%s, *) temp
         if (temp < 0.0) then
            write(fout,*) 'Error: negative outerloop' 
         endif
         outerloop = int(temp)
      else
         write(fout,*) 'Error: missing outerloop value.'
         ierr = IOT_ERR_MISSKEY
         return
      endif

!    outerloop (temporary)
      if (len(ios(IOT_KEY_INNERLOOP)) /= 0) then
         read (ios(IOT_KEY_INNERLOOP)%s, *) temp
         if (temp < 0.0) then
            write(fout,*) 'Error: negative innerloop' 
         endif
         innerloop = int(temp)
      else
         write(fout,*) 'Error: missing innerloop value.'
         ierr = IOT_ERR_MISSKEY
         return
      endif

!    debug (optional)
      if (len(ios(IOT_KEY_DEBUG)) /= 0) then
         read (unit=ios(IOT_KEY_DEBUG)%s, fmt=*, iostat=err) debug
         if (err /= 0) return
      else
         debug = .false.
      end if


      end subroutine iot_check_multipleI



