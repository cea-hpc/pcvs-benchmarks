!     $Id: singleI.f,v 1.9 1998/02/10 15:41:16 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     singleI
!     Release 1.0
!
!     Description:
!     singleI IO test subroutines
!
!     Interface:
!     call iot_wrap_singleI(comm, node, nprocs, datafilename, info,
!                          statfile, ierr)
!
!     Arguments:
!     integer :: comm, node, nprocs, info, statfile, ierr
!     character(len=IOT_MAX_STRING_LEN) :: datafilename
!
!     External components:
!     
!     Detail of test:  Asynchronous version of single
!
!-----------------------------------------------------------------------
!     individual test subroutines:
!
!     iot_run_singleI()  :
!        Subroutine to perform singleI test run.
!        Parameters specific to test.
!     iot_wrap_singleI() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!     iot_check_singleI() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_singleI

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

      end module iostat_singleI


!-----------------------------------------------------------------------
!     iot_run_singleI
!
!     individual test code
!        The parameter list varies depending on the individual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_singleI.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!
!    Low-level write test.     
!    SingleI individual file, MPI_COMM_SELF   
!    write_explicit.                         
!    Run on only one processor.              
!-----------------------------------------------------------------------


      subroutine iot_run_singleI(comm, node, nprocs, &
           datafilename, info, &
           datasz, blocksz, taskloops, repeat, &
           outerloop, innerloop,  palloc, ierr)
      use iotest
      use iostat_singleI
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
      integer(kind=MPI_OFFSET_KIND) offset_niter, offset      
      integer(kind=MPI_OFFSET_KIND), parameter :: offset_zero = 0

!    timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tinit, tcomp
      real(kind=DBL), dimension(:), allocatable :: writetime, readtime
      real(kind=DBL), dimension(:), allocatable :: wtasktime, rtasktime

!    test variables
      integer(kind=IDBL) :: datasz, blocksz
      integer :: repeat, taskloops, outerloop, innerloop
      logical :: palloc
      integer :: bsize, niter
      real(kind=DBL), dimension(:), allocatable :: wbuf, rbuf
      integer :: j,k
      real(kind=DBL) :: sumcheck
      integer :: ierra, ierrb, ierrc, ierrd


!    time
      tinit = MPI_Wtime()
      told = tinit

!    change from bytes to etype units
      call mpi_type_size(etype, etypesize, ierr)
      bsize = blocksz/etypesize
      niter = datasz/blocksz
      call save_stat(IOT_STAT_NITER, real(niter,DBL), IOT_INTEGER, ierr)

      allocate (writetime(niter),stat=ierra)
      allocate (readtime(niter),stat=ierrb)
      allocate (wtasktime(niter),stat=ierrc)
      allocate (rtasktime(niter),stat=ierrd)
      if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0) &
        .or.(ierrd /= 0)) then
         ierr = IOT_ERR_ALLOC
         write(fout,*) 'Error: singleI allocation.'
         call mpi_abort(comm, ierr, ierr)
         stop
      end if
      allocate (wbuf(bsize),stat=ierra)
      allocate (rbuf(bsize),stat=ierrb)
      if ((ierra /= 0).or.(ierrb /= 0)) then
         ierr = IOT_ERR_ALLOC
         write(fout,*) 'Error: singleI buffer allocation.'
         call mpi_abort(comm, ierr, ierr)
         stop
      end if


!    open MPI-I/O data file
!      call MPI_file_delete(datafilename, MPI_INFO_NULL, ierr)
      dmode = MPI_MODE_CREATE + MPI_MODE_RDWR
      tpre = MPI_Wtime()
      call MPI_file_open(comm, datafilename, dmode, &
                   info, datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call prtMPIerr(ierr)
         write (fout,*) "Error opening data file for writing", &
           datafilename
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
!        write (fout,*) "Node (MPI) ", node, " preallocating file"
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
         call iotMPIerr(node, ierr, "Error in file_set_view")
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
      told = MPI_Wtime()

      do j=1,niter
         offset = offset_niter
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
         offset_niter = offset_niter + bsize
      enddo

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
      told = MPI_Wtime()

      do j=1,niter
         offset = offset_niter
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

!       Check and touch data.
         sumcheck = sumcheck + rbuf(bsize) - wbuf(bsize)
         offset_niter = offset_niter + bsize
      enddo

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

!      if (delete) then
!         call MPI_file_delete(datafilename, MPI_INFO_NULL, ierr)
!         if (ierr /= MPI_SUCCESS) then
!            call iotMPIerr(node, ierr, "Error deleting data file at end ")
!            deallocate (writetime, readtime, wbuf, rbuf)
!            return
!         end if
!      end if

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


!    Save timings in this order
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

      ierr = IOT_SUCCESS 

      end subroutine iot_run_singleI


!-----------------------------------------------------------------------
!     iot_wrap_singleI
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_singleI.
!        This subroutine calls iot_check_singleI and iot_test_singleI
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_singleI(comm, node, nprocs, &
          datafilename, info, statfile, ierr)
      use iotest
      use iostat_singleI
      implicit none

      integer comm, comm_self, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename
!     n: data file size; b: block size; In bytes.
      integer :: numruns
      integer(kind=IDBL) :: n, b
      integer :: k
      integer,parameter :: MAX_NUM_PARAMS = 50
      integer(kind=IDBL),dimension(MAX_NUM_PARAMS) :: filesz, blocksz
      integer :: repeat, taskloops, outerloop, innerloop
      logical :: palloc
      integer :: statsize


!    A separate communicator for this test  
      comm_self = MPI_COMM_SELF
!    Run singleI test on node 0 
      if (node == 0) then
         call time_stamp(node, statfile, ierr)

         call iot_check_singleI(numruns, filesz, blocksz, taskloops, &
            repeat, outerloop, innerloop, palloc, ierr)
         if (ierr /= IOT_SUCCESS)   return
         
!       write input keys to file      
         call write_keys(comm_self, node, 1, statfile, &
           datafilename, info, statkey, ierr)

!       repeat over filesize,blocksize pairs
         do k = 1, numruns
            n = filesz(k)
            b = blocksz(k)

!          maximum number of statistics to record
            statsize = 4*n/b + 50
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
                  
!          singleI low-level test
            call iot_run_singleI(comm_self, node, 1, &
              datafilename, info, n, b, taskloops, repeat, &
              outerloop, innerloop, palloc, ierr)
            if (ierr /= IOT_SUCCESS) then
               write (fout,*) "ERROR: Error within singleI test"
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
!       success for all nodes /= 0
         ierr = IOT_SUCCESS
      end if

      end subroutine iot_wrap_singleI 

!-----------------------------------------------------------------------
!     iot_check_singleI
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_singleI.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!
!     Return filesize and blocksize in byte units.
!
!-----------------------------------------------------------------------


      subroutine iot_check_singleI(numruns, filesz, &
        blocksz, taskloops, repeat, outerloop, innerloop, palloc, ierr)
      use iotest
      implicit none

      integer :: numruns, ierr
      integer(kind=IDBL),dimension(*) :: filesz, blocksz
      integer :: outerloop, innerloop, repeat, taskloops
      real(kind=DBL),dimension(:),allocatable :: num
      logical :: palloc
      integer :: i, err
      real(kind=DBL) :: temp

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

!    innerloop (temporary)
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

      end subroutine iot_check_singleI








!-----------------------------------------------------------------------
!       
!     Matrix - Vector multiplication 
!      to act as the computational part of async lowlevel tests
!     
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!   Module matrix_util
!
!-----------------------------------------------------------------------

      module matrix_util
      
      contains


!-----------------------------------------------------------------------
!
!      comp task -- matrix vector multiply
!
!      Preliminary version, specify loop parameters
!-----------------------------------------------------------------------

      subroutine comp_task(repeat,innerloop,outerloop,initflag)

      use iotest

      integer :: repeat, innerloop, outerloop
      integer :: initflag

      real(kind=DBL), dimension(:,:), allocatable, save :: a
      real(kind=DBL), dimension(:), allocatable, save :: y, x
      integer :: i, j, k
      integer :: ierra, ierrb, ierrc, ierr



!    Initialise computation
      if(initflag == 1) then
!      reinitialize random generator
         allocate (x(outerloop),stat=ierra)
         allocate (y(innerloop),stat=ierrb)
         allocate (a(innerloop,outerloop),stat=ierrc)
         if ((ierra /= 0).or.(ierrb /= 0).or.(ierrc /= 0)) then
            ierr = IOT_ERR_ALLOC
!            write(fout,*) 'Error: comp_task_b allocation.'
            call mpi_abort(comm, ierr, ierr)
         end if
         x(1) = iotrand(-IOT_RSEED)
         do j=1, outerloop
            x(j) = 2.0*iotrand(IOT_RSEED) - 1.0
            do i=1, innerloop
               a(i,j) = 2.0*iotrand(IOT_RSEED) - 1.0
            enddo
         enddo
         do i=1, innerloop
            y(i) = 2.0*iotrand(IOT_RSEED) - 1.0
         enddo
!    Finalise computation
      else if(initflag == 2) then
         deallocate(x,y,a)
!    The computation itself
      else
         do k=1, repeat
            do j=1, outerloop
               do i=1, innerloop
                  y(i) = y(i) + a(i,j)*x(j)
               enddo
            enddo
         enddo
      endif

      end subroutine comp_task


      end module matrix_util












