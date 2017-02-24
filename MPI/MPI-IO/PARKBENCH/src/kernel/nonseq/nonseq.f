!     $Id: nonseq.f,v 1.8 1998/02/16 13:54:27 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     nonseq
!     Release 1.0
!
!     Description:
!     nonseq IO test subroutines
!
!     Interface:
!     call iot_wrap_nonseq(comm, node, nprocs, datafilename, info,
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
!     iot_run_nonseq()  :
!        Subroutine to perform nonseq test run.
!        Parameters specific to test.
!     iot_check_nonseq() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_nonseq() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_nonseq

!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 16
      integer,parameter :: IOT_STAT_FILESIZE = 1 
      integer,parameter :: IOT_STAT_BLOCKSIZE = 2
      integer,parameter :: IOT_STAT_NITER = 3
      integer,parameter :: IOT_STAT_UPDATE = 4
      integer,parameter :: IOT_STAT_PALLOC = 5 
      integer,parameter :: IOT_STAT_PRE = 6
      integer,parameter :: IOT_STAT_POST = 7 
      integer,parameter :: IOT_STAT_FOPEN = 8
      integer,parameter :: IOT_STAT_FCLOSE = 9
      integer,parameter :: IOT_STAT_FVIEW = 10
      integer,parameter :: IOT_STAT_WRITE = 11
      integer,parameter :: IOT_STAT_R = 12
      integer,parameter :: IOT_STAT_U = 13
      integer,parameter :: IOT_STAT_CHECK = 14
      integer,parameter :: IOT_ERR = 15
      integer,parameter :: IOT_STAT_TOTAL = 16

!     keyword strings 
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize        ", &
       "blocksize       ", &
       "niter           ", &
       "update_frac     ", &
       "palloc_time     ", &
       "pre_time        ", &
       "post_time       ", &
       "fopen_time      ", &
       "fclose_time     ", &
       "fview_time      ", &
       "write           ", &
       "r               ", &
       "u               ", &
       "sumcheck        ", &
       "error           ", &
       "total_time      " &
       /)

      end module iostat_nonseq


!-----------------------------------------------------------------------
!     iot_run_nonseq
!
!     individual test code
!        The parameter list varies depending on the individual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_nonseq.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!
!    Low-level write test.     
!    Nonseq individual file, MPI_COMM_SELF   
!    write_explicit.                         
!    Run on only one processor.              
!-----------------------------------------------------------------------

      subroutine iot_run_nonseq(comm, node, nprocs, &
              datafilename, info, filesize, &
              blocksize, update_frac, palloc, ierr)

      use iotest
      use iostat_nonseq
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
      real(kind=DBL), dimension(:), allocatable :: updatetime, readtime

!    test variables
      integer(kind=IDBL) :: filesize, blocksize
      real :: update_frac
      logical :: palloc
      integer :: bsize, niter
      real(kind=DBL), dimension(:), allocatable :: wbuf, rbuf
      real(kind=DBL), parameter :: junk = 0.0
      integer :: j,k
      real(kind=DBL) :: rand
      real :: sumcheck
      integer :: trials, updates, maxupdates



!    time
      tinit = MPI_Wtime()
      told  = tinit

!    change from bytes to etype units
      call mpi_type_size(etype, etypesize, ierr)
      bsize = blocksize/etypesize
      niter = filesize/blocksize/nprocs
      call save_stat(IOT_STAT_NITER, real(niter,DBL), IOT_INTEGER, ierr)
      offset_proc = node*bsize

! for the meantime, trials = niter, (also note stats allocation)
      trials = niter    
      allocate (readtime(trials))
      allocate (updatetime(trials))
      allocate (wbuf(bsize))
      allocate (rbuf(bsize))



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
         deallocate (readtime, wbuf, rbuf)
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
            deallocate (readtime, wbuf, rbuf)
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
         deallocate (readtime, wbuf, rbuf)
         return
      end if
      tnew = MPI_Wtime()
      tdiff = tnew - tpre
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)

      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)

!    Create the file 
!    initialize random
      wbuf(1) = iotrand(IOT_RSEED)
      do j=1,bsize
         wbuf(j) = node + iotrand(IOT_RSEED)
      enddo
      offset_niter = offset_zero

      do j=1,niter
         offset = offset_niter + offset_proc
         call MPI_file_write_at(datafilepointer, offset, & 
             wbuf(1), bsize, etype, status, ierr)
         offset_niter = offset_niter + nprocs*bsize
      enddo

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error writing data")
         return
      end if

      call MPI_file_sync(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in MPI_file_sync")
         deallocate (readtime, wbuf, rbuf)
         return
      end if

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_WRITE, tdiff, IOT_REAL, ierr)
      call mpi_barrier(comm, ierr)


!    File created -- start the nonseq test


!    Independent random seeds for each process
      rand = iotrand(-(node + 1)*IOT_RSEED)
      told = MPI_Wtime()


      updates = 0
      do j=1,trials
         offset = niter*nprocs*iotrand(IOT_RSEED)
         offset = bsize*offset
!         offset = int(bsize*niter*rand,kind=DBL)
         call MPI_file_read_at(datafilepointer, offset, & 
             rbuf(1), bsize, etype, status, ierr)
         tnew = MPI_Wtime()
         readtime(j) = tnew - told

!       Touch data.
         do k=1,bsize
            rbuf(k) = rbuf(k) - wbuf(k)
         enddo
!       Modify update_frac of the cases
         rand = iotrand(IOT_RSEED)
         told = MPI_Wtime()
         if (rand < update_frac) then
            updates = updates + 1
            call MPI_file_write_at(datafilepointer, offset, & 
              rbuf(1), bsize, etype, status, ierr)
            tnew = MPI_Wtime()
            updatetime(updates) = tnew - told
            told = tnew
         endif
      enddo

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error reading data")
         deallocate (readtime, wbuf, rbuf)
         return
      end if

      tpre = MPI_Wtime()
      call MPI_file_close(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error closing data file")
         deallocate (readtime, wbuf, rbuf)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FCLOSE, tdiff, IOT_REAL, ierr)



      do j=1,trials
         call save_stat(IOT_STAT_R, readtime(j), IOT_REAL, ierr)
      enddo
      call MPI_allreduce(updates, maxupdates, 1, MPI_INTEGER, &
        MPI_MAX, comm, ierr)
      do j=1,updates
         call save_stat(IOT_STAT_U, updatetime(j), IOT_REAL, ierr)
      enddo
      do j=updates, maxupdates - 1
         call save_stat(IOT_STAT_U, junk, IOT_REAL, ierr)
      enddo

      deallocate (updatetime)
      deallocate (readtime)
      deallocate (wbuf)
      deallocate (rbuf)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_POST, tdiff, IOT_REAL, ierr)
      tdiff = tnew - tinit
      call save_stat(IOT_STAT_TOTAL, tdiff, IOT_REAL, ierr)

      end subroutine iot_run_nonseq


!-----------------------------------------------------------------------
!     iot_wrap_nonseq
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_kernel.
!        This subroutine calls iot_check_nonseq and iot_test_nonseq
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_nonseq(comm, node, nprocs, &
          datafilename, info, statfile, ierr)
      use iotest
      use iostat_nonseq
      implicit none

      integer comm, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename
      integer :: num_blocksize, num_update
      integer(kind=IDBL) :: filesize, bsz
      real :: ufr
      integer :: i, j, k
      integer :: rep
      integer,parameter :: MAX_NUM_PARAMS = 50
      integer(kind=IDBL),dimension(MAX_NUM_PARAMS) :: blocksize
      real, dimension(MAX_NUM_PARAMS) :: update_frac
      logical :: palloc
      integer :: statsize



      call time_stamp(node, statfile, ierr)

      call iot_check_nonseq(filesize, num_blocksize, blocksize, &
        num_update, update_frac, palloc, ierr)
      if (ierr /= IOT_SUCCESS) return

!    write input keys to file      
      call write_keys(comm, node, nprocs, statfile, &
        datafilename, info, statkey, ierr)


!    repeat over block sizes
      do k = 1, num_blocksize
         bsz = blocksize(k)

!       repeat over update fractions
         do j = 1, num_update
            ufr = update_frac(j)

!          allocate storage for timing information
!          maximum number of statistics to record
            statsize = 2*filesize/bsz + 40
            call alloc_stat(statsize, ierr)
            if (ierr /= IOT_SUCCESS) then
               write(fout,*) "ERROR: failed to allocate memory for stats"
               call mpi_abort(comm, ierr, ierr)
            end if



!          save input parameters
            call save_stat(IOT_STAT_FILESIZE,  &
              real(filesize,DBL), IOT_INTEGER, ierr)
            call save_stat(IOT_STAT_BLOCKSIZE, &
              real(bsz,DBL), IOT_INTEGER, ierr)

!          nonseq low-level test
            call iot_run_nonseq(comm, node, nprocs, &
              datafilename, info, filesize, bsz, ufr, palloc, ierr)
            if (ierr /= IOT_SUCCESS) then
               write (fout,*) "ERROR: node ",node," Error in nonseq test"
            end if

!          write statistics information to file
            call write_stat(comm, node, nprocs, statfile, &
              datafilename, info, statkey, ierr)
                  
!          free storage for timing information
            call free_stat(ierr)
                
         end do                 ! block size loop
      end do                    ! data file size loop


!     write time stamp
      call time_stamp(node, statfile, ierr)

      end subroutine iot_wrap_nonseq

!-----------------------------------------------------------------------
!     iot_check_nonseq
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_nonseq.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!
!     Return filesize and blocksize in byte units.
!
!-----------------------------------------------------------------------

      subroutine iot_check_nonseq(filesize, num_blocksize, blocksize, &
        num_update, update_frac, palloc, ierr)
      use iotest
      implicit none


      integer :: num_blocksize, num_update,ierr
      real(kind=DBL) ::  realfilesize
      integer(kind=IDBL) ::  filesize
      integer(kind=IDBL),dimension(*) ::  blocksize
      real,dimension(*) ::  update_frac
      real(kind=DBL),dimension(:),allocatable :: num
      logical :: palloc
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


!    blocksize (required)
      if (len(ios(IOT_KEY_NUMBLOCKSIZE)) /= 0) then
         read (ios(IOT_KEY_NUMBLOCKSIZE)%s, *) num_blocksize
         if (len(ios(IOT_KEY_BLOCKSIZE)) /= 0) then
            allocate(num(num_blocksize))
            read (ios(IOT_KEY_BLOCKSIZE)%s, *) (num(i),i=1,num_blocksize)
            do i = 1, num_blocksize
               blocksize(i) = num(i)*IOT_BLOCKSIZE_UNIT
               if (blocksize(i) < 8) then
!             blocksize < size of one double
                  write(fout,*) 'Error: blocksize too small.'
               endif
               if (blocksize(i) > filesize) then
!             blocksize > filesize
                  write(fout,*) 'Error: blocksize larger than', & 
                  ' filesize.'
               endif
            end do
            deallocate(num)
         else
            write(fout,*) 'Error: missing blocksize values.'
            ierr = IOT_ERR_MISSKEY
            return
         endif
      else
         write(fout,*) 'Error: missing numblocksize value.'
         ierr = IOT_ERR_MISSKEY
         return
      endif


!    update fraction (required)
      if (len(ios(IOT_KEY_NUMUPDATE)) /= 0) then
         read (ios(IOT_KEY_NUMUPDATE)%s, *) num_update
         if (len(ios(IOT_KEY_UPDATE)) /= 0) then
            allocate(num(num_update))
            read (ios(IOT_KEY_UPDATE)%s, *) (num(i),i=1,num_update)
            do i = 1, num_update
               update_frac(i) = num(i)
               if (update_frac(i) < 0.0) then
!             negative fraction
                  write(fout,*) 'Error: Negative update fraction.'
               endif
               if (update_frac(i) > 1.0) then
!             not a probability
                  write(fout,*) 'Error: update fraction > 1' 
               endif
            end do
            deallocate(num)
         else
            write(fout,*) 'Error: missing update fraction values.'
            ierr = IOT_ERR_MISSKEY
            return
         endif
      else
         write(fout,*) 'Error: missing num_update value.'
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

      end subroutine iot_check_nonseq









