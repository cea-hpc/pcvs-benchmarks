!     $Id: transpose.f,v 1.6 1998/02/16 13:53:32 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     transpose
!     Release 1.0
!
!     Description:
!     transpose IO test subroutine
!
!     Interface:
!     call iot_wrap_transpose(comm, node, nprocs, datafilename, info,
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
!     iot_run_transpose()  :
!        Subroutine to perform single test run.
!        Parameters specific to test.
!     iot_check_transpose() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_transpose() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_transpose

!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 20
      integer,parameter :: IOT_STAT_FILESIZE = 1
      integer,parameter :: IOT_STAT_BLOCKSIZE = 2
      integer,parameter :: IOT_STAT_XSIZE = 3
      integer,parameter :: IOT_STAT_YSIZE = 4
      integer,parameter :: IOT_STAT_JUNK = 5
      integer,parameter :: IOT_STAT_BLANK = 6
      integer,parameter :: IOT_STAT_PRE = 7
      integer,parameter :: IOT_STAT_POST = 8
      integer,parameter :: IOT_STAT_PALLOC = 9
      integer,parameter :: IOT_STAT_SYNC = 10
      integer,parameter :: IOT_STAT_FOPEN = 11
      integer,parameter :: IOT_STAT_FCLOSE = 12
      integer,parameter :: IOT_STAT_FVIEW = 13
      integer,parameter :: IOT_STAT_DTYPE = 14
      integer,parameter :: IOT_STAT_WRITE = 15
      integer,parameter :: IOT_STAT_READ = 16
      integer,parameter :: IOT_STAT_INT_PRE = 17
      integer,parameter :: IOT_STAT_INT_POST = 18
      integer,parameter :: IOT_STAT_INTERNAL = 19
      integer,parameter :: IOT_STAT_TOTAL = 20


!     keyword strings
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize     ", &
       "blocksize    ", &
       "xsize        ", &      
       "ysize        ", &
       "nprocs       ", &      
       "             ", &      
       "pre_time     ", &
       "post_time    ", &
       "palloc_time  ", &
       "sync_time    ", &
       "fopen_time   ", &
       "fclose_time  ", &
       "fview_time   ", &
       "dtype_time   ", &
       "write        ", &
       "read         ", &
       "int_pre      ", &
       "int_post     ", &
       "internal     ", &
       "total_time   " &
       /)

      end module iostat_transpose

!-----------------------------------------------------------------------
!     iot_run_transpose
!
!     individual test code
!        The parameter list varies depending on the indiviual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_transpose.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!-----------------------------------------------------------------------


      subroutine iot_run_transpose(comm, node, nprocs, &
           datafilename, info, &
           Nx, Ny, &
           palloc, collective, ierr)
      use iotest
      use iostat_transpose
      implicit none

      integer :: comm, node, nprocs, info, ierr
      character(len=*) :: datafilename
      integer :: Nx, Ny
      logical :: palloc, collective

!     external functions
      real(kind=DBL) :: matchk2D


!     Standard MPI stuff
      character(len=*),parameter :: datarep = "native"
      integer :: datafilepointer
      integer :: dmode
      integer,dimension(MPI_STATUS_SIZE) :: status
      integer(kind=MPI_OFFSET_KIND), parameter :: offset_zero = 0
      integer, parameter :: etype = MPI_DOUBLE_PRECISION
      integer :: sendvectype, rowtype, recvectype
      integer :: coltype, submattype, strvectype
      integer :: readvectype, subtransmattype, transrowtype
      integer :: etypesize
      integer, dimension(nprocs) :: rcounts, displs
!      integer(kind=MPI_ADDRESS_KIND) :: lb, extent
      integer, dimension(nprocs) :: scnt, sdisp, sdtype
      integer, dimension(nprocs) :: rcnt, rdisp, rtype

!     timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tinit

!     general variables
      integer :: pNx, pNy
      integer :: i, j
      integer :: pj
      integer :: xi
      integer(kind=IDBL) :: totsize
      real(kind=DBL), parameter :: junk = 0.0
      real(kind=DBL) :: res, eps
      real(kind=DBL) :: rand

!     distributed matrix data
      real(kind=DBL),dimension(:,:),allocatable :: a, aTinternal, aTexternal




!    time
      tinit = MPI_Wtime()
      told = tinit
     
!    Work out sizes on each processor
!    Note that definitions are different from matrix tests
      call mpi_type_size(etype, etypesize, ierr)
      pNx = Nx/nprocs
      pNy = Ny/nprocs
      Nx = nprocs*pNx
      Ny = nprocs*pNy
      totsize = int(Nx*Ny*etypesize,IDBL)
      call save_stat(IOT_STAT_FILESIZE, real(totsize,DBL), IOT_INTEGER, ierr)


!    allocate and initialise distributed arrays
!    arrange so that the values mimic those from the global array
!    The order of x,y is indeed intended
      allocate (a(Ny,pNx)) 
      allocate (aTinternal(Nx,pNy)) 
      allocate (aTexternal(Nx,pNy)) 
!    random_seed should be different on repeats
      a(1,1) = iotrand(IOT_RSEED)
      xi = node * pNx
      do j=1,Nx
         do i=1,Ny
            rand = iotrand(IOT_RSEED)
            if ((j>xi).and.(j<=xi+pNx)) then
               pj = modulo(j,pNx)
               if (pj == 0) pj = pNx
               a(i,pj) = node + rand
            endif
         enddo
      enddo


!    write out matrix if debugging
!      if (debug) then
!         do j=1,pNx
!            do i=1,Ny
!               write(fout,*) node,i,j,a(i,j)
!            enddo
!         enddo
!         write(fout,*) ""
!         call flush(fout)
!      end if


!--------------------------------------------
!    EXTERNAL TRANSPOSE (using file)
!--------------------------------------------

!   open MPI-I/O data file 
      call mpi_file_delete(datafilename, MPI_INFO_NULL, ierr)
      dmode = MPI_MODE_CREATE + MPI_MODE_RDWR
      tpre = MPI_Wtime()
      call MPI_file_open(comm, datafilename, dmode, &
        info, datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call prtMPIerr(ierr)
         write (fout,*) "Error opening data file for writing", &
           datafilename, "node (MPI) ",node
         ierr = IOT_ERR_MPI
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FOPEN, tdiff, IOT_REAL, ierr)
      
      
!    preallocation
      if (palloc) then
         tnew = MPI_Wtime()
         tpre = tnew - told
         told = tnew
         call MPI_File_preallocate(datafilepointer, totsize, ierr)
         if (ierr /= MPI_SUCCESS) then
            call iotMPIerr(node, ierr, "Error preallocating")
            return
         end if
         tnew = MPI_Wtime()
         tdiff = tnew - told
         call save_stat(IOT_STAT_PALLOC, tdiff, IOT_REAL, ierr)
         told = tnew - tpre
      end if
      


!     create MPI filetypes for matrix IO
      tpre = MPI_Wtime()
      call create_contig_type(node, Ny, etype, coltype)
      call create_submat2D_type(pNx, Ny, nprocs, 1, &
         node, etype, submattype)
      call mpi_file_set_view(datafilepointer, offset_zero, etype, &
         submattype, datarep, info, ierr)
      tnew = MPI_Wtime()
      tdiff = tnew - tpre
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)

      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)


!------------------------------------
!     write data to file
!------------------------------------

      if (collective) then
         call mpi_file_write_all(datafilepointer, a, pNx, coltype, &
           status, ierr)
      else
         call mpi_file_write(datafilepointer, a, pNx, coltype, &
           status, ierr)
      end if

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error writing to file")
         deallocate (a)
         return
      end if

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_WRITE, tdiff, IOT_REAL, ierr)



!    Shouldn't need to sync the file for this transpose test
!    But Sync fixes an SPFS bug
      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()
      call MPI_file_sync(datafilepointer, ierr)
      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_SYNC, tdiff, IOT_REAL, ierr)


!------------------------------------
!    read distributed data from file as transpose
!------------------------------------

!    MPI datatypes for reading transpose
      call create_submat2D_type(Nx, pNy, 1, nprocs, &
         node, etype, subtransmattype)
      call create_vector_type(node, pNy, 1, Nx, etype, transrowtype)
      call create_hvector_type(node, Nx, 1, etypesize, transrowtype, &
        strvectype)
      call mpi_file_set_view(datafilepointer, offset_zero, etype, &
        subtransmattype, datarep, info, ierr)
      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)

        
      if (collective) then
         call mpi_file_read_all(datafilepointer,  &
           aTexternal, 1, strvectype, &
           status, ierr)
      else
         call mpi_file_read(datafilepointer,  &
           aTexternal, 1, strvectype, &
           status, ierr)
      end if

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error reading from file")
!            deallocate (a, b)
         return
      end if
     

      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_READ, tdiff, IOT_REAL, ierr)

      call MPI_file_close(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error closing data file")
!            deallocate (a, b)
         return
      end if
      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_FCLOSE, tdiff, IOT_REAL, ierr)


!--------------------------------------------
!    INTERNAL TRANSPOSE  (using interconnect)
!--------------------------------------------

      call mpi_barrier(comm, ierr)
      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_POST, tdiff, IOT_REAL, ierr)


      call create_vector_type(node, pNx, pNy, Ny, etype, sendvectype)
      call create_vector_type(node, pNx, 1, Ny, etype, rowtype)
      call create_hvector_type(node, pNy, 1, etypesize, rowtype, &
        recvectype)
      tdiff =  MPI_Wtime() - told 
      call save_stat(IOT_STAT_DTYPE, tdiff, IOT_REAL, ierr)


!    Prepare parameters for transpose using alltoallw
      do i = 1,nprocs
         scnt(i) = 1
         sdisp(i) = (i-1)*pNy*etypesize
         sdtype(i) = sendvectype
         rcnt(i) = 1
         rdisp(i) = (i-1)*pNx*etypesize
         rtype(i) = recvectype
      end do


      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_INT_PRE, tdiff, IOT_REAL, ierr)


!    transpose the data internally
      call mpi_alltoallw(a, scnt, sdisp, sdtype, &
         aTinternal, rcnt, rdisp, rtype, comm, ierr)


      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_INTERNAL, tdiff, IOT_REAL, ierr)

!    Check matrix
      res = matchk2D(pNy, Nx, aTinternal, aTexternal)
      if (res /= 0.0e0) then
         write (fout,*) "Node ",node,", error in saved matrix"
      endif


!    Write out matrices if debugging
!      if (debug) then
!         do j=1,pNy
!            do i=1,Nx
!               write(fout,*) node,i,j,aTinternal(i,j),aTexternal(i,j)
!            enddo
!         enddo
!         write(fout,*) ""
!         call flush(fout)
!      end if

!     deallocate arrays
      deallocate (a,aTinternal,aTexternal)

!     free types
      call mpi_type_free(submattype, ierr)
      call mpi_type_free(subtransmattype, ierr)
      call mpi_type_free(sendvectype, ierr)
      call mpi_type_free(rowtype, ierr)
      call mpi_type_free(recvectype, ierr)
      call mpi_type_free(coltype, ierr)
      call mpi_type_free(transrowtype, ierr)
      call mpi_type_free(strvectype, ierr)


      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_INT_POST, tdiff, IOT_REAL, ierr)
      tdiff = tnew - tinit
      call save_stat(IOT_STAT_TOTAL, tdiff, IOT_REAL, ierr)

      end subroutine iot_run_transpose


!-----------------------------------------------------------------------
!     iot_wrap_transpose
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_transpose.
!        This subroutine calls iot_check_transpose and iot_test_transpose
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_transpose(comm, node, nprocs, &
           datafilename, info, statfile, ierr)
      use iotest
      use iostat_transpose
      implicit none

      integer :: comm, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename

      integer :: numsizes
      integer,dimension(:),allocatable :: xsize, ysize

      integer :: Nx, Ny
      integer :: i, j, k, p
      integer :: repeat
      integer :: statsize, paramsize
      logical :: palloc, collective

!     write time stamp
      call time_stamp(node, statfile, ierr)

      paramsize = 20
      allocate(xsize(paramsize),ysize(paramsize))
      call iot_check_transpose(numsizes, xsize, ysize, &
         repeat, palloc, collective, ierr)
      if (ierr /= IOT_SUCCESS) then
         write (fout,*) "ERROR: Error in test parameters"
         call mpi_abort(comm, ierr, ierr)
      end if



!    write input keys to file      
      call write_keys(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)

!    allocate storage for timing information
      statsize = IOT_NUM_STATKEYS*numsizes*repeat + 40
      call alloc_stat(statsize, ierr)
      if (ierr /= IOT_SUCCESS) then
         write(fout,*) "ERROR: failed to allocate memory for stats"
         call mpi_abort(comm, ierr, ierr)
      end if

!    repeat over matrix sizes
      do k = 1, numsizes
         Nx = xsize(k)
         Ny = ysize(k)

!       repeat
         do i = 1, repeat

!          save input parameters
            call save_stat(IOT_STAT_XSIZE, real(Nx,DBL), IOT_INTEGER, ierr)
            call save_stat(IOT_STAT_YSIZE, real(Ny,DBL), IOT_INTEGER, ierr)
            call save_stat(IOT_STAT_NPROCS, real(nprocs,DBL), IOT_INTEGER, ierr)
            
            call iot_run_transpose(comm, node, nprocs, &
                 datafilename, info, &
                 Nx, Ny,  &
                 palloc, collective, ierr)
            if (ierr /= IOT_SUCCESS) then
               write (fout,*) "ERROR: Error in transpose test"
            end if

            call save_stat(IOT_STAT_BLANK, IOT_STAT_EMPTY, &
                 IOT_INTEGER, ierr)

         end do            ! repeat loop
      end do ! matrix size loop

!     write statistics information to file
      call write_stat(comm, node, nprocs, statfile, &
        datafilename, info, statkey, ierr)

!     free storage for timing information
      call free_stat(ierr)

!     deallocate temporary arrays
      deallocate(xsize, ysize)


      call time_stamp(node, statfile, ierr)

      end subroutine

!-----------------------------------------------------------------------
!     iot_check_transpose
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_transpose.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!-----------------------------------------------------------------------

      subroutine iot_check_transpose(numsizes, xsize, ysize, &
         repeat, palloc, collective, ierr)
      use iotest
      implicit none

      integer :: nprocs, numsizes, ierr
!     should be : not * in following line but BUGGY
      integer,dimension(*) ::  xsize, ysize
      integer :: repeat
      logical :: palloc, collective
      real(kind=DBL),dimension(:),allocatable :: fnum
      integer :: i, err

      ierr = IOT_ERR_MISSKEY

!     get matrix sizes
      if (len(ios(IOT_KEY_NUMSIZES)) /= 0) then
         read (unit=ios(IOT_KEY_NUMSIZES)%s, fmt=*, iostat=err) numsizes
         if (err /= 0) return
         if (len(ios(IOT_KEY_XSIZE)) /= 0) then
            read (unit=ios(IOT_KEY_XSIZE)%s, fmt=*, iostat=err) &
               (xsize(i),i=1,numsizes)
            if (err /= 0) return
         else
            return 
         end if
         if (len(ios(IOT_KEY_YSIZE)) /= 0) then
            read (unit=ios(IOT_KEY_YSIZE)%s, fmt=*, iostat=err) &
               (ysize(i),i=1,numsizes)
            if (err /= 0) return
         else 
            return 
         end if
      else 
         return 
      end if

!     get repeat count
      if (len(ios(IOT_KEY_REPEAT)) /= 0) then
         read (unit=ios(IOT_KEY_REPEAT)%s, fmt=*, iostat=err) repeat
         if (err /= 0) return
      else
         repeat = 1
      end if

!     get logicals
      if (len(ios(IOT_KEY_PREALLOCATE)) /= 0) then
         read (unit=ios(IOT_KEY_PREALLOCATE)%s, fmt=*, iostat=err) palloc
         if (err /= 0) return
      else
         palloc = .false.
      end if

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

      ierr = IOT_SUCCESS

      end subroutine iot_check_transpose



























