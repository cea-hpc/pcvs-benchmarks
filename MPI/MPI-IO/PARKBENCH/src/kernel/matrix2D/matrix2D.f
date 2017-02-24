!     $Id: matrix2D.f,v 1.10 1998/02/16 13:51:49 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     matrix2D
!     Release 1.0
!
!     Description:
!     matrix2D IO test subroutine
!
!     Interface:
!     call iot_wrap_matrix2D(comm, node, nprocs, datafilename, info,
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
!     iot_run_matrix2D()  :
!        Subroutine to perform single test run.
!        Parameters specific to test.
!     iot_check_matrix2D() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_matrix2D() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_matrix2D

!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 17
      integer,parameter :: IOT_STAT_FILESIZE = 1
      integer,parameter :: IOT_STAT_BLOCKSIZE = 2
      integer,parameter :: IOT_STAT_XSIZE = 3
      integer,parameter :: IOT_STAT_YSIZE = 4
      integer,parameter :: IOT_STAT_XPROC = 5
      integer,parameter :: IOT_STAT_YPROC = 6
      integer,parameter :: IOT_STAT_BLANK = 7
      integer,parameter :: IOT_STAT_PRE = 8
      integer,parameter :: IOT_STAT_POST = 9
      integer,parameter :: IOT_STAT_PALLOC = 10
      integer,parameter :: IOT_STAT_SYNC = 11
      integer,parameter :: IOT_STAT_FOPEN = 12
      integer,parameter :: IOT_STAT_FCLOSE = 13
      integer,parameter :: IOT_STAT_FVIEW = 14
      integer,parameter :: IOT_STAT_WRITE = 15
      integer,parameter :: IOT_STAT_READ = 16
      integer,parameter :: IOT_STAT_TOTAL = 17


!     keyword strings
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize     ", &
       "blocksize    ", &
       "xsize        ", &      
       "ysize        ", &
       "xproc        ", &      
       "yproc        ", &      
       "             ", &      
       "pre_time     ", &
       "post_time    ", &
       "palloc_time  ", &
       "sync_time    ", &
       "fopen_time   ", &
       "fclose_time  ", &
       "fview_time   ", &
       "write        ", &
       "read         ", &
       "total_time   " &
       /)

      end module iostat_matrix2D

!-----------------------------------------------------------------------
!     iot_run_matrix2D
!
!     individual test code
!        The parameter list varies depending on the indiviual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_matrix2D.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!-----------------------------------------------------------------------


      subroutine iot_run_matrix2D(comm, node, nprocs, &
           datafilename, info, &
           Nx, Ny, &
           xproc, yproc, &
           palloc, collective, ierr)
      use iotest
      use iostat_matrix2D
      implicit none

      integer :: comm, node, nprocs, info, ierr
      character(len=*) :: datafilename
      integer :: Nx, Ny
      integer :: xproc, yproc
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
      integer :: coltype, submattype
      integer :: etypesize

!     timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tinit

!     general variables
      integer :: pNx, pNy
      integer :: i, j
      integer :: pi, pj
      integer :: xi, yi
      integer(kind=IDBL) :: totsize
      real(kind=DBL) :: res, eps
      real(kind=DBL) :: rand

!     distributed matrix data
      real(kind=DBL),dimension(:,:),allocatable :: a, b



!    time
      tinit = MPI_Wtime()
      told = tinit

!    Work out sizes on each processor
      call mpi_type_size(etype, etypesize, ierr)
      pNx = Nx/xproc
      pNy = Ny/yproc
      totsize = int(nprocs*pNx*pNy*etypesize,IDBL)
      call save_stat(IOT_STAT_FILESIZE, real(totsize,DBL), IOT_INTEGER, ierr)


!   open MPI-I/O data file
      call mpi_file_delete(datafilename, MPI_INFO_NULL, ierr)
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
      endif


!     create MPI filetypes for matrix IO
      tpre = MPI_Wtime()
      call create_contig_type(node, pNy, etype, coltype)
      call create_submat2D_type(pNx, pNy, xproc, yproc, &
         node, etype, submattype)
      call mpi_file_set_view(datafilepointer, offset_zero, etype, &
         submattype, datarep, info, ierr)
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)


!    allocate and initialise distributed arrays
!    arrange so that the values mimic those from the global array
!    The order of x,y is indeed intended
      allocate (a(pNy,pNx),b(pNy,pNx)) 
!    should call random_seed to be different on repeats
      a(1,1) = iotrand(IOT_RSEED)
      xi = modulo(node,xproc) * pNx
      yi = (node/xproc) * pNy
      do j=1,Nx
         do i=1,Ny
            rand = iotrand(IOT_RSEED)
            if ( (j>xi).and.(j<=xi+pNx) &
               .and.(i>yi).and.(i<=yi+pNy)) then
               pi = modulo(i,pNy)
               if (pi == 0) pi = pNy
               pj = modulo(j,pNx)
               if (pj == 0) pj = pNx
               a(pi,pj) = node + rand
               b(pi,pj) = 0.0
            endif
         enddo
      enddo


      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)
      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()


!    write data to file
!    collective or non-collective versions
      if (collective) then
         call mpi_file_write_all(datafilepointer, a, pNx, coltype, &
           status, ierr)
      else
         call mpi_file_write(datafilepointer, a, pNx, coltype, &
           status, ierr)
      end if

      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error writing to file")
         deallocate (a, b)
         return
      end if


      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_WRITE, tdiff, IOT_REAL, ierr)


      call MPI_file_sync(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in MPI_file_sync")
         deallocate (a, b)
         return
      end if
      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_SYNC, tdiff, IOT_REAL, ierr)
!    May add something here to try and flush the cache 

      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()


!    Reset filepointer 
      call mpi_file_set_view(datafilepointer, offset_zero, etype, &
        submattype, datarep, info, ierr)


!    read distributed data from file

      if (collective) then
         call mpi_file_read_all(datafilepointer, b, pNx, coltype, &
           status, ierr)
      else
         call mpi_file_read_all(datafilepointer, b, pNx, coltype, &
           status, ierr)
      endif


      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error reading from file")
         deallocate (a, b)
         return
      end if

      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_READ, tdiff, IOT_REAL, ierr)

      tpre = MPI_Wtime()
      call MPI_file_close(datafilepointer, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error closing data file")
         deallocate (a, b)
         return
      end if
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FCLOSE, tdiff, IOT_REAL, ierr)

!    compare matrix
      res = matchk2D(pNx, pNy, a, b)
      if (res /= 0.0e0) then
         write (fout,*) "error in saved matrix"
      endif

!     deallocate arrays
      deallocate (a, b)

!     free types
      call mpi_type_free(coltype, ierr)
      call mpi_type_free(submattype, ierr)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_POST, tdiff, IOT_REAL, ierr)
      tdiff = tnew - tinit
      call save_stat(IOT_STAT_TOTAL, tdiff, IOT_REAL, ierr)

      end subroutine iot_run_matrix2D


!-----------------------------------------------------------------------
!     iot_wrap_matrix2D
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_matrix2D.
!        This subroutine calls iot_check_matrix2D and iot_test_matrix2D
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_matrix2D(comm, node, nprocs, &
           datafilename, info, statfile, ierr)
      use iotest
      use iostat_matrix2D
      implicit none

      integer :: comm, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename

      integer :: numsizes, numprocgrids
      integer,dimension(:),allocatable :: xproc, yproc
      integer,dimension(:),allocatable :: xsize, ysize

      integer :: m, n
      integer :: xc, yc, xp, yp, xb, yb
      integer :: i, j, k, p
      integer :: repeat
      integer :: statsize, paramsize
      logical :: palloc, collective, reorder, period(2)
      integer :: dim(2), coord(2), grid



      call time_stamp(node, statfile, ierr)

      paramsize = 20
      allocate(xsize(paramsize),ysize(paramsize))
      allocate(xproc(paramsize),yproc(paramsize))
      call iot_check_matrix2D(paramsize, numsizes, xsize, ysize, &
         numprocgrids, xproc, yproc, &
         repeat, palloc, collective, ierr)
      if (ierr /= IOT_SUCCESS) then
         write (fout,*) "ERROR: Error in test parameters"
         call mpi_abort(comm, ierr, ierr)
      end if


!    write input keys to file      
      call write_keys(comm, node, nprocs, statfile, &
           datafilename, info, statkey, ierr)

!    repeat over process grids
      do p = 1, numprocgrids
         xp = xproc(p)
         yp = yproc(p)

!       create cartesian topology
         dim(1) = xp
         dim(2) = yp
         period(1) = .false.
         period(2) = .false.
!       NO reordering. this may be suboptimal on some architectures.
!       in that case run different size grids as separate runs?
!       Without reordering -- the cartesian nature of the grid is
!       irrelevant to this code!!!
         reorder = .false.
         call mpi_cart_create(comm, 2, dim, period, reorder, grid, ierr)
         if (ierr  /= MPI_SUCCESS) then
            write(fout,*) "ERROR: cannot create process grid"
            write(fout,*) "Skipping process grid ",xp," by ",yp,"."
            cycle
         end if


!       only continue on this process if part of grid 
         if (grid == MPI_COMM_NULL) cycle


!       allocate storage for timing information
         statsize = IOT_NUM_STATKEYS*repeat*numsizes + 40
         call alloc_stat(statsize, ierr)
         if (ierr /= IOT_SUCCESS) then
            write(fout,*) "ERROR: failed to allocate memory for stats"
            call mpi_abort(comm, ierr, ierr)
         end if

!       repeat over matrix sizes
         do k = 1, numsizes
            m = xsize(k)
            n = ysize(k)

!          repeat
            do i = 1, repeat

!             save input parameters
               call save_stat(IOT_STAT_XSIZE, real(m,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_YSIZE, real(n,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_XPROC, real(xp,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_YPROC, real(yp,DBL), IOT_INTEGER, ierr)

               call iot_run_matrix2D(grid, node, xp*yp, &
                 datafilename, info, &
                 m, n, xp, yp, &
                 palloc, collective, ierr)
               if (ierr /= IOT_SUCCESS) then
                  write (fout,*) "ERROR: Error in matrix2D test"
               end if

               call save_stat(IOT_STAT_BLANK, IOT_STAT_EMPTY, &
                 IOT_INTEGER, ierr)

            end do              ! repeat loop
         end do                 ! matrix2D size loop

!       write statistics information to file
         call write_stat(grid, node, xp*yp, statfile, &
           datafilename, info, statkey, ierr)

!       free storage for timing information
         call free_stat(ierr)

!       free cartesian grid 
         call mpi_comm_free(grid, ierr)

      end do ! process grid loop

!     deallocate temporary arrays
      deallocate(xsize, ysize)
      deallocate(xproc, yproc)

      call time_stamp(node, statfile, ierr)

      end subroutine

!-----------------------------------------------------------------------
!     iot_check_matrix2D
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_matrix2D.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!-----------------------------------------------------------------------

      subroutine iot_check_matrix2D(numparams, numsizes, xsize, ysize, &
         numprocgrids, xproc, yproc, &
         repeat, palloc, collective, ierr)
      use iotest
      implicit none

      integer :: numparams, numsizes, numprocgrids, ierr
      integer :: xsize(numparams), ysize(numparams)
      integer :: xproc(numparams), yproc(numparams)
      integer :: repeat
      logical :: palloc, collective
      integer :: i, err

      ierr = IOT_ERR_MISSKEY

!     get matrix sizes
      if (len(ios(IOT_KEY_NUMSIZES)) /= 0) then
         read (unit=ios(IOT_KEY_NUMSIZES)%s, fmt=*, iostat=err) numsizes
         if (err /= 0) return
         if (numsizes <= numparams) then
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
!       numsizes > numparams
            write(fout,*) 'Error: Too many sizes.'
            return 
         end if
      else 
         return 
      end if

!     get process grid sizes
      if (len(ios(IOT_KEY_NUMPROCGRIDS)) /= 0) then
         read (unit=ios(IOT_KEY_NUMPROCGRIDS)%s, fmt=*, iostat=err) & 
            numprocgrids
         if (err /= 0) return
         if (numprocgrids <= numparams) then
            if (len(ios(IOT_KEY_XPROC)) /= 0) then
               read (unit=ios(IOT_KEY_XPROC)%s, fmt=*, iostat=err) &
               (xproc(i),i=1,numprocgrids)
               if (err /= 0) return
            else 
               return 
            end if
            if (len(ios(IOT_KEY_YPROC)) /= 0) then
               read (unit=ios(IOT_KEY_YPROC)%s, fmt=*, iostat=err) &
               (yproc(i),i=1,numprocgrids)
               if (err /= 0) return
            else 
               return 
            end if
         else
!       numsizes > numparams
            write(fout,*) 'Error: Too many processor grids.'
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

      end subroutine




!-----------------------------------------------------------------------    
!     compare 2 matrices
!-----------------------------------------------------------------------    

      function matchk2D(Nx, Ny, a, b) result (res)
      use iotest
      implicit none

      integer :: Nx, Ny, i, j
      real(kind=DBL) :: a(Ny,Nx), b(Ny,Nx), res, dif
      res = 0.0e0
      do j = 1, Nx
         do i = 1, Ny
            dif = dabs(a(i,j) - b(i,j))
            if (dif .ne. 0.0e0) then
               write (fout, *) '(',i,',',j,')', ': ',a(i,j),b(i,j)
            end if
            res = res + dif
         end do
      end do
      end function matchk2D























