!     $Id: matrix3D.f,v 1.6 1998/02/16 13:52:29 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     matrix3D
!     Release 1.0
!
!     Description:
!     matrix3D IO test subroutine
!
!     Interface:
!     call iot_wrap_matrix3D(comm, node, nprocs, datafilename, info,
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
!     iot_run_matrix3D()  :
!        Subroutine to perform single test run.
!        Parameters specific to test.
!     iot_check_matrix3D() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_matrix3D() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_matrix3D

!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 19
      integer,parameter :: IOT_STAT_FILESIZE = 1
      integer,parameter :: IOT_STAT_BLOCKSIZE = 2
      integer,parameter :: IOT_STAT_XSIZE = 3
      integer,parameter :: IOT_STAT_YSIZE = 4
      integer,parameter :: IOT_STAT_ZSIZE = 5
      integer,parameter :: IOT_STAT_XPROC = 6
      integer,parameter :: IOT_STAT_YPROC = 7
      integer,parameter :: IOT_STAT_ZPROC = 8
      integer,parameter :: IOT_STAT_BLANK = 9
      integer,parameter :: IOT_STAT_PRE = 10
      integer,parameter :: IOT_STAT_POST = 11
      integer,parameter :: IOT_STAT_PALLOC = 12
      integer,parameter :: IOT_STAT_SYNC = 13
      integer,parameter :: IOT_STAT_FOPEN = 14
      integer,parameter :: IOT_STAT_FCLOSE = 15
      integer,parameter :: IOT_STAT_FVIEW = 16
      integer,parameter :: IOT_STAT_WRITE = 17
      integer,parameter :: IOT_STAT_READ = 18
      integer,parameter :: IOT_STAT_TOTAL = 19


!     keyword strings
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize      ", &
       "blocksize     ", &
       "xsize         ", &      
       "ysize         ", &
       "zsize         ", &
       "xproc         ", &      
       "yproc         ", &      
       "zproc         ", &      
       "              ", &      
       "pre_time      ", &
       "post_time     ", &
       "palloc_time   ", &
       "sync_time     ", &
       "fopen_time    ", &
       "fclose_time   ", &
       "fview_time    ", &
       "write         ", &
       "read          ", &
       "total_time    " &
       /)

      end module iostat_matrix3D

!-----------------------------------------------------------------------
!     iot_run_matrix3D
!
!     individual test code
!        The parameter list varies depending on the indiviual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_matrix3D.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!-----------------------------------------------------------------------

      subroutine iot_run_matrix3D(comm, node, nprocs, &
           datafilename, info, &
           Nx, Ny, Nz, &
           xproc, yproc, zproc, &
           palloc, collective, ierr)
      use iotest
      use iostat_matrix3D
      implicit none

      integer :: comm, node, nprocs, info, ierr
      character(len=*) :: datafilename
      integer :: Nx, Ny, Nz
      integer :: xproc, yproc, zproc
      logical :: palloc, collective

!     external functions
      real(kind=DBL) :: matchk3D


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

      integer :: pNx, pNy, pNz
      integer :: i, j, k
      integer :: pi, pj, pk
      integer :: xi, yi, zi
      integer(kind=IDBL) :: totsize
      real(kind=DBL) :: res, eps
      real(kind=DBL) :: rand

!     distributed matrix data
      real(kind=DBL),dimension(:,:,:),allocatable :: a, b


!    time
      tinit = MPI_Wtime()
      told = tinit

!    Work out sizes on each processor
      call mpi_type_size(etype, etypesize, ierr)
      pNx = Nx/xproc
      pNy = Ny/yproc
      pNz = Nz/zproc
      totsize = int(nprocs*pNx*pNy*pNz*etypesize,IDBL)
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
      call create_contig_type(node, pNz, etype, coltype)
      call create_submat3D_type(pNx, pNy, pNz, xproc, yproc, zproc, &
         node, etype, submattype)
      call mpi_file_set_view(datafilepointer, offset_zero, etype, &
         submattype, datarep, info, ierr)
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)


!    allocate and initialise distributed arrays
!    arrange so that the values mimic those from the global array
!    The order of x,y is indeed intended
      allocate (a(pNz,pNy,pNx),b(pNz,pNy,pNx)) 
!    should call random_seed (put=rseed)   to be different on repeats
      a(1,1,1) = iotrand(IOT_RSEED)
!    Node order follows Fortran order in this, 3D, case.
      xi = (node/(yproc*zproc)) * pNx
      yi = modulo((node/zproc),yproc) * pNy
      zi = modulo(node,zproc) * pNz

!      write (fout, *) node, xi,yi,zi
!      call flush(fout)

      do k=1,Nx
         do j=1,Ny
            do i=1,Nz
               rand = iotrand(IOT_RSEED)
               if (   (k>xi).and.(k<=xi+pNx) &
                 .and.(j>yi).and.(j<=yi+pNy) &
                 .and.(i>zi).and.(i<=zi+pNz)) then
                 pi = modulo(i,pNz)
                 if (pi == 0) pi = pNz
                 pj = modulo(j,pNy)
                 if (pj == 0) pj = pNy
                 pk = modulo(k,pNx)
                 if (pk == 0) pk = pNx
                 a(pi,pj,pk) = node + rand
                 b(pi,pj,pk) = 0.0
              endif
            enddo
         enddo
      enddo

!    write out matrix if debugging
!      if (debug) then
!         do k = 1, pNx
!            do j = 1, pNy
!               do i = 1, pNz
!                  write (fout, *) node,'(',i,',',j,',',k,')', ': ',a(i,j,k)
!               end do
!            end do
!         end do
!         call flush(fout)
!      end if


      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)
      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()

!     write data to file
!    collective or non-collective versions
      if (collective) then
         call mpi_file_write_all(datafilepointer, a, pNx*pNy, coltype, &
           status, ierr)
      else
         call mpi_file_write(datafilepointer, a, pNx*pNy, coltype, &
           status, ierr)
      endif

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
      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()


!    Reset filepointer 
      call mpi_file_set_view(datafilepointer, offset_zero, etype, &
        submattype, datarep, info, ierr)

!    read distributed data from file
      if (collective) then
         call mpi_file_read_all(datafilepointer, b, pNx*pNy, coltype, &
           status, ierr)
      else
         call mpi_file_read(datafilepointer, b, pNx*pNy, coltype, &
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

!     compare matrix
      res = matchk3D(pNx, pNy, pNz, a, b)
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

      end subroutine iot_run_matrix3D


!-----------------------------------------------------------------------
!     iot_wrap_matrix3D
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_matrix3D.
!        This subroutine calls iot_check_matrix3D and iot_test_matrix3D
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_matrix3D(comm, node, nprocs, &
           datafilename, info, statfile, ierr)
      use iotest
      use iostat_matrix3D
      implicit none

      integer :: comm, node, nprocs, info, statfile, ierr
      character(len=*) :: datafilename

      integer :: numsizes, numprocgrids
      integer,dimension(:),allocatable :: xproc, yproc, zproc
      integer,dimension(:),allocatable :: xsize, ysize, zsize

      integer :: nx, ny, nz
      integer :: xc, yc, zc, xp, yp, zp, xb, yb, zb
      integer :: i, j, k, p
      integer :: repeat
      integer :: statsize, paramsize
      logical :: palloc, collective, reorder, period(3)
      integer :: dim(3), coord(3), grid



      call time_stamp(node, statfile, ierr)

      paramsize = 20
      allocate(xsize(paramsize),ysize(paramsize),zsize(paramsize))
      allocate(xproc(paramsize),yproc(paramsize),zproc(paramsize))
      call iot_check_matrix3D(numsizes, xsize, ysize, zsize, &
         numprocgrids, xproc, yproc, zproc, &
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
         zp = zproc(p)

!       create cartesian topology
         dim(1) = xp
         dim(2) = yp
         dim(3) = zp
         period(1) = .false.
         period(2) = .false.
         period(3) = .false.
!       NO reordering. this may be suboptimal on some architectures.
!       in that case run different size grids as separate runs?
         reorder = .false.
         call mpi_cart_create(comm, 3, dim, period, reorder, grid, ierr)
         if (ierr  /= MPI_SUCCESS) then
            write(fout,*) "ERROR: cannot create process grid"
            write(fout,*) "Skipping process grid ",xp," ",yp," ",zp,"."
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
            nx = xsize(k)
            ny = ysize(k)
            nz = zsize(k)

!          repeat
            do i = 1, repeat

!             save input parameters
               call save_stat(IOT_STAT_XSIZE, real(nx,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_YSIZE, real(ny,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_ZSIZE, real(nz,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_XPROC, real(xp,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_YPROC, real(yp,DBL), IOT_INTEGER, ierr)
               call save_stat(IOT_STAT_ZPROC, real(zp,DBL), IOT_INTEGER, ierr)

               call iot_run_matrix3D(grid, node, xp*yp*zp, &
                 datafilename, info, &
                 nx, ny, nz, xp, yp, zp, &
                 palloc, collective, ierr)
               if (ierr /= IOT_SUCCESS) then
                  write (fout,*) "ERROR: Error in matrix3D test"
               end if

               call save_stat(IOT_STAT_BLANK, IOT_STAT_EMPTY, &
                 IOT_INTEGER, ierr)

            end do              ! repeat loop
         end do                 ! matrix3D size loop

!     write statistics information to file
         call write_stat(grid, node, xp*yp*zp, statfile, &
           datafilename, info, statkey, ierr)

!     free storage for timing information
         call free_stat(ierr)

!     free cartesian grid 
         call mpi_comm_free(grid, ierr)

      end do ! process grid loop

!     deallocate temporary arrays
      deallocate(xsize, ysize, zsize)
      deallocate(xproc, yproc, zproc)

      call time_stamp(node, statfile, ierr)

      end subroutine

!-----------------------------------------------------------------------
!     iot_check_matrix3D
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_matrix3D.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!-----------------------------------------------------------------------

      subroutine iot_check_matrix3D(numsizes, xsize, ysize, zsize, &
         numprocgrids, xproc, yproc, zproc, &
         repeat, palloc, collective, ierr)
      use iotest
      implicit none

      integer :: nprocs, numsizes, numprocgrids, ierr
!     should be : not * in following line but BUGGY
      integer,dimension(*) ::  xsize, ysize, zsize, xproc, yproc, zproc
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
         if (len(ios(IOT_KEY_ZSIZE)) /= 0) then
            read (unit=ios(IOT_KEY_ZSIZE)%s, fmt=*, iostat=err) &
               (zsize(i),i=1,numsizes)
            if (err /= 0) return
         else 
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
         if (len(ios(IOT_KEY_ZPROC)) /= 0) then
            read (unit=ios(IOT_KEY_ZPROC)%s, fmt=*, iostat=err) &
               (zproc(i),i=1,numprocgrids)
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

      end subroutine




!-----------------------------------------------------------------------    
!     compare 2 matrices
!-----------------------------------------------------------------------    

      function matchk3D(Nx, Ny, Nz, a, b) result (res)
      use iotest
      implicit none

      integer :: Nx, Ny, Nz, i, j, k
      real(kind=DBL) :: a(Nz,Ny,Nx), b(Nz,Ny,Nx), res, dif
      res = 0.0e0
      do k = 1, Nx
         do j = 1, Ny
            do i = 1, Nz
               dif = dabs(a(i,j,k) - b(i,j,k))
               if (dif .ne. 0.0e0) then
                  write (fout, *) '(',i,',',j,',',k,')', ': ',a(i,j,k),b(i,j,k)
               end if
               res = res + dif
            end do
         end do
      end do
      end function matchk3D























