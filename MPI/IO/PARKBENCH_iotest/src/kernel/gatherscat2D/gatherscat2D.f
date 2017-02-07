!     $Id: gatherscat2D.f,v 1.8 1998/02/16 13:52:58 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     gatherscat2D
!     Release 1.0
!
!     Description:
!     gatherscat2D IO test subroutine
!
!     Interface:
!     call iot_wrap_gatherscat2D(comm, node, nprocs, datafilename, info,
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
!     iot_run_gatherscat2D()  :
!        Subroutine to perform single test run.
!        Parameters specific to test.
!     iot_check_gatherscat2D() :
!        Subroutine to check user parameters in ios structure.
!        Parameters specific to test.
!     iot_wrap_gatherscat2D() : 
!        Wrapper which calls parameter check subroutine and loops to perform 
!        calls to the test subroutine.
!        Parameters same for all tests.
!-----------------------------------------------------------------------

!     data structures for timing stats etc
!     specific to each test routine
      module iostat_gatherscat2D

!     keywords
      integer,parameter :: IOT_NUM_STATKEYS = 20
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
      integer,parameter :: IOT_STAT_GATHER = 17
      integer,parameter :: IOT_STAT_SCATTER = 18
      integer,parameter :: IOT_STAT_DTYPE = 19
      integer,parameter :: IOT_STAT_TOTAL = 20


!     keyword strings
      character(len=*),parameter,dimension(IOT_NUM_STATKEYS) :: &
        statkey = (/ &
       "filesize      ", &
       "blocksize     ", &
       "xsize         ", &      
       "ysize         ", &
       "xproc         ", &      
       "yproc         ", &      
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
       "gather        ", &
       "scatter       ", &
       "dtype_time    ", &
       "total_time    " &
       /)

      end module iostat_gatherscat2D

!-----------------------------------------------------------------------
!     iot_run_gatherscat2D
!
!     individual test code
!        The parameter list varies depending on the indiviual test.
!        Call test subroutine from individual test wrapper, 
!        eg iot_wrap_gatherscat2D.
!        This subroutine performs 1 actual test using the 
!        subroutine parameters as test parameters.
!-----------------------------------------------------------------------


      subroutine iot_run_gatherscat2D(comm, node, nprocs, &
           datafilename, info, &
           Nx, Ny, &
           xproc, yproc, &
           palloc, collective, ierr)
      use iotest
      use iostat_gatherscat2D
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
      integer :: simplemattype, submattype
      integer :: etypesize
      integer, dimension(nprocs) :: rcounts, displs
      integer(kind=8) :: lb, extent
      integer, dimension(nprocs) :: scnt, sdisp, sdtype
      integer, dimension(nprocs) :: rcnt, rdisp, rdtype

!     timing variables
      real(kind=DBL) ::  told, tnew, tdiff, tpre, tinit

!     general variables
      integer :: pNx, pNy
      integer :: i, j
      integer :: pi, pj
      integer :: xi, yi
      integer(kind=IDBL) :: totsize
      real(kind=DBL), parameter :: junk = 0.0
      real(kind=DBL) :: res, eps
      real(kind=DBL) :: rand

!     distributed matrix data
      real(kind=DBL),dimension(:,:),allocatable :: a, b, c, fulla, fullb



!    time
      tinit = MPI_Wtime()
      told = tinit


!    Work out sizes on each processor
      call mpi_type_size(etype, etypesize, ierr)
      pNx = Nx/xproc
      pNy = Ny/yproc
      totsize = int(nprocs*pNx*pNy*etypesize,IDBL)
      call save_stat(IOT_STAT_FILESIZE, real(totsize,DBL), IOT_INTEGER, ierr)

      call mpi_barrier(comm, ierr)


!   open MPI-I/O data file on the root node
      if ( node == 0) then
         call mpi_file_delete(datafilename, MPI_INFO_NULL, ierr)
         dmode = MPI_MODE_CREATE + MPI_MODE_RDWR
         tpre = MPI_Wtime()
         call MPI_file_open(MPI_COMM_SELF, datafilename, dmode, &
           info, datafilepointer, ierr)
         if (ierr /= MPI_SUCCESS) then
            call prtMPIerr(ierr)
            write (fout,*) "Error opening data file for writing", &
              datafilename, "node (MPI) ",node
            ierr = IOT_ERR_MPI
            return
         end if
         tnew = MPI_Wtime()
         tdiff = tnew - tpre
         tpre = tnew
         call save_stat(IOT_STAT_FOPEN, tdiff, IOT_REAL, ierr)
      
         call mpi_file_set_view(datafilepointer, offset_zero, etype, &
           etype, datarep, info, ierr)
         if (ierr /= MPI_SUCCESS) then
            call iotMPIerr(node, ierr, "Error in file_set_view")
!              deallocate (a,b)
            return
         end if
         tdiff = MPI_Wtime() - tpre
         call save_stat(IOT_STAT_FVIEW, tdiff, IOT_REAL, ierr)
      else
         call save_stat(IOT_STAT_FOPEN, junk, IOT_REAL, ierr)
         call save_stat(IOT_STAT_FVIEW, junk, IOT_REAL, ierr)
      end if

      call mpi_barrier(comm, ierr)

!    preallocation
      if (palloc) then
         if ( node == 0) then
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
         else
            call save_stat(IOT_STAT_PALLOC, junk, IOT_REAL, ierr)
         endif
      endif


!    allocate and initialise distributed arrays
!    arrange so that the values mimic those from the global array
!    The order of x,y is indeed intended
      allocate (a(pNy,pNx)) 
      allocate (c(pNy,pNx)) 
      allocate (b(Ny,Nx)) 
!    call random_seed (put=rseed)   to be different on repeats
      a(1,1) = iotrand(IOT_RSEED)
      xi = modulo(node,xproc) * pNx
      yi = (node/xproc) * pNy
      do j=1,Nx
         do i=1,Ny
            rand = iotrand(IOT_RSEED)
            if (    (j>xi).and.(j<=xi+pNx) &
               .and.(i>yi).and.(i<=yi+pNy)) then
               pi = modulo(i,pNy)
               if (pi == 0) pi = pNy
               pj = modulo(j,pNx)
               if (pj == 0) pj = pNx
               a(pi,pj) = node + rand
            endif
            b(i,j) = rand
         enddo
      enddo
      if (node == 0) then
         allocate (fulla(pNy*yproc,pNx*xproc)) 
         allocate (fullb(pNy*yproc,pNx*xproc)) 
      endif


!    Write out matrix if debugging
!      if (debug) then
!         do j=1,pNx
!            do i=1,pNy
!               write(fout,*) node,i,j,a(i,j)
!            enddo
!         enddo
!         write(fout,*) ""
!         call flush(fout)
!      end if

!    Prepare datatypes for gathering 
      tpre = MPI_Wtime()
      call create_contig_type(node, pNx*pNy, etype, simplemattype)
      call create_submat2D_type(pNx, pNy, xproc, yproc, &
         node, etype, submattype)
      tdiff = MPI_Wtime() - tpre
      call save_stat(IOT_STAT_DTYPE, tdiff, IOT_REAL, ierr)

!    Prepare parameters for gather using alltoallw
      if (node == 0) then
         do i = 1,nprocs
            scnt(i) = 0
            sdisp(i) = 0
            sdtype(i) = simplemattype
            rcnt(i) = 1
            rdisp(i) = (int((i-1)/xproc) + modulo(i-1,xproc)*pNx*yproc)&
              *pNy*etypesize
            rdtype(i) = submattype
         end do
         scnt(1) = 1        
      else
         do i = 1,nprocs
            scnt(i) = 0
            sdisp(i) = 0
            sdtype(i) = simplemattype
            rcnt(i) = 0
            rdisp(i) = 0
            rdtype(i) = submattype
         end do
         scnt(1) = 1         
      end if


      call mpi_barrier(comm, ierr)
      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_PRE, tdiff, IOT_REAL, ierr)

!------------------------------------
!    gather the data
!------------------------------------

      call mpi_alltoallw(a, scnt, sdisp, sdtype, &
         fulla, rcnt, rdisp, rdtype, comm, ierr)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_GATHER, tdiff, IOT_REAL, ierr)


!    Write out full matrix on node 0 if debugging
!      if (debug) then
!         write(fout,*) ""
!         if (node == 0) then
!            do j=1,Nx
!               do i=1,Ny
!                  write(fout,*) i,j,fulla(i,j),b(i,j)
!               enddo
!            enddo
!            call flush(fout)
!         endif
!      endif


!------------------------------------
!    write the data from node 0
!------------------------------------
      if ( node == 0) then
         if (collective) then
            call mpi_file_write_all(datafilepointer, fulla, &
              nprocs*pNx*pNy, etype, status, ierr)
         else
            call mpi_file_write(datafilepointer, fulla, &
              nprocs*pNx*pNy, etype, status, ierr)
         endif

         if (ierr /= MPI_SUCCESS) then
            call iotMPIerr(node, ierr, "Error writing to file")
!              deallocate (a, b)
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
!       May add something here to try and flush the cache 


!       Reset file pointer to zero
         call mpi_file_set_view(datafilepointer, offset_zero, etype, &
             etype, datarep, info, ierr)


!------------------------------------
!      read distributed data
!------------------------------------

         told = MPI_Wtime()
         if (collective) then
            call mpi_file_read_all(datafilepointer, fullb, & 
              nprocs*pNx*pNy, etype, status, ierr)
         else
            call mpi_file_read(datafilepointer, fullb, & 
              nprocs*pNx*pNy, etype, status, ierr)
         endif

         if (ierr /= MPI_SUCCESS) then
            call iotMPIerr(node, ierr, "Error reading from file")
!              deallocate (a, b)
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
         tdiff = MPI_Wtime() - told
         call save_stat(IOT_STAT_FCLOSE, tdiff, IOT_REAL, ierr)


!       compare matrix
         res = matchk2D(Nx, Ny, fulla, fullb)
         if (res /= 0.0e0) then
            write (fout,*) "error in saved matrix"
         endif
         
      else
!       Fill in stats on other nodes
         call save_stat(IOT_STAT_WRITE, junk, IOT_REAL, ierr)
         call save_stat(IOT_STAT_SYNC, junk, IOT_REAL, ierr)
         call save_stat(IOT_STAT_READ, junk, IOT_REAL, ierr)
         call save_stat(IOT_STAT_FCLOSE, junk, IOT_REAL, ierr)
      endif



!    Prepare parameters for scatter using alltoallw
      if (node == 0) then
         do i = 1,nprocs
            rcnt(i) = 0
            rdisp(i) = 0
            rdtype(i) = simplemattype
            scnt(i) = 1
            sdisp(i) = (int((i-1)/xproc) + modulo(i-1,xproc)*pNx*yproc)&
              *pNy*etypesize
            sdtype(i) = submattype
         end do
         rcnt(1) = 1        
      else
         do i = 1,nprocs
            rcnt(i) = 0
            rdisp(i) = 0
            rdtype(i) = simplemattype
            scnt(i) = 0
            sdisp(i) = 0
            sdtype(i) = submattype
         end do
         rcnt(1) = 1         
      end if

      call mpi_barrier(comm, ierr)
      told = MPI_Wtime()


!------------------------------------
!    scatter the data
!------------------------------------
      call mpi_alltoallw(fullb, scnt, sdisp, sdtype, &
         c, rcnt, rdisp, rdtype, comm, ierr)


      tnew = MPI_Wtime()
      tdiff = tnew - told
      told = tnew
      call save_stat(IOT_STAT_SCATTER, tdiff, IOT_REAL, ierr)

!    compare submatrices
      res = matchk2D(pNx, pNy, a, c)
      if (res /= 0.0e0) then
         write (fout,*) "error in scattered matrix"
      endif



!    deallocate arrays
      deallocate (a,b,c)
      if (node == 0) then
         deallocate (fulla,fullb)
      endif

!    free types
      call mpi_type_free(submattype, ierr)
      call mpi_type_free(simplemattype, ierr)

      tnew = MPI_Wtime()
      tdiff = tnew - told
      call save_stat(IOT_STAT_POST, tdiff, IOT_REAL, ierr)
      tdiff = tnew - tinit
      call save_stat(IOT_STAT_TOTAL, tdiff, IOT_REAL, ierr)

      end subroutine iot_run_gatherscat2D


!-----------------------------------------------------------------------
!     iot_wrap_gatherscat2D
!
!     individual test wrapper subroutine
!        This has a fixed parameter list and should be called from 
!        a program wrapper in src eg. iot_gatherscat2D.
!        This subroutine calls iot_check_gatherscat2D and iot_test_gatherscat2D
!        Change parameters to check and test subroutines and
!        change loop construct.
!-----------------------------------------------------------------------

      subroutine iot_wrap_gatherscat2D(comm, node, nprocs, &
           datafilename, info, statfile, ierr)
      use iotest
      use iostat_gatherscat2D
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
      call iot_check_gatherscat2D(numsizes, xsize, ysize, &
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
         statsize = IOT_NUM_STATKEYS*numsizes*repeat + 40
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

               call iot_run_gatherscat2D(grid, node, xp*yp, &
                 datafilename, info, &
                 m, n, xp, yp, &
                 palloc, collective, ierr)
               if (ierr /= IOT_SUCCESS) then
                  write (fout,*) "ERROR: Error in gatherscat2D test"
               end if

               call save_stat(IOT_STAT_BLANK, IOT_STAT_EMPTY, &
                 IOT_INTEGER, ierr)

            end do              ! repeat loop
         end do                 ! gatherscat2D size loop

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
!     iot_check_gatherscat2D
!
!     individual test parameter checking subroutine
!        The parameter list varies depending on the indiviual test.
!        Call check subroutine from individual test wrapper, 
!        eg iot_wrap_gatherscat2D.
!        This subroutine should extract test parameters from the 
!        ios data structure and set the subroutine output parameters.
!-----------------------------------------------------------------------

      subroutine iot_check_gatherscat2D(numsizes, xsize, ysize, &
         numprocgrids, xproc, yproc, &
         repeat, palloc, collective, ierr)
      use iotest
      implicit none

      integer :: nprocs, numsizes, numprocgrids, ierr
!     should be : not * in following line but BUGGY
      integer,dimension(*) ::  xsize, ysize, xproc, yproc
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

      end subroutine iot_check_gatherscat2D



























