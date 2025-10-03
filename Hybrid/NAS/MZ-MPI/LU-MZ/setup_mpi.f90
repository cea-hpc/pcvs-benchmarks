!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine setup_mpi
!
!  Set up MPI stuff, including
!     - define the active set of processes
!     - set up new communicator
!
      use lu_data
      use mpinpb
!
      implicit none
!
      integer no_nodes, color, iprov
!
! ... initialize MPI parameters
!      call mpi_init(ierror)
      call mpi_init_thread(MPI_THREAD_MULTIPLE, iprov, ierror)

      call mpi_comm_size(MPI_COMM_WORLD, no_nodes, ierror)
      call mpi_comm_rank(MPI_COMM_WORLD, myid, ierror)

      if (.not. convertdouble) then
         dp_type = MPI_DOUBLE_PRECISION
      else
         dp_type = MPI_REAL
      endif
      
!---------------------------------------------------------------------
!     let node 0 be the root for the group (there is only one)
!---------------------------------------------------------------------
      root = 0
!
      if (myid .ge. max_zones*(gx_size/x_zones)) then
         active = .false.
         color = 1
      else
         active = .true.
         color = 0
      end if
      
      call mpi_comm_split(MPI_COMM_WORLD,color,myid,comm_setup,ierror)
      if (.not. active) return

      call mpi_comm_size(comm_setup, num_procs, ierror)
      call mpi_comm_rank(comm_setup, myid, ierror)
      if (no_nodes .ne. num_procs) then
         if (myid .eq. root) write(*, 20) no_nodes, max_zones, num_procs
   20    format('Warning: Requested ',i6,' MPI processes exceeds',  &
     &          ' the number of zones ',i5/  &
     &          'The value ',i5,' is used for benchmarking')
      endif
!
! ... proc size that is a power of two and no less than num_procs
      num_procs2 = 1
      do while (num_procs2 .lt. num_procs)
         num_procs2 = num_procs2 * 2
      end do
!
      return
      end
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine env_thread_vars(mp)
!
!  Set up thread-related environment variables
!
! ... common variables
      use lu_data
      use mpinpb
!
      implicit none
!
      integer mp
!
! ... local variables
      integer ios
      character envstr*80
!
      call get_menv('OMP_NUM_THREADS', envstr, ios)
      if (ios .gt. 0 .and. mp .gt. 0) then
         read(envstr,*,iostat=ios) num_threads
         if (ios.ne.0 .or. num_threads.lt.1) num_threads = 1
!         if (mp .ne. num_threads) then
!            write(*, 10) num_threads, mp
!   10       format(' Warning: Requested ',i4,' threads per process,',
!     &             ' but the active value is ',i4)
!            num_threads = mp
!         endif
      else
         num_threads = 1
      endif
!
      call get_menv('NPB_MAX_THREADS', envstr, ios)
      max_threads = 0
      if (mz_bload.gt.0 .and. ios.gt.0) then
         read(envstr,*,iostat=ios) max_threads
         if (ios.ne.0 .or. max_threads.lt.0) max_threads = 0
         if (max_threads.gt.0 .and. max_threads.lt.num_threads) then
            write(*,20) max_threads, num_threads
   20       format(' Error: max_threads ',i5,  &
     &             ' is less than num_threads ',i5/  &
     &             ' Please redefine the value for NPB_MAX_THREADS',  &
     &             ' or OMP_NUM_THREADS')
            call error_cond( 0, ' ' )
         endif
      endif
!
      return
      end
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine env_setup(tot_threads)
!
!  Set up from environment variables
!
! ... common variables
      use lu_data
      use mpinpb
!
      implicit none
!
      integer tot_threads
!
! ... local variables
      integer ios, curr_threads, ip, mp, group, ip1, ip2
      integer, allocatable :: entry_counts(:)
      character envstr*80, line*132
!
!$    integer omp_get_max_threads
!$    external omp_get_max_threads
!
      if (myid .ne. root) goto 80
!
! ... test the OpenMP multi-threading environment
      mp = 0
!$    mp = omp_get_max_threads()
!
! ... master sets up parameters
      call get_menv('NPB_MZ_BLOAD', envstr, ios)
      mz_bload_erank = 1
      mz_bload = 1
      if (ios .gt. 0) then
         if (envstr.eq.'on' .or. envstr.eq.'ON') then
            mz_bload = 1
         else if (envstr(1:1).eq.'t' .or. envstr(1:1).eq.'T') then
            mz_bload = 1
         else if (envstr(1:5).eq.'erank' .or.  &
     &            envstr(1:5).eq.'ERANK') then
            if (envstr(6:6).eq.'0') mz_bload_erank = 0
            if (envstr(6:6).eq.'2') mz_bload_erank = 2
         else
            read(envstr,*,iostat=ios) mz_bload
            if (ios.ne.0) mz_bload = 0
            if (mz_bload .eq. 0) mz_bload_erank = 0
         endif
      endif
!
      call get_menv('NPB_VERBOSE', envstr, ios)
      npb_verbose = 0
      if (ios .gt. 0) then
         read(envstr,*,iostat=ios) npb_verbose
         if (ios.ne.0) npb_verbose = 0
      endif
!
      call env_thread_vars(mp)
!
      do ip = 1, num_zprocs
         proc_num_threads(ip) = num_threads
         proc_group(ip) = 0
      end do
!
      open(2, file='loadlu-mz.data', status='old', iostat=ios)
      if (ios.eq.0) then
         write(*,*) 'Reading load factors from loadlu-mz.data'

         if (mz_bload .ge. 1) then
            mz_bload = -mz_bload
         endif
         mz_bload_erank = 0

         allocate(entry_counts(num_zprocs), stat=ip)
         do ip = 1, num_zprocs
            entry_counts(ip) = 0
         end do

         ip1 = 0
         ip2 = num_zprocs - 1
         do while (.true.)
   25       read(2,'(a)',end=40,err=40) line
            if (line.eq.' ' .or. line(1:1).eq.'#') goto 25

            call decode_line(line, ip1, ip2, curr_threads, group, ios)
            if (ios .ne. 0) goto 40

            if (mz_bload .lt. 0 .and. group .gt. 0) then
               mz_bload = -mz_bload
            endif

            if (curr_threads .lt. 1) curr_threads = 1
            if (mp .le. 0) curr_threads = 1
            if (ip1.lt.0) ip1 = 0
            if (ip2.ge.num_zprocs) ip2 = num_zprocs - 1

            do ip = ip1+1, ip2+1
               proc_num_threads(ip) = curr_threads
               proc_group(ip) = group
               entry_counts(ip) = entry_counts(ip) + 1
            end do
            ip1 = ip2 + 1
            ip2 = num_zprocs - 1
         end do
   40    close(2)

         do ip = 1, num_zprocs
            if (entry_counts(ip) .eq. 0) then
               write(*,*) '*** Error: Missing entry for proc ',ip-1
               call error_cond( 0, ' ' )
            else if (entry_counts(ip) .gt. 1) then
               write(*,*) '*** Warning: Multiple entries for proc ',  &
     &                    ip-1, ', only the last one used'
            endif
         end do

         deallocate(entry_counts)
         ip1 = 1
      else
         write(*,*) 'Use the default load factors'
         ip1 = 0
         if (mz_bload_erank .gt. 0) then
            mz_bload = mp
            max_threads = 0
         endif
      endif
!
! ... broadcast parameters to all processes
   80 call mpi_bcast(mz_bload, 1, mpi_integer, root,  &
     &               comm_setup, ierror)
      call mpi_bcast(mz_bload_erank, 1, mpi_integer, root,  &
     &               comm_setup, ierror)
      call mpi_bcast(npb_verbose, 1, mpi_integer, root,  &
     &               comm_setup, ierror)
      call mpi_bcast(proc_group, num_zprocs, mpi_integer, root,  &
     &               comm_setup, ierror)
!
! ... if mz_bload_erank flag is set, we need to read thread flags 
!     from each rank; otherwise, broadcast the flags from rank 0
      if (mz_bload_erank .gt. 0) then
         mp = mz_bload
         mz_bload = 0
         if (mz_bload_erank .eq. 2) mz_bload = -2
         if (row .eq. 0) then
            if (myid .ne. root) call env_thread_vars(mp)
            call mpi_allgather(num_threads, 1, mpi_integer,  &
     &                         proc_num_threads, 1, mpi_integer,  &
     &                         comm_zpart, ierror)
         endif
         call mpi_bcast(proc_num_threads, num_zprocs, mpi_integer,  &
     &                  0, comm_ipart, ierror)
      else
         call mpi_bcast(num_threads, 1, mpi_integer, root,  &
     &                  comm_setup, ierror)
         call mpi_bcast(max_threads, 1, mpi_integer, root,  &
     &                  comm_setup, ierror)
         call mpi_bcast(proc_num_threads, num_zprocs, mpi_integer,  &
     &                  root, comm_setup, ierror)
      endif
!
      tot_threads = 0
      if (mp .gt. 0) then
         do ip = 1, num_zprocs
            tot_threads = tot_threads + proc_num_threads(ip)
         end do
      endif
!
      if (myid .ne. root) return
!
! ... print debug information
      if (npb_verbose .gt. 0) then
         ip1 = 0
         do ip = 1, num_zprocs
            if (ip .eq. 1 .or.  &
     &          proc_num_threads(ip) .ne. curr_threads .or.  &
     &          proc_group(ip) .ne. group) then

               ip2 = ip-2
               if (ip2 .gt. ip1+1) write(*,*) '    ...'
               if (ip2 .gt. ip1)  &
     &            write(*,30) ip2, curr_threads, group

               curr_threads = proc_num_threads(ip)
               group = proc_group(ip)

               ip1 = ip - 1
               write(*,30) ip1, curr_threads, group

            else if (ip .eq. num_zprocs) then
               ip2 = ip-1
               if (ip2 .gt. ip1+1) write(*,*) '    ...'
               write(*,30) ip2, curr_threads, group
            endif
         end do
   30    format('  zproc',i6,'  num_threads =',i5,  &
     &          '  group =',i5)
      endif
!
      if (tot_threads .gt. 0) then
         write(*, 1004) tot_threads*xdim, dble(tot_threads)/num_zprocs
      endif
 1004 format(' Total number of threads: ', i6,  &
     &       '  (', f5.1, ' threads/process)')
!
      return
      end
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine decode_line(line, ip1, ip2, curr_threads, group, ios)
      implicit none
!
!  decode a line from the load data file
!  format:  ip1[-ip2|:np] curr_threads group
!
      character line*(*)
      integer ip1, ip2, curr_threads, group, ios
!
      integer is, n
!
      ios = -1
!
      n  = len(line)
      is = 1
      do while (is.le.n .and. line(is:is).ne.'-' .and.  &
     &          line(is:is).ne.':')
         if (line(is:is).eq.'!') n = is
         is = is + 1
      end do
!
      if (is .gt. n) then	! single <proc#>
         read(line,*,err=90,end=90) ip1, curr_threads, group
         ip2 = ip1
      else			! range of procs
         if (is.gt.1) then	! keep previous value if no <ip1>
            read(line(:is-1),*,err=90,end=90) ip1
         endif
         if (is.eq.n) then	! no <ip2>, read the rest
            read(line(is+1:),*,err=90,end=90) curr_threads, group
         else
            read(line(is+1:),*,err=90,end=90) ip2, curr_threads, group
            if (line(is:is).eq.':') then
               if (ip2 .lt. 1) goto 90
               ip2 = ip1 + ip2 - 1
            endif
         endif
      endif
!
      if (ip2 .lt. ip1) then
         is  = ip2
         ip2 = ip1
         ip1 = is
      endif
      ios = 0
!
   90 return
      end
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine get_comm_index(zone, iproc, comm_index)
!
      use lu_data
      use mpinpb
!
      implicit none
!
!  Calculate the communication index of a zone within a processor group
!
      integer zone, iproc, comm_index
!
!     local variables
      integer izone, jzone 
!
      jzone  = (zone - 1)/x_zones + 1
      izone  = mod(zone - 1, x_zones) + 1
!
      comm_index = 0
      if (zone_proc_id(iz_west(zone)) .eq. iproc)  &
     &   comm_index = comm_index + y_size(jzone)
      if (zone_proc_id(iz_east(zone)) .eq. iproc)  &
     &   comm_index = comm_index + y_size(jzone)
      if (zone_proc_id(iz_south(zone)) .eq. iproc)  &
     &   comm_index = comm_index + x_size(izone)
      if (zone_proc_id(iz_north(zone)) .eq. iproc)  &
     &   comm_index = comm_index + x_size(izone)
!
      return
      end
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine map_zones(num_zones, nx, ny, nz, tot_threads)
!
!  Perform zone-process mapping for load balance
!
      use lu_data
      use mpinpb
!
      implicit none
!
      integer num_zones, nx(*), ny(*), nz(*), tot_threads
!
!     local variables
      integer z_order(max_zones)
      integer zone, iz, z2, mz, np, ip, zone_comm, comm_index
      integer n1, n2, work1, work2, np1, np2
      integer imx, imn, inc, icur_size
      double precision tot_size, cur_size, max_size, ave_size
      double precision zone_size(max_zones)
      double precision diff_ratio, tot_group_size
!
      integer group, ipg, tot_group_threads
      integer proc_group_flag(num_zprocs)
!
! ... sort the zones in decending order
      tot_size = 0.d0
      do iz = 1, num_zones
         zone_size(iz) = 1.0d0*nx(iz)*ny(iz)*nz(iz)
         z_order(iz) = iz
         tot_size = tot_size + zone_size(iz)
      end do
      do iz = 1, num_zones-1
         cur_size = zone_size(z_order(iz))
         mz = iz
         do z2 = iz+1, num_zones
            if (cur_size.lt.zone_size(z_order(z2))) then
               cur_size = zone_size(z_order(z2))
               mz = z2
            endif
         end do
         if (mz .ne. iz) then
            z2 = z_order(iz)
            z_order(iz) = z_order(mz)
            z_order(mz) = z2
         endif
      end do
!
      if (npb_verbose .gt. 1 .and. myid .eq. root) then
         write(*,10)
         do iz = 1, num_zones
            z2 = z_order(iz)
            write(*,15) iz,z2,nx(z2),ny(z2),nz(z2),zone_size(z2)
         end do
      endif
   10 format(/' Sorted zones:'/  &
     &       '  seq. zone    nx    ny    nz    size')
   15 format(i5,':',4(1x,i5),1x,f9.0)
!
! ... balance the load among processes
      do ip = 1, num_zprocs
         proc_zone_count(ip) = 0
         proc_zone_size(ip) = 0.d0
      end do

      if (abs(mz_bload).gt.1) goto 110
!
! ... try a simple block packing scheme
      n1 = 1
      n2 = num_zprocs
      work1 = mod(y_zones,num_zprocs)
      do while (n1 .le. n2)
         if (n1*n2 .eq. num_zprocs) then
            work2 = mod(x_zones,n1) + mod(y_zones,n2)
            if (work2 .le. work1) then
               work1 = work2
               np1 = n1
               np2 = n2
            endif
         endif
         n1 = n1 + 1
         n2 = num_zprocs / n1
      end do
! ... if can't find a good solution, fall back to next method
      if (work1 .ne. 0) goto 110
!
! ... amount of work for each block
      work1 = x_zones / np1
      work2 = y_zones / np2
!
      do iz = 1, num_zones
         n1 = mod(iz-1,x_zones) / work1
         n2 = (iz - 1) / x_zones / work2
         ip = n1 + n2*np1 + 1
!
!  ...   assign the zone to the current processor group
         zone_proc_id(iz) = ip - 1
         proc_zone_size(ip) = proc_zone_size(ip) + zone_size(iz)
         proc_zone_count(ip) = proc_zone_count(ip) + 1
      end do
!
      goto 150
!
! ... use a bin-packing scheme to balance the load among processes
  110 continue
      do iz = 1, num_zones
         zone_proc_id(iz) = -1
      end do

      iz = 1
      do while (iz .le. num_zones)
!
!  ...   the current most empty processor
         np = 1
         cur_size = proc_zone_size(1)
         do ip = 2, num_zprocs
            if (cur_size.gt.proc_zone_size(ip)) then
               np = ip
               cur_size = proc_zone_size(ip)
            endif
         end do
         ip = np - 1
!
!  ...   get a zone that has the largest communication index with
!        the current group and does not worsen the computation balance
         mz = z_order(iz)
         if (iz .lt. num_zones) then
            call get_comm_index(mz, ip, zone_comm)
            do z2 = iz+1, num_zones
               zone = z_order(z2)

               diff_ratio = (zone_size(z_order(iz)) -  &
     &                      zone_size(zone)) / zone_size(z_order(iz))
               if (diff_ratio .gt. 0.05D0) goto 120

               if (zone_proc_id(zone) .lt. 0) then
                  call get_comm_index(zone, ip, comm_index)
                  if (comm_index .gt. zone_comm) then
                     mz = zone
                     zone_comm = comm_index
                  endif
               endif
            end do
         endif
!
!  ...   assign the zone to the current processor group
  120    zone_proc_id(mz) = ip
         proc_zone_size(np) = proc_zone_size(np) + zone_size(mz)
         proc_zone_count(np) = proc_zone_count(np) + 1
!
!  ...   skip the previously assigned zones
         do while (iz.le.num_zones)
            if (zone_proc_id(z_order(iz)).lt.0) goto 130
            iz = iz + 1
         end do
  130    continue
      end do
!
! ... move threads around if needed
  150 mz = 1
      if (tot_threads.le.num_zprocs .or. mz_bload.lt.1) mz = 0
!
      if (mz .ne. 0) then
!
         do ipg = 1, num_zprocs
            proc_group_flag(ipg) = 0
         end do
!
         ipg = 1
!
! ...    balance load within a processor group
  200    do while (ipg .le. num_zprocs)
            if (proc_group_flag(ipg) .eq. 0) goto 210
            ipg = ipg + 1
         end do
  210    if (ipg .gt. num_zprocs) goto 300
!
         group = proc_group(ipg)
         tot_group_size = 0.d0
         tot_group_threads = 0
         do ip = ipg, num_zprocs
            if (proc_group(ip) .eq. group) then
               proc_group_flag(ip) = 1
               tot_group_size = tot_group_size + proc_zone_size(ip)
               tot_group_threads = tot_group_threads +  &
     &                             proc_num_threads(ip)
            endif
         end do
!
         ave_size = tot_group_size / tot_group_threads
!
!  ...   distribute size evenly among threads
         icur_size = 0
         do ip = 1, num_zprocs
            if (proc_group(ip) .ne. group) goto 220
            proc_num_threads(ip) = proc_zone_size(ip) / ave_size
            if (proc_num_threads(ip) .lt. 1)  &
     &          proc_num_threads(ip) = 1
            if (max_threads .gt. 0 .and.  &
     &          proc_num_threads(ip) .gt. max_threads)  &
     &          proc_num_threads(ip) = max_threads
            icur_size = icur_size + proc_num_threads(ip)
  220       continue
         end do
         mz = tot_group_threads - icur_size
!
!  ...   take care of any remainers
         inc = 1
         if (mz .lt. 0) inc = -1
         do while (mz .ne. 0)
            max_size = 0.d0
            imx = 0
            do ip = 1, num_zprocs
               if (proc_group(ip) .ne. group) goto 230
               if (mz .gt. 0) then
                  cur_size = proc_zone_size(ip) / proc_num_threads(ip)
                  if (cur_size.gt.max_size .and. (max_threads.le.0  &
     &                .or. proc_num_threads(ip).lt.max_threads)) then
                     max_size = cur_size
                     imx = ip
                  endif
               else if (proc_num_threads(ip) .gt. 1) then
                  cur_size = proc_zone_size(ip) /  &
     &                       (proc_num_threads(ip)-1)
                  if (max_size.eq.0 .or. cur_size.lt.max_size) then
                     max_size = cur_size
                     imx = ip
                  endif
               endif
  230          continue
            end do
            proc_num_threads(imx) = proc_num_threads(imx) + inc
            mz = mz - inc
         end do
!
         goto 200
      endif
!
! ... print the mapping
  300 if (npb_verbose .gt. 0 .and. myid .eq. root) then
         write(*,20)
         do ip = 1, num_zprocs
            write(*,25) ip-1,proc_zone_count(ip),  &
     &            proc_zone_size(ip),proc_num_threads(ip),  &
     &            proc_zone_size(ip)/proc_num_threads(ip)
            do iz = 1, num_zones
               if (zone_proc_id(iz) .eq. ip-1) then
                  write(*,30) iz, zone_size(iz)
               endif
            end do
         end do
      endif
   20 format(/' Zone-process mapping:'/  &
     &       ' zproc  nzones  zone_size nthreads size_per_thread')
   25 format(i6,2x,i5,2x,f10.0,2x,i5,3x,f10.0)
   30 format(3x,'zone',2x,i5,2x,f9.0)
!
      if (myid .eq. root) then
         imx = 1
         max_size = proc_zone_size(1)/proc_num_threads(1)
         imn = imx
         ave_size = max_size
         do ip = 2, num_zprocs
            cur_size = proc_zone_size(ip)/proc_num_threads(ip)
            if (cur_size.gt.max_size) then
               imx = ip
               max_size = cur_size
            endif
            if (cur_size.lt.ave_size) then
               imn = ip
               ave_size = cur_size
            endif
         end do

         if (npb_verbose .gt. 0) then
            write(*,*)
            write(*,35) 'Max', imx-1, proc_zone_count(imx),  &
     &                  proc_zone_size(imx),proc_num_threads(imx)
            write(*,35) 'Min', imn-1, proc_zone_count(imn),  &
     &                  proc_zone_size(imn),proc_num_threads(imn)
         endif
   35    format(1x,a,': zproc=',i6,' nzones=',i5,' size=',f10.0,  &
     &          ' nthreads=',i5)

         if (xdim .gt. 1) then
            z2 = z_order(1)
            cur_size = (nx(z2) + xdim - 1) / xdim
            max_size = max_size * cur_size / nx(z2)
         endif
         write(*,40) tot_size / max_size
      endif
   40 format(/' Calculated speedup = ',f9.2/)
!
! ... reorganize list of zones for this process
      zone = 0
      do iz = 1, num_zones
         if (zone_proc_id(iz) .eq. col) then
            zone = zone + 1
            proc_zone_id(zone) = iz
         endif
      end do
      proc_num_zones = zone
      if (zone .ne. proc_zone_count(col+1)) then
         write(*,*) 'Warning: ',myid, ': mis-matched zone counts -',  &
     &              zone, proc_zone_count(col+1)
      endif
!
! ... set number of threads for this process
      group = proc_group(col+1)
      np = 0
      do ip = 1, num_zprocs
         if (proc_group(ip) .eq. group) then
            proc_group(ip) = np
            np = np + 1
            proc_num_threads(np) = proc_num_threads(ip)
         endif
      end do
      ipg = proc_group(col+1)
      if (npb_verbose.gt.1) then
         write(*,50) myid, group, np, ipg, proc_num_threads(ipg+1)
      endif
   50 format(' myid',i6,' group',i5,' group_size',i5,  &
     &       ' group_pid',i5,' threads',i4)
!$    call omp_set_num_threads(proc_num_threads(ipg+1))
!
! ... pin-to-node within one process group
!      call smp_pinit_thread(np, ipg, proc_num_threads)
!
      return
      end
