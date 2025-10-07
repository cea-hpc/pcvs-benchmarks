!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine setup_omp(num_zones, nx, ny, nz, tot_threads)
!
!  Set up OMP related work, including
!     - zone-othread mapping for load balance
!     - set up number of threads
!
      use bt_data
      use ompnpb
!
      implicit none
!
      integer num_zones, nx(*), ny(*), nz(*), tot_threads
!
      integer nthreads
!
! ... map zones to outer-level OpenMP threads
      call map_zones(num_zones, nx, ny, nz, tot_threads)
!
! ... define number of outer-level threads
!$    call omp_set_dynamic(.false.)
      nthreads = num_othreads
      if (nested.eq.2) nthreads = num_threads
!$    call omp_set_num_threads(nthreads)
!
      return
      end
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine init_omp(num_zones,proc_zone_id,proc_num_zones)
!
!  Set up additional OMP related work
!
      use bt_data
      use ompnpb
!
      implicit none
!
      integer num_zones,proc_zone_id(*),proc_num_zones
!
      integer zone_count, iz
!$    integer omp_get_thread_num
!$    external omp_get_thread_num
!
! ... info for current thread
      myid = 0
!$    myid = omp_get_thread_num()
      root = 0
!
! ... reorganize list of zones for this outer thread
      zone_count = 0
      do iz = 1, num_zones
         if (zone_proc_id(iz) .eq. myid) then
            zone_count = zone_count + 1
            proc_zone_id(zone_count) = iz
         endif
      end do
      proc_num_zones = zone_count
      if (zone_count .ne. proc_zone_count(myid+1)) then
         write(*,*) 'Warning: ',myid, ': mis-matched zone counts -',  &
     &           zone_count, proc_zone_count(myid+1)
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
      use bt_data
      use ompnpb
!
      implicit none
!
      integer tot_threads
!
! ... local variables
      integer ios, curr_threads, ip, mp, group, ip1, ip2
      integer entry_counts(max_zones)
      character envstr*80, line*132
      logical nflag
!
!$    integer omp_get_max_threads
!$    logical omp_get_nested
!$    external omp_get_max_threads, omp_get_nested
!
! ... test the OpenMP multi-threading environment
      mp = 0
!$    mp = omp_get_max_threads()
!
      call get_menv('OMP_NUM_THREADS', envstr, ios)
      if (ios .gt. 0 .and. mp .gt. 0) then
         read(envstr,*,iostat=ios) num_othreads
         if (ios.ne.0 .or. num_othreads.lt.1) num_othreads = 1
         if (mp .ne. num_othreads) then
            write(*, 10) num_othreads, mp
   10       format(' Warning: Requested ',i4,' outer-level threads,',  &
     &             ' but the active value is ',i4)
            num_othreads = mp
         endif
         read(envstr,*,iostat=ios) ip, num_threads
         if (ios.ne.0 .or. num_threads.lt.1) num_threads = 1
      else
         num_othreads = 1
         num_threads = 1
      endif
!
      if (num_othreads .gt. max_zones) then
         write(*, 15) num_othreads, max_zones
   15    format(' Error: num_othreads ',i5,  &
     &          ' exceeded max_allowed ',i5/  &
     &          ' Please redefine the value for OMP_NUM_THREADS')
         stop
      endif
!
! ... no limit on how many inner-level threads we can use
      max_threads = 0
!
! ... check nested-par support
      call get_menv('NPB_OMP_NESTED', envstr, ios)
      if (ios .gt. 0 .and. mp .gt. 0) then
         read(envstr,*,iostat=ios) nested
         if (ios.ne.0 .or. nested.lt.0) nested = 0
      else
         nested = 0
      endif
      if (nested.eq.2) num_othreads = 1
      if (nested.eq.1) num_threads = 1
!
      if (nested.eq.1 .or. nested.eq.2) then
!$       call omp_set_nested(.false.)
      else
!$       call omp_set_nested(.true.)
         nflag = .true.
!$       nflag = omp_get_nested()
         if ((.not.nflag) .and. num_threads.gt.1) then
            if (nested.eq.3) then
               write(*,20) ' on the system'
            else
               write(*,20) ', inner-level threads reset to one'
               num_threads = 1
            endif
   20       format(' *** Nested OpenMP not supported', a)
         endif
      endif
!
      call get_menv('NPB_MZ_BLOAD', envstr, ios)
      if (ios .gt. 0) then
         if (envstr.eq.'on' .or. envstr.eq.'ON') then
            mz_bload = 1
         else if (envstr(1:1).eq.'t' .or. envstr(1:1).eq.'T') then
            mz_bload = 1
         else
            read(envstr,*,iostat=ios) mz_bload
            if (ios.ne.0) mz_bload = 0
         endif
      else
         mz_bload = 1
      endif
!
      call get_menv('NPB_VERBOSE', envstr, ios)
      npb_verbose = 0
      if (ios .gt. 0) then
         read(envstr,*,iostat=ios) npb_verbose
         if (ios.ne.0) npb_verbose = 0
      endif
!
      do ip = 1, num_othreads
         proc_num_threads(ip) = num_threads
         proc_group(ip) = 0
      end do
!
      open(unit=2, file='loadbt-mz.data', status='old', iostat=ios)
      if (ios.eq.0) then
         write(*,*) 'Reading load factors from loadbt-mz.data'

         if (mz_bload .ge. 1) then
            mz_bload = -mz_bload
         endif

         do ip = 1, num_othreads
            entry_counts(ip) = 0
         end do

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
            if (ip2.ge.num_othreads) ip2 = num_othreads - 1

            do ip = ip1+1, ip2+1
               proc_num_threads(ip) = curr_threads
               proc_group(ip) = group
               entry_counts(ip) = entry_counts(ip) + 1
            end do
         end do
   40    close(2)

         do ip = 1, num_othreads
            if (entry_counts(ip) .eq. 0) then
               write(*,*) '*** Error: Missing entry for othread ',ip-1
               stop
            else if (entry_counts(ip) .gt. 1) then
               write(*,*) '*** Warning: Multiple entries for othread ',  &
     &                    ip-1, ', only the last one used'
            endif
         end do

         ip1 = 1
      else
         write(*,*) 'Use the default load factors'
         ip1 = 0
      endif

      if (ip1 .gt. 0 .or. npb_verbose .gt. 0) then
         ip1 = 0
         do ip = 1, num_othreads
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

            else if (ip .eq. num_othreads) then
               ip2 = ip-1
               if (ip2 .gt. ip1+1) write(*,*) '    ...'
               write(*,30) ip2, curr_threads, group
            endif
         end do
   30    format('  othread',i6,'  num_threads =',i5,  &
     &          '  flag =',i5)
      endif
!
      tot_threads = 0
      if (mp .gt. 0) then
         do ip = 1, num_othreads
            tot_threads = tot_threads + proc_num_threads(ip)
         end do
!
         write(*, 1003) num_othreads
         write(*, 1004) tot_threads, dble(tot_threads)/num_othreads
      endif
 1003 format(' Number of outer-level threads: ', i5)
 1004 format(' Total number of threads: ', i6,  &
     &       '  (', f5.1, ' inner-threads/outer-thread)')
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
!  format:  ip1[:ip2] curr_threads group
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
      do while (is.le.n .and. line(is:is).ne.':')
         if (line(is:is).eq.'!') n = is
         is = is + 1
      end do
!
      if (is .gt. n) then
         read(line,*,err=90,end=90) ip1, curr_threads, group
         ip2 = ip1
      else if (is.eq.1 .or. is.eq.n) then
         go to 90
      else
         read(line(:is-1),*,err=90,end=90) ip1
         read(line(is+1:),*,err=90,end=90) ip2, curr_threads, group
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
      use bt_data
      use ompnpb
!
      implicit none
!
!  Calculate the communication index of a zone within a thread group
!
      integer zone, iproc, comm_index
!
!     local variables
      integer izone, jzone,  &
     &        izone_west, izone_east, jzone_south, jzone_north
!
      jzone  = (zone - 1)/x_zones + 1
      izone  = mod(zone - 1, x_zones) + 1
      izone_west  = iz_west(zone)
      izone_east  = iz_east(zone)
      jzone_south = iz_south(zone)
      jzone_north = iz_north(zone)
!
      comm_index = 0
      if (zone_proc_id(izone_west) .eq. iproc)  &
     &   comm_index = comm_index + y_size(jzone)
      if (zone_proc_id(izone_east) .eq. iproc)  &
     &   comm_index = comm_index + y_size(jzone)
      if (zone_proc_id(jzone_south) .eq. iproc)  &
     &   comm_index = comm_index + x_size(izone)
      if (zone_proc_id(jzone_north) .eq. iproc)  &
     &   comm_index = comm_index + x_size(izone)
!
      return
      end
!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
!
      subroutine map_zones(num_zones, nx, ny, nz, tot_threads)
!
!  Perform zone-othread mapping for load balance
!
      use bt_data
      use ompnpb
!
      implicit none
!
      integer num_zones, nx(*), ny(*), nz(*), tot_threads
!
!     local variables
      integer z_order(max_zones)
      integer zone, iz, z2, mz, np, ip, zone_comm, comm_index
      integer imx, imn, inc, icur_size
      double precision zone_size(max_zones), tot_size, cur_size
      double precision diff_ratio, max_size, ave_size
!
      integer group, ipg, tot_group_threads
      double precision tot_group_size
      integer proc_group_flag(max_zones)
!
! ... sort the zones in decending order
      tot_size = 0.d0
      do iz = 1, num_zones
         zone_size(iz) = nx(iz)*ny(iz)*nz(iz)
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
      if (npb_verbose .gt. 1) then
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
! ... use a simple bin-packing scheme to balance the load among othreads
      do ip = 1, num_othreads
         proc_zone_count(ip) = 0
         proc_zone_size(ip) = 0.d0
      end do
      do iz = 1, num_zones
         zone_proc_id(iz) = -1
      end do

      iz = 1
      do while (iz .le. num_zones)
!
!  ...   the current most empty thread
         np = 1
         cur_size = proc_zone_size(1)
         do ip = 2, num_othreads
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
!  ...   assign the zone to the current thread group
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
      mz = 1
      if (tot_threads.le.num_othreads .or. mz_bload.lt.1) mz = 0
!
      if (mz .ne. 0) then
!
         do ipg = 1, num_othreads
            proc_group_flag(ipg) = 0
         end do
!
         ipg = 1
!
! ...    balance load within a thread group
  200    do while (ipg .le. num_othreads)
            if (proc_group_flag(ipg) .eq. 0) goto 210
            ipg = ipg + 1
         end do
  210    if (ipg .gt. num_othreads) goto 300
!
         group = proc_group(ipg)
         tot_group_size = 0.d0
         tot_group_threads = 0
         do ip = ipg, num_othreads
            if (proc_group(ip) .eq. group) then
               proc_group_flag(ip) = 1
               tot_group_size = tot_group_size + proc_zone_size(ip)
               tot_group_threads = tot_group_threads +  &
     &                             proc_num_threads(ip)
            endif
         end do
!
         ave_size = tot_group_size/tot_group_threads
!
!  ...   distribute size evenly among threads
         icur_size = 0
         do ip = 1, num_othreads
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
            do ip = 1, num_othreads
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
  300 if (npb_verbose .gt. 0) then
         write(*,20)
         do ip = 1, num_othreads
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
   20 format(/' Zone-othread mapping:'/  &
     &       '  othread nzones  zone_size nthreads size_per_thread')
   25 format(i5,2x,i5,2x,f10.0,2x,i5,3x,f10.0)
   30 format(3x,'zone ',i5,2x,f9.0)
!
      imx = 1
      max_size = proc_zone_size(1)/proc_num_threads(1)
      imn = imx
      ave_size = max_size
      do ip = 2, num_othreads
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
     &               proc_zone_size(imx),proc_num_threads(imx)
         write(*,35) 'Min', imn-1, proc_zone_count(imn),  &
     &               proc_zone_size(imn),proc_num_threads(imn)
      endif
   35 format(1x,a,': othread=',i5,' nzones=',i5,' size=',f10.0,  &
     &       ' nthreads=',i5)

      if (tot_threads .gt. 1) write(*,40) tot_size / max_size
   40 format(/' Calculated speedup = ',f9.2)
      write(*,*)
!
      return
      end
