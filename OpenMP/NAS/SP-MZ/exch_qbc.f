       subroutine exch_qbc(u, qbc, nx, nxmax, ny, nz, 
     &                     proc_zone_id, proc_num_zones)

       include 'header.h'
       include 'omp_stuff.h'

       integer   nx(*), nxmax(*), ny(*), nz(*), 
     &           proc_zone_id(*), proc_num_zones
       double precision u(*), qbc(*)

       integer   nnx, nnxmax, nny, nnz, zone_no, iz, nthreads,
     $           izone_west, izone_east, jzone_south, jzone_north


       if (timeron) call timer_start(t_rdis2)
!$omp barrier
       if (timeron) call timer_stop(t_rdis2)
       nthreads = proc_num_threads(myid+1)

c      copy data to qbc buffer
       if (timeron) call timer_start(t_rdis1)
!$omp parallel private(iz,zone_no,nnx,nnxmax,nny,nnz)
!$omp&  num_threads(nthreads)
       do iz = 1, proc_num_zones
           zone_no = proc_zone_id(iz)
           nnx    = nx(zone_no)
           nnxmax = nxmax(zone_no)
           nny    = ny(zone_no)
           nnz    = nz(zone_no)

           call copy_x_face(u(start5(zone_no)),
     $                      qbc(qstart_west(zone_no)),
     $                      nnx, nnxmax, nny, nnz, 1, 'out')

           call copy_x_face(u(start5(zone_no)),
     $                      qbc(qstart_east(zone_no)),
     $                      nnx, nnxmax, nny, nnz, nnx-2, 'out')


           call copy_y_face(u(start5(zone_no)),
     $                      qbc(qstart_north(zone_no)),
     $                      nnx, nnxmax, nny, nnz, nny-2, 'out')

           call copy_y_face(u(start5(zone_no)),
     $                      qbc(qstart_south(zone_no)),
     $                      nnx, nnxmax, nny, nnz, 1, 'out')

       end do
!$omp end parallel
       if (timeron) call timer_stop(t_rdis1)

       if (timeron) call timer_start(t_rdis2)
!$omp barrier
       if (timeron) call timer_stop(t_rdis2)

c      copy data from qbc buffer
       if (timeron) call timer_start(t_rdis1)
!$omp parallel private(iz,zone_no,nnx,nnxmax,nny,nnz,
!$omp&  izone_west,izone_east,jzone_south,jzone_north)
!$omp&  num_threads(nthreads)
       do iz = 1, proc_num_zones
           zone_no = proc_zone_id(iz)
           nnx    = nx(zone_no)
           nnxmax = nxmax(zone_no)
           nny    = ny(zone_no)
           nnz    = nz(zone_no)

           izone_west  = iz_west(zone_no)
           izone_east  = iz_east(zone_no)
           jzone_south = iz_south(zone_no)
           jzone_north = iz_north(zone_no)

           call copy_x_face(u(start5(zone_no)),
     $                      qbc(qstart_east(izone_west)),
     $                      nnx, nnxmax, nny, nnz, 0, 'in')

           call copy_x_face(u(start5(zone_no)),
     $                      qbc(qstart_west(izone_east)),
     $                      nnx, nnxmax, nny, nnz, nnx-1, 'in')

           call copy_y_face(u(start5(zone_no)),
     $                      qbc(qstart_north(jzone_south)),
     $                      nnx, nnxmax, nny, nnz, 0, 'in')

           call copy_y_face(u(start5(zone_no)),
     $                      qbc(qstart_south(jzone_north)),
     $                      nnx, nnxmax, nny, nnz, nny-1, 'in')

       end do
!$omp end parallel
       if (timeron) call timer_stop(t_rdis1)

       return
       end


       subroutine copy_y_face(u, qbc, nx, nxmax, ny, nz, jloc, dir)

       implicit         none

       integer          nx, nxmax, ny, nz, i, j, k, jloc, m
       double precision u(5,0:nxmax-1,0:ny-1,0:nz-1), qbc(5,nx-2,nz-2)
       character        dir*(*)

       j = jloc
       if (dir(1:2) .eq. 'in') then
!$omp do
         do k = 1, nz-2
           do i = 1, nx-2
             do m = 1, 5
               u(m,i,j,k) = qbc(m,i,k) 
             end do
           end do
         end do
!$omp end do
       else if (dir(1:3) .eq. 'out') then
!$omp do
         do k = 1, nz-2
           do i = 1, nx-2
             do m = 1, 5
               qbc(m,i,k) = u(m,i,j,k) 
             end do
           end do
         end do
!$omp end do
       else
         print *, 'Erroneous data designation: ', dir
         stop
       endif

       return
       end


       subroutine copy_x_face(u, qbc, nx, nxmax, ny, nz, iloc, dir)

       implicit         none

       integer          nx, nxmax, ny, nz, i, j, k, iloc, m
       double precision u(5,0:nxmax-1,0:ny-1,0:nz-1), qbc(5,ny-2,nz-2)
       character        dir*(*)

       i = iloc
       if (dir(1:2) .eq. 'in') then
!$omp do
         do k = 1, nz-2
           do j = 1, ny-2
             do m = 1, 5
               u(m,i,j,k) = qbc(m,j,k)
             end do
           end do
         end do
!$omp end do
       else if (dir(1:3) .eq. 'out') then
!$omp do
         do k = 1, nz-2
           do j = 1, ny-2
             do m = 1, 5
               qbc(m,j,k) = u(m,i,j,k)
             end do
           end do
         end do
!$omp end do
       else
         print *, 'Erroneous data designation: ', dir
         stop
       endif

       return
       end

