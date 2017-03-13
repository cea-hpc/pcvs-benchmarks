       subroutine exch_qbc(u, qbc, nx, nxmax, ny, nz)

       include 'header.h'

       integer   nx(*), nxmax(*), ny(*), nz(*), 
     $           nnx, nnxmax, nny, nnz, zone_no, num_zones,
     $           id_west, id_east, id_south, id_north
       double precision u(*), qbc(*)

       num_zones = x_zones * y_zones

c      copy data to qbc buffer
       if (timeron) call timer_start(t_rdis1)
       do zone_no = 1, num_zones
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


c      copy data from qbc buffer
       do zone_no = 1, num_zones
           nnx    = nx(zone_no)
           nnxmax = nxmax(zone_no)
           nny    = ny(zone_no)
           nnz    = nz(zone_no)

           id_west   = iz_west (zone_no)
           id_east   = iz_east (zone_no)
           id_south  = iz_south(zone_no)
           id_north  = iz_north(zone_no)

           call copy_x_face(u(start5(zone_no)),
     $                      qbc(qstart_east(id_west)),
     $                      nnx, nnxmax, nny, nnz, 0, 'in')

           call copy_x_face(u(start5(zone_no)),
     $                      qbc(qstart_west(id_east)),
     $                      nnx, nnxmax, nny, nnz, nnx-1, 'in')

           call copy_y_face(u(start5(zone_no)),
     $                      qbc(qstart_north(id_south)),
     $                      nnx, nnxmax, nny, nnz, 0, 'in')

           call copy_y_face(u(start5(zone_no)),
     $                      qbc(qstart_south(id_north)),
     $                      nnx, nnxmax, nny, nnz, nny-1, 'in')

       end do
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
         do k = 1, nz-2
           do i = 1, nx-2
             do m = 1, 5
               u(m,i,j,k) = qbc(m,i,k) 
             end do
           end do
         end do
       else if (dir(1:3) .eq. 'out') then
         do k = 1, nz-2
           do i = 1, nx-2
             do m = 1, 5
               qbc(m,i,k) = u(m,i,j,k) 
             end do
           end do
         end do
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
         do k = 1, nz-2
           do j = 1, ny-2
             do m = 1, 5
               u(m,i,j,k) = qbc(m,j,k)
             end do
           end do
         end do
       else if (dir(1:3) .eq. 'out') then
         do k = 1, nz-2
           do j = 1, ny-2
             do m = 1, 5
               qbc(m,j,k) = u(m,i,j,k)
             end do
           end do
         end do
       else
         print *, 'Erroneous data designation: ', dir
         stop
       endif

       return
       end

