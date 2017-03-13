c
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
c
      subroutine env_setup
c
c  Set up from environment variables
c
c ... common variables
      include 'header.h'
c
c ... local variables
      integer   ios
      character envstr*80
c
      call getenv('NPB_VERBOSE', envstr)
      npb_verbose = 0
      if (envstr.ne.' ') then
         read(envstr,*,iostat=ios) npb_verbose
         if (ios.ne.0) npb_verbose = 0
      endif
c
      return
      end
c
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>c
c
       subroutine zone_setup(nx, nxmax, ny, nz)

       include 'header.h'

       integer           nx(*), nxmax(*), ny(*), nz(*), zone_size,
     $                   x_face_size, y_face_size, zone_no, i, j,
     $                   id_west, id_east, jd_south, jd_north
       double precision  x_r, y_r, x_smallest, y_smallest

       if (dabs(ratio-1.d0) .gt. 1.d-10) then

c        compute zone stretching only if the prescribed zone size ratio 
c        is substantially larger than unity       

         x_r   = dexp(dlog(ratio)/(x_zones-1))
         y_r   = dexp(dlog(ratio)/(y_zones-1))
         x_smallest = dble(gx_size)*(x_r-1.d0)/(x_r**x_zones-1.d0)
         y_smallest = dble(gy_size)*(y_r-1.d0)/(y_r**y_zones-1.d0)

c        compute tops of intervals, using a slightly tricked rounding
c        to make sure that the intervals are increasing monotonically
c        in size

         do i = 1, x_zones
            x_end(i) = x_smallest*(x_r**i-1.d0)/(x_r-1.d0)+0.45d0
         end do

         do j = 1, y_zones
            y_end(j) = y_smallest*(y_r**j-1.d0)/(y_r-1.d0)+0.45d0
         end do
 
       else

c        compute essentially equal sized zone dimensions

         do i = 1, x_zones
           x_end(i)   = (i*gx_size)/x_zones
         end do

         do j = 1, y_zones
           y_end(j)   = (j*gy_size)/y_zones
         end do

       endif

       x_start(1) = 1
       do i = 1, x_zones
          if (i .ne. x_zones) x_start(i+1) = x_end(i) + 1
          x_size(i)  = x_end(i) - x_start(i) + 1
       end do

       y_start(1) = 1
       do j = 1, y_zones
          if (j .ne. y_zones) y_start(j+1) = y_end(j) + 1
          y_size(j) = y_end(j) - y_start(j) + 1
       end do

       if (npb_verbose .gt. 0) write (*,*) 'Zone sizes:'

       do j = 1, y_zones
         do i = 1, x_zones
           zone_no = (i-1)+(j-1)*x_zones+1

           id_west  = mod(i-2+x_zones,x_zones)
           id_east  = mod(i,          x_zones)
           jd_south = mod(j-2+y_zones,y_zones)
           jd_north = mod(j,          y_zones)
           iz_west (zone_no) = id_west +  (j-1)*x_zones + 1
           iz_east (zone_no) = id_east +  (j-1)*x_zones + 1
           iz_south(zone_no) = (i-1) + jd_south*x_zones + 1
           iz_north(zone_no) = (i-1) + jd_north*x_zones + 1

c          Compute dimensions (1D and 3D), including padding of the first
c          array dimension (to ensure it is odd). Note that we don't need
c          to pad the zone face sizes. Face data is always accessed
c          with unit stride (copying from and to buffers)
           nx(zone_no) = x_size(i)
           nxmax(zone_no) = nx(zone_no) + 1 - mod(nx(zone_no),2)
           ny(zone_no) = y_size(j)
           nz(zone_no) = gz_size

           zone_size = nxmax(zone_no)*ny(zone_no)*nz(zone_no)
           x_face_size = (y_size(j)-2)*(gz_size-2)*5
           y_face_size = (x_size(i)-2)*(gz_size-2)*5           

           if (zone_no .eq. 1) then
             start1(zone_no)  = 1
             start5(zone_no)  = 1
             qstart_west(zone_no) = 1
           endif
           qstart_east(zone_no)  = qstart_west(zone_no) + x_face_size
           qstart_south(zone_no) = qstart_east(zone_no) + x_face_size
           qstart_north(zone_no) = qstart_south(zone_no)+ y_face_size
           if (zone_no .ne. x_zones*y_zones) then
             qstart_west(zone_no+1) = qstart_north(zone_no) +
     $                                y_face_size
             start1(zone_no+1) = start1(zone_no) + zone_size
             start5(zone_no+1) = start5(zone_no) + zone_size*5
           endif

           if (npb_verbose .gt. 0) then
             write (*,99) zone_no, nx(zone_no), ny(zone_no), 
     $                    nz(zone_no)
           endif
         end do
       end do

 99    format(i5,':  ',i5,' x',i5,' x',i5)

       return
       end
