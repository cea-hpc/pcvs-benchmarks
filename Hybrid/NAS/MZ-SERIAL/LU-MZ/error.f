c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine error(u, errnm, nx, nxmax, ny, nz)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   compute the solution error
c
c---------------------------------------------------------------------

      implicit none

      include 'header.h'
      integer nx, nxmax, ny, nz
      double precision u(5,nxmax,ny,nz), errnm(5)

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      double precision  tmp
      double precision  u000ijk(5)

      do m = 1, 5
         errnm(m) = 0.0d0
      end do

      do k = 2, nz-1
         do j = 2, ny-1
            do i = 2, nx-1
               call exact( i, j, k, u000ijk, nx, ny, nz )
               do m = 1, 5
                  tmp = ( u000ijk(m) - u(m,i,j,k) )
                  errnm(m) = errnm(m) + tmp ** 2
               end do
            end do
         end do
      end do

      do m = 1, 5
         errnm(m) = dsqrt ( errnm(m) / ( dble(nx-2)*(ny-2)*(nz-2) ) )
      end do

      return
      end
