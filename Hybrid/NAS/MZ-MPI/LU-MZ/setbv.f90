
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine setbv(u, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   set the boundary values of dependent variables
!---------------------------------------------------------------------

      use lu_data
      implicit none

      integer nx, nxmax, ny, nz
      double precision u(5,-1:nxmax+2,ny,nz)

!---------------------------------------------------------------------
!   local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      double precision temp1(5), temp2(5)

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(k,m,temp2,temp1,i,j)
!---------------------------------------------------------------------
!   set the dependent variable values along the top and bottom faces
!---------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
      do j = 1, ny
         do i = 1, nx
            call exact( i+ipt, j, 1, temp1, nx0, ny, nz )
            call exact( i+ipt, j, nz, temp2, nx0, ny, nz )
            do m = 1, 5
               u( m, i, j, 1 ) = temp1(m)
               u( m, i, j, nz ) = temp2(m)
            end do
         end do
      end do
!$OMP END DO

!---------------------------------------------------------------------
!   set the dependent variable values along north and south faces
!---------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
      do k = 1, nz
         do i = 1, nx
            call exact( i+ipt, 1, k, temp1, nx0, ny, nz )
            call exact( i+ipt, ny, k, temp2, nx0, ny, nz )
            do m = 1, 5
               u( m, i, 1, k ) = temp1(m)
               u( m, i, ny, k ) = temp2(m)
            end do
         end do
      end do
!$OMP END DO

!---------------------------------------------------------------------
!   set the dependent variable values along east and west faces
!---------------------------------------------------------------------
      do k = 1, nz
!$OMP DO SCHEDULE(STATIC)
         do j = 1, ny
            if (north.eq.-1) then
            call exact( 1+ipt, j, k, temp1, nx0, ny, nz )
            do m = 1, 5
               u( m, 1, j, k ) = temp1(m)
            end do
            end if
            if (south.eq.-1) then
            call exact( nx+ipt, j, k, temp2, nx0, ny, nz )
            do m = 1, 5
               u( m, nx, j, k ) = temp2(m)
            end do
            end if
         end do
!$OMP END DO nowait
      end do
!$OMP END PARALLEL

      return
      end
