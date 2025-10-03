
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine  add(u, rhs, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! addition of update to the vector u
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer nx, nxmax, ny, nz
       double precision rhs(5,0:nxmax-1,0:ny-1,0:nz-1),  &
     &                  u  (5,0:nxmax-1,0:ny-1,0:nz-1)

       integer i,j,k,m

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(m,i,j,k)  &
!$OMP&  SCHEDULE(STATIC) COLLAPSE(2)
       do k = 1, nz-2
          do j = 1, ny-2
             do i = 1, nx-2
                do m = 1, 5
                   u(m,i,j,k) = u(m,i,j,k) + rhs(m,i,j,k)
                end do
             end do
          end do
       end do
!$OMP END PARALLEL DO

       return
       end

