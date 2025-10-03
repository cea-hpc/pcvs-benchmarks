
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine error_norm(rms, u, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! this function computes the norm of the difference between the
! computed solution and the exact solution
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer nx, nxmax, ny, nz
       double precision u(5,0:nxmax-1,0:ny-1,0:nz-1)

       integer i, j, k, m
       double precision xi, eta, zeta, u_exact(5), rms(5), add
       double precision rms_loc(5)


       do    m = 1, 5
          rms(m) = 0.0d0
       end do

!$omp parallel default(shared) private(rms_loc,add,m,u_exact,xi,i,eta,  &
!$omp& j,zeta,k)
       do m=1,5
          rms_loc(m)=0.0d0
       enddo
!$omp do schedule(static) collapse(2)
       do   k = 0, nz-1
          do   j = 0, ny-1
             zeta = dble(k) * dnzm1
             eta = dble(j) * dnym1
             do   i = 0, nx-1
                xi = dble(i) * dnxm1
                call exact_solution(xi, eta, zeta, u_exact)

                do   m = 1, 5
                   add = u(m,i,j,k)-u_exact(m)
                   rms_loc(m) = rms_loc(m) + add*add
                end do
             end do
          end do
       end do
!$omp end do nowait
       do m=1,5
!$omp atomic
          rms(m)=rms(m)+rms_loc(m)
       enddo
!$omp end parallel

       do    m = 1, 5
          rms(m) = rms(m) / (dble(nz-2)*dble(ny-2)*dble(nx-2))
          rms(m) = dsqrt(rms(m))
       end do

       return
       end



       subroutine rhs_norm(rms, rhs, nx, nxmax, ny, nz)

       use sp_data
       implicit none

       integer nx, nxmax, ny, nz
       double precision rhs(5,0:nxmax-1,0:ny-1,0:nz-1)

       integer i, j, k, m
       double precision rms(5), add
       double precision rms_loc(5)


       do   m = 1, 5
          rms(m) = 0.0d0
       end do

!$omp parallel default(shared) private(rms_loc,add,m,i,j,k)
       do m=1,5
          rms_loc(m)=0.0d0
       enddo
!$omp do schedule(static) collapse(2)
       do k = 1, nz-2
          do j = 1, ny-2
             do i = 1, nx-2
               do m = 1, 5
                  add = rhs(m,i,j,k)
                  rms_loc(m) = rms_loc(m) + add*add
               end do 
             end do 
          end do 
       end do 
!$omp end do nowait
       do m=1,5
!$omp atomic
          rms(m)=rms(m)+rms_loc(m)
       enddo
!$omp end parallel

       do   m = 1, 5
          rms(m) = rms(m) / (dble(nz-2)*dble(ny-2)*dble(nx-2))
          rms(m) = dsqrt(rms(m))
       end do

       return
       end


