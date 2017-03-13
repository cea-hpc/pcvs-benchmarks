
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine error_norm(rms, u, nx, nxmax, ny, nz)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c this function computes the norm of the difference between the
c computed solution and the exact solution
c---------------------------------------------------------------------

       include 'header.h'

       integer nx, nxmax, ny, nz
       double precision u(5,0:nxmax-1,0:ny-1,0:nz-1)

       integer i, j, k, m
       double precision xi, eta, zeta, u_exact(5), rms(5), add


       do    m = 1, 5
          rms(m) = 0.0d0
       end do

       do   k = 0, nz-1
          zeta = dble(k) * dnzm1
          do   j = 0, ny-1
             eta = dble(j) * dnym1
             do   i = 0, nx-1
                xi = dble(i) * dnxm1
                call exact_solution(xi, eta, zeta, u_exact)

                do   m = 1, 5
                   add = u(m,i,j,k)-u_exact(m)
                   rms(m) = rms(m) + add*add
                end do
             end do
          end do
       end do

       do    m = 1, 5
          rms(m) = rms(m) / (dble(nz-2)*(ny-2)*(nx-2))
          rms(m) = dsqrt(rms(m))
       end do

       return
       end



       subroutine rhs_norm(rms, rhs, nx, nxmax, ny, nz)

       include 'header.h'

       integer nx, nxmax, ny, nz
       double precision rhs(5,0:nxmax-1,0:ny-1,0:nz-1)

       integer i, j, k, m
       double precision rms(5), add


       do   m = 1, 5
          rms(m) = 0.0d0
       end do

       do k = 1, nz-2
          do j = 1, ny-2
             do i = 1, nx-2
               do m = 1, 5
                  add = rhs(m,i,j,k)
                  rms(m) = rms(m) + add*add
               end do 
             end do 
          end do 
       end do 

       do   m = 1, 5
          rms(m) = rms(m) / (dble(nz-2)*(ny-2)*(nx-2))
          rms(m) = dsqrt(rms(m))
       end do

       return
       end


