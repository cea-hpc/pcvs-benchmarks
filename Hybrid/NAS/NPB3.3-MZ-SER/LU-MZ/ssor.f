c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine ssor(u, rsd, frct, qs, rho_i, a, b, c, d,
     $                nx, nxmax, ny, nz)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   to perform pseudo-time stepping SSOR iterations
c   for five nonlinear pde's.
c---------------------------------------------------------------------

      implicit none

      include 'header.h'
      integer          nx, nxmax, ny, nz
      double precision u(5,nxmax,ny,nz), rsd(5,nxmax,ny,nz), 
     $                 frct(5,nxmax,ny,nz), qs(nxmax,ny,nz), 
     $                 rho_i(nxmax,ny,nz), 
     $                 a(5,5,2:nxmax-1,ny), b(5,5,2:nxmax-1,ny), 
     $                 c(5,5,2:nxmax-1,ny), d(5,5,2:nxmax-1,ny)

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      double precision  tmp, tv(5,problem_size,problem_size)
      external timer_read
      double precision timer_read
 
      if (timeron) call timer_start(t_rhs)
      do k = 2, nz-1
        do j = 2, ny-1
          do i = 2, nx-1
            do m = 1, 5
              rsd(m,i,j,k) = dt * rsd(m,i,j,k)
            end do
          end do
        end do
      end do
      if (timeron) call timer_stop(t_rhs)
 
      do k = 2, nz-1 
c---------------------------------------------------------------------
c   form the lower triangular part of the jacobian matrix
c---------------------------------------------------------------------
        if (timeron) call timer_start(t_jacld)
        call jacld(k, u, rho_i, qs, a, b, c, d, nx, nxmax, ny, nz)
        if (timeron) call timer_stop(t_jacld)
 
c---------------------------------------------------------------------
c   perform the lower triangular solution
c---------------------------------------------------------------------
        if (timeron) call timer_start(t_blts)
        call blts( nx, nxmax, ny, nz, k, omega, rsd, a, b, c, d)
        if (timeron) call timer_stop(t_blts)
      end do
 
      do k = nz-1, 2, -1
c---------------------------------------------------------------------
c   form the strictly upper triangular part of the jacobian matrix
c---------------------------------------------------------------------
        if (timeron) call timer_start(t_jacu)
        call jacu(k, u, rho_i, qs, a, b, c, d, nx, nxmax, ny, nz)
        if (timeron) call timer_stop(t_jacu)

c---------------------------------------------------------------------
c   perform the upper triangular solution
c---------------------------------------------------------------------
        if (timeron) call timer_start(t_buts)
        call buts( nx, nxmax, ny, nz, k, omega, rsd, tv, d, a, b, c)
        if (timeron) call timer_stop(t_buts)
      end do
 
c---------------------------------------------------------------------
c   update the variables
c---------------------------------------------------------------------

      tmp = 1.0d0 / ( omega * ( 2.0d0 - omega ) ) 
      if (timeron) call timer_start(t_add)
      do k = 2, nz-1
        do j = 2, ny-1
          do i = 2, nx-1
            do m = 1, 5
              u(m,i,j,k) = u(m,i,j,k) + tmp * rsd(m,i,j,k)
            end do
          end do
        end do
      end do
      if (timeron) call timer_stop(t_add)

c---------------------------------------------------------------------
c   compute the steady-state residuals
c---------------------------------------------------------------------
      if (timeron) call timer_start(t_rhs)
         call rhs(u, rsd, frct, qs, rho_i, nx, nxmax, ny, nz)
      if (timeron) call timer_stop(t_rhs)

      return
      end
