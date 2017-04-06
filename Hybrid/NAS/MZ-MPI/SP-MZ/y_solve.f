
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine y_solve(rho_i, vs, speed, rhs, nx, nxmax, ny, nz)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c this function performs the solution of the approximate factorization
c step in the y-direction for all five matrix components
c simultaneously. The Thomas algorithm is employed to solve the
c systems for the y-lines. Boundary conditions are non-periodic
c---------------------------------------------------------------------

       include 'header.h'

       integer nx, nxmax, ny, nz
       double precision rho_i(  0:nxmax-1,0:ny-1,0:nz-1), 
     $                  vs   (  0:nxmax-1,0:ny-1,0:nz-1), 
     $                  speed(  0:nxmax-1,0:ny-1,0:nz-1), 
     $                  rhs  (5,0:nxmax-1,0:ny-1,0:nz-1)

       integer i, j, k, j1, j2, m
       double precision ru1, fac1, fac2


c---------------------------------------------------------------------
c---------------------------------------------------------------------

       if (timeron) call timer_start(t_ysolve)
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(fac2,m,fac1,j2,j1,ru1,j,i,k)
!$OMP&  SHARED(comz6,comz1,comz4,comz5,c2dtty1,dtty1,dtty2,dy1,dymax,
!$OMP& c1c5,dy5,con43,dy3,c3c4,ny,nx,nz)
       do  k = 1, nz-2

c---------------------------------------------------------------------
c Computes the left hand side for the three y-factors   
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     zap the whole left hand side for starters
c---------------------------------------------------------------------
          do  i = 1, nx-2
             do   m = 1, 5
                lhs (m,i,0) = 0.0d0
                lhsp(m,i,0) = 0.0d0
                lhsm(m,i,0) = 0.0d0
                lhs (m,i,ny-1) = 0.0d0
                lhsp(m,i,ny-1) = 0.0d0
                lhsm(m,i,ny-1) = 0.0d0
             end do

c---------------------------------------------------------------------
c      next, set all diagonal values to 1. This is overkill, but 
c      convenient
c---------------------------------------------------------------------
             lhs (3,i,0) = 1.0d0
             lhsp(3,i,0) = 1.0d0
             lhsm(3,i,0) = 1.0d0
             lhs (3,i,ny-1) = 1.0d0
             lhsp(3,i,ny-1) = 1.0d0
             lhsm(3,i,ny-1) = 1.0d0
          end do

c---------------------------------------------------------------------
c      first fill the lhs for the u-eigenvalue         
c---------------------------------------------------------------------

          do  i = 1, nx-2
             do  j = 0, ny-1
                ru1 = c3c4*rho_i(i,j,k)
                cv(j) = vs(i,j,k)
                rhoq(j) = dmax1( dy3 + con43 * ru1,
     >                           dy5 + c1c5*ru1,
     >                           dymax + ru1,
     >                           dy1)
             end do
            
             do  j = 1, ny-2
                lhs(1,i,j) =  0.0d0
                lhs(2,i,j) = -dtty2 * cv(j-1) - dtty1 * rhoq(j-1)
                lhs(3,i,j) =  1.0 + c2dtty1 * rhoq(j)
                lhs(4,i,j) =  dtty2 * cv(j+1) - dtty1 * rhoq(j+1)
                lhs(5,i,j) =  0.0d0
             end do
          end do

c---------------------------------------------------------------------
c      add fourth order dissipation                             
c---------------------------------------------------------------------

          do  i = 1, nx-2
             j = 1
             lhs(3,i,j) = lhs(3,i,j) + comz5
             lhs(4,i,j) = lhs(4,i,j) - comz4
             lhs(5,i,j) = lhs(5,i,j) + comz1
       
             j = 2
             lhs(2,i,j) = lhs(2,i,j) - comz4
             lhs(3,i,j) = lhs(3,i,j) + comz6
             lhs(4,i,j) = lhs(4,i,j) - comz4
             lhs(5,i,j) = lhs(5,i,j) + comz1
          end do

          do   j=3, ny-4
             do  i = 1, nx-2
                lhs(1,i,j) = lhs(1,i,j) + comz1
                lhs(2,i,j) = lhs(2,i,j) - comz4
                lhs(3,i,j) = lhs(3,i,j) + comz6
                lhs(4,i,j) = lhs(4,i,j) - comz4
                lhs(5,i,j) = lhs(5,i,j) + comz1
             end do
          end do

          do  i = 1, nx-2
             j = ny-3
             lhs(1,i,j) = lhs(1,i,j) + comz1
             lhs(2,i,j) = lhs(2,i,j) - comz4
             lhs(3,i,j) = lhs(3,i,j) + comz6
             lhs(4,i,j) = lhs(4,i,j) - comz4

             j = ny-2
             lhs(1,i,j) = lhs(1,i,j) + comz1
             lhs(2,i,j) = lhs(2,i,j) - comz4
             lhs(3,i,j) = lhs(3,i,j) + comz5
          end do

c---------------------------------------------------------------------
c      subsequently, do the other two factors                    
c---------------------------------------------------------------------
          do    j = 1, ny-2
             do  i = 1, nx-2
                lhsp(1,i,j) = lhs(1,i,j)
                lhsp(2,i,j) = lhs(2,i,j) - 
     >                            dtty2 * speed(i,j-1,k)
                lhsp(3,i,j) = lhs(3,i,j)
                lhsp(4,i,j) = lhs(4,i,j) + 
     >                            dtty2 * speed(i,j+1,k)
                lhsp(5,i,j) = lhs(5,i,j)
                lhsm(1,i,j) = lhs(1,i,j)
                lhsm(2,i,j) = lhs(2,i,j) + 
     >                            dtty2 * speed(i,j-1,k)
                lhsm(3,i,j) = lhs(3,i,j)
                lhsm(4,i,j) = lhs(4,i,j) - 
     >                            dtty2 * speed(i,j+1,k)
                lhsm(5,i,j) = lhs(5,i,j)
             end do
          end do


c---------------------------------------------------------------------
c                          FORWARD ELIMINATION  
c---------------------------------------------------------------------

          do    j = 0, ny-3
             j1 = j  + 1
             j2 = j  + 2
             do  i = 1, nx-2
                fac1      = 1.d0/lhs(3,i,j)
                lhs(4,i,j)  = fac1*lhs(4,i,j)
                lhs(5,i,j)  = fac1*lhs(5,i,j)
                do    m = 1, 3
                   rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                end do
                lhs(3,i,j1) = lhs(3,i,j1) -
     >                         lhs(2,i,j1)*lhs(4,i,j)
                lhs(4,i,j1) = lhs(4,i,j1) -
     >                         lhs(2,i,j1)*lhs(5,i,j)
                do    m = 1, 3
                   rhs(m,i,j1,k) = rhs(m,i,j1,k) -
     >                         lhs(2,i,j1)*rhs(m,i,j,k)
                end do
                lhs(2,i,j2) = lhs(2,i,j2) -
     >                         lhs(1,i,j2)*lhs(4,i,j)
                lhs(3,i,j2) = lhs(3,i,j2) -
     >                         lhs(1,i,j2)*lhs(5,i,j)
                do    m = 1, 3
                   rhs(m,i,j2,k) = rhs(m,i,j2,k) -
     >                         lhs(1,i,j2)*rhs(m,i,j,k)
                end do
             end do
          end do

c---------------------------------------------------------------------
c      The last two rows in this zone are a bit different, 
c      since they do not have two more rows available for the
c      elimination of off-diagonal entries
c---------------------------------------------------------------------

          j  = ny-2
          j1 = ny-1
          do  i = 1, nx-2
             fac1      = 1.d0/lhs(3,i,j)
             lhs(4,i,j)  = fac1*lhs(4,i,j)
             lhs(5,i,j)  = fac1*lhs(5,i,j)
             do    m = 1, 3
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             end do
             lhs(3,i,j1) = lhs(3,i,j1) -
     >                      lhs(2,i,j1)*lhs(4,i,j)
             lhs(4,i,j1) = lhs(4,i,j1) -
     >                      lhs(2,i,j1)*lhs(5,i,j)
             do    m = 1, 3
                rhs(m,i,j1,k) = rhs(m,i,j1,k) -
     >                      lhs(2,i,j1)*rhs(m,i,j,k)
             end do
c---------------------------------------------------------------------
c            scale the last row immediately 
c---------------------------------------------------------------------
             fac2      = 1.d0/lhs(3,i,j1)
             do    m = 1, 3
                rhs(m,i,j1,k) = fac2*rhs(m,i,j1,k)
             end do
          end do

c---------------------------------------------------------------------
c      do the u+c and the u-c factors                 
c---------------------------------------------------------------------
          do    j = 0, ny-3
             j1 = j  + 1
             j2 = j  + 2
             do  i = 1, nx-2
                m = 4
                fac1       = 1.d0/lhsp(3,i,j)
                lhsp(4,i,j)  = fac1*lhsp(4,i,j)
                lhsp(5,i,j)  = fac1*lhsp(5,i,j)
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                lhsp(3,i,j1) = lhsp(3,i,j1) -
     >                      lhsp(2,i,j1)*lhsp(4,i,j)
                lhsp(4,i,j1) = lhsp(4,i,j1) -
     >                      lhsp(2,i,j1)*lhsp(5,i,j)
                rhs(m,i,j1,k) = rhs(m,i,j1,k) -
     >                      lhsp(2,i,j1)*rhs(m,i,j,k)
                lhsp(2,i,j2) = lhsp(2,i,j2) -
     >                      lhsp(1,i,j2)*lhsp(4,i,j)
                lhsp(3,i,j2) = lhsp(3,i,j2) -
     >                      lhsp(1,i,j2)*lhsp(5,i,j)
                rhs(m,i,j2,k) = rhs(m,i,j2,k) -
     >                      lhsp(1,i,j2)*rhs(m,i,j,k)
                m = 5
                fac1       = 1.d0/lhsm(3,i,j)
                lhsm(4,i,j)  = fac1*lhsm(4,i,j)
                lhsm(5,i,j)  = fac1*lhsm(5,i,j)
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                lhsm(3,i,j1) = lhsm(3,i,j1) -
     >                      lhsm(2,i,j1)*lhsm(4,i,j)
                lhsm(4,i,j1) = lhsm(4,i,j1) -
     >                      lhsm(2,i,j1)*lhsm(5,i,j)
                rhs(m,i,j1,k) = rhs(m,i,j1,k) -
     >                      lhsm(2,i,j1)*rhs(m,i,j,k)
                lhsm(2,i,j2) = lhsm(2,i,j2) -
     >                      lhsm(1,i,j2)*lhsm(4,i,j)
                lhsm(3,i,j2) = lhsm(3,i,j2) -
     >                      lhsm(1,i,j2)*lhsm(5,i,j)
                rhs(m,i,j2,k) = rhs(m,i,j2,k) -
     >                      lhsm(1,i,j2)*rhs(m,i,j,k)
             end do
          end do

c---------------------------------------------------------------------
c         And again the last two rows separately
c---------------------------------------------------------------------
          j  = ny-2
          j1 = ny-1
          do  i = 1, nx-2
             m = 4
             fac1       = 1.d0/lhsp(3,i,j)
             lhsp(4,i,j)  = fac1*lhsp(4,i,j)
             lhsp(5,i,j)  = fac1*lhsp(5,i,j)
             rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             lhsp(3,i,j1) = lhsp(3,i,j1) -
     >                   lhsp(2,i,j1)*lhsp(4,i,j)
             lhsp(4,i,j1) = lhsp(4,i,j1) -
     >                   lhsp(2,i,j1)*lhsp(5,i,j)
             rhs(m,i,j1,k)   = rhs(m,i,j1,k) -
     >                   lhsp(2,i,j1)*rhs(m,i,j,k)
             m = 5
             fac1       = 1.d0/lhsm(3,i,j)
             lhsm(4,i,j)  = fac1*lhsm(4,i,j)
             lhsm(5,i,j)  = fac1*lhsm(5,i,j)
             rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             lhsm(3,i,j1) = lhsm(3,i,j1) -
     >                   lhsm(2,i,j1)*lhsm(4,i,j)
             lhsm(4,i,j1) = lhsm(4,i,j1) -
     >                   lhsm(2,i,j1)*lhsm(5,i,j)
             rhs(m,i,j1,k)   = rhs(m,i,j1,k) -
     >                   lhsm(2,i,j1)*rhs(m,i,j,k)
c---------------------------------------------------------------------
c               Scale the last row immediately 
c---------------------------------------------------------------------
             rhs(4,i,j1,k)   = rhs(4,i,j1,k)/lhsp(3,i,j1)
             rhs(5,i,j1,k)   = rhs(5,i,j1,k)/lhsm(3,i,j1)
          end do


c---------------------------------------------------------------------
c                         BACKSUBSTITUTION 
c---------------------------------------------------------------------

          j  = ny-2
          j1 = ny-1
          do  i = 1, nx-2
             do   m = 1, 3
                rhs(m,i,j,k) = rhs(m,i,j,k) -
     >                           lhs(4,i,j)*rhs(m,i,j1,k)
             end do

             rhs(4,i,j,k) = rhs(4,i,j,k) -
     >                           lhsp(4,i,j)*rhs(4,i,j1,k)
             rhs(5,i,j,k) = rhs(5,i,j,k) -
     >                           lhsm(4,i,j)*rhs(5,i,j1,k)
          end do

c---------------------------------------------------------------------
c      The first three factors
c---------------------------------------------------------------------
          do   j = ny-3, 0, -1
             j1 = j  + 1
             j2 = j  + 2
             do  i = 1, nx-2
                do   m = 1, 3
                   rhs(m,i,j,k) = rhs(m,i,j,k) - 
     >                          lhs(4,i,j)*rhs(m,i,j1,k) -
     >                          lhs(5,i,j)*rhs(m,i,j2,k)
                end do

c---------------------------------------------------------------------
c      And the remaining two
c---------------------------------------------------------------------
                rhs(4,i,j,k) = rhs(4,i,j,k) - 
     >                          lhsp(4,i,j)*rhs(4,i,j1,k) -
     >                          lhsp(5,i,j)*rhs(4,i,j2,k)
                rhs(5,i,j,k) = rhs(5,i,j,k) - 
     >                          lhsm(4,i,j)*rhs(5,i,j1,k) -
     >                          lhsm(5,i,j)*rhs(5,i,j2,k)
             end do
          end do

       end do
!$OMP END PARALLEL DO
       if (timeron) call timer_stop(t_ysolve)

       if (timeron) call timer_start(t_pinvr)
       call pinvr(rhs, nx, nxmax, ny, nz)
       if (timeron) call timer_stop(t_pinvr)

       return
       end
    






