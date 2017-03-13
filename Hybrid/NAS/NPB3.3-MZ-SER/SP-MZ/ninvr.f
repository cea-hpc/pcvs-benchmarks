
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  ninvr(rhs, nx, nxmax, ny, nz)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   block-diagonal matrix-vector multiplication              
c---------------------------------------------------------------------

       include 'header.h'

       integer nx, nxmax, ny, nz
       double precision rhs(5,0:nxmax-1,0:ny-1,0:nz-1)

       integer  i, j, k
       double precision r1, r2, r3, r4, r5, t1, t2

       do k = 1, nz-2
          do j = 1, ny-2
             do i = 1, nx-2

                r1 = rhs(1,i,j,k)
                r2 = rhs(2,i,j,k)
                r3 = rhs(3,i,j,k)
                r4 = rhs(4,i,j,k)
                r5 = rhs(5,i,j,k)
               
                t1 = bt * r3
                t2 = 0.5d0 * ( r4 + r5 )

                rhs(1,i,j,k) = -r2
                rhs(2,i,j,k) =  r1
                rhs(3,i,j,k) = bt * ( r4 - r5 )
                rhs(4,i,j,k) = -t1 + t2
                rhs(5,i,j,k) =  t1 + t2
             enddo    
          enddo
       enddo

       return
       end
