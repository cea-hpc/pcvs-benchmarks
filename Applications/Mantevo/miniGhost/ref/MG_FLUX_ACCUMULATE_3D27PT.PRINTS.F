! ************************************************************************
!
!               miniGhost: stencil computations with boundary exchange.
!                 Copyright (2012) Sandia Corporation
!
! Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
! license for use of this work by or on behalf of the U.S. Government.
!
! This library is free software; you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation; either version 2.1 of the
! License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
! USA
! Questions? Contact Richard F. Barrett (rfbarre@sandia.gov) or
!                    Michael A. Heroux (maherou@sandia.gov)
!
! ************************************************************************

MODULE MG_FLUX_ACCUMULATE_3D27PT_MOD

   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Keeps track of heat dissapation out of physical domain.
   ! 2D 9PT and 3D 27PT.

CONTAINS
   
   SUBROUTINE MG_FLUX_ACCUMULATE_3D27PT ( GRID, IVAR, IERR )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(IN) :: &
         IVAR

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(INOUT) :: &
         GRID

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER ::         &
         I, J, K            ! Counters

      integer ii
      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0

      do ii=0, numpes-1
      if ( mype==ii)then
      IF ( MYPY == 0 ) THEN    ! X-Z plane for Y=1
         !print *,'X-Z plane for Y=1'
         DO K = 1, NZ
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) +                                      &
                                ( GRID(I-1,1,K-1) + GRID(I,1,K-1) + GRID(I+1,1,K-1) + &
                                  GRID(I-1,1,K  ) + GRID(I,1,K  ) + GRID(I+1,1,K  ) + &
                                  GRID(I-1,1,K+1) + GRID(I,1,K+1) + GRID(I+1,1,K+1) ) / TWENTYSEVEN
              !write(*,99) mype, i, 1, k, &
              !             GRID(I-1,1,K-1) + GRID(I,1,K-1) + GRID(I+1,1,K-1) +     &
              !             GRID(I-1,1,K  ) + GRID(I,1,K  ) + GRID(I+1,1,K  ) +     &
              !             GRID(I-1,1,K+1) + GRID(I,1,K+1) + GRID(I+1,1,K+1)

            END DO
         END DO
         ! Corner cases : off-diagonals
         IF ( MYPX == 0 ) THEN  ! X-Z plane for X=1, Y=1
         !print *,'X-Z plane for X=1, Y=1'
            DO K = 1, NZ
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,1,K-1) + GRID(1,1,K) + GRID(1,1,K+1) ) / TWENTYSEVEN
               !write(*,99) mype, 1, 1, k, GRID(1,1,K-1) + GRID(1,1,K) + GRID(1,1,K+1)
            END DO
         END IF
         IF ( MYPX == ( NPX - 1 ) ) THEN  ! X-Z plane for X=NX, Y=1
            !print *,'X-Z plane for X=NX, Y=1'
            DO K = 1, NZ
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,1,K-1) + GRID(NX,1,K) + GRID(NX,1,K+1) ) / TWENTYSEVEN
               !write(*,99) mype, nx, 1, k, GRID(NX,1,K-1) + GRID(NX,1,K) + GRID(NX,1,K+1)
            END DO
         END IF
      END IF

      IF ( MYPY == ( NPY - 1 ) ) THEN  ! X-Z plane for Y=NY
         !print *,'X-Z plane for Y=NY'
         DO K = 1, NZ
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) +                                             &
                                ( GRID(I-1,NY,K-1) + GRID(I,NY,K-1) + GRID(I+1,NY,K-1) +     &
                                  GRID(I-1,NY,K  ) + GRID(I,NY,K  ) + GRID(I+1,NY,K  ) +     & 
                                  GRID(I-1,NY,K+1) + GRID(I,NY,K+1) + GRID(I+1,NY,K+1) ) / TWENTYSEVEN
               !write(*,99) mype, i, ny, k, &
               !           GRID(I-1,NY,K-1) + GRID(I,NY,K-1) + GRID(I+1,NY,K-1) +     &
               !           GRID(I-1,NY,K  ) + GRID(I,NY,K  ) + GRID(I+1,NY,K  ) +     &
               !           GRID(I-1,NY,K+1) + GRID(I,NY,K+1) + GRID(I+1,NY,K+1)
            END DO
         END DO
         ! Corner cases : off-diagonals
         IF ( MYPX == 0 ) THEN  ! X-Z plane for X=1, Y=NY
            !print *,'X-Z plane for X=1, Y=NY'
            DO K = 1, NZ
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,NY,K-1) + GRID(1,NY,K) + GRID(1,NY,K+1) ) / TWENTYSEVEN
               !write(*,99) mype, 1, ny, k, GRID(1,NY,K-1) + GRID(1,NY,K) + GRID(1,NY,K+1)
            END DO
         END IF
         IF ( MYPX == ( NPX - 1 ) ) THEN  ! X-Z plane for X=NX, Y=NY
            !print *,'X-Z plane for X=NX, Y=NY'
            DO K = 1, NZ
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,NY,K-1) + GRID(NX,NY,K) + GRID(NX,NY,K+1) ) / TWENTYSEVEN
               !write(*,99) mype, nx, ny, k, GRID(nx,NY,K-1) + GRID(nx,NY,K) + GRID(nx,NY,K+1)
            END DO
         END IF
      END IF

      IF ( MYPX == 0 ) THEN  ! X=1
         !print *,'Y-Z plane for X=1'
         DO K = 1, NZ
            DO J = 1, NY
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) +                                    &
                              ( GRID(1,J-1,K-1) + GRID(1,J,K-1) + GRID(1,J+1,K-1) + &
                                GRID(1,J-1,K  ) + GRID(1,J,K  ) + GRID(1,J+1,K  ) + &
                                GRID(1,J-1,K+1) + GRID(1,J,K+1) + GRID(1,J+1,K+1) ) / TWENTYSEVEN
               !write(*,99) mype, 1, j, k, &
               !           GRID(1,J-1,K-1) + GRID(1,J,K-1) + GRID(1,J+1,K-1) + &
               !           GRID(1,J-1,K  ) + GRID(1,J,K  ) + GRID(1,J+1,K  ) + &
               !           GRID(1,J-1,K+1) + GRID(1,J,K+1) + GRID(1,J+1,K+1)
            END DO
         END DO
      END IF

      IF ( MYPX == ( NPX - 1 ) ) THEN  ! X=NX
         !print *,'Y-Z plane for X=NX'
         DO K = 1, NZ
            DO J = 1, NY
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) +                                       &
                              ( GRID(NX,J-1,K-1) + GRID(NX,J,K-1) + GRID(NX,J+1,K-1) + &
                                GRID(NX,J-1,K  ) + GRID(NX,J,K  ) + GRID(NX,J+1,K  ) + &
                                GRID(NX,J-1,K+1) + GRID(NX,J,K+1) + GRID(NX,J+1,K+1) ) / TWENTYSEVEN
               !write(*,99) mype, nx, j, k, &
               !            GRID(NX,J-1,K-1) + GRID(NX,J,K-1) + GRID(NX,J+1,K-1) + &
               !            GRID(NX,J-1,K  ) + GRID(NX,J,K  ) + GRID(NX,J+1,K  ) + &
               !            GRID(NX,J-1,K+1) + GRID(NX,J,K+1) + GRID(NX,J+1,K+1)
            END DO
         END DO
      END IF
      !print *

      IF ( MYPZ == 0 ) THEN  ! Z=1
         !print *,'X-Y plane for Z=1'
         DO J = 1, NY
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) +                                    &
                              ( GRID(I-1,J-1,1) + GRID(I-1,J,1) + GRID(I-1,J+1,1) + &
                                GRID(I,  J-1,1) + GRID(I,  J,1) + GRID(I,  J+1,1) + &
                                GRID(I+1,J-1,1) + GRID(I+1,J,1) + GRID(I+1,J+1,1) ) / TWENTYSEVEN
               !write(*,99) mype, i, j, 1, &
               !            GRID(I-1,J-1,1) + GRID(I-1,J,1) + GRID(I-1,J+1,1) + &
               !            GRID(I,  J-1,1) + GRID(I,  J,1) + GRID(I,  J+1,1) + &
               !            GRID(I+1,J-1,1) + GRID(I+1,J,1) + GRID(I+1,J+1,1)
            END DO
         END DO
         ! Corner cases : off-diagonals
         IF ( MYPX == 0 ) THEN
         !print *,'X-Y plane for X=1, Z=1'
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,1,1) / TWENTYSEVEN )
            !write(*,99) mype, 1, 1, 1, GRID(1,1,1)
            DO J = 1, NY ! X-Y plane for X=1, Z=1
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,J-1,1) + GRID(1,J,1) + GRID(1,J+1,1) ) / TWENTYSEVEN
               !write(*,99) mype, 1, j, 1, &
               !            GRID(1,J-1,1) + GRID(1,J,1) + GRID(1,J+1,1)
            END DO
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,NY,1) / TWENTYSEVEN )
            !write(*,99) mype, 1, NY, 1, GRID(1,NY,1)
         END IF
         IF ( MYPX == ( NPX - 1) ) THEN
         !print *,'X-Y plane for X=NX, Z=1'
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,1,1) / TWENTYSEVEN )
            !write(*,99) mype, nx, 1, 1, GRID(NX,1,1)
            DO J = 1, NY ! X-Y plane for X=NX, Z=1
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,J-1,1) + GRID(NX,J,1) + GRID(NX,J+1,1) ) / TWENTYSEVEN
               !write(*,99) mype, nx, j, 1, &
               !            GRID(NX,J-1,1) + GRID(NX,J,1) + GRID(NX,J+1,1)
            END DO
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,NY,1) / TWENTYSEVEN )
            !write(*,99) mype, nx, ny, 1, GRID(nx,NY,1)
         END IF
         !print *
         IF ( MYPY == 0 ) THEN
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(I-1,1,1) + GRID(I,1,1) + GRID(I+1,1,1) ) / TWENTYSEVEN
               !write(*,99) mype, i, 1, 1, GRID(I-1,1,1) + GRID(I,1,1) + GRID(I+1,1,1)
            END DO
         END IF
         !print *
         IF ( MYPY == ( NPY - 1 ) ) THEN
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(I-1,NY,1) + GRID(I,NY,1) + GRID(I+1,NY,1) ) / TWENTYSEVEN
               !write(*,99) mype, i, ny, 1, GRID(I-1,NY,1) + GRID(I,NY,1) + GRID(I+1,NY,1)
            END DO
         END IF
      END IF

      !print *

      IF ( MYPZ == ( NPZ - 1 ) ) THEN  ! Z=NZ
         !print *,'X-Y plane for Z=NZ'
         DO J = 1, NY
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) +                                    &
                              ( GRID(I-1,J-1,NZ) + GRID(I-1,J,NZ) + GRID(I-1,J+1,NZ) + &
                                GRID(I,  J-1,NZ) + GRID(I,  J,NZ) + GRID(I,  J+1,NZ) + &
                                GRID(I+1,J-1,NZ) + GRID(I+1,J,NZ) + GRID(I+1,J+1,NZ) ) / TWENTYSEVEN
               !write(*,99) mype, i, j, nz, &
               !           GRID(I-1,J-1,NZ) + GRID(I-1,J,NZ) + GRID(I-1,J+1,NZ) + &
               !           GRID(I,  J-1,NZ) + GRID(I,  J,NZ) + GRID(I,  J+1,NZ) + &
               !           GRID(I+1,J-1,NZ) + GRID(I+1,J,NZ) + GRID(I+1,J+1,NZ)
            END DO
         END DO
         ! Corner cases : off-diagonals
         IF ( MYPX == 0 ) THEN
         !print *,'X-Y plane for X=1, Z=NZ'
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,1,NZ) / TWENTYSEVEN )
            !write(*,99) mype, 1, 1, NZ, GRID(1,1,NZ)
            DO J = 1, NY ! X-Y plane for X=1, Z=NZ
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,J-1,NZ) + GRID(1,J,NZ) + GRID(1,J+1,NZ) ) / TWENTYSEVEN
               !write(*,99) mype, 1, j, NZ, &
                           !GRID(1,J-1,NZ) + GRID(1,J,NZ) + GRID(1,J+1,NZ)
            END DO
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(1,NY,NZ) / TWENTYSEVEN )
            !write(*,99) mype, 1, NY, NZ, GRID(1,NY,NZ)
         END IF
         IF ( MYPX == ( NPX - 1) ) THEN
         !print *,'X-Y plane for X=NX, Z=NZ'
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,1,NZ) / TWENTYSEVEN )
            !write(*,99) mype, nx, 1, NZ, GRID(NX,1,NZ)
            DO J = 1, NY ! X-Y plane for X=NX, Z=NZ
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,J-1,NZ) + GRID(NX,J,NZ) + GRID(NX,J+1,NZ) ) / TWENTYSEVEN
               !write(*,99) mype, nx, j, NZ, &
               !            GRID(NX,J-1,NZ) + GRID(NX,J,NZ) + GRID(NX,J+1,NZ)
            END DO
            FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(NX,NY,NZ) / TWENTYSEVEN )
            !write(*,99) mype, nx, ny, NZ, GRID(nx,NY,NZ)
         END IF
         IF ( MYPY == 0 ) THEN
         !print *
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(I-1,1,NZ) + GRID(I,1,NZ) + GRID(I+1,1,NZ) ) / TWENTYSEVEN
               !write(*,99) mype, i, 1, NZ, GRID(I-1,1,NZ) + GRID(I,1,NZ) + GRID(I+1,1,NZ)
            END DO
         END IF
         IF ( MYPY == ( NPY - 1 ) ) THEN
            DO I = 1, NX
               FLUX_OUT(IVAR) = FLUX_OUT(IVAR) + ( GRID(I-1,NY,NZ) + GRID(I,NY,NZ) + GRID(I+1,NY,NZ) ) / TWENTYSEVEN
               !write(*,99) mype, i, ny, NZ, GRID(I-1,NY,NZ) + GRID(I,NY,NZ) + GRID(I+1,NY,NZ)
            END DO
         END IF
      END IF

  11  format ( '[pe ', i2, '] ', 1pe10.3,' ', A20, ' GRID(', i2, ',', i2, ',', i2,')=', 1pe10.3 )
  12  format ( '          ', 1pe10.3,' ', 1pe10.3,' ', 1pe10.3/, &
               '          ', 1pe10.3,' ', 1pe10.3,' ', 1pe10.3/, & 
               '          ', 1pe10.3,' ', 1pe10.3,' ', 1pe10.3/, &
               '        = ', 1pe10.3 )

  99  format ( '[pe ', i2, '] (', i2, ',', i2, ',', i2,'):', 2pe10.3 )
      endif

#if defined _MG_MPI
      call mpi_barrier ( MPI_COMM_WORLD, ierr )
#endif
!      if ( mype==ii) print *
#if defined _MG_MPI
      call mpi_barrier ( MPI_COMM_WORLD, ierr )
#endif
      enddo

   END SUBROUTINE MG_FLUX_ACCUMULATE_3D27PT
   
END MODULE MG_FLUX_ACCUMULATE_3D27PT_MOD
