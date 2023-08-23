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

MODULE MG_BOUNDARY_CONDITIONS_MOD

   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   USE MG_FLUX_ACCUMULATE_2D5PT_3D7PT_MOD
   USE MG_FLUX_ACCUMULATE_2D9PT_MOD
   USE MG_FLUX_ACCUMULATE_3D27PT_MOD

   IMPLICIT NONE

   ! Driver for calling the appopriate boundary conditions.

CONTAINS
   
   SUBROUTINE MG_BOUNDARY_CONDITIONS ( GRID, IVAR, IERR )
   
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

      REAL(KIND=MG_REAL) :: &
         DIVISOR

      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0

      SELECT CASE ( BC )

         CASE ( BC_DIRICHLET )

            SELECT CASE ( STENCIL )

               CASE ( STENCIL_3D7PT )

                  CALL MG_FLUX_ACCUMULATE_2D5PT_3D7PT ( GRID(0,0,0), IVAR, IERR )

               CASE ( STENCIL_3D27PT )

                  CALL MG_FLUX_ACCUMULATE_3D27PT ( GRID(0,0,0), IVAR, IERR )

               CASE ( STENCIL_2D5PT )

                  CALL MG_FLUX_ACCUMULATE_2D5PT_3D7PT ( GRID(0,0,0), IVAR, IERR )

               CASE ( STENCIL_2D9PT )

                  CALL MG_FLUX_ACCUMULATE_2D9PT ( GRID(0,0,0), IVAR, IERR )

               CASE DEFAULT

                  WRITE(*,*) ' ** Error ** MG_BOUNDARY_CONDITIONS: Unknown STENCIL = ' , &
                     STENCIL
                  IERR = -1

            END SELECT

         CASE DEFAULT
            WRITE(*,*) ' ** Error ** MG_BOUNDARY_CONDITIONS: Unknown BOUNDARY CONDITIONS = ' , &
               BC 
            IERR = -1

      END SELECT

   END SUBROUTINE MG_BOUNDARY_CONDITIONS
   
END MODULE MG_BOUNDARY_CONDITIONS_MOD
