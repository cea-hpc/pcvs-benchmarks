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
 
MODULE MG_CONSTANTS_MOD

   ! Purpose
   ! =======
   ! Declarations of various constant values used throughout miniGhost.

   IMPLICIT NONE

#if defined _MG_MPI
   include 'mpif.h'
#endif

   ! This is miniGhost version :
   CHARACTER*7, PARAMETER :: MG_VERSION = "1.0.0  "

   ! Default types should be used unless otherwise required (e.g. MPI_Wtime is REAL8).
   ! Fortran datatype parameters

   INTEGER(KIND=1_4), PARAMETER :: MG_INT4 = 4 
   INTEGER(KIND=1_4), PARAMETER :: MG_INT8 = 8

   INTEGER(KIND=1_4), PARAMETER :: MG_REAL4 = 4
   INTEGER(KIND=1_4), PARAMETER :: MG_REAL8 = 8

#if defined ( _MG_INT4 )
   INTEGER(KIND=1_4), PARAMETER :: MG_INT  = 4
#elif defined ( _MG_INT8 )
   INTEGER(KIND=1_4), PARAMETER :: MG_INT = 8
#else
   Set INTEGER precision: -D_INT4 or -D_INT8. Default (INT4) was over-ridden?
#endif

#if defined ( _MG_REAL4 )
   INTEGER(KIND=1_4), PARAMETER :: MG_REAL = 4
#elif defined ( _MG_REAL8 )
   INTEGER(KIND=1_4), PARAMETER :: MG_REAL = 8
#else
   Set REAL precision: -D_REAL4 or -D_REAL8. Default (REAL8) was over-ridden?
#endif

#if defined _MG_MPI
#if defined ( _MG_INT4 )
   INTEGER(MG_INT4), PARAMETER :: MG_MPI_INT = MPI_INTEGER4
#elif defined ( _MG_INT8 )
   INTEGER(MG_INT4), PARAMETER :: MG_MPI_INT = MPI_INTEGER8
#endif

#if defined ( _MG_REAL4 )
   INTEGER(MG_INT4), PARAMETER :: &
      MG_MPI_REAL = MPI_REAL4,    &
      SIZE_OF_DATA = 4
#elif defined ( _MG_REAL8 )
   INTEGER(MG_INT4), PARAMETER :: &
      MG_MPI_REAL = MPI_REAL8,    &
      SIZE_OF_DATA = 8
#endif
#endif

   REAL(KIND=MG_REAL), PARAMETER ::        &
      EPS           = 1.0E-4,              &
      GIGA          = 1000000000.0,        &
      ONE_THOUSAND  = 1000.0,              &
      ZERO          = 0.0

   ! ---------
   ! Variables
   ! ---------

   REAL(KIND=MG_REAL), PARAMETER :: &
#if defined ( _MG_REAL4 )
      FIVE        = 5.0E+0,            &
      SEVEN       = 7.0E+0,            &
      NINE        = 9.0E+0,            &
      TWENTYSEVEN = 27.0E+0
#elif defined ( _MG_REAL8 )
      FIVE        = 5.0D+0,            &
      SEVEN       = 7.0D+0,            &     
      NINE        = 9.0D+0,            &     
      TWENTYSEVEN = 27.0D+0
#endif

   INTEGER(KIND=MG_INT) ::             &

      GLOBAL_NX,                       &  ! Global dimensions.
      GLOBAL_NY,                       &
      GLOBAL_NZ,                       &

      MY_GLOBAL_NX_START,              &  ! Starting index of local NX in global grid.
      MY_GLOBAL_NY_START,              &  ! Starting index of local NY in global grid.
      MY_GLOBAL_NZ_START,              &  ! Starting index of local NZ in global grid.
      MY_GLOBAL_NX_END,                &  ! Ending index of local NX in global grid.
      MY_GLOBAL_NY_END,                &  ! Ending index of local NY in global grid.
      MY_GLOBAL_NZ_END,                &  ! Ending index of local NZ in global grid.
      NUM_NEIGHS,                      &  ! Number of parallel process (rank) neighbors.
      NUM_SUM_GRID,                    &  ! Number of variables globally summed each time step.

      ! Parallel env parameters.
      MYPE,                            &  ! MPI rank.
      NUMPES,                          &  ! Number of MPI ranks.
      MPI_COMM_MG,                     &  ! Duplicate of MPI_COMM_WORLD
      MYPX, MYPY, MYPZ                    ! Processor position in the processor grid.

   REAL(KIND=MG_REAL) ::               &

      ERROR_TOL                        ! Error tolerance.

   LOGICAL, DIMENSION(:), ALLOCATABLE :: &
      GRIDS_TO_SUM                     ! List of GRID arrays to sum.

   INTEGER(KIND=MG_INT), DIMENSION(:,:), ALLOCATABLE ::   &
      SPIKE_LOC                        ! Location of spike.

   REAL(KIND=MG_REAL),   DIMENSION(:), ALLOCATABLE ::   &

      FLUX_OUT,                     &  ! Keeps track of heat dissapation out of physical domain.
      SOURCE_TOTAL                     ! Keeps track of heat inserted into each variable.

   REAL(KIND=MG_REAL),   DIMENSION(:,:), ALLOCATABLE ::   &
      SPIKES                           ! Heat spike to be inserted.

   REAL(KIND=MG_REAL),   DIMENSION(:,:,:),  ALLOCATABLE ::   &
      WORK                             ! Workspace for computation

   INTEGER(KIND=MG_INT), PARAMETER :: &

      ROOTPE = 0,                    &

      NORTH = 1,                      &
      SOUTH = 2,                      &
      EAST  = 3,                      &
      WEST  = 4,                      &
      BACK  = 5,                      &
      FRONT = 6,                      &

      MAX_NUM_NEIGHBORS = 6

   INTEGER(KIND=MG_INT), PARAMETER ::  &

      DIR_NORTH_SOUTH = 11,            &
      DIR_EAST_WEST   = 13,            &
      DIR_BACK_FRONT  = 15

   ! ------------------------------
   ! Message buffer workspace, etc.
   ! ------------------------------

   INTEGER(KIND=MG_INT) ::        &

      !TSTEP,                      &  ! Time step counter.

      MAX_NUM_SENDS,              &
      MAX_NUM_RECVS,              &

      NUM_RECVS,                  &  ! Number of recvs posted.
      NUM_SENDS,                  &

      COUNT_RECV_BACK,            &
      COUNT_RECV_FRONT,           &
      COUNT_RECV_EAST,            &
      COUNT_RECV_WEST,            &
      COUNT_RECV_NORTH,           &
      COUNT_RECV_SOUTH,           &

      COUNT_SEND_BACK,            &
      COUNT_SEND_FRONT,           &
      COUNT_SEND_EAST,            &
      COUNT_SEND_WEST,            &
      COUNT_SEND_NORTH,           &
      COUNT_SEND_SOUTH,           &

      RECV_BUFFER_NORTH_SIZE,     &
      RECV_BUFFER_SOUTH_SIZE,     &
      RECV_BUFFER_EAST_SIZE,      &
      RECV_BUFFER_WEST_SIZE,      &
      RECV_BUFFER_BACK_SIZE,      &
      RECV_BUFFER_FRONT_SIZE,     &

      SEND_BUFFER_NORTH_SIZE,     & 
      SEND_BUFFER_SOUTH_SIZE,     & 
      SEND_BUFFER_EAST_SIZE,      & 
      SEND_BUFFER_WEST_SIZE,      & 
      SEND_BUFFER_BACK_SIZE,      & 
      SEND_BUFFER_FRONT_SIZE

   INTEGER(KIND=MG_INT),   DIMENSION(:), ALLOCATABLE ::      &

      MSG_REQS,                    &
      MSG_TAGS,                    &
      NEIGHBORS,                   &
      NEIGHBORS_ORIG                  ! Temporary storage for DIAG exchanges. May be needed for AMR?

   REAL(KIND=MG_REAL),   DIMENSION(:), ALLOCATABLE ::      &

      RECV_BUFFER_BACK,            &
      RECV_BUFFER_FRONT,           &
      RECV_BUFFER_EAST,            &
      RECV_BUFFER_WEST,            &
      RECV_BUFFER_NORTH,           &
      RECV_BUFFER_SOUTH,           &

      SEND_BUFFER_BACK,            & 
      SEND_BUFFER_FRONT,           & 
      SEND_BUFFER_EAST,            & 
      SEND_BUFFER_WEST,            & 
      SEND_BUFFER_NORTH,           & 
      SEND_BUFFER_SOUTH              

END MODULE MG_CONSTANTS_MOD
