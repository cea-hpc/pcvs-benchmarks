!Crown Copyright 2012 AWE.
!
! This file is part of CloverLeaf.
!
! CloverLeaf is free software: you can redistribute it and/or modify it under 
! the terms of the GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or (at your option) 
! any later version.
!
! CloverLeaf is distributed in the hope that it will be useful, but 
! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
! details.
!
! You should have received a copy of the GNU General Public License along with 
! CloverLeaf. If not, see http://www.gnu.org/licenses/.

!>  @brief Reset field driver
!>  @author Wayne Gaudin
!>  @details Invokes the user specified field reset kernel.

MODULE reset_field_module

CONTAINS

  SUBROUTINE reset_field()

    USE clover_module
    USE reset_field_kernel_module

    IMPLICIT NONE

    INTEGER :: tile

    REAL(KIND=8) :: kernel_time,timer

    IF(profiler_on) kernel_time=timer()

    IF(use_fortran_kernels) THEN

      DO tile=1,tiles_per_chunk


        CALL reset_field_kernel(chunk%tiles(tile)%t_xmin,   &
          chunk%tiles(tile)%t_xmax,     &
          chunk%tiles(tile)%t_ymin,     &
          chunk%tiles(tile)%t_ymax,     &
          chunk%tiles(tile)%field%density0,  &
          chunk%tiles(tile)%field%density1,  &
          chunk%tiles(tile)%field%energy0,   &
          chunk%tiles(tile)%field%energy1,   &
          chunk%tiles(tile)%field%xvel0,     &
          chunk%tiles(tile)%field%xvel1,     &
          chunk%tiles(tile)%field%yvel0,     &
          chunk%tiles(tile)%field%yvel1      )

      ENDDO
  
    ELSEIF(use_C_kernels) THEN
      DO tile=1,tiles_per_chunk


        CALL reset_field_kernel_c(chunk%tiles(tile)%t_xmin,   &
          chunk%tiles(tile)%t_xmax,     &
          chunk%tiles(tile)%t_ymin,     &
          chunk%tiles(tile)%t_ymax,     &
          chunk%tiles(tile)%field%density0,  &
          chunk%tiles(tile)%field%density1,  &
          chunk%tiles(tile)%field%energy0,   &
          chunk%tiles(tile)%field%energy1,   &
          chunk%tiles(tile)%field%xvel0,     &
          chunk%tiles(tile)%field%xvel1,     &
          chunk%tiles(tile)%field%yvel0,     &
          chunk%tiles(tile)%field%yvel1      )

      ENDDO
    ENDIF


    IF(profiler_on) profiler%reset=profiler%reset+(timer()-kernel_time)

  END SUBROUTINE reset_field

END MODULE reset_field_module
