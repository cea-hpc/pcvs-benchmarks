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

!>  @brief Driver for the halo updates
!>  @author Wayne Gaudin
!>  @details Invokes the kernels for the internal and external halo cells for
!>  the fields specified.

MODULE update_halo_module

CONTAINS

  SUBROUTINE update_halo(fields,depth)

    USE clover_module
    USE update_tile_halo_module
    USE update_halo_kernel_module

    IMPLICIT NONE

    INTEGER :: tile,fields(NUM_FIELDS),depth
    REAL(KIND=8) :: kernel_time,timer

      !TODO: fix the chunk comms phase
    IF(profiler_on) kernel_time=timer()
    CALL update_tile_halo(fields,depth)
    IF(profiler_on) profiler%tile_halo_exchange=profiler%tile_halo_exchange+(timer()-kernel_time)
    IF(profiler_on) kernel_time=timer()
    CALL clover_exchange(fields,depth)
    IF(profiler_on) profiler%mpi_halo_exchange=profiler%mpi_halo_exchange+(timer()-kernel_time)
 
    IF(profiler_on) kernel_time=timer()
    IF (  (chunk%chunk_neighbours(CHUNK_LEFT) .EQ. EXTERNAL_FACE) .OR.     &
      (chunk%chunk_neighbours(CHUNK_RIGHT) .EQ. EXTERNAL_FACE) .OR.    &
      (chunk%chunk_neighbours(CHUNK_BOTTOM) .EQ. EXTERNAL_FACE) .OR.   &
      (chunk%chunk_neighbours(CHUNK_TOP) .EQ. EXTERNAL_FACE) ) THEN


      IF(use_fortran_kernels) THEN
        DO tile=1,tiles_per_chunk

          CALL update_halo_kernel(chunk%tiles(tile)%t_xmin,          &
            chunk%tiles(tile)%t_xmax,          &
            chunk%tiles(tile)%t_ymin,          &
            chunk%tiles(tile)%t_ymax,          &
            chunk%chunk_neighbours,     &
            chunk%tiles(tile)%tile_neighbours,     &
            chunk%tiles(tile)%field%density0,       &
            chunk%tiles(tile)%field%energy0,        &
            chunk%tiles(tile)%field%pressure,       &
            chunk%tiles(tile)%field%viscosity,      &
            chunk%tiles(tile)%field%soundspeed,     &
            chunk%tiles(tile)%field%density1,       &
            chunk%tiles(tile)%field%energy1,        &
            chunk%tiles(tile)%field%xvel0,          &
            chunk%tiles(tile)%field%yvel0,          &
            chunk%tiles(tile)%field%xvel1,          &
            chunk%tiles(tile)%field%yvel1,          &
            chunk%tiles(tile)%field%vol_flux_x,     &
            chunk%tiles(tile)%field%vol_flux_y,     &
            chunk%tiles(tile)%field%mass_flux_x,    &
            chunk%tiles(tile)%field%mass_flux_y,    &
            fields,                         &
            depth                          )


        ENDDO

      ELSEIF(use_C_kernels) THEN
        DO tile=1,tiles_per_chunk

          CALL update_halo_kernel_c(chunk%tiles(tile)%t_xmin,          &
            chunk%tiles(tile)%t_xmax,          &
            chunk%tiles(tile)%t_ymin,          &
            chunk%tiles(tile)%t_ymax,          &
            chunk%chunk_neighbours,     &
            chunk%tiles(tile)%tile_neighbours,     &
            chunk%tiles(tile)%field%density0,       &
            chunk%tiles(tile)%field%energy0,        &
            chunk%tiles(tile)%field%pressure,       &
            chunk%tiles(tile)%field%viscosity,      &
            chunk%tiles(tile)%field%soundspeed,     &
            chunk%tiles(tile)%field%density1,       &
            chunk%tiles(tile)%field%energy1,        &
            chunk%tiles(tile)%field%xvel0,          &
            chunk%tiles(tile)%field%yvel0,          &
            chunk%tiles(tile)%field%xvel1,          &
            chunk%tiles(tile)%field%yvel1,          &
            chunk%tiles(tile)%field%vol_flux_x,     &
            chunk%tiles(tile)%field%vol_flux_y,     &
            chunk%tiles(tile)%field%mass_flux_x,    &
            chunk%tiles(tile)%field%mass_flux_y,    &
            fields,                         &
            depth                          )


        ENDDO
      ENDIF
    ENDIF

    IF(profiler_on) profiler%self_halo_exchange=profiler%self_halo_exchange+(timer()-kernel_time)

  END SUBROUTINE update_halo

END MODULE update_halo_module
