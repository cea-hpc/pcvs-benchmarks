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

MODULE update_tile_halo_module

CONTAINS

  SUBROUTINE update_tile_halo(fields,depth)

    USE clover_module
    USE update_tile_halo_kernel_module

    IMPLICIT NONE

    INTEGER :: tile,fields(NUM_FIELDS), depth
    INTEGER :: t_left, t_right, t_up, t_down

        !TODO: fix the chunk comms phase
      !CALL clover_exchange(fields,depth)



    ! Update Top Bottom - Real to Real


    DO tile=1,tiles_per_chunk
      t_up   =chunk%tiles(tile)%tile_neighbours(TILE_TOP)
      t_down =chunk%tiles(tile)%tile_neighbours(TILE_BOTTOM)

      IF(t_up.NE.EXTERNAL_TILE) THEN
        call update_tile_halo_t_kernel(                                        &
          chunk%tiles(tile)%t_xmin,                              &
          chunk%tiles(tile)%t_xmax,                              &
          chunk%tiles(tile)%t_ymin,                              &
          chunk%tiles(tile)%t_ymax,                              &
          chunk%tiles(tile)%field%density0,                      &
          chunk%tiles(tile)%field%energy0,                       &
          chunk%tiles(tile)%field%pressure,                      &
          chunk%tiles(tile)%field%viscosity,                     &
          chunk%tiles(tile)%field%soundspeed,                    &
          chunk%tiles(tile)%field%density1,                      &
          chunk%tiles(tile)%field%energy1,                       &
          chunk%tiles(tile)%field%xvel0,                         &
          chunk%tiles(tile)%field%yvel0,                         &
          chunk%tiles(tile)%field%xvel1,                         &
          chunk%tiles(tile)%field%yvel1,                         &
          chunk%tiles(tile)%field%vol_flux_x,                    &
          chunk%tiles(tile)%field%vol_flux_y,                    &
          chunk%tiles(tile)%field%mass_flux_x,                   &
          chunk%tiles(tile)%field%mass_flux_y,                   &
          chunk%tiles(t_up)%t_xmin,                           &
          chunk%tiles(t_up)%t_xmax,                           &
          chunk%tiles(t_up)%t_ymin,                           &
          chunk%tiles(t_up)%t_ymax,                           &
          chunk%tiles(t_up)%field%density0,                   &
          chunk%tiles(t_up)%field%energy0,                    &
          chunk%tiles(t_up)%field%pressure,                   &
          chunk%tiles(t_up)%field%viscosity,                  &
          chunk%tiles(t_up)%field%soundspeed,                 &
          chunk%tiles(t_up)%field%density1,                   &
          chunk%tiles(t_up)%field%energy1,                    &
          chunk%tiles(t_up)%field%xvel0,                      &
          chunk%tiles(t_up)%field%yvel0,                      &
          chunk%tiles(t_up)%field%xvel1,                      &
          chunk%tiles(t_up)%field%yvel1,                      &
          chunk%tiles(t_up)%field%vol_flux_x,                 &
          chunk%tiles(t_up)%field%vol_flux_y,                 &
          chunk%tiles(t_up)%field%mass_flux_x,                &
          chunk%tiles(t_up)%field%mass_flux_y,                &
          fields,                                                   &
          depth)
      !ELSE
     
      END IF

      IF(t_down.NE.EXTERNAL_TILE) THEN
        call update_tile_halo_b_kernel(                                        &
          chunk%tiles(tile)%t_xmin,                              &
          chunk%tiles(tile)%t_xmax,                              &
          chunk%tiles(tile)%t_ymin,                              &
          chunk%tiles(tile)%t_ymax,                              &
          chunk%tiles(tile)%field%density0,                      &
          chunk%tiles(tile)%field%energy0,                       &
          chunk%tiles(tile)%field%pressure,                      &
          chunk%tiles(tile)%field%viscosity,                     &
          chunk%tiles(tile)%field%soundspeed,                    &
          chunk%tiles(tile)%field%density1,                      &
          chunk%tiles(tile)%field%energy1,                       &
          chunk%tiles(tile)%field%xvel0,                         &
          chunk%tiles(tile)%field%yvel0,                         &
          chunk%tiles(tile)%field%xvel1,                         &
          chunk%tiles(tile)%field%yvel1,                         &
          chunk%tiles(tile)%field%vol_flux_x,                    &
          chunk%tiles(tile)%field%vol_flux_y,                    &
          chunk%tiles(tile)%field%mass_flux_x,                   &
          chunk%tiles(tile)%field%mass_flux_y,                   &
          chunk%tiles(t_down)%t_xmin,                           &
          chunk%tiles(t_down)%t_xmax,                           &
          chunk%tiles(t_down)%t_ymin,                           &
          chunk%tiles(t_down)%t_ymax,                           &
          chunk%tiles(t_down)%field%density0,                   &
          chunk%tiles(t_down)%field%energy0,                    &
          chunk%tiles(t_down)%field%pressure,                   &
          chunk%tiles(t_down)%field%viscosity,                  &
          chunk%tiles(t_down)%field%soundspeed,                 &
          chunk%tiles(t_down)%field%density1,                   &
          chunk%tiles(t_down)%field%energy1,                    &
          chunk%tiles(t_down)%field%xvel0,                      &
          chunk%tiles(t_down)%field%yvel0,                      &
          chunk%tiles(t_down)%field%xvel1,                      &
          chunk%tiles(t_down)%field%yvel1,                      &
          chunk%tiles(t_down)%field%vol_flux_x,                 &
          chunk%tiles(t_down)%field%vol_flux_y,                 &
          chunk%tiles(t_down)%field%mass_flux_x,                &
          chunk%tiles(t_down)%field%mass_flux_y,                &
          fields,                                                   &
          depth)
      !ELSE

      END IF

  
    END DO

    ! Update Left Right - Ghost, Real, Ghost - > Real

    DO tile=1,tiles_per_chunk
      t_left   =chunk%tiles(tile)%tile_neighbours(TILE_LEFT)
      t_right  =chunk%tiles(tile)%tile_neighbours(TILE_RIGHT)
  
      IF(t_left.NE.EXTERNAL_TILE) THEN
        call update_tile_halo_l_kernel(                                        &
          chunk%tiles(tile)%t_xmin,                              &
          chunk%tiles(tile)%t_xmax,                              &
          chunk%tiles(tile)%t_ymin,                              &
          chunk%tiles(tile)%t_ymax,                              &
          chunk%tiles(tile)%field%density0,                      &
          chunk%tiles(tile)%field%energy0,                       &
          chunk%tiles(tile)%field%pressure,                      &
          chunk%tiles(tile)%field%viscosity,                     &
          chunk%tiles(tile)%field%soundspeed,                    &
          chunk%tiles(tile)%field%density1,                      &
          chunk%tiles(tile)%field%energy1,                       &
          chunk%tiles(tile)%field%xvel0,                         &
          chunk%tiles(tile)%field%yvel0,                         &
          chunk%tiles(tile)%field%xvel1,                         &
          chunk%tiles(tile)%field%yvel1,                         &
          chunk%tiles(tile)%field%vol_flux_x,                    &
          chunk%tiles(tile)%field%vol_flux_y,                    &
          chunk%tiles(tile)%field%mass_flux_x,                   &
          chunk%tiles(tile)%field%mass_flux_y,                   &
          chunk%tiles(t_left)%t_xmin,                           &
          chunk%tiles(t_left)%t_xmax,                           &
          chunk%tiles(t_left)%t_ymin,                           &
          chunk%tiles(t_left)%t_ymax,                           &
          chunk%tiles(t_left)%field%density0,                   &
          chunk%tiles(t_left)%field%energy0,                    &
          chunk%tiles(t_left)%field%pressure,                   &
          chunk%tiles(t_left)%field%viscosity,                  &
          chunk%tiles(t_left)%field%soundspeed,                 &
          chunk%tiles(t_left)%field%density1,                   &
          chunk%tiles(t_left)%field%energy1,                    &
          chunk%tiles(t_left)%field%xvel0,                      &
          chunk%tiles(t_left)%field%yvel0,                      &
          chunk%tiles(t_left)%field%xvel1,                      &
          chunk%tiles(t_left)%field%yvel1,                      &
          chunk%tiles(t_left)%field%vol_flux_x,                 &
          chunk%tiles(t_left)%field%vol_flux_y,                 &
          chunk%tiles(t_left)%field%mass_flux_x,                &
          chunk%tiles(t_left)%field%mass_flux_y,                &
          fields,                                                   &
          depth)
      !ELSE

      END IF

      IF(t_right.NE.EXTERNAL_TILE) THEN
        call update_tile_halo_r_kernel(                                        &
          chunk%tiles(tile)%t_xmin,                              &
          chunk%tiles(tile)%t_xmax,                              &
          chunk%tiles(tile)%t_ymin,                              &
          chunk%tiles(tile)%t_ymax,                              &
          chunk%tiles(tile)%field%density0,                      &
          chunk%tiles(tile)%field%energy0,                       &
          chunk%tiles(tile)%field%pressure,                      &
          chunk%tiles(tile)%field%viscosity,                     &
          chunk%tiles(tile)%field%soundspeed,                    &
          chunk%tiles(tile)%field%density1,                      &
          chunk%tiles(tile)%field%energy1,                       &
          chunk%tiles(tile)%field%xvel0,                         &
          chunk%tiles(tile)%field%yvel0,                         &
          chunk%tiles(tile)%field%xvel1,                         &
          chunk%tiles(tile)%field%yvel1,                         &
          chunk%tiles(tile)%field%vol_flux_x,                    &
          chunk%tiles(tile)%field%vol_flux_y,                    &
          chunk%tiles(tile)%field%mass_flux_x,                   &
          chunk%tiles(tile)%field%mass_flux_y,                   &
          chunk%tiles(t_right)%t_xmin,                           &
          chunk%tiles(t_right)%t_xmax,                           &
          chunk%tiles(t_right)%t_ymin,                           &
          chunk%tiles(t_right)%t_ymax,                           &
          chunk%tiles(t_right)%field%density0,                   &
          chunk%tiles(t_right)%field%energy0,                    &
          chunk%tiles(t_right)%field%pressure,                   &
          chunk%tiles(t_right)%field%viscosity,                  &
          chunk%tiles(t_right)%field%soundspeed,                 &
          chunk%tiles(t_right)%field%density1,                   &
          chunk%tiles(t_right)%field%energy1,                    &
          chunk%tiles(t_right)%field%xvel0,                      &
          chunk%tiles(t_right)%field%yvel0,                      &
          chunk%tiles(t_right)%field%xvel1,                      &
          chunk%tiles(t_right)%field%yvel1,                      &
          chunk%tiles(t_right)%field%vol_flux_x,                 &
          chunk%tiles(t_right)%field%vol_flux_y,                 &
          chunk%tiles(t_right)%field%mass_flux_x,                &
          chunk%tiles(t_right)%field%mass_flux_y,                &
          fields,                                                   &
          depth)
      !ELSE

      END IF


    END DO





  END SUBROUTINE update_tile_halo

END MODULE update_tile_halo_module
