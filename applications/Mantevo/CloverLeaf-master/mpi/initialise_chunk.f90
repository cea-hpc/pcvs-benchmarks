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

!>  @brief Driver for chunk initialisation.
!>  @author Wayne Gaudin
!>  @details Invokes the user specified chunk initialisation kernel.

SUBROUTINE initialise_chunk(tile)

  USE clover_module
  USE initialise_chunk_kernel_module

  IMPLICIT NONE

  INTEGER :: tile

  REAL(KIND=8) :: xmin,ymin,dx,dy

  dx=(grid%xmax-grid%xmin)/float(grid%x_cells)
  dy=(grid%ymax-grid%ymin)/float(grid%y_cells)

  xmin=grid%xmin+dx*float(chunk%tiles(tile)%t_left-1)

  ymin=grid%ymin+dy*float(chunk%tiles(tile)%t_bottom-1)

  
  IF(use_fortran_kernels) THEN

    CALL initialise_chunk_kernel(chunk%tiles(tile)%t_xmin,    &
      chunk%tiles(tile)%t_xmax,    &
      chunk%tiles(tile)%t_ymin,    &
      chunk%tiles(tile)%t_ymax,    &
      xmin,ymin,dx,dy,              &
      chunk%tiles(tile)%field%vertexx,  &
      chunk%tiles(tile)%field%vertexdx, &
      chunk%tiles(tile)%field%vertexy,  &
      chunk%tiles(tile)%field%vertexdy, &
      chunk%tiles(tile)%field%cellx,    &
      chunk%tiles(tile)%field%celldx,   &
      chunk%tiles(tile)%field%celly,    &
      chunk%tiles(tile)%field%celldy,   &
      chunk%tiles(tile)%field%volume,   &
      chunk%tiles(tile)%field%xarea,    &
      chunk%tiles(tile)%field%yarea     )

  ELSEIF(use_C_kernels) THEN
    CALL initialise_chunk_kernel_c(chunk%tiles(tile)%t_xmin,    &
      chunk%tiles(tile)%t_xmax,    &
      chunk%tiles(tile)%t_ymin,    &
      chunk%tiles(tile)%t_ymax,    &
      xmin,ymin,dx,dy,              &
      chunk%tiles(tile)%field%vertexx,  &
      chunk%tiles(tile)%field%vertexdx, &
      chunk%tiles(tile)%field%vertexy,  &
      chunk%tiles(tile)%field%vertexdy, &
      chunk%tiles(tile)%field%cellx,    &
      chunk%tiles(tile)%field%celldx,   &
      chunk%tiles(tile)%field%celly,    &
      chunk%tiles(tile)%field%celldy,   &
      chunk%tiles(tile)%field%volume,   &
      chunk%tiles(tile)%field%xarea,    &
      chunk%tiles(tile)%field%yarea     )

  ENDIF

END SUBROUTINE initialise_chunk
