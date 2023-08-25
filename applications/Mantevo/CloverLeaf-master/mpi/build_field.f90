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

!>  @brief  Allocates the data for each mesh chunk
!>  @author Wayne Gaudin
!>  @details The data fields for the mesh chunk are allocated based on the mesh
!>  size.

SUBROUTINE build_field()

  USE clover_module

  IMPLICIT NONE

  INTEGER :: tile,j,k

  DO tile=1, tiles_per_chunk

    ALLOCATE(chunk%tiles(tile)%field%density0  (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%density1  (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%energy0   (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%energy1   (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%pressure  (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%viscosity (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%soundspeed(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))

    ALLOCATE(chunk%tiles(tile)%field%xvel0(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%xvel1(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%yvel0(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%yvel1(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))

    ALLOCATE(chunk%tiles(tile)%field%vol_flux_x (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%mass_flux_x(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%vol_flux_y (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%mass_flux_y(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))

    ALLOCATE(chunk%tiles(tile)%field%work_array1(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%work_array2(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%work_array3(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%work_array4(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%work_array5(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%work_array6(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%work_array7(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))

    ALLOCATE(chunk%tiles(tile)%field%cellx   (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2))
    ALLOCATE(chunk%tiles(tile)%field%celly   (chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%vertexx (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3))
    ALLOCATE(chunk%tiles(tile)%field%vertexy (chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%celldx  (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2))
    ALLOCATE(chunk%tiles(tile)%field%celldy  (chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%vertexdx(chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3))
    ALLOCATE(chunk%tiles(tile)%field%vertexdy(chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))
    ALLOCATE(chunk%tiles(tile)%field%volume  (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%xarea   (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+3, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+2))
    ALLOCATE(chunk%tiles(tile)%field%yarea   (chunk%tiles(tile)%t_xmin-2:chunk%tiles(tile)%t_xmax+2, &
      chunk%tiles(tile)%t_ymin-2:chunk%tiles(tile)%t_ymax+3))

    ! Zeroing isn't strictly neccessary but it ensures physical pages
    ! are allocated. This prevents first touch overheads in the main code
    ! cycle which can skew timings in the first step



    DO k=chunk%tiles(tile)%t_ymin-2,chunk%tiles(tile)%t_ymax+3
      DO j=chunk%tiles(tile)%t_xmin-2,chunk%tiles(tile)%t_xmax+3
        chunk%tiles(tile)%field%work_array1(j,k)=0.0
        chunk%tiles(tile)%field%work_array2(j,k)=0.0
        chunk%tiles(tile)%field%work_array3(j,k)=0.0
        chunk%tiles(tile)%field%work_array4(j,k)=0.0
        chunk%tiles(tile)%field%work_array5(j,k)=0.0
        chunk%tiles(tile)%field%work_array6(j,k)=0.0
        chunk%tiles(tile)%field%work_array7(j,k)=0.0

        chunk%tiles(tile)%field%xvel0(j,k)=0.0
        chunk%tiles(tile)%field%xvel1(j,k)=0.0
        chunk%tiles(tile)%field%yvel0(j,k)=0.0
        chunk%tiles(tile)%field%yvel1(j,k)=0.0
      ENDDO
    ENDDO



    DO k=chunk%tiles(tile)%t_ymin-2,chunk%tiles(tile)%t_ymax+2
      DO j=chunk%tiles(tile)%t_xmin-2,chunk%tiles(tile)%t_xmax+2
        chunk%tiles(tile)%field%density0(j,k)=0.0
        chunk%tiles(tile)%field%density1(j,k)=0.0
        chunk%tiles(tile)%field%energy0(j,k)=0.0
        chunk%tiles(tile)%field%energy1(j,k)=0.0
        chunk%tiles(tile)%field%pressure(j,k)=0.0
        chunk%tiles(tile)%field%viscosity(j,k)=0.0
        chunk%tiles(tile)%field%soundspeed(j,k)=0.0
        chunk%tiles(tile)%field%volume(j,k)=0.0
      ENDDO
    ENDDO



    DO k=chunk%tiles(tile)%t_ymin-2,chunk%tiles(tile)%t_ymax+2
      DO j=chunk%tiles(tile)%t_xmin-2,chunk%tiles(tile)%t_xmax+3
        chunk%tiles(tile)%field%vol_flux_x(j,k)=0.0
        chunk%tiles(tile)%field%mass_flux_x(j,k)=0.0
        chunk%tiles(tile)%field%xarea(j,k)=0.0
      ENDDO
    ENDDO


    DO k=chunk%tiles(tile)%t_ymin-2,chunk%tiles(tile)%t_ymax+3
      DO j=chunk%tiles(tile)%t_xmin-2,chunk%tiles(tile)%t_xmax+2
        chunk%tiles(tile)%field%vol_flux_y(j,k)=0.0
        chunk%tiles(tile)%field%mass_flux_y(j,k)=0.0
        chunk%tiles(tile)%field%yarea(j,k)=0.0
      ENDDO
    ENDDO




    DO j=chunk%tiles(tile)%t_xmin-2,chunk%tiles(tile)%t_xmax+2
      chunk%tiles(tile)%field%cellx(j)=0.0
      chunk%tiles(tile)%field%celldx(j)=0.0
    ENDDO


    DO k=chunk%tiles(tile)%t_ymin-2,chunk%tiles(tile)%t_ymax+2
      chunk%tiles(tile)%field%celly(k)=0.0
      chunk%tiles(tile)%field%celldy(k)=0.0
    ENDDO



    DO j=chunk%tiles(tile)%t_xmin-2,chunk%tiles(tile)%t_xmax+3
      chunk%tiles(tile)%field%vertexx(j)=0.0
      chunk%tiles(tile)%field%vertexdx(j)=0.0
    ENDDO


    DO k=chunk%tiles(tile)%t_ymin-2,chunk%tiles(tile)%t_ymax+3
      chunk%tiles(tile)%field%vertexy(k)=0.0
      chunk%tiles(tile)%field%vertexdy(k)=0.0
    ENDDO



 
  END DO
 
END SUBROUTINE build_field
