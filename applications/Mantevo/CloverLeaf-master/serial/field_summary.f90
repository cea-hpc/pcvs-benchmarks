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

!>  @brief Driver for the field summary kernels
!>  @author Wayne Gaudin
!>  @details The user specified field summary kernel is invoked here. A summation
!>  across all mesh chunks is then performed and the information outputed.
!>  If the run is a test problem, the final result is compared with the expected
!>  result and the difference output.
!>  Note the reference solution is the value returned from an Intel compiler with
!>  ieee options set on a single core crun.

SUBROUTINE field_summary()

  USE clover_module
  USE ideal_gas_module
  USE field_summary_kernel_module

  IMPLICIT NONE

  REAL(KIND=8) :: vol,mass,ie,ke,press
  REAL(KIND=8) :: t_vol,t_mass,t_ie,t_ke,t_press
  REAL(KIND=8) :: qa_diff


  INTEGER      :: tile

  REAL(KIND=8) :: kernel_time,timer

  IF(parallel%boss)THEN
    WRITE(g_out,*)
    WRITE(g_out,*) 'Time ',time
    WRITE(g_out,'(a13,7a16)')'           ','Volume','Mass','Density','Pressure', &
      'Internal Energy','Kinetic Energy','Total Energy'
  ENDIF

  IF(profiler_on) kernel_time=timer()
  DO tile=1,tiles_per_chunk
    CALL ideal_gas(tile,.FALSE.)
  ENDDO

  IF(profiler_on) profiler%ideal_gas=profiler%ideal_gas+(timer()-kernel_time)

  IF(profiler_on) kernel_time=timer()

  t_vol=0.0
  t_mass=0.0
  t_ie=0.0
  t_ke=0.0
  t_press=0.0


  IF(use_fortran_kernels) THEN
    DO tile=1,tiles_per_chunk
      CALL field_summary_kernel(chunk%tiles(tile)%t_xmin,                   &
        chunk%tiles(tile)%t_xmax,                   &
        chunk%tiles(tile)%t_ymin,                   &
        chunk%tiles(tile)%t_ymax,                   &
        chunk%tiles(tile)%field%volume,                  &
        chunk%tiles(tile)%field%density0,                &
        chunk%tiles(tile)%field%energy0,                 &
        chunk%tiles(tile)%field%pressure,                &
        chunk%tiles(tile)%field%xvel0,                   &
        chunk%tiles(tile)%field%yvel0,                   &
        vol,mass,ie,ke,press                     )
      t_vol=t_vol+vol
      t_mass=t_mass+mass
      t_ie=t_ie+ie
      t_ke=t_ke+ke
      t_press=t_press+press

    ENDDO

  ELSEIF(use_C_kernels) THEN
    DO tile=1,tiles_per_chunk
      CALL field_summary_kernel_c(chunk%tiles(tile)%t_xmin,                   &
        chunk%tiles(tile)%t_xmax,                   &
        chunk%tiles(tile)%t_ymin,                   &
        chunk%tiles(tile)%t_ymax,                   &
        chunk%tiles(tile)%field%volume,                  &
        chunk%tiles(tile)%field%density0,                &
        chunk%tiles(tile)%field%energy0,                 &
        chunk%tiles(tile)%field%pressure,                &
        chunk%tiles(tile)%field%xvel0,                   &
        chunk%tiles(tile)%field%yvel0,                   &
        vol,mass,ie,ke,press                     )
      t_vol=t_vol+vol
      t_mass=t_mass+mass
      t_ie=t_ie+ie
      t_ke=t_ke+ke
      t_press=t_press+press
      
    ENDDO

  ENDIF
    
  vol=t_vol
  ie=t_ie
  ke=t_ke
  mass=t_mass
  press=t_press


  ! For mpi I need a reduction here
  CALL clover_sum(vol)
  CALL clover_sum(mass)
  CALL clover_sum(press)
  CALL clover_sum(ie)
  CALL clover_sum(ke)
  IF(profiler_on) profiler%summary=profiler%summary+(timer()-kernel_time)

  IF(parallel%boss) THEN
    WRITE(g_out,'(a6,i7,7e16.4)')' step:',step,vol,mass,mass/vol,press/vol,ie,ke,ie+ke
    WRITE(g_out,*)
  !$  ENDIF
  ENDIF

  !Check if this is the final call and if it is a test problem, check the result.
  IF(complete) THEN
    IF(parallel%boss) THEN
      IF(test_problem.GE.1) THEN
        IF(test_problem.EQ.1) qa_diff=ABS((100.0_8*(ke/1.82280367310258_8))-100.0_8)
        IF(test_problem.EQ.2) qa_diff=ABS((100.0_8*(ke/1.19316898756307_8))-100.0_8)
        IF(test_problem.EQ.3) qa_diff=ABS((100.0_8*(ke/2.58984003503994_8))-100.0_8)
        IF(test_problem.EQ.4) qa_diff=ABS((100.0_8*(ke/0.307475452287895_8))-100.0_8)
        IF(test_problem.EQ.5) qa_diff=ABS((100.0_8*(ke/4.85350315783719_8))-100.0_8)
        WRITE(*,'(a,i4,a,e16.7,a)')"Test problem", Test_problem," is within",qa_diff,"% of the expected solution"
        WRITE(g_out,'(a,i4,a,e16.7,a)')"Test problem", Test_problem," is within",qa_diff,"% of the expected solution"
        IF(qa_diff.LT.0.001) THEN
          WRITE(*,*)"This test is considered PASSED"
          WRITE(g_out,*)"This test is considered PASSED"
        ELSE
          WRITE(*,*)"This test is considered NOT PASSED"
          WRITE(g_out,*)"This is test is considered NOT PASSED"
        ENDIF
      ENDIF
    ENDIF
  ENDIF


END SUBROUTINE field_summary
