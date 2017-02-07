/*BHEADER**********************************************************************
 * Copyright (c) 2006   The Regents of the University of California.
 * Produced at the Lawrence Livermore National Laboratory.
 * Written by the HYPRE team. UCRL-CODE-222953.
 * All rights reserved.
 *
 * This file is part of HYPRE (see http://www.llnl.gov/CASC/hypre/).
 * Please see the COPYRIGHT_and_LICENSE file for the copyright notice, 
 * disclaimer, contact information and the GNU Lesser General Public License.
 *
 * HYPRE is free software; you can redistribute it and/or modify it under the 
 * terms of the GNU General Public License (as published by the Free Software
 * Foundation) version 2.1 dated February 1999.
 *
 * HYPRE is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the IMPLIED WARRANTY OF MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE.  See the terms and conditions of the GNU General
 * Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * $Revision: 2.20 $
 ***********************************************************************EHEADER*/




/******************************************************************************
 *
 * ParAMG cycling routine
 *
 *****************************************************************************/

#include "headers.h"
#include "par_amg.h"
/*#include "par_csr_block_matrix.h"*/

/*--------------------------------------------------------------------------
 * hypre_BoomerAMGCycle
 *--------------------------------------------------------------------------*/

int
hypre_BoomerAMGCycle( void              *amg_vdata, 
                   hypre_ParVector  **F_array,
                   hypre_ParVector  **U_array   )
{
   hypre_ParAMGData *amg_data = amg_vdata;

   MPI_Comm comm;
   /*HYPRE_Solver *smoother;*/
   /* Data Structure variables */

   hypre_ParCSRMatrix    **A_array;
   hypre_ParCSRMatrix    **P_array;
   hypre_ParCSRMatrix    **R_array;
   /*hypre_ParVector    *Utemp;*/
   hypre_ParVector    *Vtemp;
   hypre_ParVector    *Rtemp;
   hypre_ParVector    *Ptemp;
   hypre_ParVector    *Ztemp;
   hypre_ParVector    *Aux_U;
   hypre_ParVector    *Aux_F;

   int     **CF_marker_array;
   /* int     **unknown_map_array;
   int     **point_map_array;
   int     **v_at_point_array; */

   double    cycle_op_count;   
   int       cycle_type;
   int       num_levels;
   int       max_levels;

   double   *num_coeffs;
   int      *num_grid_sweeps;   
   int      *grid_relax_type;   
   int     **grid_relax_points;  

   /*int     block_mode;*/
   

 /* Local variables  */ 
   int      *lev_counter;
   int       Solve_err_flag;
   int       k;
   int       j, jj;
   int       level;
   int       cycle_param;
   int       coarse_grid;
   int       fine_grid;
   int       Not_Finished;
   int       num_sweep;
   int       cg_num_sweep = 1;
   int       relax_type;
   int       relax_points;
   int       relax_order;
   int       relax_local;
   int       old_version = 0;
   double   *relax_weight;
   double   *omega;
   double    beta;
   int       local_size;

   double    alpha;

#if 0
   double   *D_mat;
   double   *S_vec;
#endif
   
   /* Acquire data and allocate storage */

   A_array           = hypre_ParAMGDataAArray(amg_data);
   P_array           = hypre_ParAMGDataPArray(amg_data);
   R_array           = hypre_ParAMGDataRArray(amg_data);
   CF_marker_array   = hypre_ParAMGDataCFMarkerArray(amg_data);
   Vtemp             = hypre_ParAMGDataVtemp(amg_data);
   Rtemp             = hypre_ParAMGDataRtemp(amg_data);
   Ptemp             = hypre_ParAMGDataPtemp(amg_data);
   Ztemp             = hypre_ParAMGDataZtemp(amg_data);
   num_levels        = hypre_ParAMGDataNumLevels(amg_data);
   max_levels        = hypre_ParAMGDataMaxLevels(amg_data);
   cycle_type        = hypre_ParAMGDataCycleType(amg_data);

   num_grid_sweeps     = hypre_ParAMGDataNumGridSweeps(amg_data);
   grid_relax_type     = hypre_ParAMGDataGridRelaxType(amg_data);
   grid_relax_points   = hypre_ParAMGDataGridRelaxPoints(amg_data);
   relax_order         = hypre_ParAMGDataRelaxOrder(amg_data);
   relax_weight        = hypre_ParAMGDataRelaxWeight(amg_data); 
   omega               = hypre_ParAMGDataOmega(amg_data); 

   cycle_op_count = hypre_ParAMGDataCycleOpCount(amg_data);

   lev_counter = hypre_CTAlloc(int, num_levels);

   /* Initialize */

   Solve_err_flag = 0;

   if (grid_relax_points) old_version = 1;

   num_coeffs = hypre_CTAlloc(double, num_levels);
   num_coeffs[0]    = hypre_ParCSRMatrixDNumNonzeros(A_array[0]);
   comm = hypre_ParCSRMatrixComm(A_array[0]);

   for (j = 1; j < num_levels; j++)
         num_coeffs[j] = hypre_ParCSRMatrixDNumNonzeros(A_array[j]);
   
   
   /*---------------------------------------------------------------------
    *    Initialize cycling control counter
    *
    *     Cycling is controlled using a level counter: lev_counter[k]
    *     
    *     Each time relaxation is performed on level k, the
    *     counter is decremented by 1. If the counter is then
    *     negative, we go to the next finer level. If non-
    *     negative, we go to the next coarser level. The
    *     following actions control cycling:
    *     
    *     a. lev_counter[0] is initialized to 1.
    *     b. lev_counter[k] is initialized to cycle_type for k>0.
    *     
    *     c. During cycling, when going down to level k, lev_counter[k]
    *        is set to the max of (lev_counter[k],cycle_type)
    *---------------------------------------------------------------------*/

   Not_Finished = 1;

   lev_counter[0] = 1;
   for (k = 1; k < num_levels; ++k) 
   {
      lev_counter[k] = cycle_type;
   }

   level = 0;
   cycle_param = 1;

   /*---------------------------------------------------------------------
    * Main loop of cycling
    *--------------------------------------------------------------------*/
  
   while (Not_Finished)
   {
      if (num_levels > 1) 
      {
        local_size 
            = hypre_VectorSize(hypre_ParVectorLocalVector(F_array[level]));
        hypre_VectorSize(hypre_ParVectorLocalVector(Vtemp)) = local_size;
        cg_num_sweep = 1;
	num_sweep = hypre_ParAMGDataSmoothNumSweeps(amg_data);
        Aux_U = U_array[level];
        Aux_F = F_array[level];
        relax_type = grid_relax_type[cycle_param];
      }
      else if (max_levels > 1)
      {
        /* If no coarsening occurred, apply a simple smoother once */
        Aux_U = U_array[level];
        Aux_F = F_array[level];
        num_sweep = 1;
        relax_type = 0;
      }

      /*------------------------------------------------------------------
       * Do the relaxation num_sweep times
       *-----------------------------------------------------------------*/
      for (jj = 0; jj < cg_num_sweep; jj++)
      {

         for (j = 0; j < num_sweep; j++)
         {
            if (num_levels == 1 && max_levels > 1)
            {
               relax_points = 0;
               relax_local = 0;
            }
            else
            {
               if (old_version)
		  relax_points = grid_relax_points[cycle_param][j];
               relax_local = relax_order;
            }

            /*-----------------------------------------------
             * VERY sloppy approximation to cycle complexity
             *-----------------------------------------------*/

            if (old_version && level < num_levels -1)
            {
               switch (relax_points)
               {
                  case 1:
                  cycle_op_count += num_coeffs[level+1];
                  break;
  
                  case -1: 
                  cycle_op_count += (num_coeffs[level]-num_coeffs[level+1]); 
                  break;
               }
            }
	    else
            {
               cycle_op_count += num_coeffs[level]; 
            }

	    if (old_version)
	    {
               Solve_err_flag = hypre_BoomerAMGRelax(A_array[level], 
                                            Aux_F,
                                            CF_marker_array[level],
                                            relax_type,
                                            relax_points,
                                            relax_weight[level],
                                            omega[level],
                                            Aux_U,
                                            Vtemp);
	    }
	    else 
	    {
                  Solve_err_flag = hypre_BoomerAMGRelaxIF(A_array[level], 
                                                          Aux_F,
                                                          CF_marker_array[level],
                                                          relax_type,
                                                          relax_local,
                                                          cycle_param,
                                                          relax_weight[level],
                                                          omega[level],
                                                          Aux_U,
                                                          Vtemp);
               
	    }
 
            if (Solve_err_flag != 0)
               return(Solve_err_flag);
         }
      }


      /*------------------------------------------------------------------
       * Decrement the control counter and determine which grid to visit next
       *-----------------------------------------------------------------*/

      --lev_counter[level];
       
      if (lev_counter[level] >= 0 && level != num_levels-1)
      {
                               
         /*---------------------------------------------------------------
          * Visit coarser level next.  
 	  * Compute residual using hypre_ParCSRMatrixMatvec.
          * Perform restriction using hypre_ParCSRMatrixMatvecT.
          * Reset counters and cycling parameters for coarse level
          *--------------------------------------------------------------*/

         fine_grid = level;
         coarse_grid = level + 1;

         hypre_ParVectorSetConstantValues(U_array[coarse_grid], 0.0); 
          
         hypre_ParVectorCopy(F_array[fine_grid],Vtemp);
         alpha = -1.0;
         beta = 1.0;

         hypre_ParCSRMatrixMatvec(alpha, A_array[fine_grid], U_array[fine_grid],
                                     beta, Vtemp);

         alpha = 1.0;
         beta = 0.0;

         hypre_ParCSRMatrixMatvecT(alpha,R_array[fine_grid],Vtemp,
                                      beta,F_array[coarse_grid]);

         ++level;
         lev_counter[level] = hypre_max(lev_counter[level],cycle_type);
         cycle_param = 1;
         if (level == num_levels-1) cycle_param = 3;
      }

      else if (level != 0)
      {
         /*---------------------------------------------------------------
          * Visit finer level next.
          * Interpolate and add correction using hypre_ParCSRMatrixMatvec.
          * Reset counters and cycling parameters for finer level.
          *--------------------------------------------------------------*/

         fine_grid = level - 1;
         coarse_grid = level;
         alpha = 1.0;
         beta = 1.0;
         hypre_ParCSRMatrixMatvec(alpha, P_array[fine_grid], 
                                     U_array[coarse_grid],
                                     beta, U_array[fine_grid]);            
         
         --level;
         cycle_param = 2;
      }
      else
      {
         Not_Finished = 0;
      }
   }

   hypre_ParAMGDataCycleOpCount(amg_data) = cycle_op_count;

   hypre_TFree(lev_counter);
   hypre_TFree(num_coeffs);

   return(Solve_err_flag);
}
