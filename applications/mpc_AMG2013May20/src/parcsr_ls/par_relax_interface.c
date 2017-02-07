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
 * $Revision: 1.4 $
 ***********************************************************************EHEADER*/




/******************************************************************************
 *
 * Relaxation scheme
 *
 *****************************************************************************/

#include "headers.h"


/*--------------------------------------------------------------------------
 * hypre_BoomerAMGRelax
 *--------------------------------------------------------------------------*/

int  hypre_BoomerAMGRelaxIF( hypre_ParCSRMatrix *A,
                             hypre_ParVector    *f,
                             int                *cf_marker,
                             int                 relax_type,
                             int                 relax_order,
                             int                 cycle_type,
                             double              relax_weight,
                             double              omega,
                             double             *l1_norms,
                             hypre_ParVector    *u,
                             hypre_ParVector    *Vtemp,
                             hypre_ParVector    *Ztemp )
{
   int i, Solve_err_flag = 0;
   int relax_points[2];
   if (relax_order == 1 && cycle_type < 3)
   {
      if (cycle_type < 2)
      {
         relax_points[0] = 1;
	 relax_points[1] = -1;
      }
      else
      {
	 relax_points[0] = -1;
	 relax_points[1] = 1;
      }
      
      {
         for (i=0; i < 2; i++)
            Solve_err_flag = hypre_BoomerAMGRelax(A,
                                                  f,
                                                  cf_marker,
                                                  relax_type,
                                                  relax_points[i],
                                                  relax_weight,
                                                  omega,
                                                  l1_norms,
                                                  u,
                                                  Vtemp, Ztemp); 
      }
   }
   else
   {
      
         Solve_err_flag = hypre_BoomerAMGRelax(A,
                                               f,
                                               cf_marker,
                                               relax_type,
                                               0,
                                               relax_weight,
                                               omega,
                                               l1_norms,
                                               u,
                                               Vtemp, Ztemp); 
   }

   return Solve_err_flag;
}
