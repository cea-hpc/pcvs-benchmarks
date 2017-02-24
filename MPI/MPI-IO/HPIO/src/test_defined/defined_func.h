/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef DEFINED_FUNC_H
#define DEFINED_FUNC_H

#include <sys/types.h>
#include "mpi.h"

#define XX -1

int d_cnt_vec_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p);
int d_idx_idx_init(MPI_Datatype *dtype_p,
		   MPI_Offset *disp_p);

#endif
