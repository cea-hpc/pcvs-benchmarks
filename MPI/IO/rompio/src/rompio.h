/*************************************************************************
* Copyright (c) 2007, Los Alamos National Security, LLC
* All rights reserved.
*  Copyright 2007. Los Alamos National Security, LLC. This software was
*  produced under U.S. Government contract DE-AC52-06NA25396 for
*  Los Alamos National Laboratory (LANL), which is operated by Los Alamos
*  National Security, LLC for the U.S. Department of Energy. The U.S.
*  Government has rights to use, reproduce, and distribute this software.
*  NEITHER THE GOVERNMENT NOR LOS ALAMOS NATIONAL SECURITY, LLC MAKES
*  ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE
*  USE OF THIS SOFTWARE.  If software is modified to produce derivative
*  works, such modified software should be clearly marked, so as not to
*  confuse it with the version available from LANL.
*
*  Additionally, redistribution and use in source and binary forms,
*  with or without modification, are permitted provided that the
*  following conditions are met:
*    o Redistributions of source code must retain the above
*      copyright notice, this list of conditions and the
*      following disclaimer.
*    o Redistributions in binary form must reproduce the above
*      copyright notice, this list of conditions and the following
*      disclaimer in the documentation and/or other materials provided
*      with the distribution.
*    o Neither the name of Los Alamos National Security, LLC,
*      Los Alamos National Laboratory, LANL, the U.S. Government, nor
*      the names of its contributors may be used to endorse or
*      promote products derived from this software without
*      specific prior written permission.
*
*     THIS SOFTWARE IS PROVIDED BY LOS ALAMOS NATIONAL SECURITY, LLC
*     AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
*     INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
*     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*     DISCLAIMED. IN NO EVENT SHALL LOS ALAMOS NATIONAL SECURITY, LLC
*     OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
*     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
*     NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*     LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
*     CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
*     STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
*     ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
*     ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*************************************************************************/
 
/************************************************************/
/* Stephen W. Hodson, HPC-5, HPC Systems Integration Group, */
/* Los Alamos National Laboratory, February, 2007           */
/************************************************************/
 
#include <stdio.h>
/* The following indices are used by rompio and rompio_timing */
#define TBEG     0
#define WBEG     1
#define WOPN     2
#define WBUF     3
#define WSLP     4
#define WBAR     5
#define WALG     6
#define WA2A     7
#define WARD     8
#define WRAW     9
#define WSYN    10
#define WAIO    11
#define CSYN    12
#define TRNC    13
#define WCLS    14
#define WEND    15
#define WEFF    16
#define WBYT    17
#define WRBS    18
#define WEBS    19
#define BBAR    20
#define RBEG    21
#define ROPN    22
#define RCHK    23
#define RRAW    24
#define RCLS    25
#define REND    26
#define REFF    27
#define RBYT    28
#define RRBS    29
#define REBS    30
#define EBAR    31
#define TTOT    32

#define RTT     40
#define RMX     64

char test_params [256];
char write_params[256];
char read_params [256];

#ifdef _ROMPIO_GLOBALS_DEFINED_HERE
  double rompio_time[RTT]         = { 0.0 };
  double rompio_tims[RTT];
  char  rompio_desc[RTT][RMX] = {
                        " Timing indices rompio - start  "
                       ," Write  indices rompio - start  "
                       ," Write - open                   "
                       ," Set write buffer               "
                       ," usleep        -before ea. write"
                       ," MPI_Barrier   -before ea. write"
                       ," MPI_Allgather -before ea. write"
                       ," MPI_Alltoall  -before ea. write"
                       ," MPI_Allreduce -before ea. write"
                       ," Write - Raw                    "
                       ," File sync   - after ea. write  "
                       ," MPIO_Wait                      "
                       ," File sync before Write - close "
                       ," File trnc before Write - close "
                       ," Write - close                  "
                       ," Write  indices rompio - end    "
                       ," Write - Total seconds  by PE   "
                       ," Write - Total MB       by PE   "
                       ," Write - Raw   MB/s     by PE   "
                       ," Write - Net   MB/s     by PE   "
                       ," MPI_Barrier - Write/Read test  "
                       ," Read    indices rompio - start "
                       ," Read  - open                   "
                       ," Read  - check                  "
                       ," Read  - Raw                    "
                       ," Read  - close                  "
                       ," Read    indices rompio - end   "
                       ," Read  - Total seconds  by PE   "
                       ," Read  - Total MB       by PE   "
                       ," Read  - Raw   MB/s     by PE   "
                       ," Read  - Net   MB/s     by PE   "
                       ," MPI_Barrier - end all tests    "
                       ," Tests - TOTAL secs     by PE   "
                       ," Unused                         "
                       ," Unused                         "
                       ," Unused                         "
                       ," Unused                         "
                       ," Unused                         "
                       ," Unused                         "
                       ," Unused                         "
                       };
  double rompio_file_size_check  =  0;
  double rompio_test_time_check  =  0;
#else
  extern void rompio_timing (void);
  extern double rompio_file_size_check;
  extern double rompio_test_time_check;
  extern double rompio_time[];
  extern double rompio_tims[];
  void sleep_lite (unsigned long);
#endif
