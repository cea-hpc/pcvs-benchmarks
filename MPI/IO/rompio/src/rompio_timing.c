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

#define _XOPEN_SOURCE 500
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64
 
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <mpi.h>
#include <limits.h>
 
/* For debugging in the deep depths of the romio software stack */
#define _ROMPIO_GLOBALS_DEFINED_HERE
#include "rompio.h"

int idx_write;
int nprocs_for_coll_min;
int nprocs_for_coll_max;
unsigned long long min_pe_request;
unsigned long long max_pe_request;
unsigned long long min_rd_request;
unsigned long long max_rd_request;
unsigned long long tot_ind_calls;
unsigned long long tot_agg_calls;

#define NPROCS     1024 /* max number of procs for stats */
    double rompio_times    [NPROCS][RTT];

void rompio_timing (void)
{
  int my_rank;
  int numpes;
  int i,j,k;
  int npepg   = 8;
  int npepgmax;
  int ncout;
  double *total_by_proc;
  double *total_by_line;
  double  *vmin_by_line;
  double  *vmax_by_line;
  int      *min_by_line;
  int      *max_by_line;
  double one_MB          = (double)(1024*1024);
  double one_KB          = (double)(1024);
  double prtscale;
  char cdesc[RMX+1];

  memset(rompio_times,     0, NPROCS*RTT*sizeof(double));

  if (MPI_Comm_rank(MPI_COMM_WORLD, &my_rank) != MPI_SUCCESS) {
    fprintf(stderr, "[RANK %d] ERROR: Problem getting processor rank.\n",my_rank);
    return; 
  }
  if (MPI_Comm_size( MPI_COMM_WORLD, &numpes) != MPI_SUCCESS){
    fprintf(stderr, "[RANK %d] ERROR: Problem getting number of processors.\n",my_rank);
    return;
  }

  if (numpes > NPROCS)
  {
    fprintf(stderr, "[RANK %d] ERROR: numpes %d > NPROCS: increase NPROCS and recompile\n",my_rank,numpes);
    return;
  }
  /* get write, read, test total times for this rank before gather */
  for (i=WBEG; i<=WEND; i++) rompio_time[TTOT] += rompio_time[i];
  for (i=RBEG; i<=REND; i++) rompio_time[TTOT] += rompio_time[i];
                             rompio_time[TTOT] += rompio_time[BBAR]+rompio_time[EBAR];
  rompio_time[WEFF]  =  rompio_time[WOPN];
  rompio_time[WEFF] +=  rompio_time[WRAW];
  rompio_time[WEFF] +=  rompio_time[WBAR] + rompio_time[WALG] + rompio_time[WSYN] + rompio_time[WA2A] + rompio_time[WARD];
  rompio_time[WEFF] +=  rompio_time[WSLP];
  rompio_time[WEFF] +=  rompio_time[WAIO] + rompio_time[CSYN] + rompio_time[TRNC];
  rompio_time[WEFF] +=  rompio_time[WCLS];
  rompio_time[REFF]  =  rompio_time[ROPN];
  rompio_time[REFF] +=  rompio_time[RRAW];
  rompio_time[REFF] +=  rompio_time[RCLS];
  rompio_time[WEBS]  = (rompio_time[WEFF] > 0.0) ? rompio_time[WBYT]/rompio_time[WEFF] : 0;
  rompio_time[REBS]  = (rompio_time[REFF] > 0.0) ? rompio_time[RBYT]/rompio_time[REFF] : 0;
  rompio_time[WRBS]  = (rompio_time[WRAW] > 0.0) ? rompio_time[WBYT]/rompio_time[WRAW] : 0;
  rompio_time[RRBS]  = (rompio_time[RRAW] > 0.0) ? rompio_time[RBYT]/rompio_time[RRAW] : 0;

  if( MPI_Gather (rompio_time,(int)RTT,MPI_DOUBLE,rompio_times,(int)RTT,MPI_DOUBLE,0,MPI_COMM_WORLD) != MPI_SUCCESS)
  {
     printf("[RANK %d]- ERROR: Unable to gather timing results",my_rank);
     MPI_Finalize();
     return;
  }
  if (!my_rank)
  {

/* ----------------------- */
/* Print by pe/line number */
/* ----------------------- */
    if( (total_by_proc = (double *)malloc(numpes * sizeof(double))) == NULL ||
        (total_by_line = (double *)malloc(   RTT * sizeof(double))) == NULL ||
        ( vmin_by_line = (double *)malloc(   RTT * sizeof(double))) == NULL ||
        ( vmax_by_line = (double *)malloc(   RTT * sizeof(double))) == NULL ||
        (  min_by_line = (int    *)malloc(   RTT * sizeof(int   ))) == NULL ||
        (  max_by_line = (int    *)malloc(   RTT * sizeof(int   ))) == NULL
      )
    {
      printf("RANK %d - ERROR: Unable to allocate memory for stats\n",my_rank);
      MPI_Finalize();
      return;
    }
    memset(total_by_proc, 0, numpes*sizeof(double));
    memset(total_by_line, 0,    RTT*sizeof(double));
    memset(  min_by_line, 0,    RTT*sizeof(int   ));
    memset(  max_by_line, 0,    RTT*sizeof(int   ));
    memset( vmax_by_line, 0,    RTT*sizeof(double));
    for (i=0; i<RTT; i++)
    {
      vmin_by_line[i] = (double)999999999;
    }
    for (j=0; j<numpes;j++) 
    {
      for (i=0; i<RTT;   i++) 
      { 
        total_by_proc[j] += rompio_times[j][i];
        total_by_line[i] += rompio_times[j][i];
        if (rompio_times[j][i] < vmin_by_line[i]&& rompio_times[j][i] > 0.0)
                              { vmin_by_line[i] = rompio_times[j][i]; min_by_line[i] = j; }
        if (rompio_times[j][i] > vmax_by_line[i])
                              { vmax_by_line[i] = rompio_times[j][i]; max_by_line[i] = j; }
      }
    }
    if ( vmax_by_line[WRAW] > 0.0 ) total_by_line[WRBS] = total_by_line[WBYT]/vmax_by_line[WRAW];
    if ( vmax_by_line[WEFF] > 0.0 ) total_by_line[WEBS] = total_by_line[WBYT]/vmax_by_line[WEFF];
    if ( vmax_by_line[RRAW] > 0.0 ) total_by_line[RRBS] = total_by_line[RBYT]/vmax_by_line[RRAW];
    if ( vmax_by_line[REFF] > 0.0 ) total_by_line[REBS] = total_by_line[RBYT]/vmax_by_line[REFF];
    for (j=0; j<numpes;j+=npepg) 
    {     
      npepgmax = (j+npepg > numpes) ? numpes : j+npepg;
      fprintf(stdout,"############################# Timing for PEs %d to %d ################################################\n",j,npepgmax-1); 
      fprintf(stdout,"Index"); 
      for (k=j; k<npepgmax; k++) fprintf(stdout," %7d",k); 
                                 fprintf(stdout," |     min |     max |    total |\n");
      for (i=0; i<RTT; i++)   
      {                                       
       if (total_by_line[i] > (double)0)
       {
         fprintf(stdout,"%04d ",i);
         strncpy(cdesc, &rompio_desc[i][0], RMX);
                                         prtscale = (double)1;
         if (strstr(cdesc,"MB") != NULL) prtscale = (double)one_MB;
         if (strstr(cdesc,"KB") != NULL) prtscale = (double)one_KB;
         for (k=j; k<npepgmax; k++) fprintf(stdout," %7.2f",   rompio_times[k][i]/prtscale);
                                    fprintf(stdout," |%8.2f",    vmin_by_line[i]/prtscale);
                                    fprintf(stdout," |%8.2f",    vmax_by_line[i]/prtscale);
                                    fprintf(stdout," |%9.2f |", total_by_line[i]/prtscale);
         fprintf(stdout," %s\n",cdesc);
         if (i==WEBS || i==REBS || i==BBAR || i==EBAR)
         fprintf(stdout,"     -----------------------------------------------------------------------------------------------------\n",cdesc);
       }
      }
/*    for (k=j; k<npepgmax; k++) fprintf(stdout," %10.3f",total_by_proc[k]); 
                                 fprintf(stdout,"       total\n");
*/
    } 
    char cstr[1024];
    char ctmp[128];
 
    memset(cstr,0,1024);
    memset(ctmp,0,128);
    strcat(cstr,test_params);
    if (total_by_line[WSLP] > (double)0)
    {
      strncpy(cdesc, &rompio_desc[WSLP][0], RMX);
      sprintf(ctmp," %8.2f %8.2f %9.2f", vmin_by_line[WSLP],
                                         vmax_by_line[WSLP],
                                        total_by_line[WSLP]/(double)numpes);
      strcat(cstr,ctmp);
      strcat(cstr," | ");
      strncat(cstr,cdesc,20);
      strcat(cstr," | ");
      strcat(cstr,write_params);
      strcat(cstr," | EXCEL");
      fprintf(stdout,"%s\n",cstr);
    }

    memset(cstr,0,1024);
    memset(ctmp,0,128);
    strcat(cstr,test_params);
    if (total_by_line[WBAR] > (double)0)
    {
      strncpy(cdesc, &rompio_desc[WBAR][0], RMX);
      sprintf(ctmp," %8.2f %8.2f %9.2f", vmin_by_line[WBAR],
                                         vmax_by_line[WBAR],
                                        total_by_line[WBAR]/(double)numpes);
      strcat(cstr,ctmp);
      strcat(cstr," | ");
      strncat(cstr,cdesc,20);
      strcat(cstr," | ");
      strcat(cstr,write_params);
      strcat(cstr," | EXCEL");
      fprintf(stdout,"%s\n",cstr);
    }
    memset(cstr,0,1024);
    memset(ctmp,0,128);
    strcat(cstr,test_params);
    if (total_by_line[WALG] > (double)0) 
    {
      strncpy(cdesc, &rompio_desc[WALG][0], RMX);
      sprintf(ctmp," %8.2f %8.2f %9.2f", vmin_by_line[WALG],
                                         vmax_by_line[WALG], 
                                        total_by_line[WALG]/(double)numpes); 
      strcat(cstr,ctmp);
      strcat(cstr," | ");
      strncat(cstr,cdesc,20);
      strcat(cstr," | ");
      strcat(cstr,write_params);
      strcat(cstr," | EXCEL");
      fprintf(stdout,"%s\n",cstr);
    }

    memset(cstr,0,1024);
    memset(ctmp,0,128);
    strcat(cstr,test_params);
    if (total_by_line[WA2A] > (double)0) 
    {
      strncpy(cdesc, &rompio_desc[WA2A][0], RMX);
      sprintf(ctmp," %8.2f %8.2f %9.2f", vmin_by_line[WA2A]*10,
                                         vmax_by_line[WA2A]*10, 
                                        total_by_line[WA2A]*10/(double)numpes); 
      strcat(cstr,ctmp);
      strcat(cstr," | ");
      strncat(cstr,cdesc,20);
      strcat(cstr," | ");
      strcat(cstr,write_params);
      strcat(cstr," | EXCEL");
      fprintf(stdout,"%s\n",cstr);
    }
 
    memset(cstr,0,1024);
    memset(ctmp,0,128);
    strcat(cstr,test_params);
    if (total_by_line[WARD] > (double)0)
    {
      strncpy(cdesc, &rompio_desc[WARD][0], RMX);
      sprintf(ctmp," %8.2f %8.2f %9.2f", vmin_by_line[WARD],
                                         vmax_by_line[WARD],
                                        total_by_line[WARD]/(double)numpes);
      strcat(cstr,ctmp);
      strcat(cstr," | ");
      strncat(cstr,cdesc,20);
      strcat(cstr," | ");
      strcat(cstr,write_params);
      strcat(cstr," | EXCEL");
      fprintf(stdout,"%s\n",cstr);
    }
 
    memset(cstr,0,1024);
    memset(ctmp,0,128);
    strcat(cstr,test_params);
    if (total_by_line[WEBS] > (double)0)
    {
      strncpy(cdesc, &rompio_desc[WEBS][0], RMX);
                                      prtscale = (double)1;
      if (strstr(cdesc,"MB") != NULL) prtscale = (double)one_MB;
      if (strstr(cdesc,"KB") != NULL) prtscale = (double)one_KB;
      sprintf(ctmp," %8.2f %8.2f %9.2f", vmin_by_line[WEBS]/prtscale,
                                         vmax_by_line[WEBS]/prtscale,
                                        total_by_line[WEBS]/prtscale);
      strcat(cstr,ctmp);
      strcat(cstr," | ");
      strncat(cstr,cdesc,20);
      strcat(cstr," | ");
      strcat(cstr,write_params);
      strcat(cstr," | EXCEL");
      fprintf(stdout,"%s\n",cstr);
    }

    memset(cstr,0,1024);
    memset(ctmp,0,128);
    strcat(cstr,test_params); 
    if (total_by_line[REBS] > (double)0) 
    { 
      strncpy(cdesc, &rompio_desc[REBS][0], RMX); 
                                      prtscale = (double)1; 
      if (strstr(cdesc,"MB") != NULL) prtscale = (double)one_MB; 
      if (strstr(cdesc,"KB") != NULL) prtscale = (double)one_KB; 
      sprintf(ctmp," %8.2f %8.2f %9.2f", vmin_by_line[REBS]/prtscale, 
                                         vmax_by_line[REBS]/prtscale, 
                                        total_by_line[REBS]/prtscale); 
      strcat(cstr,ctmp); 
      strcat(cstr," | "); 
      strncat(cstr,cdesc,20);
      strcat(cstr," | ");
      strcat(cstr,read_params); 
      strcat(cstr," | EXCEL");
      fprintf(stdout,"%s\n",cstr);
    } 
    fprintf(stdout,"%87.2lf  Percent of time accounted for   ######\n", vmax_by_line[TTOT]/rompio_test_time_check*(double)100);
    /* ----------------------- */
    /* accounting check prints */   
    /* ----------------------- */
    if (!strncmp(&write_params[2],"1",1))
    {
      if ( rompio_file_size_check *  total_by_line[WBYT] > 0.0 &&
           rompio_file_size_check != total_by_line[WBYT] ) 
        fprintf(stderr,"acct ERROR: file_size = %12lfB, written %12lfB\n", rompio_file_size_check, total_by_line[WBYT]);
     
      if ( rompio_file_size_check *  total_by_line[RBYT] > 0.0 &&
           rompio_file_size_check != total_by_line[RBYT] ) 
        fprintf(stderr,"acct ERROR: file_size = %12lfB, read    %12lfB\n", rompio_file_size_check, total_by_line[RBYT]);
    }
 
    if (tot_ind_calls || tot_agg_calls)
    fprintf(stdout,"min,max request per pe=%Lu, %Lu,  per coll=%Lu, %Lu, #coll pes min,max = %d, %d, total calls %Lu ind, %Lu coll\n",
    min_pe_request, max_pe_request, min_rd_request, max_rd_request, nprocs_for_coll_min, nprocs_for_coll_max,tot_ind_calls,tot_agg_calls);
 
    free (total_by_proc);
    free (total_by_line);
    free ( vmin_by_line);
    free ( vmax_by_line);
    free (  min_by_line);
    free (  max_by_line);
  }

  memset(rompio_time, 0, RTT*sizeof(double));

  return;
}
