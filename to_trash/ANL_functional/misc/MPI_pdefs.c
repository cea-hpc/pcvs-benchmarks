/*-----------------------------------------------------------------------------
MESSAGE PASSING INTERFACE TEST CASE SUITE

Copyright - 1996 Intel Corporation

Intel Corporation hereby grants a non-exclusive license under Intel's
copyright to copy, modify and distribute this software for any purpose
and without fee, provided that the above copyright notice and the following
paragraphs appear on all copies.

Intel Corporation makes no representation that the test cases comprising
this suite are correct or are an accurate representation of any standard.

IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING, CONSEQUENTIAL,
INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO INFRINGEMENT,
LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS, IRRESPECTIVE OF
WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY SUCH DAMAGES.

INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
-----------------------------------------------------------------------------*/
/*******************************************************************************
*      Environment test for MPI.
*
* This test verifies that all required constants and routines are defined
* correctly for profiling in the MPI implementation.  Most errors will be found
* at compile time; a few (such as error codes) are validated by running the
* test.
*
*
* Test history:
*    1  01/12/96     gt   Created
*
*******************************************************************************/
#include "mpitest_cfg.h"
#include "mpitest.h"

int    copyfunction (MPI_Comm *oldcomm, int *keyval, void *extra_state,
                                 void *attr_in, void *attr_out, int *flag)
{
   return MPI_SUCCESS;
}

int deletefunction ( MPI_Comm *comm, int *keyval, void *attr_val,
                                   void *extra_state )
{
   return MPI_SUCCESS;
}


void handlerfunction( MPI_Comm *oldcomm, int *code )
{
}

void userfunction( void *invec, void *inoutvec, int *len,
                                   MPI_Datatype *datatype)

{
}

int main (int argc, char *argv[])
{

    int                 /* Variables used by the test */
	 fail,
	 pass;

    char
	 testmsg[150],
	 testname[128];

    int  /* Variables referenced by the calls */
	 err           = 0,
	 count1        = 0,
	 count2        = 0,
	 commute       = 0,
	 level         = 0,
	 dims          = 0,
	 ndims         = 0,
	 maxdims       = 0,
	 maxindex      = 0,
	 maxedges      = 0,
	 nedges        = 0,
	 coords        = 0,
	 nnodes        = 0,
	 high          = 0,
	 neighbors     = 0,
	 nneighbors    = 0,
	 maxneighbors  = 0,
	 periods       = 0,
	 dest          = 0,
	 source        = 0,
	 direction     = 0,
	 errorcode     = 0,
	 errorclass    = 0,
	 stride        = 0,
	 tag1          = 0,
	 tag2          = 0,
	 flag          = 0,
	 attribute_val = 0,
	 attr1         = 0,
	 buffer1       = 0,
	 buffer2       = 0,
	 size          = 0,
	 rank1         = 0,
	 rank2         = 0,
	 reorder       = 0,
	 root          = 0,
	 length        = 0,
	 position      = 0,
	 displs        = 0,
	 displs1       = 0,
	 displs2       = 0,
	 keyval        = 0,
	 color         = 0,
	 location      = 0,
	 leader1       = 0,
	 leader2       = 0,
	 result        = 0,
	 state         = 0;
    int *bottom = NULL;
    int
	 indicie_array[10]      = {0},
	 length_array[10]       = {0},
	 index_array[10]        = {0},
	 edge_array[10]         = {0},
	 displacement_array[10] = {0},
	 rank_array[10]         = {0};

    int
	 ranges[10][3];

    char
	 string[MPI_MAX_ERROR_STRING],
	 name[MPI_MAX_PROCESSOR_NAME];

    double
	 time;

    MPI_Comm
	 comm1   = MPI_COMM_NULL,
	 comm2   = MPI_COMM_NULL,
	 newcomm = MPI_COMM_NULL;
	
    MPI_Datatype
	 datatype = MPI_DATATYPE_NULL,
	 newtype  = MPI_DATATYPE_NULL,
	 oldtype  = MPI_DATATYPE_NULL,
	 sendtype = MPI_DATATYPE_NULL,
	 recvtype = MPI_DATATYPE_NULL,
	 type_array[10];
 
    MPI_Status
	 status,
	 status_array[10];
 
    MPI_Request
	 request = MPI_REQUEST_NULL,
	 request_array[10];
 
 
    MPI_Aint
	 address      = 0,
	 astride      = 0,
	 displacement = 0,
	 extent       = 0,
	 asize        = 0,
	 displacement_aarray[10];

    MPI_Group
	  group1 = MPI_GROUP_NULL,
	  group2 = MPI_GROUP_NULL,
	  newgroup = MPI_GROUP_NULL;

    MPI_Op
	  op = MPI_OP_NULL;

    MPI_Errhandler
	 errhandler = MPI_ERRHANDLER_NULL;


MPI_Init(&argc, &argv);
MPITEST_init(argc, argv);
sprintf( testname, "MPI_pdefs");
MPITEST_init( argc, argv);
if (MPITEST_me==0)
  {
    sprintf(testmsg, "Starting %s test", testname);
    MPITEST_message(MPITEST_INFO0, testmsg);
  }
else
  {
    /* only one node need run this, but must report overall results  */
    MPITEST_report(0, 0, 0, testname);

    MPI_Finalize();
    return 0;
  }

fail = 0;
pass = 0;

/*
**    Ensure all error types are defined.
*/
err = MPI_SUCCESS;
if (err != 0)
  {
    sprintf (testmsg, "MPI_SUCCESS = %d, expected 0", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_BUFFER;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_BUFFER = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_BUFFER = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_COUNT;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_COUNT = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_COUNT = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_TYPE;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_TYPE = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_TYPE = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_TAG;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_TAG = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_TAG = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_COMM;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_COMM = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_COMM = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_RANK;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_RANK = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_RANK = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;

err = MPI_ERR_REQUEST;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_REQUEST = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_REQUEST = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;

err = MPI_ERR_ROOT;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_ROOT = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_ROOT = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_GROUP;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_GROUP = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_GROUP = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_OP;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_OP = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_OP = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_TOPOLOGY;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_TOPOLOGY = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_TOPOLOGY = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_DIMS;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_DIMS = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_DIMS = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_ARG;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_ARG = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_ARG = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_UNKNOWN;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_UNKNOWN = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_UNKNOWN = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_TRUNCATE;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_TRUNCATE = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_TRUNCATE = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_OTHER;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_OTHER = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_OTHER = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_INTERN;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_INTERN = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_INTERN = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_PENDING;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_PENDING = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_PENDING = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;


err = MPI_ERR_IN_STATUS;
if (err == 0)
  {
    sprintf (testmsg,"MPI_ERR_IN_STATUS = 0");
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;
if (err > MPI_ERR_LASTCODE)
  {
    sprintf (testmsg,"MPI_ERR_IN_STATUS = %d, exceeds MPI_ERR_LASTCODE", err);
    MPITEST_message(MPITEST_NONFATAL, testmsg);
    fail++;
  }
else
    pass++;

if (fail == -1)      /* These are not executed, merely validating existance */
  {
err = MPI_TAG_UB;
bottom = MPI_BOTTOM;
err = MPI_PROC_NULL;
err = MPI_ANY_SOURCE;
err = MPI_ANY_TAG;
err = MPI_UNDEFINED;
err = MPI_BSEND_OVERHEAD;
err = MPI_KEYVAL_INVALID;
errhandler = MPI_ERRORS_ARE_FATAL;
errhandler = MPI_ERRORS_RETURN;

datatype = MPI_CHAR;
datatype = MPI_SHORT;
datatype = MPI_INT;
datatype = MPI_LONG;
datatype = MPI_UNSIGNED_CHAR;
datatype = MPI_UNSIGNED_SHORT;
datatype = MPI_UNSIGNED;
datatype = MPI_UNSIGNED_LONG;
datatype = MPI_FLOAT;
datatype = MPI_DOUBLE;
datatype = MPI_LONG_DOUBLE;
datatype = MPI_BYTE;
datatype = MPI_PACKED;

datatype = MPI_FLOAT_INT;
datatype = MPI_DOUBLE_INT;
datatype = MPI_LONG_INT;
datatype = MPI_2INT;
datatype = MPI_SHORT_INT;
datatype = MPI_LONG_DOUBLE_INT;

datatype = MPI_UB;
datatype = MPI_LB;

comm1 = MPI_COMM_WORLD;
comm1 = MPI_COMM_SELF;

err = MPI_IDENT;
err = MPI_CONGRUENT;
err = MPI_SIMILAR;
err = MPI_UNEQUAL;

err = MPI_IO;
err = MPI_HOST;
err = MPI_WTIME_IS_GLOBAL;

op = MPI_MAX;
op = MPI_MIN;
op = MPI_SUM;
op = MPI_PROD;
op = MPI_MAXLOC;
op = MPI_MINLOC;
op = MPI_BAND;
op = MPI_BOR;
op = MPI_BXOR;
op = MPI_LAND;
op = MPI_LOR;
op = MPI_LXOR;

group1 = MPI_GROUP_NULL;
comm1 = MPI_COMM_NULL;
datatype = MPI_DATATYPE_NULL;
request = MPI_REQUEST_NULL;
op = MPI_OP_NULL;
err = MPI_ERRHANDLER_NULL;

group1 = MPI_GROUP_EMPTY;

err = MPI_GRAPH;
err = MPI_CART;

/*
**  A.3 Point-to-Point Communication Bindings
*/
err = PMPI_Send(&buffer1, count1, datatype, dest, tag1, comm1);
err = PMPI_Recv(&buffer1, count1, datatype, source, tag1, comm1, &status);
err = PMPI_Get_count(&status, datatype, &count1);
err = PMPI_Bsend(&buffer1, count1, datatype, dest, tag1, comm1);
err = PMPI_Ssend(&buffer1, count1, datatype, dest, tag1, comm1);
err = PMPI_Rsend(&buffer1, count1, datatype, dest, tag1, comm1);
err = PMPI_Buffer_attach( &buffer1, size);
err = PMPI_Buffer_detach( &buffer1, &size);
err = PMPI_Isend(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Ibsend(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Issend(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Irsend(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Irecv(&buffer1, count1, datatype, source, tag1, comm1, &request);
err = PMPI_Wait(&request, &status);
err = PMPI_Test(&request, &flag, &status);
err = PMPI_Request_free(&request);
err = PMPI_Waitany(count1, &request_array[0], &tag1, &status);
err = PMPI_Testany(count1, &request_array[0], &tag1, &flag, &status);
err = PMPI_Waitall(count1, &request_array[0], &status_array[0]);
err = PMPI_Testall(count1, &request_array[0], &flag, &status_array[0]);
err = PMPI_Waitsome(count1, &request_array[0], &count2,
         &indicie_array[0], &status_array[0]);
err = PMPI_Testsome(count1, &request_array[0], &count2,
         &indicie_array[0], &status_array[0]);
err = PMPI_Iprobe(source, tag1, comm1, &flag, &status);
err = PMPI_Probe(source, tag1, comm1, &status);
err = PMPI_Cancel(&request);
err = PMPI_Test_cancelled(&status, &flag);
err = PMPI_Send_init(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Bsend_init(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Ssend_init(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Rsend_init(&buffer1, count1, datatype, dest, tag1, comm1, &request);
err = PMPI_Recv_init(&buffer1, count1, datatype, source, tag1, comm1, &request);
err = PMPI_Start(&request);
err = PMPI_Startall(count1, &request_array[0]);
err = PMPI_Sendrecv(&buffer1, count1, sendtype, dest, tag1, &buffer2,
                   count2, recvtype, source, tag2, comm1, &status);
err = PMPI_Sendrecv_replace(&buffer1, count1, datatype,
             dest, tag1, source, tag2, comm1, &status);
err = PMPI_Type_contiguous(count1, oldtype, &newtype);
err = PMPI_Type_vector(count1, length, stride, oldtype, &newtype);
err = PMPI_Type_hvector(count1, length, astride, oldtype, &newtype);
err = PMPI_Type_indexed(count1, &length_array[0],
             &displacement_array[0], oldtype, &newtype);
err = PMPI_Type_hindexed(count1, &length_array[0],
              &displacement_aarray[0], oldtype, &newtype);
err = PMPI_Type_struct(count1, &length_array[0],
            &displacement_aarray[0], &type_array[0], &newtype);
err = PMPI_Address(&location, &address);
err = PMPI_Type_extent(datatype, &extent);
err = PMPI_Type_size(datatype, &size);
err = PMPI_Type_lb(datatype, &displacement);
err = PMPI_Type_ub(datatype, &displacement);
err = PMPI_Type_commit(&datatype);
err = PMPI_Type_free(&datatype);
err = PMPI_Get_elements(&status, datatype, &count1);
err = PMPI_Pack(&buffer1, count1, datatype, &buffer2,
         size, &position,  comm1);
err = PMPI_Unpack(&buffer1, size, &position, &buffer2,
           count2, datatype, comm1);
err = PMPI_Pack_size(count1, datatype, comm1, &size);

/*
**  A.4 Collective Communication Bindings
*/
err = PMPI_Barrier(comm1);
err = PMPI_Bcast(&buffer1, count1, datatype, root, comm1);
err = PMPI_Gather(&buffer1, count1, datatype,
           &buffer2, count2, recvtype, root, comm1);
err = PMPI_Gatherv(&buffer1, count1, datatype,
        &buffer2, &count2, &displs, recvtype, root, comm1);
err = PMPI_Scatter(&buffer1, count1, datatype,
        &buffer2, count2, recvtype, root, comm1);
err = PMPI_Scatterv(&buffer1, &count1, &displs,
         datatype, &buffer2, count2, recvtype, root, comm1);
err = PMPI_Allgather(&buffer1, count1, datatype,
          &buffer2, count2, recvtype, comm1);
err = PMPI_Allgatherv(&buffer1, count1, datatype,
           &buffer2, &count2, &displs, recvtype, comm1);
err = PMPI_Alltoall(&buffer1, count1, datatype,
         &buffer2, count2, recvtype, comm1);
err = PMPI_Alltoallv(&buffer1, &count1, &displs1,
          datatype, &buffer2, &count2,
          &displs2, recvtype, comm1);
err = PMPI_Reduce(&buffer1, &buffer2, count1, datatype, op, root, comm1);
err = PMPI_Op_create((MPI_User_function *)&userfunction, commute, &op);
err = PMPI_Op_free(&op);
err = PMPI_Allreduce(&buffer1, &buffer2, count1, datatype, op, comm1);
err = PMPI_Reduce_scatter(&buffer1, &buffer2, &count2,
               datatype, op, comm1);
err = PMPI_Scan(&buffer1, &buffer2, count1, datatype, op, comm1);

/*
**  A.5 Groups, Contexts and Communicators Bindings
*/
err = PMPI_Group_size(group1, &size);
err = PMPI_Group_rank(group1, &rank1);
err = PMPI_Group_translate_ranks (group1, count1, &rank1, group2, &rank2);
err = PMPI_Group_compare(group1, group2, &result);
err = PMPI_Comm_group(comm1, &group1);
err = PMPI_Group_union(group1, group2, &newgroup);
err = PMPI_Group_intersection(group1, group2, &newgroup);
err = PMPI_Group_difference(group1, group2, &newgroup);
err = PMPI_Group_incl(group1, count1, &rank_array[0], &newgroup);
err = PMPI_Group_excl(group1, count1, &rank_array[0], &newgroup);
err = PMPI_Group_range_incl(group1, count1, &ranges[0], &newgroup);
err = PMPI_Group_range_excl(group1, count1, &ranges[0], &newgroup);
err = PMPI_Group_free(&group1);
err = PMPI_Comm_size(comm1, &size);
err = PMPI_Comm_rank(comm1, &rank1);
err = PMPI_Comm_compare(comm1, comm2, &result);
err = PMPI_Comm_dup(comm1, &newcomm);
err = PMPI_Comm_create(comm1, group1, &newcomm);
err = PMPI_Comm_split(comm1, color, keyval, &newcomm);
err = PMPI_Comm_free(&comm1);
err = PMPI_Comm_test_inter(comm1, &flag);
err = PMPI_Comm_remote_size(comm1, &size);
err = PMPI_Comm_remote_group(comm1, &group1);
err = PMPI_Intercomm_create(comm1, leader1, comm2, leader2, tag1, &newcomm);
err = PMPI_Intercomm_merge(comm1, high, &newcomm);
err = PMPI_Keyval_create((MPI_Copy_function *)&copyfunction,
                        (MPI_Delete_function *)&deletefunction,
                        &keyval, &state);
err = PMPI_Keyval_free(&keyval);
err = PMPI_Attr_put(comm1, keyval, &attribute_val);
err = PMPI_Attr_get(comm1, keyval, &attribute_val, &flag);
err = PMPI_Attr_delete(comm1, keyval);

/*
**  A.6 Process Topologies Bindings
*/
err = PMPI_Cart_create(comm1, ndims, &dims, &periods, reorder, &comm2);
err = PMPI_Dims_create(nnodes, ndims, &dims);
err = PMPI_Graph_create(comm1, nnodes, &index_array[0], &edge_array[0],
             reorder, &comm2);
err = PMPI_Topo_test(comm1, &result);
err = PMPI_Graphdims_get(comm1, &nnodes, &nedges);
err = PMPI_Graph_get(comm1, maxindex, maxedges, &index_array[0], &edge_array[0]);
err = PMPI_Cartdim_get(comm1, &ndims);
err = PMPI_Cart_get(comm1, maxdims, &dims, &periods, &coords);
err = PMPI_Cart_rank(comm1, &coords, &rank1);
err = PMPI_Cart_coords(comm1, rank1, maxdims, &coords);
err = PMPI_Graph_neighbors_count(comm1, rank1, &nneighbors);
err = PMPI_Graph_neighbors(comm1, rank1, maxneighbors, &neighbors);
err = PMPI_Cart_shift(comm1, direction, displs, &rank1, &rank2);
err = PMPI_Cart_sub(comm1, &dims, &newcomm);
err = PMPI_Cart_map(comm1, ndims, &dims, &periods, &rank1);
err = PMPI_Graph_map(comm1, nnodes, &index_array[0], &edge_array[0], &rank1);

/*
**  A.7 Environmental Inquiry Bindings
*/
err = PMPI_Get_processor_name(&name[0], &result);
err = PMPI_Errhandler_create((MPI_Handler_function *)&handlerfunction,
                          &errhandler);
err = PMPI_Errhandler_set(comm1, errhandler);
err = PMPI_Errhandler_get(comm1, &errhandler);
err = PMPI_Errhandler_free(&errhandler);
err = PMPI_Error_string(errorcode, &string[0], &result);
err = PMPI_Error_class(errorcode, &errorclass);
time = PMPI_Wtime();
time = PMPI_Wtick();
err = PMPI_Initialized(&flag);
err = PMPI_Abort(comm1, errorcode);

/*
**  A.8 Profiling Bindings
*/
err = PMPI_Pcontrol(level, errorcode);

}
else
    pass++;

/* report overall results  */
MPITEST_report(pass, fail, 0, testname);

err = MPI_Finalize();
return fail;
}
