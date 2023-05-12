
#include <mpc_mpi.h>
#include <sctk_alloc.h>

#include "mpc_fortran_helpers.h"

static inline char * char_fortran_to_c(char *buf, int size, char **free_ptr)
{
        char *   tmp;
        long int i;

        tmp = sctk_malloc(size + 1);
        assume(tmp != NULL);
        *free_ptr = tmp;

        for(i = 0; i < size; i++)
        {
                tmp[i] = buf[i];
        }
        tmp[i] = '\0';

        /* Trim */

        while(*tmp == ' ')
        {
                tmp++;
        }

        size_t len = strlen(tmp);

        char *begin = tmp;

        while( (tmp[len - 1] == ' ') && (&tmp[len] != begin) )
        {
                tmp[len - 1] = '\0';
                len--;
        }

        return tmp;
}

static inline void char_c_to_fortran(char *buf, int size)
{
        size_t i;

        for(i = strlen(buf); i < size; i++)
        {
                buf[i] = ' ';
        }
}

#if defined(USE_CHAR_MIXED)
        #define CHAR_END(thename)
        #define CHAR_MIXED(thename) long int thename,
#else
        #define CHAR_END(thename) ,long int thename
        #define CHAR_MIXED(thename)
#endif


static inline int buffer_is_bottom(void * buffer)
{
    return( (buffer == *mpi_predef_bottom()) ||
            (buffer == *mpi_predef08_bottom()) );
}

static inline int buffer_is_mpiinplace(void * buffer)
{
    return( (buffer == *mpi_predef_inplace()) ||
            (buffer == *mpi_predef08_inplace()) );
}


int mpi_abort_(MPI_Fint* comm, int* errorcode, int *ierror)
{
/* MPI_Abort */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Abort(c_comm, *errorcode);


}
int mpi_abort__(MPI_Comm* comm, int* errorcode, int *ierror)
{
	mpi_abort_(comm, errorcode, ierror);
}
int mpi_accumulate_(const void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* op, MPI_Fint* win, int *ierror)
{
/* MPI_Accumulate */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Accumulate(origin_addr, *origin_count, c_origin_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_op, c_win);


}
int mpi_accumulate__(const void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Op* op, MPI_Win* win, int *ierror)
{
	mpi_accumulate_(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierror);
}
int mpi_add_error_class_(int *errorclass, int *ierror)
{
/* MPI_Add_error_class */

*ierror = MPI_Add_error_class(errorclass);


}
int mpi_add_error_class__(int *errorclass, int *ierror)
{
	mpi_add_error_class_(errorclass, ierror);
}
int mpi_add_error_code_(int* errorclass, int *errorcode, int *ierror)
{
/* MPI_Add_error_code */

*ierror = MPI_Add_error_code(*errorclass, errorcode);


}
int mpi_add_error_code__(int* errorclass, int *errorcode, int *ierror)
{
	mpi_add_error_code_(errorclass, errorcode, ierror);
}
int mpi_add_error_string_(int* errorcode, const char *string CHAR_MIXED(size_string), int *ierror CHAR_END(size_string))
{
/* MPI_Add_error_string */
char *tmp_string = NULL, *ptr_string = NULL;
tmp_string = char_fortran_to_c((char *)string, size_string, &ptr_string);

*ierror = MPI_Add_error_string(*errorcode, tmp_string);
sctk_free(ptr_string);


}
int mpi_add_error_string__(int* errorcode, const char *string CHAR_MIXED(size_string), int *ierror CHAR_END(size_string))
{
	mpi_add_error_string_(errorcode, string, ierror);
}
MPI_Aint mpi_aint_add_(MPI_Aint* base, MPI_Aint* disp)
{
/* MPI_Aint_add */

MPI_Aint ret = MPI_Aint_add(*base, *disp);


}
MPI_Aint mpi_aint_add__(MPI_Aint* base, MPI_Aint* disp)
{
	mpi_aint_add_(base, disp, ierror);
}
MPI_Aint mpi_aint_diff_(MPI_Aint* addr1, MPI_Aint* addr2)
{
/* MPI_Aint_diff */

MPI_Aint ret = MPI_Aint_diff(*addr1, *addr2);


}
MPI_Aint mpi_aint_diff__(MPI_Aint* addr1, MPI_Aint* addr2)
{
	mpi_aint_diff_(addr1, addr2, ierror);
}
int mpi_allgather_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Allgather */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Allgather(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm);


}
int mpi_allgather__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_allgather_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror);
}
int mpi_allgather_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Allgather_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Allgather_init(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_allgather_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_allgather_init_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierror);
}
int mpi_allgatherv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Allgatherv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Allgatherv(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, c_comm);


}
int mpi_allgatherv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_allgatherv_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierror);
}
int mpi_allgatherv_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Allgatherv_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Allgatherv_init(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_allgatherv_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_allgatherv_init_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierror);
}
int mpi_alloc_mem_(MPI_Aint* size, MPI_Fint* info, void *baseptr, int *ierror)
{
/* MPI_Alloc_mem */
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Alloc_mem(*size, c_info, baseptr);


}
int mpi_alloc_mem__(MPI_Aint* size, MPI_Info* info, void *baseptr, int *ierror)
{
	mpi_alloc_mem_(size, info, baseptr, ierror);
}
int mpi_allreduce_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, int *ierror)
{
/* MPI_Allreduce */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Allreduce(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm);


}
int mpi_allreduce__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, int *ierror)
{
	mpi_allreduce_(sendbuf, recvbuf, count, datatype, op, comm, ierror);
}
int mpi_allreduce_init_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Allreduce_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Allreduce_init(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_allreduce_init__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_allreduce_init_(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierror);
}
int mpi_alltoall_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Alltoall */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Alltoall(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm);


}
int mpi_alltoall__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_alltoall_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror);
}
int mpi_alltoall_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Alltoall_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Alltoall_init(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_alltoall_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_alltoall_init_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierror);
}
int mpi_alltoallv_(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Alltoallv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Alltoallv(sendbuf, sendcounts, sdispls, c_sendtype, recvbuf, recvcounts, rdispls, c_recvtype, c_comm);


}
int mpi_alltoallv__(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_alltoallv_(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierror);
}
int mpi_alltoallv_init_(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Alltoallv_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Alltoallv_init(sendbuf, sendcounts, sdispls, c_sendtype, recvbuf, recvcounts, rdispls, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_alltoallv_init__(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_alltoallv_init_(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierror);
}
int mpi_alltoallw_(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Fint sendtypes[], void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Fint recvtypes[], MPI_Fint* comm, int *ierror)
{
/* MPI_Alltoallw */
int alltoallwlen = 0;
PMPI_Comm_size(comm, &alltoallwlen);
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;

int incnt_sendtypes = 0;
MPI_Datatype *c_sendtypes = NULL;

c_sendtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_sendtypes = 0; incnt_sendtypes < alltoallwlen ; incnt_sendtypes++)
        c_sendtypes[incnt_sendtypes] = PMPI_Type_f2c(sendtypes[incnt_sendtypes]);
        
int incnt_recvtypes = 0;
MPI_Datatype *c_recvtypes = NULL;

c_recvtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_recvtypes = 0; incnt_recvtypes < alltoallwlen ; incnt_recvtypes++)
        c_recvtypes[incnt_recvtypes] = PMPI_Type_f2c(recvtypes[incnt_recvtypes]);
        MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Alltoallw(sendbuf, sendcounts, sdispls, c_sendtypes, recvbuf, recvcounts, rdispls, c_recvtypes, c_comm);
sctk_free(c_sendtypes);
sctk_free(c_recvtypes);


}
int mpi_alltoallw__(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm* comm, int *ierror)
{
	mpi_alltoallw_(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierror);
}
int mpi_alltoallw_init_(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Fint sendtypes[], void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Fint recvtypes[], MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Alltoallw_init */
int alltoallwlen = 0;
PMPI_Comm_size(comm, &alltoallwlen);
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;

int incnt_sendtypes = 0;
MPI_Datatype *c_sendtypes = NULL;

c_sendtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_sendtypes = 0; incnt_sendtypes < alltoallwlen ; incnt_sendtypes++)
        c_sendtypes[incnt_sendtypes] = PMPI_Type_f2c(sendtypes[incnt_sendtypes]);
        
int incnt_recvtypes = 0;
MPI_Datatype *c_recvtypes = NULL;

c_recvtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_recvtypes = 0; incnt_recvtypes < alltoallwlen ; incnt_recvtypes++)
        c_recvtypes[incnt_recvtypes] = PMPI_Type_f2c(recvtypes[incnt_recvtypes]);
        MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Alltoallw_init(sendbuf, sendcounts, sdispls, c_sendtypes, recvbuf, recvcounts, rdispls, c_recvtypes, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);
sctk_free(c_sendtypes);
sctk_free(c_recvtypes);


}
int mpi_alltoallw_init__(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_alltoallw_init_(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierror);
}
int mpi_attr_delete_(MPI_Fint* comm, int* keyval, int *ierror)
{
/* MPI_Attr_delete */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Attr_delete(c_comm, *keyval);


}
int mpi_attr_delete__(MPI_Comm* comm, int* keyval, int *ierror)
{
	mpi_attr_delete_(comm, keyval, ierror);
}
int mpi_attr_get_(MPI_Fint* comm, int* keyval, void *attribute_val, int *flag, int *ierror)
{
/* MPI_Attr_get */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Attr_get(c_comm, *keyval, attribute_val, flag);


}
int mpi_attr_get__(MPI_Comm* comm, int* keyval, void *attribute_val, int *flag, int *ierror)
{
	mpi_attr_get_(comm, keyval, attribute_val, flag, ierror);
}
int mpi_attr_put_(MPI_Fint* comm, int* keyval, void *attribute_val, int *ierror)
{
/* MPI_Attr_put */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Attr_put(c_comm, *keyval, attribute_val);


}
int mpi_attr_put__(MPI_Comm* comm, int* keyval, void *attribute_val, int *ierror)
{
	mpi_attr_put_(comm, keyval, attribute_val, ierror);
}
int mpi_barrier_(MPI_Fint* comm, int *ierror)
{
/* MPI_Barrier */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Barrier(c_comm);


}
int mpi_barrier__(MPI_Comm* comm, int *ierror)
{
	mpi_barrier_(comm, ierror);
}
int mpi_barrier_init_(MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Barrier_init */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Barrier_init(c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_barrier_init__(MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_barrier_init_(comm, info, request, ierror);
}
int mpi_bcast_(void *buffer, int* count, MPI_Fint* datatype, int* root, MPI_Fint* comm, int *ierror)
{
/* MPI_Bcast */
if( buffer_is_bottom((void *)buffer) )
	buffer = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Bcast(buffer, *count, c_datatype, *root, c_comm);


}
int mpi_bcast__(void *buffer, int* count, MPI_Datatype* datatype, int* root, MPI_Comm* comm, int *ierror)
{
	mpi_bcast_(buffer, count, datatype, root, comm, ierror);
}
int mpi_bcast_init_(void *buffer, int* count, MPI_Fint* datatype, int* root, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Bcast_init */
if( buffer_is_bottom((void *)buffer) )
	buffer = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Bcast_init(buffer, *count, c_datatype, *root, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_bcast_init__(void *buffer, int* count, MPI_Datatype* datatype, int* root, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_bcast_init_(buffer, count, datatype, root, comm, info, request, ierror);
}
int mpi_bsend_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, int *ierror)
{
/* MPI_Bsend */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Bsend(buf, *count, c_datatype, *dest, *tag, c_comm);


}
int mpi_bsend__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, int *ierror)
{
	mpi_bsend_(buf, count, datatype, dest, tag, comm, ierror);
}
int mpi_bsend_init_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Bsend_init */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Bsend_init(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_bsend_init__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_bsend_init_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_buffer_attach_(void *buffer, int* size, int *ierror)
{
/* MPI_Buffer_attach */
if( buffer_is_bottom((void *)buffer) )
	buffer = MPI_BOTTOM;

*ierror = MPI_Buffer_attach(buffer, *size);


}
int mpi_buffer_attach__(void *buffer, int* size, int *ierror)
{
	mpi_buffer_attach_(buffer, size, ierror);
}
int mpi_buffer_detach_(void *buffer_addr, int *size, int *ierror)
{
/* MPI_Buffer_detach */

*ierror = MPI_Buffer_detach(buffer_addr, size);


}
int mpi_buffer_detach__(void *buffer_addr, int *size, int *ierror)
{
	mpi_buffer_detach_(buffer_addr, size, ierror);
}
int mpi_cancel_(MPI_Fint *request, int *ierror)
{
/* MPI_Cancel */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Cancel(&c_request);


}
int mpi_cancel__(MPI_Request *request, int *ierror)
{
	mpi_cancel_(request, ierror);
}
int mpi_cart_coords_(MPI_Fint* comm, int* rank, int* maxdims, int coords[], int *ierror)
{
/* MPI_Cart_coords */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Cart_coords(c_comm, *rank, *maxdims, coords);


}
int mpi_cart_coords__(MPI_Comm* comm, int* rank, int* maxdims, int coords[], int *ierror)
{
	mpi_cart_coords_(comm, rank, maxdims, coords, ierror);
}
int mpi_cart_create_(MPI_Fint* comm_old, int* ndims, const int dims[], const int periods[], int* reorder, MPI_Fint *comm_cart, int *ierror)
{
/* MPI_Cart_create */
MPI_Comm c_comm_old = PMPI_Comm_f2c(*comm_old);
MPI_Comm c_comm_cart;

*ierror = MPI_Cart_create(c_comm_old, *ndims, dims, periods, *reorder, &c_comm_cart);
*comm_cart = PMPI_Comm_c2f(c_comm_cart);


}
int mpi_cart_create__(MPI_Comm* comm_old, int* ndims, const int dims[], const int periods[], int* reorder, MPI_Comm *comm_cart, int *ierror)
{
	mpi_cart_create_(comm_old, ndims, dims, periods, reorder, comm_cart, ierror);
}
int mpi_cart_get_(MPI_Fint* comm, int* maxdims, int dims[], int periods[], int coords[], int *ierror)
{
/* MPI_Cart_get */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Cart_get(c_comm, *maxdims, dims, periods, coords);


}
int mpi_cart_get__(MPI_Comm* comm, int* maxdims, int dims[], int periods[], int coords[], int *ierror)
{
	mpi_cart_get_(comm, maxdims, dims, periods, coords, ierror);
}
int mpi_cart_map_(MPI_Fint* comm, int* ndims, const int dims[], const int periods[], int *newrank, int *ierror)
{
/* MPI_Cart_map */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Cart_map(c_comm, *ndims, dims, periods, newrank);


}
int mpi_cart_map__(MPI_Comm* comm, int* ndims, const int dims[], const int periods[], int *newrank, int *ierror)
{
	mpi_cart_map_(comm, ndims, dims, periods, newrank, ierror);
}
int mpi_cart_rank_(MPI_Fint* comm, const int coords[], int *rank, int *ierror)
{
/* MPI_Cart_rank */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Cart_rank(c_comm, coords, rank);


}
int mpi_cart_rank__(MPI_Comm* comm, const int coords[], int *rank, int *ierror)
{
	mpi_cart_rank_(comm, coords, rank, ierror);
}
int mpi_cart_shift_(MPI_Fint* comm, int* direction, int* disp, int *rank_source, int *rank_dest, int *ierror)
{
/* MPI_Cart_shift */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Cart_shift(c_comm, *direction, *disp, rank_source, rank_dest);


}
int mpi_cart_shift__(MPI_Comm* comm, int* direction, int* disp, int *rank_source, int *rank_dest, int *ierror)
{
	mpi_cart_shift_(comm, direction, disp, rank_source, rank_dest, ierror);
}
int mpi_cart_sub_(MPI_Fint* comm, const int remain_dims[], MPI_Fint *newcomm, int *ierror)
{
/* MPI_Cart_sub */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_newcomm;

*ierror = MPI_Cart_sub(c_comm, remain_dims, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);


}
int mpi_cart_sub__(MPI_Comm* comm, const int remain_dims[], MPI_Comm *newcomm, int *ierror)
{
	mpi_cart_sub_(comm, remain_dims, newcomm, ierror);
}
int mpi_cartdim_get_(MPI_Fint* comm, int *ndims, int *ierror)
{
/* MPI_Cartdim_get */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Cartdim_get(c_comm, ndims);


}
int mpi_cartdim_get__(MPI_Comm* comm, int *ndims, int *ierror)
{
	mpi_cartdim_get_(comm, ndims, ierror);
}
int mpi_close_port_(const char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_port_name))
{
/* MPI_Close_port */
char *tmp_port_name = NULL, *ptr_port_name = NULL;
tmp_port_name = char_fortran_to_c((char *)port_name, size_port_name, &ptr_port_name);

*ierror = MPI_Close_port(tmp_port_name);
sctk_free(ptr_port_name);


}
int mpi_close_port__(const char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_port_name))
{
	mpi_close_port_(port_name, ierror);
}
int mpi_comm_accept_(const char *port_name CHAR_MIXED(size_port_name), MPI_Fint* info, int* root, MPI_Fint* comm, MPI_Fint *newcomm, int *ierror CHAR_END(size_port_name))
{
/* MPI_Comm_accept */
char *tmp_port_name = NULL, *ptr_port_name = NULL;
tmp_port_name = char_fortran_to_c((char *)port_name, size_port_name, &ptr_port_name);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_accept(tmp_port_name, c_info, *root, c_comm, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);
sctk_free(ptr_port_name);


}
int mpi_comm_accept__(const char *port_name CHAR_MIXED(size_port_name), MPI_Info* info, int* root, MPI_Comm* comm, MPI_Comm *newcomm, int *ierror CHAR_END(size_port_name))
{
	mpi_comm_accept_(port_name, info, root, comm, newcomm, ierror);
}
int mpi_comm_call_errhandler_(MPI_Fint* comm, int* errorcode, int *ierror)
{
/* MPI_Comm_call_errhandler */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_call_errhandler(c_comm, *errorcode);


}
int mpi_comm_call_errhandler__(MPI_Comm* comm, int* errorcode, int *ierror)
{
	mpi_comm_call_errhandler_(comm, errorcode, ierror);
}
int mpi_comm_compare_(MPI_Fint* comm1, MPI_Fint* comm2, int *result, int *ierror)
{
/* MPI_Comm_compare */
MPI_Comm c_comm1 = PMPI_Comm_f2c(*comm1);
MPI_Comm c_comm2 = PMPI_Comm_f2c(*comm2);

*ierror = MPI_Comm_compare(c_comm1, c_comm2, result);


}
int mpi_comm_compare__(MPI_Comm* comm1, MPI_Comm* comm2, int *result, int *ierror)
{
	mpi_comm_compare_(comm1, comm2, result, ierror);
}
int mpi_comm_connect_(const char *port_name CHAR_MIXED(size_port_name), MPI_Fint* info, int* root, MPI_Fint* comm, MPI_Fint *newcomm, int *ierror CHAR_END(size_port_name))
{
/* MPI_Comm_connect */
char *tmp_port_name = NULL, *ptr_port_name = NULL;
tmp_port_name = char_fortran_to_c((char *)port_name, size_port_name, &ptr_port_name);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_connect(tmp_port_name, c_info, *root, c_comm, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);
sctk_free(ptr_port_name);


}
int mpi_comm_connect__(const char *port_name CHAR_MIXED(size_port_name), MPI_Info* info, int* root, MPI_Comm* comm, MPI_Comm *newcomm, int *ierror CHAR_END(size_port_name))
{
	mpi_comm_connect_(port_name, info, root, comm, newcomm, ierror);
}
int mpi_comm_create_(MPI_Fint* comm, MPI_Fint* group, MPI_Fint *newcomm, int *ierror)
{
/* MPI_Comm_create */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_create(c_comm, c_group, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);


}
int mpi_comm_create__(MPI_Comm* comm, MPI_Group* group, MPI_Comm *newcomm, int *ierror)
{
	mpi_comm_create_(comm, group, newcomm, ierror);
}
int mpi_comm_create_errhandler_(MPI_Comm_errhandler_function *comm_errhandler_fn, MPI_Fint *errhandler, int *ierror)
{
/* MPI_Comm_create_errhandler */
MPI_Errhandler c_errhandler;

*ierror = MPI_Comm_create_errhandler(comm_errhandler_fn, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_comm_create_errhandler__(MPI_Comm_errhandler_function *comm_errhandler_fn, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_comm_create_errhandler_(comm_errhandler_fn, errhandler, ierror);
}
int mpi_comm_create_from_group_(MPI_Fint* group, const char *stringtag CHAR_MIXED(size_stringtag), MPI_Fint* info, MPI_Fint* errhandler, MPI_Fint *newcomm, int *ierror CHAR_END(size_stringtag))
{
/* MPI_Comm_create_from_group */
MPI_Group c_group = PMPI_Group_f2c(*group);
char *tmp_stringtag = NULL, *ptr_stringtag = NULL;
tmp_stringtag = char_fortran_to_c((char *)stringtag, size_stringtag, &ptr_stringtag);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_create_from_group(c_group, tmp_stringtag, c_info, c_errhandler, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);
sctk_free(ptr_stringtag);


}
int mpi_comm_create_from_group__(MPI_Group* group, const char *stringtag CHAR_MIXED(size_stringtag), MPI_Info* info, MPI_Errhandler* errhandler, MPI_Comm *newcomm, int *ierror CHAR_END(size_stringtag))
{
	mpi_comm_create_from_group_(group, stringtag, info, errhandler, newcomm, ierror);
}
int mpi_comm_create_group_(MPI_Fint* comm, MPI_Fint* group, int* tag, MPI_Fint *newcomm, int *ierror)
{
/* MPI_Comm_create_group */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_create_group(c_comm, c_group, *tag, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);


}
int mpi_comm_create_group__(MPI_Comm* comm, MPI_Group* group, int* tag, MPI_Comm *newcomm, int *ierror)
{
	mpi_comm_create_group_(comm, group, tag, newcomm, ierror);
}
int mpi_comm_create_keyval_(MPI_Comm_copy_attr_function *comm_copy_attr_fn, MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval, void *extra_state, int *ierror)
{
/* MPI_Comm_create_keyval */

*ierror = MPI_Comm_create_keyval(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state);


}
int mpi_comm_create_keyval__(MPI_Comm_copy_attr_function *comm_copy_attr_fn, MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval, void *extra_state, int *ierror)
{
	mpi_comm_create_keyval_(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierror);
}
int mpi_comm_delete_attr_(MPI_Fint* comm, int* comm_keyval, int *ierror)
{
/* MPI_Comm_delete_attr */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_delete_attr(c_comm, *comm_keyval);


}
int mpi_comm_delete_attr__(MPI_Comm* comm, int* comm_keyval, int *ierror)
{
	mpi_comm_delete_attr_(comm, comm_keyval, ierror);
}
int mpi_comm_disconnect_(MPI_Fint *comm, int *ierror)
{
/* MPI_Comm_disconnect */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_disconnect(&c_comm);
*comm = PMPI_Comm_c2f(c_comm);


}
int mpi_comm_disconnect__(MPI_Comm *comm, int *ierror)
{
	mpi_comm_disconnect_(comm, ierror);
}
int mpi_comm_dup_(MPI_Fint* comm, MPI_Fint *newcomm, int *ierror)
{
/* MPI_Comm_dup */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_dup(c_comm, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);


}
int mpi_comm_dup__(MPI_Comm* comm, MPI_Comm *newcomm, int *ierror)
{
	mpi_comm_dup_(comm, newcomm, ierror);
}
int mpi_comm_dup_with_info_(MPI_Fint* comm, MPI_Fint* info, MPI_Fint *newcomm, int *ierror)
{
/* MPI_Comm_dup_with_info */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_dup_with_info(c_comm, c_info, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);


}
int mpi_comm_dup_with_info__(MPI_Comm* comm, MPI_Info* info, MPI_Comm *newcomm, int *ierror)
{
	mpi_comm_dup_with_info_(comm, info, newcomm, ierror);
}
int mpi_comm_free_(MPI_Fint *comm, int *ierror)
{
/* MPI_Comm_free */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_free(&c_comm);
*comm = PMPI_Comm_c2f(c_comm);


}
int mpi_comm_free__(MPI_Comm *comm, int *ierror)
{
	mpi_comm_free_(comm, ierror);
}
int mpi_comm_free_keyval_(int *comm_keyval, int *ierror)
{
/* MPI_Comm_free_keyval */

*ierror = MPI_Comm_free_keyval(comm_keyval);


}
int mpi_comm_free_keyval__(int *comm_keyval, int *ierror)
{
	mpi_comm_free_keyval_(comm_keyval, ierror);
}
int mpi_comm_get_attr_(MPI_Fint* comm, int* comm_keyval, void *attribute_val, int *flag, int *ierror)
{
/* MPI_Comm_get_attr */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_get_attr(c_comm, *comm_keyval, attribute_val, flag);


}
int mpi_comm_get_attr__(MPI_Comm* comm, int* comm_keyval, void *attribute_val, int *flag, int *ierror)
{
	mpi_comm_get_attr_(comm, comm_keyval, attribute_val, flag, ierror);
}
int mpi_comm_get_errhandler_(MPI_Fint* comm, MPI_Fint *errhandler, int *ierror)
{
/* MPI_Comm_get_errhandler */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Errhandler c_errhandler;

*ierror = MPI_Comm_get_errhandler(c_comm, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_comm_get_errhandler__(MPI_Comm* comm, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_comm_get_errhandler_(comm, errhandler, ierror);
}
int mpi_comm_get_info_(MPI_Fint* comm, MPI_Fint *info_used, int *ierror)
{
/* MPI_Comm_get_info */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info_used;

*ierror = MPI_Comm_get_info(c_comm, &c_info_used);
*info_used = PMPI_Info_c2f(c_info_used);


}
int mpi_comm_get_info__(MPI_Comm* comm, MPI_Info *info_used, int *ierror)
{
	mpi_comm_get_info_(comm, info_used, ierror);
}
int mpi_comm_get_name_(MPI_Fint* comm, char *comm_name CHAR_MIXED(size_comm_name), int *resultlen, int *ierror CHAR_END(size_comm_name))
{
/* MPI_Comm_get_name */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_get_name(c_comm, comm_name, resultlen);
char_c_to_fortran(comm_name,size_comm_name);


}
int mpi_comm_get_name__(MPI_Comm* comm, char *comm_name CHAR_MIXED(size_comm_name), int *resultlen, int *ierror CHAR_END(size_comm_name))
{
	mpi_comm_get_name_(comm, comm_name, resultlen, ierror);
}
int mpi_comm_get_parent_(MPI_Fint *parent, int *ierror)
{
/* MPI_Comm_get_parent */
MPI_Comm c_parent;

*ierror = MPI_Comm_get_parent(&c_parent);
*parent = PMPI_Comm_c2f(c_parent);


}
int mpi_comm_get_parent__(MPI_Comm *parent, int *ierror)
{
	mpi_comm_get_parent_(parent, ierror);
}
int mpi_comm_group_(MPI_Fint* comm, MPI_Fint *group, int *ierror)
{
/* MPI_Comm_group */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Group c_group;

*ierror = MPI_Comm_group(c_comm, &c_group);
*group = PMPI_Group_c2f(c_group);


}
int mpi_comm_group__(MPI_Comm* comm, MPI_Group *group, int *ierror)
{
	mpi_comm_group_(comm, group, ierror);
}
int mpi_comm_idup_(MPI_Fint* comm, MPI_Fint *newcomm, MPI_Fint *request, int *ierror)
{
/* MPI_Comm_idup */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_newcomm;
MPI_Request c_request;

*ierror = MPI_Comm_idup(c_comm, &c_newcomm, &c_request);
*newcomm = PMPI_Comm_c2f(c_newcomm);
*request = PMPI_Request_c2f(c_request);


}
int mpi_comm_idup__(MPI_Comm* comm, MPI_Comm *newcomm, MPI_Request *request, int *ierror)
{
	mpi_comm_idup_(comm, newcomm, request, ierror);
}
int mpi_comm_idup_with_info_(MPI_Fint* comm, MPI_Fint* info, MPI_Fint *newcomm, MPI_Fint *request, int *ierror)
{
/* MPI_Comm_idup_with_info */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_newcomm;
MPI_Request c_request;

*ierror = MPI_Comm_idup_with_info(c_comm, c_info, &c_newcomm, &c_request);
*newcomm = PMPI_Comm_c2f(c_newcomm);
*request = PMPI_Request_c2f(c_request);


}
int mpi_comm_idup_with_info__(MPI_Comm* comm, MPI_Info* info, MPI_Comm *newcomm, MPI_Request *request, int *ierror)
{
	mpi_comm_idup_with_info_(comm, info, newcomm, request, ierror);
}
int mpi_comm_join_(int* fd, MPI_Fint *intercomm, int *ierror)
{
/* MPI_Comm_join */
MPI_Comm c_intercomm;

*ierror = MPI_Comm_join(*fd, &c_intercomm);
*intercomm = PMPI_Comm_c2f(c_intercomm);


}
int mpi_comm_join__(int* fd, MPI_Comm *intercomm, int *ierror)
{
	mpi_comm_join_(fd, intercomm, ierror);
}
int mpi_comm_rank_(MPI_Fint* comm, int *rank, int *ierror)
{
/* MPI_Comm_rank */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_rank(c_comm, rank);


}
int mpi_comm_rank__(MPI_Comm* comm, int *rank, int *ierror)
{
	mpi_comm_rank_(comm, rank, ierror);
}
int mpi_comm_remote_group_(MPI_Fint* comm, MPI_Fint *group, int *ierror)
{
/* MPI_Comm_remote_group */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Group c_group;

*ierror = MPI_Comm_remote_group(c_comm, &c_group);
*group = PMPI_Group_c2f(c_group);


}
int mpi_comm_remote_group__(MPI_Comm* comm, MPI_Group *group, int *ierror)
{
	mpi_comm_remote_group_(comm, group, ierror);
}
int mpi_comm_remote_size_(MPI_Fint* comm, int *size, int *ierror)
{
/* MPI_Comm_remote_size */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_remote_size(c_comm, size);


}
int mpi_comm_remote_size__(MPI_Comm* comm, int *size, int *ierror)
{
	mpi_comm_remote_size_(comm, size, ierror);
}
int mpi_comm_set_attr_(MPI_Fint* comm, int* comm_keyval, void *attribute_val, int *ierror)
{
/* MPI_Comm_set_attr */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_set_attr(c_comm, *comm_keyval, attribute_val);


}
int mpi_comm_set_attr__(MPI_Comm* comm, int* comm_keyval, void *attribute_val, int *ierror)
{
	mpi_comm_set_attr_(comm, comm_keyval, attribute_val, ierror);
}
int mpi_comm_set_errhandler_(MPI_Fint* comm, MPI_Fint* errhandler, int *ierror)
{
/* MPI_Comm_set_errhandler */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);

*ierror = MPI_Comm_set_errhandler(c_comm, c_errhandler);


}
int mpi_comm_set_errhandler__(MPI_Comm* comm, MPI_Errhandler* errhandler, int *ierror)
{
	mpi_comm_set_errhandler_(comm, errhandler, ierror);
}
int mpi_comm_set_info_(MPI_Fint* comm, MPI_Fint* info, int *ierror)
{
/* MPI_Comm_set_info */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Comm_set_info(c_comm, c_info);


}
int mpi_comm_set_info__(MPI_Comm* comm, MPI_Info* info, int *ierror)
{
	mpi_comm_set_info_(comm, info, ierror);
}
int mpi_comm_set_name_(MPI_Fint* comm, const char *comm_name CHAR_MIXED(size_comm_name), int *ierror CHAR_END(size_comm_name))
{
/* MPI_Comm_set_name */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
char *tmp_comm_name = NULL, *ptr_comm_name = NULL;
tmp_comm_name = char_fortran_to_c((char *)comm_name, size_comm_name, &ptr_comm_name);

*ierror = MPI_Comm_set_name(c_comm, tmp_comm_name);
sctk_free(ptr_comm_name);


}
int mpi_comm_set_name__(MPI_Comm* comm, const char *comm_name CHAR_MIXED(size_comm_name), int *ierror CHAR_END(size_comm_name))
{
	mpi_comm_set_name_(comm, comm_name, ierror);
}
int mpi_comm_size_(MPI_Fint* comm, int *size, int *ierror)
{
/* MPI_Comm_size */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_size(c_comm, size);


}
int mpi_comm_size__(MPI_Comm* comm, int *size, int *ierror)
{
	mpi_comm_size_(comm, size, ierror);
}
int mpi_comm_spawn_(const char *command CHAR_MIXED(size_command), char *argv[], int* maxprocs, MPI_Fint* info, int* root, MPI_Fint* comm, MPI_Fint *intercomm, int array_of_errcodes[], int *ierror CHAR_END(size_command))
{
/* MPI_Comm_spawn */
char *tmp_command = NULL, *ptr_command = NULL;
tmp_command = char_fortran_to_c((char *)command, size_command, &ptr_command);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_intercomm;

*ierror = MPI_Comm_spawn(tmp_command, argv, *maxprocs, c_info, *root, c_comm, &c_intercomm, array_of_errcodes);
*intercomm = PMPI_Comm_c2f(c_intercomm);
sctk_free(ptr_command);


}
int mpi_comm_spawn__(const char *command CHAR_MIXED(size_command), char *argv[], int* maxprocs, MPI_Info* info, int* root, MPI_Comm* comm, MPI_Comm *intercomm, int array_of_errcodes[], int *ierror CHAR_END(size_command))
{
	mpi_comm_spawn_(command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, ierror);
}
int mpi_comm_spawn_multiple_(int* count, char *array_of_commands[], char **array_of_argv[], const int array_of_maxprocs[], const MPI_Fint array_of_info[], int* root, MPI_Fint* comm, MPI_Fint *intercomm, int array_of_errcodes[], int *ierror)
{
/* MPI_Comm_spawn_multiple */

int incnt_array_of_info = 0;
MPI_Info *c_array_of_info = NULL;

c_array_of_info = (MPI_Info*) sctk_malloc(sizeof(MPI_Info) * *count);

for(incnt_array_of_info = 0; incnt_array_of_info < *count ; incnt_array_of_info++)
        c_array_of_info[incnt_array_of_info] = PMPI_Info_f2c(array_of_info[incnt_array_of_info]);
        MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_intercomm;

*ierror = MPI_Comm_spawn_multiple(*count, array_of_commands, array_of_argv, array_of_maxprocs, c_array_of_info, *root, c_comm, &c_intercomm, array_of_errcodes);
*intercomm = PMPI_Comm_c2f(c_intercomm);
sctk_free(c_array_of_info);


}
int mpi_comm_spawn_multiple__(int* count, char *array_of_commands[], char **array_of_argv[], const int array_of_maxprocs[], const MPI_Info array_of_info[], int* root, MPI_Comm* comm, MPI_Comm *intercomm, int array_of_errcodes[], int *ierror)
{
	mpi_comm_spawn_multiple_(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierror);
}
int mpi_comm_split_(MPI_Fint* comm, int* color, int* key, MPI_Fint *newcomm, int *ierror)
{
/* MPI_Comm_split */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_split(c_comm, *color, *key, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);


}
int mpi_comm_split__(MPI_Comm* comm, int* color, int* key, MPI_Comm *newcomm, int *ierror)
{
	mpi_comm_split_(comm, color, key, newcomm, ierror);
}
int mpi_comm_split_type_(MPI_Fint* comm, int* split_type, int* key, MPI_Fint* info, MPI_Fint *newcomm, int *ierror)
{
/* MPI_Comm_split_type */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_newcomm;

*ierror = MPI_Comm_split_type(c_comm, *split_type, *key, c_info, &c_newcomm);
*newcomm = PMPI_Comm_c2f(c_newcomm);


}
int mpi_comm_split_type__(MPI_Comm* comm, int* split_type, int* key, MPI_Info* info, MPI_Comm *newcomm, int *ierror)
{
	mpi_comm_split_type_(comm, split_type, key, info, newcomm, ierror);
}
int mpi_comm_test_inter_(MPI_Fint* comm, int *flag, int *ierror)
{
/* MPI_Comm_test_inter */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Comm_test_inter(c_comm, flag);


}
int mpi_comm_test_inter__(MPI_Comm* comm, int *flag, int *ierror)
{
	mpi_comm_test_inter_(comm, flag, ierror);
}
int mpi_compare_and_swap_(const void *origin_addr, const void *compare_addr, void *result_addr, MPI_Fint* datatype, int* target_rank, MPI_Aint* target_disp, MPI_Fint* win, int *ierror)
{
/* MPI_Compare_and_swap */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
if( buffer_is_bottom((void *)compare_addr) )
	compare_addr = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Compare_and_swap(origin_addr, compare_addr, result_addr, c_datatype, *target_rank, *target_disp, c_win);


}
int mpi_compare_and_swap__(const void *origin_addr, const void *compare_addr, void *result_addr, MPI_Datatype* datatype, int* target_rank, MPI_Aint* target_disp, MPI_Win* win, int *ierror)
{
	mpi_compare_and_swap_(origin_addr, compare_addr, result_addr, datatype, target_rank, target_disp, win, ierror);
}
int mpi_dims_create_(int* nnodes, int* ndims, int dims[], int *ierror)
{
/* MPI_Dims_create */

*ierror = MPI_Dims_create(*nnodes, *ndims, dims);


}
int mpi_dims_create__(int* nnodes, int* ndims, int dims[], int *ierror)
{
	mpi_dims_create_(nnodes, ndims, dims, ierror);
}
int mpi_dist_graph_create_(MPI_Fint* comm_old, int* n, const int sources[], const int degrees[], const int destinations[], const int weights[], MPI_Fint* info, int* reorder, MPI_Fint *comm_dist_graph, int *ierror)
{
/* MPI_Dist_graph_create */
MPI_Comm c_comm_old = PMPI_Comm_f2c(*comm_old);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm_dist_graph;

*ierror = MPI_Dist_graph_create(c_comm_old, *n, sources, degrees, destinations, weights, c_info, *reorder, &c_comm_dist_graph);
*comm_dist_graph = PMPI_Comm_c2f(c_comm_dist_graph);


}
int mpi_dist_graph_create__(MPI_Comm* comm_old, int* n, const int sources[], const int degrees[], const int destinations[], const int weights[], MPI_Info* info, int* reorder, MPI_Comm *comm_dist_graph, int *ierror)
{
	mpi_dist_graph_create_(comm_old, n, sources, degrees, destinations, weights, info, reorder, comm_dist_graph, ierror);
}
int mpi_dist_graph_create_adjacent_(MPI_Fint* comm_old, int* indegree, const int sources[], const int sourceweights[], int* outdegree, const int destinations[], const int destweights[], MPI_Fint* info, int* reorder, MPI_Fint *comm_dist_graph, int *ierror)
{
/* MPI_Dist_graph_create_adjacent */
MPI_Comm c_comm_old = PMPI_Comm_f2c(*comm_old);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm_dist_graph;

*ierror = MPI_Dist_graph_create_adjacent(c_comm_old, *indegree, sources, sourceweights, *outdegree, destinations, destweights, c_info, *reorder, &c_comm_dist_graph);
*comm_dist_graph = PMPI_Comm_c2f(c_comm_dist_graph);


}
int mpi_dist_graph_create_adjacent__(MPI_Comm* comm_old, int* indegree, const int sources[], const int sourceweights[], int* outdegree, const int destinations[], const int destweights[], MPI_Info* info, int* reorder, MPI_Comm *comm_dist_graph, int *ierror)
{
	mpi_dist_graph_create_adjacent_(comm_old, indegree, sources, sourceweights, outdegree, destinations, destweights, info, reorder, comm_dist_graph, ierror);
}
int mpi_dist_graph_neighbors_(MPI_Fint* comm, int* maxindegree, int sources[], int sourceweights[], int* maxoutdegree, int destinations[], int destweights[], int *ierror)
{
/* MPI_Dist_graph_neighbors */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Dist_graph_neighbors(c_comm, *maxindegree, sources, sourceweights, *maxoutdegree, destinations, destweights);


}
int mpi_dist_graph_neighbors__(MPI_Comm* comm, int* maxindegree, int sources[], int sourceweights[], int* maxoutdegree, int destinations[], int destweights[], int *ierror)
{
	mpi_dist_graph_neighbors_(comm, maxindegree, sources, sourceweights, maxoutdegree, destinations, destweights, ierror);
}
int mpi_dist_graph_neighbors_count_(MPI_Fint* comm, int *indegree, int *outdegree, int *weighted, int *ierror)
{
/* MPI_Dist_graph_neighbors_count */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Dist_graph_neighbors_count(c_comm, indegree, outdegree, weighted);


}
int mpi_dist_graph_neighbors_count__(MPI_Comm* comm, int *indegree, int *outdegree, int *weighted, int *ierror)
{
	mpi_dist_graph_neighbors_count_(comm, indegree, outdegree, weighted, ierror);
}
int mpi_errhandler_free_(MPI_Fint *errhandler, int *ierror)
{
/* MPI_Errhandler_free */
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);

*ierror = MPI_Errhandler_free(&c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_errhandler_free__(MPI_Errhandler *errhandler, int *ierror)
{
	mpi_errhandler_free_(errhandler, ierror);
}
int mpi_error_class_(int* errorcode, int *errorclass, int *ierror)
{
/* MPI_Error_class */

*ierror = MPI_Error_class(*errorcode, errorclass);


}
int mpi_error_class__(int* errorcode, int *errorclass, int *ierror)
{
	mpi_error_class_(errorcode, errorclass, ierror);
}
int mpi_error_string_(int* errorcode, char *string CHAR_MIXED(size_string), int *resultlen, int *ierror CHAR_END(size_string))
{
/* MPI_Error_string */

*ierror = MPI_Error_string(*errorcode, string, resultlen);
char_c_to_fortran(string,size_string);


}
int mpi_error_string__(int* errorcode, char *string CHAR_MIXED(size_string), int *resultlen, int *ierror CHAR_END(size_string))
{
	mpi_error_string_(errorcode, string, resultlen, ierror);
}
int mpi_exscan_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, int *ierror)
{
/* MPI_Exscan */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Exscan(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm);


}
int mpi_exscan__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, int *ierror)
{
	mpi_exscan_(sendbuf, recvbuf, count, datatype, op, comm, ierror);
}
int mpi_exscan_init_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Exscan_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Exscan_init(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_exscan_init__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_exscan_init_(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierror);
}
int mpi_f_sync_reg_(void *buf)
{
}
int mpi_f_sync_reg__(void *buf)
{
	mpi_f_sync_reg_(buf, ierror);
}
int mpi_fetch_and_op_(const void *origin_addr, void *result_addr, MPI_Fint* datatype, int* target_rank, MPI_Aint* target_disp, MPI_Fint* op, MPI_Fint* win, int *ierror)
{
/* MPI_Fetch_and_op */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Fetch_and_op(origin_addr, result_addr, c_datatype, *target_rank, *target_disp, c_op, c_win);


}
int mpi_fetch_and_op__(const void *origin_addr, void *result_addr, MPI_Datatype* datatype, int* target_rank, MPI_Aint* target_disp, MPI_Op* op, MPI_Win* win, int *ierror)
{
	mpi_fetch_and_op_(origin_addr, result_addr, datatype, target_rank, target_disp, op, win, ierror);
}
int mpi_file_call_errhandler_(MPI_Fint* fh, int* errorcode, int *ierror)
{
/* MPI_File_call_errhandler */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_call_errhandler(c_fh, *errorcode);


}
int mpi_file_call_errhandler__(MPI_File* fh, int* errorcode, int *ierror)
{
	mpi_file_call_errhandler_(fh, errorcode, ierror);
}
int mpi_file_close_(MPI_Fint *fh, int *ierror)
{
/* MPI_File_close */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_close(&c_fh);
*fh = PMPI_File_c2f(c_fh);


}
int mpi_file_close__(MPI_File *fh, int *ierror)
{
	mpi_file_close_(fh, ierror);
}
int mpi_file_create_errhandler_(MPI_File_errhandler_function *file_errhandler_fn, MPI_Fint *errhandler, int *ierror)
{
/* MPI_File_create_errhandler */
MPI_Errhandler c_errhandler;

*ierror = MPI_File_create_errhandler(file_errhandler_fn, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_file_create_errhandler__(MPI_File_errhandler_function *file_errhandler_fn, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_file_create_errhandler_(file_errhandler_fn, errhandler, ierror);
}
int mpi_file_delete_(const char *filename CHAR_MIXED(size_filename), MPI_Fint* info, int *ierror CHAR_END(size_filename))
{
/* MPI_File_delete */
char *tmp_filename = NULL, *ptr_filename = NULL;
tmp_filename = char_fortran_to_c((char *)filename, size_filename, &ptr_filename);
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_File_delete(tmp_filename, c_info);
sctk_free(ptr_filename);


}
int mpi_file_delete__(const char *filename CHAR_MIXED(size_filename), MPI_Info* info, int *ierror CHAR_END(size_filename))
{
	mpi_file_delete_(filename, info, ierror);
}
int mpi_file_get_amode_(MPI_Fint* fh, int *amode, int *ierror)
{
/* MPI_File_get_amode */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_get_amode(c_fh, amode);


}
int mpi_file_get_amode__(MPI_File* fh, int *amode, int *ierror)
{
	mpi_file_get_amode_(fh, amode, ierror);
}
int mpi_file_get_atomicity_(MPI_Fint* fh, int *flag, int *ierror)
{
/* MPI_File_get_atomicity */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_get_atomicity(c_fh, flag);


}
int mpi_file_get_atomicity__(MPI_File* fh, int *flag, int *ierror)
{
	mpi_file_get_atomicity_(fh, flag, ierror);
}
int mpi_file_get_byte_offset_(MPI_Fint* fh, MPI_Offset* offset, MPI_Offset *disp, int *ierror)
{
/* MPI_File_get_byte_offset */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_get_byte_offset(c_fh, *offset, disp);


}
int mpi_file_get_byte_offset__(MPI_File* fh, MPI_Offset* offset, MPI_Offset *disp, int *ierror)
{
	mpi_file_get_byte_offset_(fh, offset, disp, ierror);
}
int mpi_file_get_errhandler_(MPI_Fint* file, MPI_Fint *errhandler, int *ierror)
{
/* MPI_File_get_errhandler */
MPI_File c_file = PMPI_File_f2c(*file);
MPI_Errhandler c_errhandler;

*ierror = MPI_File_get_errhandler(c_file, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_file_get_errhandler__(MPI_File* file, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_file_get_errhandler_(file, errhandler, ierror);
}
int mpi_file_get_group_(MPI_Fint* fh, MPI_Fint *group, int *ierror)
{
/* MPI_File_get_group */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Group c_group;

*ierror = MPI_File_get_group(c_fh, &c_group);
*group = PMPI_Group_c2f(c_group);


}
int mpi_file_get_group__(MPI_File* fh, MPI_Group *group, int *ierror)
{
	mpi_file_get_group_(fh, group, ierror);
}
int mpi_file_get_info_(MPI_Fint* fh, MPI_Fint *info_used, int *ierror)
{
/* MPI_File_get_info */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Info c_info_used;

*ierror = MPI_File_get_info(c_fh, &c_info_used);
*info_used = PMPI_Info_c2f(c_info_used);


}
int mpi_file_get_info__(MPI_File* fh, MPI_Info *info_used, int *ierror)
{
	mpi_file_get_info_(fh, info_used, ierror);
}
int mpi_file_get_position_(MPI_Fint* fh, MPI_Offset *offset, int *ierror)
{
/* MPI_File_get_position */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_get_position(c_fh, offset);


}
int mpi_file_get_position__(MPI_File* fh, MPI_Offset *offset, int *ierror)
{
	mpi_file_get_position_(fh, offset, ierror);
}
int mpi_file_get_position_shared_(MPI_Fint* fh, MPI_Offset *offset, int *ierror)
{
/* MPI_File_get_position_shared */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_get_position_shared(c_fh, offset);


}
int mpi_file_get_position_shared__(MPI_File* fh, MPI_Offset *offset, int *ierror)
{
	mpi_file_get_position_shared_(fh, offset, ierror);
}
int mpi_file_get_size_(MPI_Fint* fh, MPI_Offset *size, int *ierror)
{
/* MPI_File_get_size */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_get_size(c_fh, size);


}
int mpi_file_get_size__(MPI_File* fh, MPI_Offset *size, int *ierror)
{
	mpi_file_get_size_(fh, size, ierror);
}
int mpi_file_get_type_extent_(MPI_Fint* fh, MPI_Fint* datatype, MPI_Aint *extent, int *ierror)
{
/* MPI_File_get_type_extent */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_get_type_extent(c_fh, c_datatype, extent);


}
int mpi_file_get_type_extent__(MPI_File* fh, MPI_Datatype* datatype, MPI_Aint *extent, int *ierror)
{
	mpi_file_get_type_extent_(fh, datatype, extent, ierror);
}
int mpi_file_get_view_(MPI_Fint* fh, MPI_Offset *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep CHAR_MIXED(size_datarep), int *ierror CHAR_END(size_datarep))
{
/* MPI_File_get_view */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_etype;
MPI_Datatype c_filetype;

*ierror = MPI_File_get_view(c_fh, disp, &c_etype, &c_filetype, datarep);
*etype = PMPI_Type_c2f(c_etype);
*filetype = PMPI_Type_c2f(c_filetype);
char_c_to_fortran(datarep,size_datarep);


}
int mpi_file_get_view__(MPI_File* fh, MPI_Offset *disp, MPI_Datatype *etype, MPI_Datatype *filetype, char *datarep CHAR_MIXED(size_datarep), int *ierror CHAR_END(size_datarep))
{
	mpi_file_get_view_(fh, disp, etype, filetype, datarep, ierror);
}
int mpi_file_iread_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iread */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iread(c_fh, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iread__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iread_(fh, buf, count, datatype, request, ierror);
}
int mpi_file_iread_all_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iread_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iread_all(c_fh, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iread_all__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iread_all_(fh, buf, count, datatype, request, ierror);
}
int mpi_file_iread_at_(MPI_Fint* fh, MPI_Offset* offset, void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iread_at */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iread_at(c_fh, *offset, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iread_at__(MPI_File* fh, MPI_Offset* offset, void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iread_at_(fh, offset, buf, count, datatype, request, ierror);
}
int mpi_file_iread_at_all_(MPI_Fint* fh, MPI_Offset* offset, void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iread_at_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iread_at_all(c_fh, *offset, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iread_at_all__(MPI_File* fh, MPI_Offset* offset, void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iread_at_all_(fh, offset, buf, count, datatype, request, ierror);
}
int mpi_file_iread_shared_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iread_shared */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iread_shared(c_fh, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iread_shared__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iread_shared_(fh, buf, count, datatype, request, ierror);
}
int mpi_file_iwrite_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iwrite */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iwrite(c_fh, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iwrite__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iwrite_(fh, buf, count, datatype, request, ierror);
}
int mpi_file_iwrite_all_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iwrite_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iwrite_all(c_fh, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iwrite_all__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iwrite_all_(fh, buf, count, datatype, request, ierror);
}
int mpi_file_iwrite_at_(MPI_Fint* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iwrite_at */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iwrite_at(c_fh, *offset, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iwrite_at__(MPI_File* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iwrite_at_(fh, offset, buf, count, datatype, request, ierror);
}
int mpi_file_iwrite_at_all_(MPI_Fint* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iwrite_at_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iwrite_at_all(c_fh, *offset, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iwrite_at_all__(MPI_File* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iwrite_at_all_(fh, offset, buf, count, datatype, request, ierror);
}
int mpi_file_iwrite_shared_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, MPI_Fint *request, int *ierror)
{
/* MPI_File_iwrite_shared */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_File_iwrite_shared(c_fh, buf, *count, c_datatype, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_file_iwrite_shared__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, MPI_Request *request, int *ierror)
{
	mpi_file_iwrite_shared_(fh, buf, count, datatype, request, ierror);
}
int mpi_file_open_(MPI_Fint* comm, const char *filename CHAR_MIXED(size_filename), int* amode, MPI_Fint* info, MPI_Fint *fh, int *ierror CHAR_END(size_filename))
{
/* MPI_File_open */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
char *tmp_filename = NULL, *ptr_filename = NULL;
tmp_filename = char_fortran_to_c((char *)filename, size_filename, &ptr_filename);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_File c_fh;

*ierror = MPI_File_open(c_comm, tmp_filename, *amode, c_info, &c_fh);
*fh = PMPI_File_c2f(c_fh);
sctk_free(ptr_filename);


}
int mpi_file_open__(MPI_Comm* comm, const char *filename CHAR_MIXED(size_filename), int* amode, MPI_Info* info, MPI_File *fh, int *ierror CHAR_END(size_filename))
{
	mpi_file_open_(comm, filename, amode, info, fh, ierror);
}
int mpi_file_preallocate_(MPI_Fint* fh, MPI_Offset* size, int *ierror)
{
/* MPI_File_preallocate */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_preallocate(c_fh, *size);


}
int mpi_file_preallocate__(MPI_File* fh, MPI_Offset* size, int *ierror)
{
	mpi_file_preallocate_(fh, size, ierror);
}
int mpi_file_read_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_read */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_read__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_read_(fh, buf, count, datatype, status, ierror);
}
int mpi_file_read_all_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_read_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_all(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_read_all__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_read_all_(fh, buf, count, datatype, status, ierror);
}
int mpi_file_read_all_begin_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, int *ierror)
{
/* MPI_File_read_all_begin */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_all_begin(c_fh, buf, *count, c_datatype);


}
int mpi_file_read_all_begin__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, int *ierror)
{
	mpi_file_read_all_begin_(fh, buf, count, datatype, ierror);
}
int mpi_file_read_all_end_(MPI_Fint* fh, void *buf, MPI_Status *status, int *ierror)
{
/* MPI_File_read_all_end */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_read_all_end(c_fh, buf, status);


}
int mpi_file_read_all_end__(MPI_File* fh, void *buf, MPI_Status *status, int *ierror)
{
	mpi_file_read_all_end_(fh, buf, status, ierror);
}
int mpi_file_read_at_(MPI_Fint* fh, MPI_Offset* offset, void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_read_at */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_at(c_fh, *offset, buf, *count, c_datatype, status);


}
int mpi_file_read_at__(MPI_File* fh, MPI_Offset* offset, void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_read_at_(fh, offset, buf, count, datatype, status, ierror);
}
int mpi_file_read_at_all_(MPI_Fint* fh, MPI_Offset* offset, void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_read_at_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_at_all(c_fh, *offset, buf, *count, c_datatype, status);


}
int mpi_file_read_at_all__(MPI_File* fh, MPI_Offset* offset, void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_read_at_all_(fh, offset, buf, count, datatype, status, ierror);
}
int mpi_file_read_at_all_begin_(MPI_Fint* fh, MPI_Offset* offset, void *buf, int* count, MPI_Fint* datatype, int *ierror)
{
/* MPI_File_read_at_all_begin */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_at_all_begin(c_fh, *offset, buf, *count, c_datatype);


}
int mpi_file_read_at_all_begin__(MPI_File* fh, MPI_Offset* offset, void *buf, int* count, MPI_Datatype* datatype, int *ierror)
{
	mpi_file_read_at_all_begin_(fh, offset, buf, count, datatype, ierror);
}
int mpi_file_read_at_all_end_(MPI_Fint* fh, void *buf, MPI_Status *status, int *ierror)
{
/* MPI_File_read_at_all_end */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_read_at_all_end(c_fh, buf, status);


}
int mpi_file_read_at_all_end__(MPI_File* fh, void *buf, MPI_Status *status, int *ierror)
{
	mpi_file_read_at_all_end_(fh, buf, status, ierror);
}
int mpi_file_read_ordered_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_read_ordered */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_ordered(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_read_ordered__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_read_ordered_(fh, buf, count, datatype, status, ierror);
}
int mpi_file_read_ordered_begin_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, int *ierror)
{
/* MPI_File_read_ordered_begin */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_ordered_begin(c_fh, buf, *count, c_datatype);


}
int mpi_file_read_ordered_begin__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, int *ierror)
{
	mpi_file_read_ordered_begin_(fh, buf, count, datatype, ierror);
}
int mpi_file_read_ordered_end_(MPI_Fint* fh, void *buf, MPI_Status *status, int *ierror)
{
/* MPI_File_read_ordered_end */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_read_ordered_end(c_fh, buf, status);


}
int mpi_file_read_ordered_end__(MPI_File* fh, void *buf, MPI_Status *status, int *ierror)
{
	mpi_file_read_ordered_end_(fh, buf, status, ierror);
}
int mpi_file_read_shared_(MPI_Fint* fh, void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_read_shared */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_read_shared(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_read_shared__(MPI_File* fh, void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_read_shared_(fh, buf, count, datatype, status, ierror);
}
int mpi_file_seek_(MPI_Fint* fh, MPI_Offset* offset, int* whence, int *ierror)
{
/* MPI_File_seek */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_seek(c_fh, *offset, *whence);


}
int mpi_file_seek__(MPI_File* fh, MPI_Offset* offset, int* whence, int *ierror)
{
	mpi_file_seek_(fh, offset, whence, ierror);
}
int mpi_file_seek_shared_(MPI_Fint* fh, MPI_Offset* offset, int* whence, int *ierror)
{
/* MPI_File_seek_shared */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_seek_shared(c_fh, *offset, *whence);


}
int mpi_file_seek_shared__(MPI_File* fh, MPI_Offset* offset, int* whence, int *ierror)
{
	mpi_file_seek_shared_(fh, offset, whence, ierror);
}
int mpi_file_set_atomicity_(MPI_Fint* fh, int* flag, int *ierror)
{
/* MPI_File_set_atomicity */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_set_atomicity(c_fh, *flag);


}
int mpi_file_set_atomicity__(MPI_File* fh, int* flag, int *ierror)
{
	mpi_file_set_atomicity_(fh, flag, ierror);
}
int mpi_file_set_errhandler_(MPI_Fint* file, MPI_Fint* errhandler, int *ierror)
{
/* MPI_File_set_errhandler */
MPI_File c_file = PMPI_File_f2c(*file);
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);

*ierror = MPI_File_set_errhandler(c_file, c_errhandler);


}
int mpi_file_set_errhandler__(MPI_File* file, MPI_Errhandler* errhandler, int *ierror)
{
	mpi_file_set_errhandler_(file, errhandler, ierror);
}
int mpi_file_set_info_(MPI_Fint* fh, MPI_Fint* info, int *ierror)
{
/* MPI_File_set_info */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_File_set_info(c_fh, c_info);


}
int mpi_file_set_info__(MPI_File* fh, MPI_Info* info, int *ierror)
{
	mpi_file_set_info_(fh, info, ierror);
}
int mpi_file_set_size_(MPI_Fint* fh, MPI_Offset* size, int *ierror)
{
/* MPI_File_set_size */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_set_size(c_fh, *size);


}
int mpi_file_set_size__(MPI_File* fh, MPI_Offset* size, int *ierror)
{
	mpi_file_set_size_(fh, size, ierror);
}
int mpi_file_set_view_(MPI_Fint* fh, MPI_Offset* disp, MPI_Fint* etype, MPI_Fint* filetype, const char *datarep CHAR_MIXED(size_datarep), MPI_Fint* info, int *ierror CHAR_END(size_datarep))
{
/* MPI_File_set_view */
MPI_File c_fh = PMPI_File_f2c(*fh);
MPI_Datatype c_etype = PMPI_Type_f2c(*etype);
MPI_Datatype c_filetype = PMPI_Type_f2c(*filetype);
char *tmp_datarep = NULL, *ptr_datarep = NULL;
tmp_datarep = char_fortran_to_c((char *)datarep, size_datarep, &ptr_datarep);
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_File_set_view(c_fh, *disp, c_etype, c_filetype, tmp_datarep, c_info);
sctk_free(ptr_datarep);


}
int mpi_file_set_view__(MPI_File* fh, MPI_Offset* disp, MPI_Datatype* etype, MPI_Datatype* filetype, const char *datarep CHAR_MIXED(size_datarep), MPI_Info* info, int *ierror CHAR_END(size_datarep))
{
	mpi_file_set_view_(fh, disp, etype, filetype, datarep, info, ierror);
}
int mpi_file_sync_(MPI_Fint* fh, int *ierror)
{
/* MPI_File_sync */
MPI_File c_fh = PMPI_File_f2c(*fh);

*ierror = MPI_File_sync(c_fh);


}
int mpi_file_sync__(MPI_File* fh, int *ierror)
{
	mpi_file_sync_(fh, ierror);
}
int mpi_file_write_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_write */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_write__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_write_(fh, buf, count, datatype, status, ierror);
}
int mpi_file_write_all_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_write_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_all(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_write_all__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_write_all_(fh, buf, count, datatype, status, ierror);
}
int mpi_file_write_all_begin_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, int *ierror)
{
/* MPI_File_write_all_begin */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_all_begin(c_fh, buf, *count, c_datatype);


}
int mpi_file_write_all_begin__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, int *ierror)
{
	mpi_file_write_all_begin_(fh, buf, count, datatype, ierror);
}
int mpi_file_write_all_end_(MPI_Fint* fh, const void *buf, MPI_Status *status, int *ierror)
{
/* MPI_File_write_all_end */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;

*ierror = MPI_File_write_all_end(c_fh, buf, status);


}
int mpi_file_write_all_end__(MPI_File* fh, const void *buf, MPI_Status *status, int *ierror)
{
	mpi_file_write_all_end_(fh, buf, status, ierror);
}
int mpi_file_write_at_(MPI_Fint* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_write_at */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_at(c_fh, *offset, buf, *count, c_datatype, status);


}
int mpi_file_write_at__(MPI_File* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_write_at_(fh, offset, buf, count, datatype, status, ierror);
}
int mpi_file_write_at_all_(MPI_Fint* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_write_at_all */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_at_all(c_fh, *offset, buf, *count, c_datatype, status);


}
int mpi_file_write_at_all__(MPI_File* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_write_at_all_(fh, offset, buf, count, datatype, status, ierror);
}
int mpi_file_write_at_all_begin_(MPI_Fint* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Fint* datatype, int *ierror)
{
/* MPI_File_write_at_all_begin */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_at_all_begin(c_fh, *offset, buf, *count, c_datatype);


}
int mpi_file_write_at_all_begin__(MPI_File* fh, MPI_Offset* offset, const void *buf, int* count, MPI_Datatype* datatype, int *ierror)
{
	mpi_file_write_at_all_begin_(fh, offset, buf, count, datatype, ierror);
}
int mpi_file_write_at_all_end_(MPI_Fint* fh, const void *buf, MPI_Status *status, int *ierror)
{
/* MPI_File_write_at_all_end */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;

*ierror = MPI_File_write_at_all_end(c_fh, buf, status);


}
int mpi_file_write_at_all_end__(MPI_File* fh, const void *buf, MPI_Status *status, int *ierror)
{
	mpi_file_write_at_all_end_(fh, buf, status, ierror);
}
int mpi_file_write_ordered_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_write_ordered */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_ordered(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_write_ordered__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_write_ordered_(fh, buf, count, datatype, status, ierror);
}
int mpi_file_write_ordered_begin_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, int *ierror)
{
/* MPI_File_write_ordered_begin */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_ordered_begin(c_fh, buf, *count, c_datatype);


}
int mpi_file_write_ordered_begin__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, int *ierror)
{
	mpi_file_write_ordered_begin_(fh, buf, count, datatype, ierror);
}
int mpi_file_write_ordered_end_(MPI_Fint* fh, const void *buf, MPI_Status *status, int *ierror)
{
/* MPI_File_write_ordered_end */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;

*ierror = MPI_File_write_ordered_end(c_fh, buf, status);


}
int mpi_file_write_ordered_end__(MPI_File* fh, const void *buf, MPI_Status *status, int *ierror)
{
	mpi_file_write_ordered_end_(fh, buf, status, ierror);
}
int mpi_file_write_shared_(MPI_Fint* fh, const void *buf, int* count, MPI_Fint* datatype, MPI_Status *status, int *ierror)
{
/* MPI_File_write_shared */
MPI_File c_fh = PMPI_File_f2c(*fh);
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_File_write_shared(c_fh, buf, *count, c_datatype, status);


}
int mpi_file_write_shared__(MPI_File* fh, const void *buf, int* count, MPI_Datatype* datatype, MPI_Status *status, int *ierror)
{
	mpi_file_write_shared_(fh, buf, count, datatype, status, ierror);
}
int mpi_finalize_(int *ierror)
{
/* MPI_Finalize */

*ierror = MPI_Finalize();


}
int mpi_finalize__(int *ierror)
{
	mpi_finalize_(, ierror);
}
int mpi_finalized_(int *flag, int *ierror)
{
/* MPI_Finalized */

*ierror = MPI_Finalized(flag);


}
int mpi_finalized__(int *flag, int *ierror)
{
	mpi_finalized_(flag, ierror);
}
int mpi_free_mem_(void *base, int *ierror)
{
/* MPI_Free_mem */
if( buffer_is_bottom((void *)base) )
	base = MPI_BOTTOM;

*ierror = MPI_Free_mem(base);


}
int mpi_free_mem__(void *base, int *ierror)
{
	mpi_free_mem_(base, ierror);
}
int mpi_gather_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, int *ierror)
{
/* MPI_Gather */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Gather(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm);


}
int mpi_gather__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, int *ierror)
{
	mpi_gather_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror);
}
int mpi_gather_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Gather_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Gather_init(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_gather_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_gather_init_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierror);
}
int mpi_gatherv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, int* root, MPI_Fint* comm, int *ierror)
{
/* MPI_Gatherv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Gatherv(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, *root, c_comm);


}
int mpi_gatherv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, int* root, MPI_Comm* comm, int *ierror)
{
	mpi_gatherv_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierror);
}
int mpi_gatherv_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Gatherv_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Gatherv_init(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, *root, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_gatherv_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_gatherv_init_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, info, request, ierror);
}
int mpi_get_(void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* win, int *ierror)
{
/* MPI_Get */
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Get(origin_addr, *origin_count, c_origin_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_win);


}
int mpi_get__(void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Win* win, int *ierror)
{
	mpi_get_(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierror);
}
int mpi_get_accumulate_(const void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, void *result_addr, int* result_count, MPI_Fint* result_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* op, MPI_Fint* win, int *ierror)
{
/* MPI_Get_accumulate */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_result_datatype = PMPI_Type_f2c(*result_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Get_accumulate(origin_addr, *origin_count, c_origin_datatype, result_addr, *result_count, c_result_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_op, c_win);


}
int mpi_get_accumulate__(const void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, void *result_addr, int* result_count, MPI_Datatype* result_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Op* op, MPI_Win* win, int *ierror)
{
	mpi_get_accumulate_(origin_addr, origin_count, origin_datatype, result_addr, result_count, result_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierror);
}
int mpi_get_address_(const void *location, MPI_Aint *address, int *ierror)
{
/* MPI_Get_address */
if( buffer_is_bottom((void *)location) )
	location = MPI_BOTTOM;

*ierror = MPI_Get_address(location, address);


}
int mpi_get_address__(const void *location, MPI_Aint *address, int *ierror)
{
	mpi_get_address_(location, address, ierror);
}
int mpi_get_count_(const MPI_Status *status, MPI_Fint* datatype, int *count, int *ierror)
{
/* MPI_Get_count */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Get_count(status, c_datatype, count);


}
int mpi_get_count__(const MPI_Status *status, MPI_Datatype* datatype, int *count, int *ierror)
{
	mpi_get_count_(status, datatype, count, ierror);
}
int mpi_get_elements_(const MPI_Status *status, MPI_Fint* datatype, int *count, int *ierror)
{
/* MPI_Get_elements */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Get_elements(status, c_datatype, count);


}
int mpi_get_elements__(const MPI_Status *status, MPI_Datatype* datatype, int *count, int *ierror)
{
	mpi_get_elements_(status, datatype, count, ierror);
}
int mpi_get_elements_x_(const MPI_Status *status, MPI_Fint* datatype, MPI_Count *count, int *ierror)
{
/* MPI_Get_elements_x */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Get_elements_x(status, c_datatype, count);


}
int mpi_get_elements_x__(const MPI_Status *status, MPI_Datatype* datatype, MPI_Count *count, int *ierror)
{
	mpi_get_elements_x_(status, datatype, count, ierror);
}
int mpi_get_library_version_(char *version CHAR_MIXED(size_version), int *resultlen, int *ierror CHAR_END(size_version))
{
/* MPI_Get_library_version */

*ierror = MPI_Get_library_version(version, resultlen);
char_c_to_fortran(version,size_version);


}
int mpi_get_library_version__(char *version CHAR_MIXED(size_version), int *resultlen, int *ierror CHAR_END(size_version))
{
	mpi_get_library_version_(version, resultlen, ierror);
}
int mpi_get_processor_name_(char *name CHAR_MIXED(size_name), int *resultlen, int *ierror CHAR_END(size_name))
{
/* MPI_Get_processor_name */

*ierror = MPI_Get_processor_name(name, resultlen);
char_c_to_fortran(name,size_name);


}
int mpi_get_processor_name__(char *name CHAR_MIXED(size_name), int *resultlen, int *ierror CHAR_END(size_name))
{
	mpi_get_processor_name_(name, resultlen, ierror);
}
int mpi_get_version_(int *version, int *subversion, int *ierror)
{
/* MPI_Get_version */

*ierror = MPI_Get_version(version, subversion);


}
int mpi_get_version__(int *version, int *subversion, int *ierror)
{
	mpi_get_version_(version, subversion, ierror);
}
int mpi_graph_create_(MPI_Fint* comm_old, int* nnodes, const int index[], const int edges[], int* reorder, MPI_Fint *comm_graph, int *ierror)
{
/* MPI_Graph_create */
MPI_Comm c_comm_old = PMPI_Comm_f2c(*comm_old);
MPI_Comm c_comm_graph;

*ierror = MPI_Graph_create(c_comm_old, *nnodes, index, edges, *reorder, &c_comm_graph);
*comm_graph = PMPI_Comm_c2f(c_comm_graph);


}
int mpi_graph_create__(MPI_Comm* comm_old, int* nnodes, const int index[], const int edges[], int* reorder, MPI_Comm *comm_graph, int *ierror)
{
	mpi_graph_create_(comm_old, nnodes, index, edges, reorder, comm_graph, ierror);
}
int mpi_graph_get_(MPI_Fint* comm, int* maxindex, int* maxedges, int index[], int edges[], int *ierror)
{
/* MPI_Graph_get */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Graph_get(c_comm, *maxindex, *maxedges, index, edges);


}
int mpi_graph_get__(MPI_Comm* comm, int* maxindex, int* maxedges, int index[], int edges[], int *ierror)
{
	mpi_graph_get_(comm, maxindex, maxedges, index, edges, ierror);
}
int mpi_graph_map_(MPI_Fint* comm, int* nnodes, const int index[], const int edges[], int *newrank, int *ierror)
{
/* MPI_Graph_map */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Graph_map(c_comm, *nnodes, index, edges, newrank);


}
int mpi_graph_map__(MPI_Comm* comm, int* nnodes, const int index[], const int edges[], int *newrank, int *ierror)
{
	mpi_graph_map_(comm, nnodes, index, edges, newrank, ierror);
}
int mpi_graph_neighbors_(MPI_Fint* comm, int* rank, int* maxneighbors, int neighbors[], int *ierror)
{
/* MPI_Graph_neighbors */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Graph_neighbors(c_comm, *rank, *maxneighbors, neighbors);


}
int mpi_graph_neighbors__(MPI_Comm* comm, int* rank, int* maxneighbors, int neighbors[], int *ierror)
{
	mpi_graph_neighbors_(comm, rank, maxneighbors, neighbors, ierror);
}
int mpi_graph_neighbors_count_(MPI_Fint* comm, int* rank, int *nneighbors, int *ierror)
{
/* MPI_Graph_neighbors_count */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Graph_neighbors_count(c_comm, *rank, nneighbors);


}
int mpi_graph_neighbors_count__(MPI_Comm* comm, int* rank, int *nneighbors, int *ierror)
{
	mpi_graph_neighbors_count_(comm, rank, nneighbors, ierror);
}
int mpi_graphdims_get_(MPI_Fint* comm, int *nnodes, int *nedges, int *ierror)
{
/* MPI_Graphdims_get */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Graphdims_get(c_comm, nnodes, nedges);


}
int mpi_graphdims_get__(MPI_Comm* comm, int *nnodes, int *nedges, int *ierror)
{
	mpi_graphdims_get_(comm, nnodes, nedges, ierror);
}
int mpi_grequest_complete_(MPI_Fint* request, int *ierror)
{
/* MPI_Grequest_complete */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Grequest_complete(c_request);


}
int mpi_grequest_complete__(MPI_Request* request, int *ierror)
{
	mpi_grequest_complete_(request, ierror);
}
int mpi_grequest_start_(MPI_Grequest_query_function *query_fn, MPI_Grequest_free_function *free_fn, MPI_Grequest_cancel_function *cancel_fn, void *extra_state, MPI_Fint *request, int *ierror)
{
/* MPI_Grequest_start */
MPI_Request c_request;

*ierror = MPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_grequest_start__(MPI_Grequest_query_function *query_fn, MPI_Grequest_free_function *free_fn, MPI_Grequest_cancel_function *cancel_fn, void *extra_state, MPI_Request *request, int *ierror)
{
	mpi_grequest_start_(query_fn, free_fn, cancel_fn, extra_state, request, ierror);
}
int mpi_group_compare_(MPI_Fint* group1, MPI_Fint* group2, int *result, int *ierror)
{
/* MPI_Group_compare */
MPI_Group c_group1 = PMPI_Group_f2c(*group1);
MPI_Group c_group2 = PMPI_Group_f2c(*group2);

*ierror = MPI_Group_compare(c_group1, c_group2, result);


}
int mpi_group_compare__(MPI_Group* group1, MPI_Group* group2, int *result, int *ierror)
{
	mpi_group_compare_(group1, group2, result, ierror);
}
int mpi_group_difference_(MPI_Fint* group1, MPI_Fint* group2, MPI_Fint *newgroup, int *ierror)
{
/* MPI_Group_difference */
MPI_Group c_group1 = PMPI_Group_f2c(*group1);
MPI_Group c_group2 = PMPI_Group_f2c(*group2);
MPI_Group c_newgroup;

*ierror = MPI_Group_difference(c_group1, c_group2, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);


}
int mpi_group_difference__(MPI_Group* group1, MPI_Group* group2, MPI_Group *newgroup, int *ierror)
{
	mpi_group_difference_(group1, group2, newgroup, ierror);
}
int mpi_group_excl_(MPI_Fint* group, int* n, const int ranks[], MPI_Fint *newgroup, int *ierror)
{
/* MPI_Group_excl */
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Group c_newgroup;

*ierror = MPI_Group_excl(c_group, *n, ranks, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);


}
int mpi_group_excl__(MPI_Group* group, int* n, const int ranks[], MPI_Group *newgroup, int *ierror)
{
	mpi_group_excl_(group, n, ranks, newgroup, ierror);
}
int mpi_group_free_(MPI_Fint *group, int *ierror)
{
/* MPI_Group_free */
MPI_Group c_group = PMPI_Group_f2c(*group);

*ierror = MPI_Group_free(&c_group);
*group = PMPI_Group_c2f(c_group);


}
int mpi_group_free__(MPI_Group *group, int *ierror)
{
	mpi_group_free_(group, ierror);
}
int mpi_group_from_session_pset_(ERR* session, const char *pset_name CHAR_MIXED(size_pset_name), MPI_Fint *newgroup, int *ierror CHAR_END(size_pset_name))
{
/* MPI_Group_from_session_pset */
char *tmp_pset_name = NULL, *ptr_pset_name = NULL;
tmp_pset_name = char_fortran_to_c((char *)pset_name, size_pset_name, &ptr_pset_name);
MPI_Group c_newgroup;

*ierror = MPI_Group_from_session_pset(*session, tmp_pset_name, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);
sctk_free(ptr_pset_name);


}
int mpi_group_from_session_pset__(ERR* session, const char *pset_name CHAR_MIXED(size_pset_name), MPI_Group *newgroup, int *ierror CHAR_END(size_pset_name))
{
	mpi_group_from_session_pset_(session, pset_name, newgroup, ierror);
}
int mpi_group_incl_(MPI_Fint* group, int* n, const int ranks[], MPI_Fint *newgroup, int *ierror)
{
/* MPI_Group_incl */
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Group c_newgroup;

*ierror = MPI_Group_incl(c_group, *n, ranks, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);


}
int mpi_group_incl__(MPI_Group* group, int* n, const int ranks[], MPI_Group *newgroup, int *ierror)
{
	mpi_group_incl_(group, n, ranks, newgroup, ierror);
}
int mpi_group_intersection_(MPI_Fint* group1, MPI_Fint* group2, MPI_Fint *newgroup, int *ierror)
{
/* MPI_Group_intersection */
MPI_Group c_group1 = PMPI_Group_f2c(*group1);
MPI_Group c_group2 = PMPI_Group_f2c(*group2);
MPI_Group c_newgroup;

*ierror = MPI_Group_intersection(c_group1, c_group2, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);


}
int mpi_group_intersection__(MPI_Group* group1, MPI_Group* group2, MPI_Group *newgroup, int *ierror)
{
	mpi_group_intersection_(group1, group2, newgroup, ierror);
}
int mpi_group_range_excl_(MPI_Fint* group, int* n, int ranges[][3], MPI_Fint *newgroup, int *ierror)
{
/* MPI_Group_range_excl */
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Group c_newgroup;

*ierror = MPI_Group_range_excl(c_group, *n, ranges, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);


}
int mpi_group_range_excl__(MPI_Group* group, int* n, int ranges[][3], MPI_Group *newgroup, int *ierror)
{
	mpi_group_range_excl_(group, n, ranges, newgroup, ierror);
}
int mpi_group_range_incl_(MPI_Fint* group, int* n, int ranges[][3], MPI_Fint *newgroup, int *ierror)
{
/* MPI_Group_range_incl */
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Group c_newgroup;

*ierror = MPI_Group_range_incl(c_group, *n, ranges, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);


}
int mpi_group_range_incl__(MPI_Group* group, int* n, int ranges[][3], MPI_Group *newgroup, int *ierror)
{
	mpi_group_range_incl_(group, n, ranges, newgroup, ierror);
}
int mpi_group_rank_(MPI_Fint* group, int *rank, int *ierror)
{
/* MPI_Group_rank */
MPI_Group c_group = PMPI_Group_f2c(*group);

*ierror = MPI_Group_rank(c_group, rank);


}
int mpi_group_rank__(MPI_Group* group, int *rank, int *ierror)
{
	mpi_group_rank_(group, rank, ierror);
}
int mpi_group_size_(MPI_Fint* group, int *size, int *ierror)
{
/* MPI_Group_size */
MPI_Group c_group = PMPI_Group_f2c(*group);

*ierror = MPI_Group_size(c_group, size);


}
int mpi_group_size__(MPI_Group* group, int *size, int *ierror)
{
	mpi_group_size_(group, size, ierror);
}
int mpi_group_translate_ranks_(MPI_Fint* group1, int* n, const int ranks1[], MPI_Fint* group2, int ranks2[], int *ierror)
{
/* MPI_Group_translate_ranks */
MPI_Group c_group1 = PMPI_Group_f2c(*group1);
MPI_Group c_group2 = PMPI_Group_f2c(*group2);

*ierror = MPI_Group_translate_ranks(c_group1, *n, ranks1, c_group2, ranks2);


}
int mpi_group_translate_ranks__(MPI_Group* group1, int* n, const int ranks1[], MPI_Group* group2, int ranks2[], int *ierror)
{
	mpi_group_translate_ranks_(group1, n, ranks1, group2, ranks2, ierror);
}
int mpi_group_union_(MPI_Fint* group1, MPI_Fint* group2, MPI_Fint *newgroup, int *ierror)
{
/* MPI_Group_union */
MPI_Group c_group1 = PMPI_Group_f2c(*group1);
MPI_Group c_group2 = PMPI_Group_f2c(*group2);
MPI_Group c_newgroup;

*ierror = MPI_Group_union(c_group1, c_group2, &c_newgroup);
*newgroup = PMPI_Group_c2f(c_newgroup);


}
int mpi_group_union__(MPI_Group* group1, MPI_Group* group2, MPI_Group *newgroup, int *ierror)
{
	mpi_group_union_(group1, group2, newgroup, ierror);
}
int mpi_iallgather_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Iallgather */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Iallgather(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_iallgather__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_iallgather_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierror);
}
int mpi_iallgatherv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Iallgatherv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Iallgatherv(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_iallgatherv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_iallgatherv_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, request, ierror);
}
int mpi_iallreduce_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Iallreduce */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Iallreduce(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_iallreduce__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_iallreduce_(sendbuf, recvbuf, count, datatype, op, comm, request, ierror);
}
int mpi_ialltoall_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ialltoall */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ialltoall(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ialltoall__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ialltoall_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierror);
}
int mpi_ialltoallv_(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ialltoallv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ialltoallv(sendbuf, sendcounts, sdispls, c_sendtype, recvbuf, recvcounts, rdispls, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ialltoallv__(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ialltoallv_(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, request, ierror);
}
int mpi_ialltoallw_(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Fint sendtypes[], void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Fint recvtypes[], MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ialltoallw */
int alltoallwlen = 0;
PMPI_Comm_size(comm, &alltoallwlen);
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;

int incnt_sendtypes = 0;
MPI_Datatype *c_sendtypes = NULL;

c_sendtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_sendtypes = 0; incnt_sendtypes < alltoallwlen ; incnt_sendtypes++)
        c_sendtypes[incnt_sendtypes] = PMPI_Type_f2c(sendtypes[incnt_sendtypes]);
        
int incnt_recvtypes = 0;
MPI_Datatype *c_recvtypes = NULL;

c_recvtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_recvtypes = 0; incnt_recvtypes < alltoallwlen ; incnt_recvtypes++)
        c_recvtypes[incnt_recvtypes] = PMPI_Type_f2c(recvtypes[incnt_recvtypes]);
        MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ialltoallw(sendbuf, sendcounts, sdispls, c_sendtypes, recvbuf, recvcounts, rdispls, c_recvtypes, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);
sctk_free(c_sendtypes);
sctk_free(c_recvtypes);


}
int mpi_ialltoallw__(const void *sendbuf, const int sendcounts[], const int sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[], const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ialltoallw_(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, request, ierror);
}
int mpi_ibarrier_(MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ibarrier */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ibarrier(c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ibarrier__(MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ibarrier_(comm, request, ierror);
}
int mpi_ibcast_(void *buffer, int* count, MPI_Fint* datatype, int* root, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ibcast */
if( buffer_is_bottom((void *)buffer) )
	buffer = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ibcast(buffer, *count, c_datatype, *root, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ibcast__(void *buffer, int* count, MPI_Datatype* datatype, int* root, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ibcast_(buffer, count, datatype, root, comm, request, ierror);
}
int mpi_ibsend_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ibsend */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ibsend(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ibsend__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ibsend_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_iexscan_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Iexscan */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Iexscan(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_iexscan__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_iexscan_(sendbuf, recvbuf, count, datatype, op, comm, request, ierror);
}
int mpi_igather_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Igather */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Igather(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_igather__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_igather_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierror);
}
int mpi_igatherv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Igatherv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Igatherv(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, *root, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_igatherv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_igatherv_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, request, ierror);
}
int mpi_improbe_(int* source, int* tag, MPI_Fint* comm, int *flag, MPI_Message *message, MPI_Status *status, int *ierror)
{
/* MPI_Improbe */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Improbe(*source, *tag, c_comm, flag, message, status);


}
int mpi_improbe__(int* source, int* tag, MPI_Comm* comm, int *flag, MPI_Message *message, MPI_Status *status, int *ierror)
{
	mpi_improbe_(source, tag, comm, flag, message, status, ierror);
}
int mpi_imrecv_(void *buf, int* count, MPI_Fint* datatype, MPI_Message *message, MPI_Fint *request, int *ierror)
{
/* MPI_Imrecv */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Request c_request;

*ierror = MPI_Imrecv(buf, *count, c_datatype, message, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_imrecv__(void *buf, int* count, MPI_Datatype* datatype, MPI_Message *message, MPI_Request *request, int *ierror)
{
	mpi_imrecv_(buf, count, datatype, message, request, ierror);
}
int mpi_ineighbor_allgather_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ineighbor_allgather */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ineighbor_allgather(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ineighbor_allgather__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ineighbor_allgather_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierror);
}
int mpi_ineighbor_allgatherv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ineighbor_allgatherv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ineighbor_allgatherv(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ineighbor_allgatherv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ineighbor_allgatherv_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, request, ierror);
}
int mpi_ineighbor_alltoall_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ineighbor_alltoall */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ineighbor_alltoall(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ineighbor_alltoall__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ineighbor_alltoall_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierror);
}
int mpi_ineighbor_alltoallv_(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ineighbor_alltoallv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ineighbor_alltoallv(sendbuf, sendcounts, sdispls, c_sendtype, recvbuf, recvcounts, rdispls, c_recvtype, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ineighbor_alltoallv__(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ineighbor_alltoallv_(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, request, ierror);
}
int mpi_ineighbor_alltoallw_(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[], const MPI_Fint sendtypes[], void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[], const MPI_Fint recvtypes[], MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ineighbor_alltoallw */
int alltoallwlen = 0;
PMPI_Comm_size(comm, &alltoallwlen);
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;

int incnt_sendtypes = 0;
MPI_Datatype *c_sendtypes = NULL;

c_sendtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_sendtypes = 0; incnt_sendtypes < alltoallwlen ; incnt_sendtypes++)
        c_sendtypes[incnt_sendtypes] = PMPI_Type_f2c(sendtypes[incnt_sendtypes]);
        
int incnt_recvtypes = 0;
MPI_Datatype *c_recvtypes = NULL;

c_recvtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_recvtypes = 0; incnt_recvtypes < alltoallwlen ; incnt_recvtypes++)
        c_recvtypes[incnt_recvtypes] = PMPI_Type_f2c(recvtypes[incnt_recvtypes]);
        MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ineighbor_alltoallw(sendbuf, sendcounts, sdispls, c_sendtypes, recvbuf, recvcounts, rdispls, c_recvtypes, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);
sctk_free(c_sendtypes);
sctk_free(c_recvtypes);


}
int mpi_ineighbor_alltoallw__(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ineighbor_alltoallw_(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, request, ierror);
}
int mpi_info_create_(MPI_Fint *info, int *ierror)
{
/* MPI_Info_create */
MPI_Info c_info;

*ierror = MPI_Info_create(&c_info);
*info = PMPI_Info_c2f(c_info);


}
int mpi_info_create__(MPI_Info *info, int *ierror)
{
	mpi_info_create_(info, ierror);
}
int mpi_info_create_env_(int* argc, char argv[], MPI_Fint *info, int *ierror)
{
/* MPI_Info_create_env */
MPI_Info c_info;

*ierror = MPI_Info_create_env(*argc, argv, &c_info);
*info = PMPI_Info_c2f(c_info);


}
int mpi_info_create_env__(int* argc, char argv[], MPI_Info *info, int *ierror)
{
	mpi_info_create_env_(argc, argv, info, ierror);
}
int mpi_info_delete_(MPI_Fint* info, const char *key CHAR_MIXED(size_key), int *ierror CHAR_END(size_key))
{
/* MPI_Info_delete */
MPI_Info c_info = PMPI_Info_f2c(*info);
char *tmp_key = NULL, *ptr_key = NULL;
tmp_key = char_fortran_to_c((char *)key, size_key, &ptr_key);

*ierror = MPI_Info_delete(c_info, tmp_key);
sctk_free(ptr_key);


}
int mpi_info_delete__(MPI_Info* info, const char *key CHAR_MIXED(size_key), int *ierror CHAR_END(size_key))
{
	mpi_info_delete_(info, key, ierror);
}
int mpi_info_dup_(MPI_Fint* info, MPI_Fint *newinfo, int *ierror)
{
/* MPI_Info_dup */
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Info c_newinfo;

*ierror = MPI_Info_dup(c_info, &c_newinfo);
*newinfo = PMPI_Info_c2f(c_newinfo);


}
int mpi_info_dup__(MPI_Info* info, MPI_Info *newinfo, int *ierror)
{
	mpi_info_dup_(info, newinfo, ierror);
}
int mpi_info_free_(MPI_Fint *info, int *ierror)
{
/* MPI_Info_free */
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Info_free(&c_info);
*info = PMPI_Info_c2f(c_info);


}
int mpi_info_free__(MPI_Info *info, int *ierror)
{
	mpi_info_free_(info, ierror);
}
int mpi_info_get_(MPI_Fint* info, const char *key CHAR_MIXED(size_key), int* valuelen, char *value CHAR_MIXED(size_value), int *flag, int *ierror CHAR_END(size_key) CHAR_END(size_value))
{
/* MPI_Info_get */
MPI_Info c_info = PMPI_Info_f2c(*info);
char *tmp_key = NULL, *ptr_key = NULL;
tmp_key = char_fortran_to_c((char *)key, size_key, &ptr_key);

*ierror = MPI_Info_get(c_info, tmp_key, *valuelen, value, flag);
char_c_to_fortran(value,size_value);
sctk_free(ptr_key);


}
int mpi_info_get__(MPI_Info* info, const char *key CHAR_MIXED(size_key), int* valuelen, char *value CHAR_MIXED(size_value), int *flag, int *ierror CHAR_END(size_key) CHAR_END(size_value))
{
	mpi_info_get_(info, key, valuelen, value, flag, ierror);
}
int mpi_info_get_nkeys_(MPI_Fint* info, int *nkeys, int *ierror)
{
/* MPI_Info_get_nkeys */
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Info_get_nkeys(c_info, nkeys);


}
int mpi_info_get_nkeys__(MPI_Info* info, int *nkeys, int *ierror)
{
	mpi_info_get_nkeys_(info, nkeys, ierror);
}
int mpi_info_get_nthkey_(MPI_Fint* info, int* n, char *key CHAR_MIXED(size_key), int *ierror CHAR_END(size_key))
{
/* MPI_Info_get_nthkey */
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Info_get_nthkey(c_info, *n, key);
char_c_to_fortran(key,size_key);


}
int mpi_info_get_nthkey__(MPI_Info* info, int* n, char *key CHAR_MIXED(size_key), int *ierror CHAR_END(size_key))
{
	mpi_info_get_nthkey_(info, n, key, ierror);
}
int mpi_info_get_string_(MPI_Fint* info, const char *key CHAR_MIXED(size_key), int *buflen, char *value CHAR_MIXED(size_value), int *flag, int *ierror CHAR_END(size_key) CHAR_END(size_value))
{
/* MPI_Info_get_string */
MPI_Info c_info = PMPI_Info_f2c(*info);
char *tmp_key = NULL, *ptr_key = NULL;
tmp_key = char_fortran_to_c((char *)key, size_key, &ptr_key);

*ierror = MPI_Info_get_string(c_info, tmp_key, buflen, value, flag);
char_c_to_fortran(value,size_value);
sctk_free(ptr_key);


}
int mpi_info_get_string__(MPI_Info* info, const char *key CHAR_MIXED(size_key), int *buflen, char *value CHAR_MIXED(size_value), int *flag, int *ierror CHAR_END(size_key) CHAR_END(size_value))
{
	mpi_info_get_string_(info, key, buflen, value, flag, ierror);
}
int mpi_info_get_valuelen_(MPI_Fint* info, const char *key CHAR_MIXED(size_key), int *valuelen, int *flag, int *ierror CHAR_END(size_key))
{
/* MPI_Info_get_valuelen */
MPI_Info c_info = PMPI_Info_f2c(*info);
char *tmp_key = NULL, *ptr_key = NULL;
tmp_key = char_fortran_to_c((char *)key, size_key, &ptr_key);

*ierror = MPI_Info_get_valuelen(c_info, tmp_key, valuelen, flag);
sctk_free(ptr_key);


}
int mpi_info_get_valuelen__(MPI_Info* info, const char *key CHAR_MIXED(size_key), int *valuelen, int *flag, int *ierror CHAR_END(size_key))
{
	mpi_info_get_valuelen_(info, key, valuelen, flag, ierror);
}
int mpi_info_set_(MPI_Fint* info, const char *key CHAR_MIXED(size_key), const char *value CHAR_MIXED(size_value), int *ierror CHAR_END(size_key) CHAR_END(size_value))
{
/* MPI_Info_set */
MPI_Info c_info = PMPI_Info_f2c(*info);
char *tmp_key = NULL, *ptr_key = NULL;
tmp_key = char_fortran_to_c((char *)key, size_key, &ptr_key);
char *tmp_value = NULL, *ptr_value = NULL;
tmp_value = char_fortran_to_c((char *)value, size_value, &ptr_value);

*ierror = MPI_Info_set(c_info, tmp_key, tmp_value);
sctk_free(ptr_key);
sctk_free(ptr_value);


}
int mpi_info_set__(MPI_Info* info, const char *key CHAR_MIXED(size_key), const char *value CHAR_MIXED(size_value), int *ierror CHAR_END(size_key) CHAR_END(size_value))
{
	mpi_info_set_(info, key, value, ierror);
}
int mpi_init_(int *ierror)
{
int *argc = NULL;
char ***argv = NULL;
/* MPI_Init */

*ierror = MPI_Init(argc, argv);


}
int mpi_init__(int *argc, char ***argv, int *ierror)
{
	mpi_init_(argc, argv, ierror);
}
int mpi_init_thread_(int* required, int *provided, int *ierror)
{
int *argc = NULL;
char ***argv = NULL;
/* MPI_Init_thread */

*ierror = MPI_Init_thread(argc, argv, *required, provided);


}
int mpi_init_thread__(int *argc, char ***argv, int* required, int *provided, int *ierror)
{
	mpi_init_thread_(argc, argv, required, provided, ierror);
}
int mpi_initialized_(int *flag, int *ierror)
{
/* MPI_Initialized */

*ierror = MPI_Initialized(flag);


}
int mpi_initialized__(int *flag, int *ierror)
{
	mpi_initialized_(flag, ierror);
}
int mpi_intercomm_create_(MPI_Fint* local_comm, int* local_leader, MPI_Fint* peer_comm, int* remote_leader, int* tag, MPI_Fint *newintercomm, int *ierror)
{
/* MPI_Intercomm_create */
MPI_Comm c_local_comm = PMPI_Comm_f2c(*local_comm);
MPI_Comm c_peer_comm = PMPI_Comm_f2c(*peer_comm);
MPI_Comm c_newintercomm;

*ierror = MPI_Intercomm_create(c_local_comm, *local_leader, c_peer_comm, *remote_leader, *tag, &c_newintercomm);
*newintercomm = PMPI_Comm_c2f(c_newintercomm);


}
int mpi_intercomm_create__(MPI_Comm* local_comm, int* local_leader, MPI_Comm* peer_comm, int* remote_leader, int* tag, MPI_Comm *newintercomm, int *ierror)
{
	mpi_intercomm_create_(local_comm, local_leader, peer_comm, remote_leader, tag, newintercomm, ierror);
}
int mpi_intercomm_create_from_groups_(MPI_Fint* local_group, int* local_leader, MPI_Fint* remote_group, int* remote_leader, const char *stringtag CHAR_MIXED(size_stringtag), MPI_Fint* info, MPI_Fint* errhandler, MPI_Fint *newintercomm, int *ierror CHAR_END(size_stringtag))
{
/* MPI_Intercomm_create_from_groups */
MPI_Group c_local_group = PMPI_Group_f2c(*local_group);
MPI_Group c_remote_group = PMPI_Group_f2c(*remote_group);
char *tmp_stringtag = NULL, *ptr_stringtag = NULL;
tmp_stringtag = char_fortran_to_c((char *)stringtag, size_stringtag, &ptr_stringtag);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);
MPI_Comm c_newintercomm;

*ierror = MPI_Intercomm_create_from_groups(c_local_group, *local_leader, c_remote_group, *remote_leader, tmp_stringtag, c_info, c_errhandler, &c_newintercomm);
*newintercomm = PMPI_Comm_c2f(c_newintercomm);
sctk_free(ptr_stringtag);


}
int mpi_intercomm_create_from_groups__(MPI_Group* local_group, int* local_leader, MPI_Group* remote_group, int* remote_leader, const char *stringtag CHAR_MIXED(size_stringtag), MPI_Info* info, MPI_Errhandler* errhandler, MPI_Comm *newintercomm, int *ierror CHAR_END(size_stringtag))
{
	mpi_intercomm_create_from_groups_(local_group, local_leader, remote_group, remote_leader, stringtag, info, errhandler, newintercomm, ierror);
}
int mpi_intercomm_merge_(MPI_Fint* intercomm, int* high, MPI_Fint *newintracomm, int *ierror)
{
/* MPI_Intercomm_merge */
MPI_Comm c_intercomm = PMPI_Comm_f2c(*intercomm);
MPI_Comm c_newintracomm;

*ierror = MPI_Intercomm_merge(c_intercomm, *high, &c_newintracomm);
*newintracomm = PMPI_Comm_c2f(c_newintracomm);


}
int mpi_intercomm_merge__(MPI_Comm* intercomm, int* high, MPI_Comm *newintracomm, int *ierror)
{
	mpi_intercomm_merge_(intercomm, high, newintracomm, ierror);
}
int mpi_iprobe_(int* source, int* tag, MPI_Fint* comm, int *flag, MPI_Status *status, int *ierror)
{
/* MPI_Iprobe */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Iprobe(*source, *tag, c_comm, flag, status);


}
int mpi_iprobe__(int* source, int* tag, MPI_Comm* comm, int *flag, MPI_Status *status, int *ierror)
{
	mpi_iprobe_(source, tag, comm, flag, status, ierror);
}
int mpi_irecv_(void *buf, int* count, MPI_Fint* datatype, int* source, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Irecv */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Irecv(buf, *count, c_datatype, *source, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_irecv__(void *buf, int* count, MPI_Datatype* datatype, int* source, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_irecv_(buf, count, datatype, source, tag, comm, request, ierror);
}
int mpi_ireduce_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, int* root, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ireduce */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ireduce(sendbuf, recvbuf, *count, c_datatype, c_op, *root, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ireduce__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, int* root, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ireduce_(sendbuf, recvbuf, count, datatype, op, root, comm, request, ierror);
}
int mpi_ireduce_scatter_(const void *sendbuf, void *recvbuf, const int recvcounts[], MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ireduce_scatter */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ireduce_scatter(sendbuf, recvbuf, recvcounts, c_datatype, c_op, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ireduce_scatter__(const void *sendbuf, void *recvbuf, const int recvcounts[], MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ireduce_scatter_(sendbuf, recvbuf, recvcounts, datatype, op, comm, request, ierror);
}
int mpi_ireduce_scatter_block_(const void *sendbuf, void *recvbuf, int* recvcount, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ireduce_scatter_block */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ireduce_scatter_block(sendbuf, recvbuf, *recvcount, c_datatype, c_op, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ireduce_scatter_block__(const void *sendbuf, void *recvbuf, int* recvcount, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ireduce_scatter_block_(sendbuf, recvbuf, recvcount, datatype, op, comm, request, ierror);
}
int mpi_irsend_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Irsend */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Irsend(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_irsend__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_irsend_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_is_thread_main_(int *flag, int *ierror)
{
/* MPI_Is_thread_main */

*ierror = MPI_Is_thread_main(flag);


}
int mpi_is_thread_main__(int *flag, int *ierror)
{
	mpi_is_thread_main_(flag, ierror);
}
int mpi_iscan_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Iscan */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Iscan(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_iscan__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_iscan_(sendbuf, recvbuf, count, datatype, op, comm, request, ierror);
}
int mpi_iscatter_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Iscatter */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Iscatter(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_iscatter__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_iscatter_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierror);
}
int mpi_iscatterv_(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Iscatterv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Iscatterv(sendbuf, sendcounts, displs, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_iscatterv__(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_iscatterv_(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierror);
}
int mpi_isend_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Isend */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Isend(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_isend__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_isend_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_isendrecv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, int* dest, int* sendtag, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* source, int* recvtag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Isendrecv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Isendrecv(sendbuf, *sendcount, c_sendtype, *dest, *sendtag, recvbuf, *recvcount, c_recvtype, *source, *recvtag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_isendrecv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, int* dest, int* sendtag, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* source, int* recvtag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_isendrecv_(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, request, ierror);
}
int mpi_isendrecv_replace_(void *buf, int* count, MPI_Fint* datatype, int* dest, int* sendtag, int* source, int* recvtag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Isendrecv_replace */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Isendrecv_replace(buf, *count, c_datatype, *dest, *sendtag, *source, *recvtag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_isendrecv_replace__(void *buf, int* count, MPI_Datatype* datatype, int* dest, int* sendtag, int* source, int* recvtag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_isendrecv_replace_(buf, count, datatype, dest, sendtag, source, recvtag, comm, request, ierror);
}
int mpi_issend_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Issend */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Issend(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_issend__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_issend_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_keyval_create_(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn, int *keyval, void *extra_state, int *ierror)
{
/* MPI_Keyval_create */

*ierror = MPI_Keyval_create(copy_fn, delete_fn, keyval, extra_state);


}
int mpi_keyval_create__(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn, int *keyval, void *extra_state, int *ierror)
{
	mpi_keyval_create_(copy_fn, delete_fn, keyval, extra_state, ierror);
}
int mpi_keyval_free_(int *keyval, int *ierror)
{
/* MPI_Keyval_free */

*ierror = MPI_Keyval_free(keyval);


}
int mpi_keyval_free__(int *keyval, int *ierror)
{
	mpi_keyval_free_(keyval, ierror);
}
int mpi_lookup_name_(const char *service_name CHAR_MIXED(size_service_name), MPI_Fint* info, char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_service_name) CHAR_END(size_port_name))
{
/* MPI_Lookup_name */
char *tmp_service_name = NULL, *ptr_service_name = NULL;
tmp_service_name = char_fortran_to_c((char *)service_name, size_service_name, &ptr_service_name);
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Lookup_name(tmp_service_name, c_info, port_name);
char_c_to_fortran(port_name,size_port_name);
sctk_free(ptr_service_name);


}
int mpi_lookup_name__(const char *service_name CHAR_MIXED(size_service_name), MPI_Info* info, char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_service_name) CHAR_END(size_port_name))
{
	mpi_lookup_name_(service_name, info, port_name, ierror);
}
int mpi_mprobe_(int* source, int* tag, MPI_Fint* comm, MPI_Message *message, MPI_Status *status, int *ierror)
{
/* MPI_Mprobe */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Mprobe(*source, *tag, c_comm, message, status);


}
int mpi_mprobe__(int* source, int* tag, MPI_Comm* comm, MPI_Message *message, MPI_Status *status, int *ierror)
{
	mpi_mprobe_(source, tag, comm, message, status, ierror);
}
int mpi_mrecv_(void *buf, int* count, MPI_Fint* datatype, MPI_Message *message, MPI_Status *status, int *ierror)
{
/* MPI_Mrecv */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Mrecv(buf, *count, c_datatype, message, status);


}
int mpi_mrecv__(void *buf, int* count, MPI_Datatype* datatype, MPI_Message *message, MPI_Status *status, int *ierror)
{
	mpi_mrecv_(buf, count, datatype, message, status, ierror);
}
int mpi_neighbor_allgather_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Neighbor_allgather */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Neighbor_allgather(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm);


}
int mpi_neighbor_allgather__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_neighbor_allgather_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror);
}
int mpi_neighbor_allgather_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Neighbor_allgather_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Neighbor_allgather_init(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_neighbor_allgather_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_neighbor_allgather_init_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierror);
}
int mpi_neighbor_allgatherv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Neighbor_allgatherv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Neighbor_allgatherv(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, c_comm);


}
int mpi_neighbor_allgatherv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_neighbor_allgatherv_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierror);
}
int mpi_neighbor_allgatherv_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Neighbor_allgatherv_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Neighbor_allgatherv_init(sendbuf, *sendcount, c_sendtype, recvbuf, recvcounts, displs, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_neighbor_allgatherv_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int displs[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_neighbor_allgatherv_init_(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierror);
}
int mpi_neighbor_alltoall_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Neighbor_alltoall */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Neighbor_alltoall(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm);


}
int mpi_neighbor_alltoall__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_neighbor_alltoall_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror);
}
int mpi_neighbor_alltoall_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Neighbor_alltoall_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Neighbor_alltoall_init(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_neighbor_alltoall_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_neighbor_alltoall_init_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierror);
}
int mpi_neighbor_alltoallv_(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Fint* recvtype, MPI_Fint* comm, int *ierror)
{
/* MPI_Neighbor_alltoallv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Neighbor_alltoallv(sendbuf, sendcounts, sdispls, c_sendtype, recvbuf, recvcounts, rdispls, c_recvtype, c_comm);


}
int mpi_neighbor_alltoallv__(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Datatype* recvtype, MPI_Comm* comm, int *ierror)
{
	mpi_neighbor_alltoallv_(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierror);
}
int mpi_neighbor_alltoallv_init_(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Fint* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Fint* recvtype, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Neighbor_alltoallv_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Neighbor_alltoallv_init(sendbuf, sendcounts, sdispls, c_sendtype, recvbuf, recvcounts, rdispls, c_recvtype, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_neighbor_alltoallv_init__(const void *sendbuf, const int sendcounts[], const int sdispls[], MPI_Datatype* sendtype, void *recvbuf, const int recvcounts[], const int rdispls[], MPI_Datatype* recvtype, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_neighbor_alltoallv_init_(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierror);
}
int mpi_neighbor_alltoallw_(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[], const MPI_Fint sendtypes[], void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[], const MPI_Fint recvtypes[], MPI_Fint* comm, int *ierror)
{
/* MPI_Neighbor_alltoallw */
int alltoallwlen = 0;
PMPI_Comm_size(comm, &alltoallwlen);
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;

int incnt_sendtypes = 0;
MPI_Datatype *c_sendtypes = NULL;

c_sendtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_sendtypes = 0; incnt_sendtypes < alltoallwlen ; incnt_sendtypes++)
        c_sendtypes[incnt_sendtypes] = PMPI_Type_f2c(sendtypes[incnt_sendtypes]);
        
int incnt_recvtypes = 0;
MPI_Datatype *c_recvtypes = NULL;

c_recvtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_recvtypes = 0; incnt_recvtypes < alltoallwlen ; incnt_recvtypes++)
        c_recvtypes[incnt_recvtypes] = PMPI_Type_f2c(recvtypes[incnt_recvtypes]);
        MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Neighbor_alltoallw(sendbuf, sendcounts, sdispls, c_sendtypes, recvbuf, recvcounts, rdispls, c_recvtypes, c_comm);
sctk_free(c_sendtypes);
sctk_free(c_recvtypes);


}
int mpi_neighbor_alltoallw__(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm* comm, int *ierror)
{
	mpi_neighbor_alltoallw_(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierror);
}
int mpi_neighbor_alltoallw_init_(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[], const MPI_Fint sendtypes[], void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[], const MPI_Fint recvtypes[], MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Neighbor_alltoallw_init */
int alltoallwlen = 0;
PMPI_Comm_size(comm, &alltoallwlen);
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;

int incnt_sendtypes = 0;
MPI_Datatype *c_sendtypes = NULL;

c_sendtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_sendtypes = 0; incnt_sendtypes < alltoallwlen ; incnt_sendtypes++)
        c_sendtypes[incnt_sendtypes] = PMPI_Type_f2c(sendtypes[incnt_sendtypes]);
        
int incnt_recvtypes = 0;
MPI_Datatype *c_recvtypes = NULL;

c_recvtypes = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * alltoallwlen);

for(incnt_recvtypes = 0; incnt_recvtypes < alltoallwlen ; incnt_recvtypes++)
        c_recvtypes[incnt_recvtypes] = PMPI_Type_f2c(recvtypes[incnt_recvtypes]);
        MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Neighbor_alltoallw_init(sendbuf, sendcounts, sdispls, c_sendtypes, recvbuf, recvcounts, rdispls, c_recvtypes, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);
sctk_free(c_sendtypes);
sctk_free(c_recvtypes);


}
int mpi_neighbor_alltoallw_init__(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_neighbor_alltoallw_init_(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierror);
}
int mpi_op_commutative_(MPI_Fint* op, int *commute, int *ierror)
{
/* MPI_Op_commutative */
MPI_Op c_op = PMPI_Op_f2c(*op);

*ierror = MPI_Op_commutative(c_op, commute);


}
int mpi_op_commutative__(MPI_Op* op, int *commute, int *ierror)
{
	mpi_op_commutative_(op, commute, ierror);
}
int mpi_op_create_(MPI_User_function *user_fn, int* commute, MPI_Fint *op, int *ierror)
{
/* MPI_Op_create */
MPI_Op c_op;

*ierror = MPI_Op_create(user_fn, *commute, &c_op);
*op = PMPI_Op_c2f(c_op);


}
int mpi_op_create__(MPI_User_function *user_fn, int* commute, MPI_Op *op, int *ierror)
{
	mpi_op_create_(user_fn, commute, op, ierror);
}
int mpi_op_free_(MPI_Fint *op, int *ierror)
{
/* MPI_Op_free */
MPI_Op c_op = PMPI_Op_f2c(*op);

*ierror = MPI_Op_free(&c_op);
*op = PMPI_Op_c2f(c_op);


}
int mpi_op_free__(MPI_Op *op, int *ierror)
{
	mpi_op_free_(op, ierror);
}
int mpi_open_port_(MPI_Fint* info, char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_port_name))
{
/* MPI_Open_port */
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Open_port(c_info, port_name);
char_c_to_fortran(port_name,size_port_name);


}
int mpi_open_port__(MPI_Info* info, char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_port_name))
{
	mpi_open_port_(info, port_name, ierror);
}
int mpi_pack_(const void *inbuf, int* incount, MPI_Fint* datatype, void *outbuf, int* outsize, int *position, MPI_Fint* comm, int *ierror)
{
/* MPI_Pack */
if( buffer_is_bottom((void *)inbuf) )
	inbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Pack(inbuf, *incount, c_datatype, outbuf, *outsize, position, c_comm);


}
int mpi_pack__(const void *inbuf, int* incount, MPI_Datatype* datatype, void *outbuf, int* outsize, int *position, MPI_Comm* comm, int *ierror)
{
	mpi_pack_(inbuf, incount, datatype, outbuf, outsize, position, comm, ierror);
}
int mpi_pack_external_(const char datarep[] CHAR_MIXED(size_datarep), const void *inbuf, int* incount, MPI_Fint* datatype, void *outbuf, MPI_Aint* outsize, MPI_Aint *position, int *ierror CHAR_END(size_datarep))
{
/* MPI_Pack_external */
char *tmp_datarep = NULL, *ptr_datarep = NULL;
tmp_datarep = char_fortran_to_c((char *)datarep, size_datarep, &ptr_datarep);
if( buffer_is_bottom((void *)inbuf) )
	inbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Pack_external(tmp_datarep, inbuf, *incount, c_datatype, outbuf, *outsize, position);
sctk_free(ptr_datarep);


}
int mpi_pack_external__(const char datarep[] CHAR_MIXED(size_datarep), const void *inbuf, int* incount, MPI_Datatype* datatype, void *outbuf, MPI_Aint* outsize, MPI_Aint *position, int *ierror CHAR_END(size_datarep))
{
	mpi_pack_external_(datarep, inbuf, incount, datatype, outbuf, outsize, position, ierror);
}
int mpi_pack_external_size_(const char datarep[] CHAR_MIXED(size_datarep), int* incount, MPI_Fint* datatype, MPI_Aint *size, int *ierror CHAR_END(size_datarep))
{
/* MPI_Pack_external_size */
char *tmp_datarep = NULL, *ptr_datarep = NULL;
tmp_datarep = char_fortran_to_c((char *)datarep, size_datarep, &ptr_datarep);
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Pack_external_size(tmp_datarep, *incount, c_datatype, size);
sctk_free(ptr_datarep);


}
int mpi_pack_external_size__(const char datarep[] CHAR_MIXED(size_datarep), int* incount, MPI_Datatype* datatype, MPI_Aint *size, int *ierror CHAR_END(size_datarep))
{
	mpi_pack_external_size_(datarep, incount, datatype, size, ierror);
}
int mpi_pack_size_(int* incount, MPI_Fint* datatype, MPI_Fint* comm, int *size, int *ierror)
{
/* MPI_Pack_size */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Pack_size(*incount, c_datatype, c_comm, size);


}
int mpi_pack_size__(int* incount, MPI_Datatype* datatype, MPI_Comm* comm, int *size, int *ierror)
{
	mpi_pack_size_(incount, datatype, comm, size, ierror);
}
int mpi_parrived_(MPI_Fint *request, ERR* partition, int *flag, int *ierror)
{
/* MPI_Parrived */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Parrived(&c_request, *partition, flag);
*request = PMPI_Request_c2f(c_request);


}
int mpi_parrived__(MPI_Request *request, ERR* partition, int *flag, int *ierror)
{
	mpi_parrived_(request, partition, flag, ierror);
}
int mpi_pready_(ERR* partition, MPI_Fint *request, int *ierror)
{
/* MPI_Pready */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Pready(*partition, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_pready__(ERR* partition, MPI_Request *request, int *ierror)
{
	mpi_pready_(partition, request, ierror);
}
int mpi_pready_list_(int* length, ERR array_of_partitions[], MPI_Fint *request, int *ierror)
{
/* MPI_Pready_list */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Pready_list(*length, array_of_partitions, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_pready_list__(int* length, ERR array_of_partitions[], MPI_Request *request, int *ierror)
{
	mpi_pready_list_(length, array_of_partitions, request, ierror);
}
int mpi_pready_range_(ERR* partition_low, ERR* partition_high, MPI_Fint *request, int *ierror)
{
/* MPI_Pready_range */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Pready_range(*partition_low, *partition_high, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_pready_range__(ERR* partition_low, ERR* partition_high, MPI_Request *request, int *ierror)
{
	mpi_pready_range_(partition_low, partition_high, request, ierror);
}
int mpi_precv_init_(void *buf, ERR* partitions, MPI_Count* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Precv_init */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Precv_init(buf, *partitions, *count, c_datatype, *dest, *tag, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_precv_init__(void *buf, ERR* partitions, MPI_Count* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_precv_init_(buf, partitions, count, datatype, dest, tag, comm, info, request, ierror);
}
int mpi_probe_(int* source, int* tag, MPI_Fint* comm, MPI_Status *status, int *ierror)
{
/* MPI_Probe */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Probe(*source, *tag, c_comm, status);


}
int mpi_probe__(int* source, int* tag, MPI_Comm* comm, MPI_Status *status, int *ierror)
{
	mpi_probe_(source, tag, comm, status, ierror);
}
int mpi_psend_init_(void *buf, ERR* partitions, MPI_Count* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Psend_init */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Psend_init(buf, *partitions, *count, c_datatype, *dest, *tag, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_psend_init__(void *buf, ERR* partitions, MPI_Count* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_psend_init_(buf, partitions, count, datatype, dest, tag, comm, info, request, ierror);
}
int mpi_publish_name_(const char *service_name CHAR_MIXED(size_service_name), MPI_Fint* info, const char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_service_name) CHAR_END(size_port_name))
{
/* MPI_Publish_name */
char *tmp_service_name = NULL, *ptr_service_name = NULL;
tmp_service_name = char_fortran_to_c((char *)service_name, size_service_name, &ptr_service_name);
MPI_Info c_info = PMPI_Info_f2c(*info);
char *tmp_port_name = NULL, *ptr_port_name = NULL;
tmp_port_name = char_fortran_to_c((char *)port_name, size_port_name, &ptr_port_name);

*ierror = MPI_Publish_name(tmp_service_name, c_info, tmp_port_name);
sctk_free(ptr_service_name);
sctk_free(ptr_port_name);


}
int mpi_publish_name__(const char *service_name CHAR_MIXED(size_service_name), MPI_Info* info, const char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_service_name) CHAR_END(size_port_name))
{
	mpi_publish_name_(service_name, info, port_name, ierror);
}
int mpi_put_(const void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* win, int *ierror)
{
/* MPI_Put */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Put(origin_addr, *origin_count, c_origin_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_win);


}
int mpi_put__(const void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Win* win, int *ierror)
{
	mpi_put_(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierror);
}
int mpi_query_thread_(int *provided, int *ierror)
{
/* MPI_Query_thread */

*ierror = MPI_Query_thread(provided);


}
int mpi_query_thread__(int *provided, int *ierror)
{
	mpi_query_thread_(provided, ierror);
}
int mpi_raccumulate_(const void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* op, MPI_Fint* win, MPI_Fint *request, int *ierror)
{
/* MPI_Raccumulate */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Request c_request;

*ierror = MPI_Raccumulate(origin_addr, *origin_count, c_origin_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_op, c_win, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_raccumulate__(const void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Op* op, MPI_Win* win, MPI_Request *request, int *ierror)
{
	mpi_raccumulate_(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, request, ierror);
}
int mpi_recv_(void *buf, int* count, MPI_Fint* datatype, int* source, int* tag, MPI_Fint* comm, MPI_Status *status, int *ierror)
{
/* MPI_Recv */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Recv(buf, *count, c_datatype, *source, *tag, c_comm, status);


}
int mpi_recv__(void *buf, int* count, MPI_Datatype* datatype, int* source, int* tag, MPI_Comm* comm, MPI_Status *status, int *ierror)
{
	mpi_recv_(buf, count, datatype, source, tag, comm, status, ierror);
}
int mpi_recv_init_(void *buf, int* count, MPI_Fint* datatype, int* source, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Recv_init */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Recv_init(buf, *count, c_datatype, *source, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_recv_init__(void *buf, int* count, MPI_Datatype* datatype, int* source, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_recv_init_(buf, count, datatype, source, tag, comm, request, ierror);
}
int mpi_reduce_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, int* root, MPI_Fint* comm, int *ierror)
{
/* MPI_Reduce */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Reduce(sendbuf, recvbuf, *count, c_datatype, c_op, *root, c_comm);


}
int mpi_reduce__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, int* root, MPI_Comm* comm, int *ierror)
{
	mpi_reduce_(sendbuf, recvbuf, count, datatype, op, root, comm, ierror);
}
int mpi_reduce_init_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, int* root, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Reduce_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Reduce_init(sendbuf, recvbuf, *count, c_datatype, c_op, *root, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_reduce_init__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, int* root, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_reduce_init_(sendbuf, recvbuf, count, datatype, op, root, comm, info, request, ierror);
}
int mpi_reduce_local_(const void *inbuf, void *inoutbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, int *ierror)
{
/* MPI_Reduce_local */
if( buffer_is_bottom((void *)inbuf) )
	inbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)inbuf) )
	inbuf = MPI_IN_PLACE;
if( buffer_is_bottom((void *)inoutbuf) )
	inoutbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)inoutbuf) )
	inoutbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);

*ierror = MPI_Reduce_local(inbuf, inoutbuf, *count, c_datatype, c_op);


}
int mpi_reduce_local__(const void *inbuf, void *inoutbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, int *ierror)
{
	mpi_reduce_local_(inbuf, inoutbuf, count, datatype, op, ierror);
}
int mpi_reduce_scatter_(const void *sendbuf, void *recvbuf, const int recvcounts[], MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, int *ierror)
{
/* MPI_Reduce_scatter */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, c_datatype, c_op, c_comm);


}
int mpi_reduce_scatter__(const void *sendbuf, void *recvbuf, const int recvcounts[], MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, int *ierror)
{
	mpi_reduce_scatter_(sendbuf, recvbuf, recvcounts, datatype, op, comm, ierror);
}
int mpi_reduce_scatter_block_(const void *sendbuf, void *recvbuf, int* recvcount, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, int *ierror)
{
/* MPI_Reduce_scatter_block */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Reduce_scatter_block(sendbuf, recvbuf, *recvcount, c_datatype, c_op, c_comm);


}
int mpi_reduce_scatter_block__(const void *sendbuf, void *recvbuf, int* recvcount, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, int *ierror)
{
	mpi_reduce_scatter_block_(sendbuf, recvbuf, recvcount, datatype, op, comm, ierror);
}
int mpi_reduce_scatter_block_init_(const void *sendbuf, void *recvbuf, int* recvcount, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Reduce_scatter_block_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Reduce_scatter_block_init(sendbuf, recvbuf, *recvcount, c_datatype, c_op, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_reduce_scatter_block_init__(const void *sendbuf, void *recvbuf, int* recvcount, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_reduce_scatter_block_init_(sendbuf, recvbuf, recvcount, datatype, op, comm, info, request, ierror);
}
int mpi_reduce_scatter_init_(const void *sendbuf, void *recvbuf, const int recvcounts[], MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Reduce_scatter_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Reduce_scatter_init(sendbuf, recvbuf, recvcounts, c_datatype, c_op, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_reduce_scatter_init__(const void *sendbuf, void *recvbuf, const int recvcounts[], MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_reduce_scatter_init_(sendbuf, recvbuf, recvcounts, datatype, op, comm, info, request, ierror);
}
int mpi_register_datarep_(const char *datarep CHAR_MIXED(size_datarep), MPI_Datarep_conversion_function *read_conversion_fn, MPI_Datarep_conversion_function *write_conversion_fn, MPI_Datarep_extent_function *dtype_file_extent_fn, void *extra_state, int *ierror CHAR_END(size_datarep))
{
/* MPI_Register_datarep */
char *tmp_datarep = NULL, *ptr_datarep = NULL;
tmp_datarep = char_fortran_to_c((char *)datarep, size_datarep, &ptr_datarep);

*ierror = MPI_Register_datarep(tmp_datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state);
sctk_free(ptr_datarep);


}
int mpi_register_datarep__(const char *datarep CHAR_MIXED(size_datarep), MPI_Datarep_conversion_function *read_conversion_fn, MPI_Datarep_conversion_function *write_conversion_fn, MPI_Datarep_extent_function *dtype_file_extent_fn, void *extra_state, int *ierror CHAR_END(size_datarep))
{
	mpi_register_datarep_(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierror);
}
int mpi_request_free_(MPI_Fint *request, int *ierror)
{
/* MPI_Request_free */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Request_free(&c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_request_free__(MPI_Request *request, int *ierror)
{
	mpi_request_free_(request, ierror);
}
int mpi_request_get_status_(MPI_Fint* request, int *flag, MPI_Status *status, int *ierror)
{
/* MPI_Request_get_status */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Request_get_status(c_request, flag, status);


}
int mpi_request_get_status__(MPI_Request* request, int *flag, MPI_Status *status, int *ierror)
{
	mpi_request_get_status_(request, flag, status, ierror);
}
int mpi_rget_(void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* win, MPI_Fint *request, int *ierror)
{
/* MPI_Rget */
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Request c_request;

*ierror = MPI_Rget(origin_addr, *origin_count, c_origin_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_win, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_rget__(void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Win* win, MPI_Request *request, int *ierror)
{
	mpi_rget_(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, request, ierror);
}
int mpi_rget_accumulate_(const void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, void *result_addr, int* result_count, MPI_Fint* result_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* op, MPI_Fint* win, MPI_Fint *request, int *ierror)
{
/* MPI_Rget_accumulate */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_result_datatype = PMPI_Type_f2c(*result_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Request c_request;

*ierror = MPI_Rget_accumulate(origin_addr, *origin_count, c_origin_datatype, result_addr, *result_count, c_result_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_op, c_win, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_rget_accumulate__(const void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, void *result_addr, int* result_count, MPI_Datatype* result_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Op* op, MPI_Win* win, MPI_Request *request, int *ierror)
{
	mpi_rget_accumulate_(origin_addr, origin_count, origin_datatype, result_addr, result_count, result_datatype, target_rank, target_disp, target_count, target_datatype, op, win, request, ierror);
}
int mpi_rput_(const void *origin_addr, int* origin_count, MPI_Fint* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Fint* target_datatype, MPI_Fint* win, MPI_Fint *request, int *ierror)
{
/* MPI_Rput */
if( buffer_is_bottom((void *)origin_addr) )
	origin_addr = MPI_BOTTOM;
MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*origin_datatype);
MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Request c_request;

*ierror = MPI_Rput(origin_addr, *origin_count, c_origin_datatype, *target_rank, *target_disp, *target_count, c_target_datatype, c_win, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_rput__(const void *origin_addr, int* origin_count, MPI_Datatype* origin_datatype, int* target_rank, MPI_Aint* target_disp, int* target_count, MPI_Datatype* target_datatype, MPI_Win* win, MPI_Request *request, int *ierror)
{
	mpi_rput_(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, request, ierror);
}
int mpi_rsend_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, int *ierror)
{
/* MPI_Rsend */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Rsend(buf, *count, c_datatype, *dest, *tag, c_comm);


}
int mpi_rsend__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, int *ierror)
{
	mpi_rsend_(buf, count, datatype, dest, tag, comm, ierror);
}
int mpi_rsend_init_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Rsend_init */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Rsend_init(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_rsend_init__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_rsend_init_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_scan_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, int *ierror)
{
/* MPI_Scan */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Scan(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm);


}
int mpi_scan__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, int *ierror)
{
	mpi_scan_(sendbuf, recvbuf, count, datatype, op, comm, ierror);
}
int mpi_scan_init_(const void *sendbuf, void *recvbuf, int* count, MPI_Fint* datatype, MPI_Fint* op, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Scan_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Op c_op = PMPI_Op_f2c(*op);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Scan_init(sendbuf, recvbuf, *count, c_datatype, c_op, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_scan_init__(const void *sendbuf, void *recvbuf, int* count, MPI_Datatype* datatype, MPI_Op* op, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_scan_init_(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierror);
}
int mpi_scatter_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, int *ierror)
{
/* MPI_Scatter */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Scatter(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm);


}
int mpi_scatter__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, int *ierror)
{
	mpi_scatter_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror);
}
int mpi_scatter_init_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Scatter_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Scatter_init(sendbuf, *sendcount, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_scatter_init__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_scatter_init_(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierror);
}
int mpi_scatterv_(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, int *ierror)
{
/* MPI_Scatterv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Scatterv(sendbuf, sendcounts, displs, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm);


}
int mpi_scatterv__(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, int *ierror)
{
	mpi_scatterv_(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror);
}
int mpi_scatterv_init_(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Fint* sendtype, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* root, MPI_Fint* comm, MPI_Fint* info, MPI_Fint *request, int *ierror)
{
/* MPI_Scatterv_init */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
if( buffer_is_mpiinplace((void *)sendbuf) )
	sendbuf = MPI_IN_PLACE;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Request c_request;

*ierror = MPI_Scatterv_init(sendbuf, sendcounts, displs, c_sendtype, recvbuf, *recvcount, c_recvtype, *root, c_comm, c_info, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_scatterv_init__(const void *sendbuf, const int sendcounts[], const int displs[], MPI_Datatype* sendtype, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* root, MPI_Comm* comm, MPI_Info* info, MPI_Request *request, int *ierror)
{
	mpi_scatterv_init_(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierror);
}
int mpi_send_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, int *ierror)
{
/* MPI_Send */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Send(buf, *count, c_datatype, *dest, *tag, c_comm);


}
int mpi_send__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, int *ierror)
{
	mpi_send_(buf, count, datatype, dest, tag, comm, ierror);
}
int mpi_send_init_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Send_init */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Send_init(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_send_init__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_send_init_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_sendrecv_(const void *sendbuf, int* sendcount, MPI_Fint* sendtype, int* dest, int* sendtag, void *recvbuf, int* recvcount, MPI_Fint* recvtype, int* source, int* recvtag, MPI_Fint* comm, MPI_Status *status, int *ierror)
{
/* MPI_Sendrecv */
if( buffer_is_bottom((void *)sendbuf) )
	sendbuf = MPI_BOTTOM;
MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Sendrecv(sendbuf, *sendcount, c_sendtype, *dest, *sendtag, recvbuf, *recvcount, c_recvtype, *source, *recvtag, c_comm, status);


}
int mpi_sendrecv__(const void *sendbuf, int* sendcount, MPI_Datatype* sendtype, int* dest, int* sendtag, void *recvbuf, int* recvcount, MPI_Datatype* recvtype, int* source, int* recvtag, MPI_Comm* comm, MPI_Status *status, int *ierror)
{
	mpi_sendrecv_(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierror);
}
int mpi_sendrecv_replace_(void *buf, int* count, MPI_Fint* datatype, int* dest, int* sendtag, int* source, int* recvtag, MPI_Fint* comm, MPI_Status *status, int *ierror)
{
/* MPI_Sendrecv_replace */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Sendrecv_replace(buf, *count, c_datatype, *dest, *sendtag, *source, *recvtag, c_comm, status);


}
int mpi_sendrecv_replace__(void *buf, int* count, MPI_Datatype* datatype, int* dest, int* sendtag, int* source, int* recvtag, MPI_Comm* comm, MPI_Status *status, int *ierror)
{
	mpi_sendrecv_replace_(buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierror);
}
int mpi_session_call_errhandler_(ERR* session, int* errorcode, int *ierror)
{
/* MPI_Session_call_errhandler */

*ierror = MPI_Session_call_errhandler(*session, *errorcode);


}
int mpi_session_call_errhandler__(ERR* session, int* errorcode, int *ierror)
{
	mpi_session_call_errhandler_(session, errorcode, ierror);
}
int mpi_session_create_errhandler_(MPI_Session_errhandler_function *session_errhandler_fn, MPI_Fint *errhandler, int *ierror)
{
/* MPI_Session_create_errhandler */
MPI_Errhandler c_errhandler;

*ierror = MPI_Session_create_errhandler(session_errhandler_fn, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_session_create_errhandler__(MPI_Session_errhandler_function *session_errhandler_fn, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_session_create_errhandler_(session_errhandler_fn, errhandler, ierror);
}
int mpi_session_finalize_(ERR *session, int *ierror)
{
/* MPI_Session_finalize */

*ierror = MPI_Session_finalize(session);


}
int mpi_session_finalize__(ERR *session, int *ierror)
{
	mpi_session_finalize_(session, ierror);
}
int mpi_session_get_errhandler_(ERR* session, MPI_Fint *errhandler, int *ierror)
{
/* MPI_Session_get_errhandler */
MPI_Errhandler c_errhandler;

*ierror = MPI_Session_get_errhandler(*session, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_session_get_errhandler__(ERR* session, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_session_get_errhandler_(session, errhandler, ierror);
}
int mpi_session_get_info_(ERR* session, MPI_Fint *info_used, int *ierror)
{
/* MPI_Session_get_info */
MPI_Info c_info_used;

*ierror = MPI_Session_get_info(*session, &c_info_used);
*info_used = PMPI_Info_c2f(c_info_used);


}
int mpi_session_get_info__(ERR* session, MPI_Info *info_used, int *ierror)
{
	mpi_session_get_info_(session, info_used, ierror);
}
int mpi_session_get_nth_pset_(ERR* session, MPI_Fint* info, int* n, int *pset_len, char *pset_name CHAR_MIXED(size_pset_name), int *ierror CHAR_END(size_pset_name))
{
/* MPI_Session_get_nth_pset */
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Session_get_nth_pset(*session, c_info, *n, pset_len, pset_name);
char_c_to_fortran(pset_name,size_pset_name);


}
int mpi_session_get_nth_pset__(ERR* session, MPI_Info* info, int* n, int *pset_len, char *pset_name CHAR_MIXED(size_pset_name), int *ierror CHAR_END(size_pset_name))
{
	mpi_session_get_nth_pset_(session, info, n, pset_len, pset_name, ierror);
}
int mpi_session_get_num_psets_(ERR* session, MPI_Fint* info, int *npset_names, int *ierror)
{
/* MPI_Session_get_num_psets */
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Session_get_num_psets(*session, c_info, npset_names);


}
int mpi_session_get_num_psets__(ERR* session, MPI_Info* info, int *npset_names, int *ierror)
{
	mpi_session_get_num_psets_(session, info, npset_names, ierror);
}
int mpi_session_get_pset_info_(ERR* session, const char *pset_name CHAR_MIXED(size_pset_name), MPI_Fint *info, int *ierror CHAR_END(size_pset_name))
{
/* MPI_Session_get_pset_info */
char *tmp_pset_name = NULL, *ptr_pset_name = NULL;
tmp_pset_name = char_fortran_to_c((char *)pset_name, size_pset_name, &ptr_pset_name);
MPI_Info c_info;

*ierror = MPI_Session_get_pset_info(*session, tmp_pset_name, &c_info);
*info = PMPI_Info_c2f(c_info);
sctk_free(ptr_pset_name);


}
int mpi_session_get_pset_info__(ERR* session, const char *pset_name CHAR_MIXED(size_pset_name), MPI_Info *info, int *ierror CHAR_END(size_pset_name))
{
	mpi_session_get_pset_info_(session, pset_name, info, ierror);
}
int mpi_session_init_(MPI_Fint* info, MPI_Fint* errhandler, ERR *session, int *ierror)
{
/* MPI_Session_init */
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);

*ierror = MPI_Session_init(c_info, c_errhandler, session);


}
int mpi_session_init__(MPI_Info* info, MPI_Errhandler* errhandler, ERR *session, int *ierror)
{
	mpi_session_init_(info, errhandler, session, ierror);
}
int mpi_session_set_errhandler_(ERR* session, MPI_Fint* errhandler, int *ierror)
{
/* MPI_Session_set_errhandler */
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);

*ierror = MPI_Session_set_errhandler(*session, c_errhandler);


}
int mpi_session_set_errhandler__(ERR* session, MPI_Errhandler* errhandler, int *ierror)
{
	mpi_session_set_errhandler_(session, errhandler, ierror);
}
int mpi_ssend_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, int *ierror)
{
/* MPI_Ssend */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Ssend(buf, *count, c_datatype, *dest, *tag, c_comm);


}
int mpi_ssend__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, int *ierror)
{
	mpi_ssend_(buf, count, datatype, dest, tag, comm, ierror);
}
int mpi_ssend_init_(const void *buf, int* count, MPI_Fint* datatype, int* dest, int* tag, MPI_Fint* comm, MPI_Fint *request, int *ierror)
{
/* MPI_Ssend_init */
if( buffer_is_bottom((void *)buf) )
	buf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Request c_request;

*ierror = MPI_Ssend_init(buf, *count, c_datatype, *dest, *tag, c_comm, &c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_ssend_init__(const void *buf, int* count, MPI_Datatype* datatype, int* dest, int* tag, MPI_Comm* comm, MPI_Request *request, int *ierror)
{
	mpi_ssend_init_(buf, count, datatype, dest, tag, comm, request, ierror);
}
int mpi_start_(MPI_Fint *request, int *ierror)
{
/* MPI_Start */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Start(&c_request);
*request = PMPI_Request_c2f(c_request);


}
int mpi_start__(MPI_Request *request, int *ierror)
{
	mpi_start_(request, ierror);
}
int mpi_startall_(int* count, MPI_Fint array_of_requests[], int *ierror)
{
/* MPI_Startall */

int incnt_array_of_requests = 0;
MPI_Request *c_array_of_requests = NULL;

c_array_of_requests = (MPI_Request*) sctk_malloc(sizeof(MPI_Request) * *count);

for(incnt_array_of_requests = 0; incnt_array_of_requests < *count ; incnt_array_of_requests++)
        c_array_of_requests[incnt_array_of_requests] = PMPI_Request_f2c(array_of_requests[incnt_array_of_requests]);
        
*ierror = MPI_Startall(*count, c_array_of_requests);

int outcnt_array_of_requests = 0;

for(outcnt_array_of_requests = 0; outcnt_array_of_requests < *count ; outcnt_array_of_requests++)
        array_of_requests[outcnt_array_of_requests] = PMPI_Request_c2f(c_array_of_requests[outcnt_array_of_requests]);

sctk_free(c_array_of_requests);


}
int mpi_startall__(int* count, MPI_Request array_of_requests[], int *ierror)
{
	mpi_startall_(count, array_of_requests, ierror);
}
int mpi_status_set_cancelled_(MPI_Status *status, int* flag, int *ierror)
{
/* MPI_Status_set_cancelled */

*ierror = MPI_Status_set_cancelled(status, *flag);


}
int mpi_status_set_cancelled__(MPI_Status *status, int* flag, int *ierror)
{
	mpi_status_set_cancelled_(status, flag, ierror);
}
int mpi_status_set_elements_(MPI_Status *status, MPI_Fint* datatype, int* count, int *ierror)
{
/* MPI_Status_set_elements */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Status_set_elements(status, c_datatype, *count);


}
int mpi_status_set_elements__(MPI_Status *status, MPI_Datatype* datatype, int* count, int *ierror)
{
	mpi_status_set_elements_(status, datatype, count, ierror);
}
int mpi_status_set_elements_x_(MPI_Status *status, MPI_Fint* datatype, MPI_Count* count, int *ierror)
{
/* MPI_Status_set_elements_x */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Status_set_elements_x(status, c_datatype, *count);


}
int mpi_status_set_elements_x__(MPI_Status *status, MPI_Datatype* datatype, MPI_Count* count, int *ierror)
{
	mpi_status_set_elements_x_(status, datatype, count, ierror);
}
int mpi_test_(MPI_Fint *request, int *flag, MPI_Status *status, int *ierror)
{
/* MPI_Test */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Test(&c_request, flag, status);
*request = PMPI_Request_c2f(c_request);


}
int mpi_test__(MPI_Request *request, int *flag, MPI_Status *status, int *ierror)
{
	mpi_test_(request, flag, status, ierror);
}
int mpi_test_cancelled_(const MPI_Status *status, int *flag, int *ierror)
{
/* MPI_Test_cancelled */

*ierror = MPI_Test_cancelled(status, flag);


}
int mpi_test_cancelled__(const MPI_Status *status, int *flag, int *ierror)
{
	mpi_test_cancelled_(status, flag, ierror);
}
int mpi_testall_(int* count, MPI_Fint array_of_requests[], int *flag, MPI_Status array_of_statuses[], int *ierror)
{
/* MPI_Testall */

int incnt_array_of_requests = 0;
MPI_Request *c_array_of_requests = NULL;

c_array_of_requests = (MPI_Request*) sctk_malloc(sizeof(MPI_Request) * *count);

for(incnt_array_of_requests = 0; incnt_array_of_requests < *count ; incnt_array_of_requests++)
        c_array_of_requests[incnt_array_of_requests] = PMPI_Request_f2c(array_of_requests[incnt_array_of_requests]);
        
*ierror = MPI_Testall(*count, c_array_of_requests, flag, array_of_statuses);

int outcnt_array_of_requests = 0;

for(outcnt_array_of_requests = 0; outcnt_array_of_requests < *count ; outcnt_array_of_requests++)
        array_of_requests[outcnt_array_of_requests] = PMPI_Request_c2f(c_array_of_requests[outcnt_array_of_requests]);

sctk_free(c_array_of_requests);


}
int mpi_testall__(int* count, MPI_Request array_of_requests[], int *flag, MPI_Status array_of_statuses[], int *ierror)
{
	mpi_testall_(count, array_of_requests, flag, array_of_statuses, ierror);
}
int mpi_testany_(int* count, MPI_Fint array_of_requests[], int *index, int *flag, MPI_Status *status, int *ierror)
{
/* MPI_Testany */

int incnt_array_of_requests = 0;
MPI_Request *c_array_of_requests = NULL;

c_array_of_requests = (MPI_Request*) sctk_malloc(sizeof(MPI_Request) * *count);

for(incnt_array_of_requests = 0; incnt_array_of_requests < *count ; incnt_array_of_requests++)
        c_array_of_requests[incnt_array_of_requests] = PMPI_Request_f2c(array_of_requests[incnt_array_of_requests]);
        
*ierror = MPI_Testany(*count, c_array_of_requests, index, flag, status);

int outcnt_array_of_requests = 0;

for(outcnt_array_of_requests = 0; outcnt_array_of_requests < *count ; outcnt_array_of_requests++)
        array_of_requests[outcnt_array_of_requests] = PMPI_Request_c2f(c_array_of_requests[outcnt_array_of_requests]);

sctk_free(c_array_of_requests);


}
int mpi_testany__(int* count, MPI_Request array_of_requests[], int *index, int *flag, MPI_Status *status, int *ierror)
{
	mpi_testany_(count, array_of_requests, index, flag, status, ierror);
}
int mpi_testsome_(int* incount, MPI_Fint array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[], int *ierror)
{
/* MPI_Testsome */

int incnt_array_of_requests = 0;
MPI_Request *c_array_of_requests = NULL;

c_array_of_requests = (MPI_Request*) sctk_malloc(sizeof(MPI_Request) * *incount);

for(incnt_array_of_requests = 0; incnt_array_of_requests < *incount ; incnt_array_of_requests++)
        c_array_of_requests[incnt_array_of_requests] = PMPI_Request_f2c(array_of_requests[incnt_array_of_requests]);
        
*ierror = MPI_Testsome(*incount, c_array_of_requests, outcount, array_of_indices, array_of_statuses);

int outcnt_array_of_requests = 0;

for(outcnt_array_of_requests = 0; outcnt_array_of_requests < *incount ; outcnt_array_of_requests++)
        array_of_requests[outcnt_array_of_requests] = PMPI_Request_c2f(c_array_of_requests[outcnt_array_of_requests]);

sctk_free(c_array_of_requests);


}
int mpi_testsome__(int* incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[], int *ierror)
{
	mpi_testsome_(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror);
}
int mpi_topo_test_(MPI_Fint* comm, int *status, int *ierror)
{
/* MPI_Topo_test */
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Topo_test(c_comm, status);


}
int mpi_topo_test__(MPI_Comm* comm, int *status, int *ierror)
{
	mpi_topo_test_(comm, status, ierror);
}
int mpi_type_commit_(MPI_Fint *datatype, int *ierror)
{
/* MPI_Type_commit */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_commit(&c_datatype);
*datatype = PMPI_Type_c2f(c_datatype);


}
int mpi_type_commit__(MPI_Datatype *datatype, int *ierror)
{
	mpi_type_commit_(datatype, ierror);
}
int mpi_type_contiguous_(int* count, MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_contiguous */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_contiguous(*count, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_contiguous__(int* count, MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_contiguous_(count, oldtype, newtype, ierror);
}
int mpi_type_create_darray_(int* size, int* rank, int* ndims, const int array_of_gsizes[], const int array_of_distribs[], const int array_of_dargs[], const int array_of_psizes[], int* order, MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_darray */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_darray(*size, *rank, *ndims, array_of_gsizes, array_of_distribs, array_of_dargs, array_of_psizes, *order, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_darray__(int* size, int* rank, int* ndims, const int array_of_gsizes[], const int array_of_distribs[], const int array_of_dargs[], const int array_of_psizes[], int* order, MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_darray_(size, rank, ndims, array_of_gsizes, array_of_distribs, array_of_dargs, array_of_psizes, order, oldtype, newtype, ierror);
}
int mpi_type_create_f90_complex_(int* p, int* r, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_f90_complex */
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_f90_complex(*p, *r, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_f90_complex__(int* p, int* r, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_f90_complex_(p, r, newtype, ierror);
}
int mpi_type_create_f90_integer_(int* r, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_f90_integer */
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_f90_integer(*r, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_f90_integer__(int* r, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_f90_integer_(r, newtype, ierror);
}
int mpi_type_create_f90_real_(int* p, int* r, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_f90_real */
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_f90_real(*p, *r, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_f90_real__(int* p, int* r, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_f90_real_(p, r, newtype, ierror);
}
int mpi_type_create_hindexed_(int* count, const int array_of_blocklengths[], const MPI_Aint array_of_displacements[], MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_hindexed */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_hindexed(*count, array_of_blocklengths, array_of_displacements, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_hindexed__(int* count, const int array_of_blocklengths[], const MPI_Aint array_of_displacements[], MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_hindexed_(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror);
}
int mpi_type_create_hindexed_block_(int* count, int* blocklength, const MPI_Aint array_of_displacements[], MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_hindexed_block */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_hindexed_block(*count, *blocklength, array_of_displacements, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_hindexed_block__(int* count, int* blocklength, const MPI_Aint array_of_displacements[], MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_hindexed_block_(count, blocklength, array_of_displacements, oldtype, newtype, ierror);
}
int mpi_type_create_hvector_(int* count, int* blocklength, MPI_Aint* stride, MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_hvector */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_hvector(*count, *blocklength, *stride, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_hvector__(int* count, int* blocklength, MPI_Aint* stride, MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_hvector_(count, blocklength, stride, oldtype, newtype, ierror);
}
int mpi_type_create_indexed_block_(int* count, int* blocklength, const int array_of_displacements[], MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_indexed_block */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_indexed_block(*count, *blocklength, array_of_displacements, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_indexed_block__(int* count, int* blocklength, const int array_of_displacements[], MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_indexed_block_(count, blocklength, array_of_displacements, oldtype, newtype, ierror);
}
int mpi_type_create_keyval_(MPI_Type_copy_attr_function *type_copy_attr_fn, MPI_Type_delete_attr_function *type_delete_attr_fn, int *type_keyval, void *extra_state, int *ierror)
{
/* MPI_Type_create_keyval */

*ierror = MPI_Type_create_keyval(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state);


}
int mpi_type_create_keyval__(MPI_Type_copy_attr_function *type_copy_attr_fn, MPI_Type_delete_attr_function *type_delete_attr_fn, int *type_keyval, void *extra_state, int *ierror)
{
	mpi_type_create_keyval_(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierror);
}
int mpi_type_create_resized_(MPI_Fint* oldtype, MPI_Aint* lb, MPI_Aint* extent, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_resized */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_resized(c_oldtype, *lb, *extent, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_resized__(MPI_Datatype* oldtype, MPI_Aint* lb, MPI_Aint* extent, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_resized_(oldtype, lb, extent, newtype, ierror);
}
int mpi_type_create_struct_(int* count, const int array_of_blocklengths[], const MPI_Aint array_of_displacements[], const MPI_Fint array_of_types[], MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_struct */

int incnt_array_of_types = 0;
MPI_Datatype *c_array_of_types = NULL;

c_array_of_types = (MPI_Datatype*) sctk_malloc(sizeof(MPI_Datatype) * *count);

for(incnt_array_of_types = 0; incnt_array_of_types < *count ; incnt_array_of_types++)
        c_array_of_types[incnt_array_of_types] = PMPI_Type_f2c(array_of_types[incnt_array_of_types]);
        MPI_Datatype c_newtype;

*ierror = MPI_Type_create_struct(*count, array_of_blocklengths, array_of_displacements, c_array_of_types, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);
sctk_free(c_array_of_types);


}
int mpi_type_create_struct__(int* count, const int array_of_blocklengths[], const MPI_Aint array_of_displacements[], const MPI_Datatype array_of_types[], MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_struct_(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierror);
}
int mpi_type_create_subarray_(int* ndims, const int array_of_sizes[], const int array_of_subsizes[], const int array_of_starts[], int* order, MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_create_subarray */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_create_subarray(*ndims, array_of_sizes, array_of_subsizes, array_of_starts, *order, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_create_subarray__(int* ndims, const int array_of_sizes[], const int array_of_subsizes[], const int array_of_starts[], int* order, MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_create_subarray_(ndims, array_of_sizes, array_of_subsizes, array_of_starts, order, oldtype, newtype, ierror);
}
int mpi_type_delete_attr_(MPI_Fint* datatype, int* type_keyval, int *ierror)
{
/* MPI_Type_delete_attr */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_delete_attr(c_datatype, *type_keyval);


}
int mpi_type_delete_attr__(MPI_Datatype* datatype, int* type_keyval, int *ierror)
{
	mpi_type_delete_attr_(datatype, type_keyval, ierror);
}
int mpi_type_dup_(MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_dup */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_dup(c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_dup__(MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_dup_(oldtype, newtype, ierror);
}
int mpi_type_free_(MPI_Fint *datatype, int *ierror)
{
/* MPI_Type_free */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_free(&c_datatype);
*datatype = PMPI_Type_c2f(c_datatype);


}
int mpi_type_free__(MPI_Datatype *datatype, int *ierror)
{
	mpi_type_free_(datatype, ierror);
}
int mpi_type_free_keyval_(int *type_keyval, int *ierror)
{
/* MPI_Type_free_keyval */

*ierror = MPI_Type_free_keyval(type_keyval);


}
int mpi_type_free_keyval__(int *type_keyval, int *ierror)
{
	mpi_type_free_keyval_(type_keyval, ierror);
}
int mpi_type_get_attr_(MPI_Fint* datatype, int* type_keyval, void *attribute_val, int *flag, int *ierror)
{
/* MPI_Type_get_attr */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_get_attr(c_datatype, *type_keyval, attribute_val, flag);


}
int mpi_type_get_attr__(MPI_Datatype* datatype, int* type_keyval, void *attribute_val, int *flag, int *ierror)
{
	mpi_type_get_attr_(datatype, type_keyval, attribute_val, flag, ierror);
}
int mpi_type_get_contents_(MPI_Fint* datatype, int* max_integers, int* max_addresses, int* max_datatypes, int array_of_integers[], MPI_Aint array_of_addresses[], MPI_Fint array_of_datatypes[], int *ierror)
{
/* MPI_Type_get_contents */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Datatype * c_array_of_datatypes = (MPI_Datatype*)sctk_malloc(sizeof(MPI_Datatype) * *max_datatypes);

*ierror = MPI_Type_get_contents(c_datatype, *max_integers, *max_addresses, *max_datatypes, array_of_integers, array_of_addresses, c_array_of_datatypes);

int outcnt_array_of_datatypes = 0;

for(outcnt_array_of_datatypes = 0; outcnt_array_of_datatypes < *max_datatypes ; outcnt_array_of_datatypes++)
        array_of_datatypes[outcnt_array_of_datatypes] = PMPI_Type_c2f(c_array_of_datatypes[outcnt_array_of_datatypes]);

sctk_free(c_array_of_datatypes);


}
int mpi_type_get_contents__(MPI_Datatype* datatype, int* max_integers, int* max_addresses, int* max_datatypes, int array_of_integers[], MPI_Aint array_of_addresses[], MPI_Datatype array_of_datatypes[], int *ierror)
{
	mpi_type_get_contents_(datatype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierror);
}
int mpi_type_get_envelope_(MPI_Fint* datatype, int *num_integers, int *num_addresses, int *num_datatypes, int *combiner, int *ierror)
{
/* MPI_Type_get_envelope */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_get_envelope(c_datatype, num_integers, num_addresses, num_datatypes, combiner);


}
int mpi_type_get_envelope__(MPI_Datatype* datatype, int *num_integers, int *num_addresses, int *num_datatypes, int *combiner, int *ierror)
{
	mpi_type_get_envelope_(datatype, num_integers, num_addresses, num_datatypes, combiner, ierror);
}
int mpi_type_get_extent_(MPI_Fint* datatype, MPI_Aint *lb, MPI_Aint *extent, int *ierror)
{
/* MPI_Type_get_extent */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_get_extent(c_datatype, lb, extent);


}
int mpi_type_get_extent__(MPI_Datatype* datatype, MPI_Aint *lb, MPI_Aint *extent, int *ierror)
{
	mpi_type_get_extent_(datatype, lb, extent, ierror);
}
int mpi_type_get_extent_x_(MPI_Fint* datatype, MPI_Count *lb, MPI_Count *extent, int *ierror)
{
/* MPI_Type_get_extent_x */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_get_extent_x(c_datatype, lb, extent);


}
int mpi_type_get_extent_x__(MPI_Datatype* datatype, MPI_Count *lb, MPI_Count *extent, int *ierror)
{
	mpi_type_get_extent_x_(datatype, lb, extent, ierror);
}
int mpi_type_get_name_(MPI_Fint* datatype, char *type_name CHAR_MIXED(size_type_name), int *resultlen, int *ierror CHAR_END(size_type_name))
{
/* MPI_Type_get_name */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_get_name(c_datatype, type_name, resultlen);
char_c_to_fortran(type_name,size_type_name);


}
int mpi_type_get_name__(MPI_Datatype* datatype, char *type_name CHAR_MIXED(size_type_name), int *resultlen, int *ierror CHAR_END(size_type_name))
{
	mpi_type_get_name_(datatype, type_name, resultlen, ierror);
}
int mpi_type_get_true_extent_(MPI_Fint* datatype, MPI_Aint *true_lb, MPI_Aint *true_extent, int *ierror)
{
/* MPI_Type_get_true_extent */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_get_true_extent(c_datatype, true_lb, true_extent);


}
int mpi_type_get_true_extent__(MPI_Datatype* datatype, MPI_Aint *true_lb, MPI_Aint *true_extent, int *ierror)
{
	mpi_type_get_true_extent_(datatype, true_lb, true_extent, ierror);
}
int mpi_type_get_true_extent_x_(MPI_Fint* datatype, MPI_Count *true_lb, MPI_Count *true_extent, int *ierror)
{
/* MPI_Type_get_true_extent_x */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_get_true_extent_x(c_datatype, true_lb, true_extent);


}
int mpi_type_get_true_extent_x__(MPI_Datatype* datatype, MPI_Count *true_lb, MPI_Count *true_extent, int *ierror)
{
	mpi_type_get_true_extent_x_(datatype, true_lb, true_extent, ierror);
}
int mpi_type_indexed_(int* count, const int array_of_blocklengths[], const int array_of_displacements[], MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_indexed */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_indexed(*count, array_of_blocklengths, array_of_displacements, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_indexed__(int* count, const int array_of_blocklengths[], const int array_of_displacements[], MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_indexed_(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror);
}
int mpi_type_match_size_(int* typeclass, int* size, MPI_Fint *datatype, int *ierror)
{
/* MPI_Type_match_size */
MPI_Datatype c_datatype;

*ierror = MPI_Type_match_size(*typeclass, *size, &c_datatype);
*datatype = PMPI_Type_c2f(c_datatype);


}
int mpi_type_match_size__(int* typeclass, int* size, MPI_Datatype *datatype, int *ierror)
{
	mpi_type_match_size_(typeclass, size, datatype, ierror);
}
int mpi_type_set_attr_(MPI_Fint* datatype, int* type_keyval, void *attribute_val, int *ierror)
{
/* MPI_Type_set_attr */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_set_attr(c_datatype, *type_keyval, attribute_val);


}
int mpi_type_set_attr__(MPI_Datatype* datatype, int* type_keyval, void *attribute_val, int *ierror)
{
	mpi_type_set_attr_(datatype, type_keyval, attribute_val, ierror);
}
int mpi_type_set_name_(MPI_Fint* datatype, const char *type_name CHAR_MIXED(size_type_name), int *ierror CHAR_END(size_type_name))
{
/* MPI_Type_set_name */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
char *tmp_type_name = NULL, *ptr_type_name = NULL;
tmp_type_name = char_fortran_to_c((char *)type_name, size_type_name, &ptr_type_name);

*ierror = MPI_Type_set_name(c_datatype, tmp_type_name);
sctk_free(ptr_type_name);


}
int mpi_type_set_name__(MPI_Datatype* datatype, const char *type_name CHAR_MIXED(size_type_name), int *ierror CHAR_END(size_type_name))
{
	mpi_type_set_name_(datatype, type_name, ierror);
}
int mpi_type_size_(MPI_Fint* datatype, int *size, int *ierror)
{
/* MPI_Type_size */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_size(c_datatype, size);


}
int mpi_type_size__(MPI_Datatype* datatype, int *size, int *ierror)
{
	mpi_type_size_(datatype, size, ierror);
}
int mpi_type_size_x_(MPI_Fint* datatype, MPI_Count *size, int *ierror)
{
/* MPI_Type_size_x */
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Type_size_x(c_datatype, size);


}
int mpi_type_size_x__(MPI_Datatype* datatype, MPI_Count *size, int *ierror)
{
	mpi_type_size_x_(datatype, size, ierror);
}
int mpi_type_vector_(int* count, int* blocklength, int* stride, MPI_Fint* oldtype, MPI_Fint *newtype, int *ierror)
{
/* MPI_Type_vector */
MPI_Datatype c_oldtype = PMPI_Type_f2c(*oldtype);
MPI_Datatype c_newtype;

*ierror = MPI_Type_vector(*count, *blocklength, *stride, c_oldtype, &c_newtype);
*newtype = PMPI_Type_c2f(c_newtype);


}
int mpi_type_vector__(int* count, int* blocklength, int* stride, MPI_Datatype* oldtype, MPI_Datatype *newtype, int *ierror)
{
	mpi_type_vector_(count, blocklength, stride, oldtype, newtype, ierror);
}
int mpi_unpack_(const void *inbuf, int* insize, int *position, void *outbuf, int* outcount, MPI_Fint* datatype, MPI_Fint* comm, int *ierror)
{
/* MPI_Unpack */
if( buffer_is_bottom((void *)inbuf) )
	inbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

*ierror = MPI_Unpack(inbuf, *insize, position, outbuf, *outcount, c_datatype, c_comm);


}
int mpi_unpack__(const void *inbuf, int* insize, int *position, void *outbuf, int* outcount, MPI_Datatype* datatype, MPI_Comm* comm, int *ierror)
{
	mpi_unpack_(inbuf, insize, position, outbuf, outcount, datatype, comm, ierror);
}
int mpi_unpack_external_(const char datarep[] CHAR_MIXED(size_datarep), const void *inbuf, MPI_Aint* insize, MPI_Aint *position, void *outbuf, int* outcount, MPI_Fint* datatype, int *ierror CHAR_END(size_datarep))
{
/* MPI_Unpack_external */
char *tmp_datarep = NULL, *ptr_datarep = NULL;
tmp_datarep = char_fortran_to_c((char *)datarep, size_datarep, &ptr_datarep);
if( buffer_is_bottom((void *)inbuf) )
	inbuf = MPI_BOTTOM;
MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);

*ierror = MPI_Unpack_external(tmp_datarep, inbuf, *insize, position, outbuf, *outcount, c_datatype);
sctk_free(ptr_datarep);


}
int mpi_unpack_external__(const char datarep[] CHAR_MIXED(size_datarep), const void *inbuf, MPI_Aint* insize, MPI_Aint *position, void *outbuf, int* outcount, MPI_Datatype* datatype, int *ierror CHAR_END(size_datarep))
{
	mpi_unpack_external_(datarep, inbuf, insize, position, outbuf, outcount, datatype, ierror);
}
int mpi_unpublish_name_(const char *service_name CHAR_MIXED(size_service_name), MPI_Fint* info, const char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_service_name) CHAR_END(size_port_name))
{
/* MPI_Unpublish_name */
char *tmp_service_name = NULL, *ptr_service_name = NULL;
tmp_service_name = char_fortran_to_c((char *)service_name, size_service_name, &ptr_service_name);
MPI_Info c_info = PMPI_Info_f2c(*info);
char *tmp_port_name = NULL, *ptr_port_name = NULL;
tmp_port_name = char_fortran_to_c((char *)port_name, size_port_name, &ptr_port_name);

*ierror = MPI_Unpublish_name(tmp_service_name, c_info, tmp_port_name);
sctk_free(ptr_service_name);
sctk_free(ptr_port_name);


}
int mpi_unpublish_name__(const char *service_name CHAR_MIXED(size_service_name), MPI_Info* info, const char *port_name CHAR_MIXED(size_port_name), int *ierror CHAR_END(size_service_name) CHAR_END(size_port_name))
{
	mpi_unpublish_name_(service_name, info, port_name, ierror);
}
int mpi_wait_(MPI_Fint *request, MPI_Status *status, int *ierror)
{
/* MPI_Wait */
MPI_Request c_request = PMPI_Request_f2c(*request);

*ierror = MPI_Wait(&c_request, status);
*request = PMPI_Request_c2f(c_request);


}
int mpi_wait__(MPI_Request *request, MPI_Status *status, int *ierror)
{
	mpi_wait_(request, status, ierror);
}
int mpi_waitall_(int* count, MPI_Fint array_of_requests[], MPI_Status array_of_statuses[], int *ierror)
{
/* MPI_Waitall */

int incnt_array_of_requests = 0;
MPI_Request *c_array_of_requests = NULL;

c_array_of_requests = (MPI_Request*) sctk_malloc(sizeof(MPI_Request) * *count);

for(incnt_array_of_requests = 0; incnt_array_of_requests < *count ; incnt_array_of_requests++)
        c_array_of_requests[incnt_array_of_requests] = PMPI_Request_f2c(array_of_requests[incnt_array_of_requests]);
        
*ierror = MPI_Waitall(*count, c_array_of_requests, array_of_statuses);

int outcnt_array_of_requests = 0;

for(outcnt_array_of_requests = 0; outcnt_array_of_requests < *count ; outcnt_array_of_requests++)
        array_of_requests[outcnt_array_of_requests] = PMPI_Request_c2f(c_array_of_requests[outcnt_array_of_requests]);

sctk_free(c_array_of_requests);


}
int mpi_waitall__(int* count, MPI_Request array_of_requests[], MPI_Status array_of_statuses[], int *ierror)
{
	mpi_waitall_(count, array_of_requests, array_of_statuses, ierror);
}
int mpi_waitany_(int* count, MPI_Fint array_of_requests[], int *index, MPI_Status *status, int *ierror)
{
/* MPI_Waitany */

int incnt_array_of_requests = 0;
MPI_Request *c_array_of_requests = NULL;

c_array_of_requests = (MPI_Request*) sctk_malloc(sizeof(MPI_Request) * *count);

for(incnt_array_of_requests = 0; incnt_array_of_requests < *count ; incnt_array_of_requests++)
        c_array_of_requests[incnt_array_of_requests] = PMPI_Request_f2c(array_of_requests[incnt_array_of_requests]);
        
*ierror = MPI_Waitany(*count, c_array_of_requests, index, status);

int outcnt_array_of_requests = 0;

for(outcnt_array_of_requests = 0; outcnt_array_of_requests < *count ; outcnt_array_of_requests++)
        array_of_requests[outcnt_array_of_requests] = PMPI_Request_c2f(c_array_of_requests[outcnt_array_of_requests]);

sctk_free(c_array_of_requests);


}
int mpi_waitany__(int* count, MPI_Request array_of_requests[], int *index, MPI_Status *status, int *ierror)
{
	mpi_waitany_(count, array_of_requests, index, status, ierror);
}
int mpi_waitsome_(int* incount, MPI_Fint array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[], int *ierror)
{
/* MPI_Waitsome */

int incnt_array_of_requests = 0;
MPI_Request *c_array_of_requests = NULL;

c_array_of_requests = (MPI_Request*) sctk_malloc(sizeof(MPI_Request) * *incount);

for(incnt_array_of_requests = 0; incnt_array_of_requests < *incount ; incnt_array_of_requests++)
        c_array_of_requests[incnt_array_of_requests] = PMPI_Request_f2c(array_of_requests[incnt_array_of_requests]);
        
*ierror = MPI_Waitsome(*incount, c_array_of_requests, outcount, array_of_indices, array_of_statuses);

int outcnt_array_of_requests = 0;

for(outcnt_array_of_requests = 0; outcnt_array_of_requests < *incount ; outcnt_array_of_requests++)
        array_of_requests[outcnt_array_of_requests] = PMPI_Request_c2f(c_array_of_requests[outcnt_array_of_requests]);

sctk_free(c_array_of_requests);


}
int mpi_waitsome__(int* incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[], int *ierror)
{
	mpi_waitsome_(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror);
}
int mpi_win_allocate_(MPI_Aint* size, int* disp_unit, MPI_Fint* info, MPI_Fint* comm, void *baseptr, MPI_Fint *win, int *ierror)
{
/* MPI_Win_allocate */
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Win c_win;

*ierror = MPI_Win_allocate(*size, *disp_unit, c_info, c_comm, baseptr, &c_win);
*win = PMPI_Win_c2f(c_win);


}
int mpi_win_allocate__(MPI_Aint* size, int* disp_unit, MPI_Info* info, MPI_Comm* comm, void *baseptr, MPI_Win *win, int *ierror)
{
	mpi_win_allocate_(size, disp_unit, info, comm, baseptr, win, ierror);
}
int mpi_win_allocate_shared_(MPI_Aint* size, int* disp_unit, MPI_Fint* info, MPI_Fint* comm, void *baseptr, MPI_Fint *win, int *ierror)
{
/* MPI_Win_allocate_shared */
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Win c_win;

*ierror = MPI_Win_allocate_shared(*size, *disp_unit, c_info, c_comm, baseptr, &c_win);
*win = PMPI_Win_c2f(c_win);


}
int mpi_win_allocate_shared__(MPI_Aint* size, int* disp_unit, MPI_Info* info, MPI_Comm* comm, void *baseptr, MPI_Win *win, int *ierror)
{
	mpi_win_allocate_shared_(size, disp_unit, info, comm, baseptr, win, ierror);
}
int mpi_win_attach_(MPI_Fint* win, void *base, MPI_Aint* size, int *ierror)
{
/* MPI_Win_attach */
MPI_Win c_win = PMPI_Win_f2c(*win);
if( buffer_is_bottom((void *)base) )
	base = MPI_BOTTOM;

*ierror = MPI_Win_attach(c_win, base, *size);


}
int mpi_win_attach__(MPI_Win* win, void *base, MPI_Aint* size, int *ierror)
{
	mpi_win_attach_(win, base, size, ierror);
}
int mpi_win_call_errhandler_(MPI_Fint* win, int* errorcode, int *ierror)
{
/* MPI_Win_call_errhandler */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_call_errhandler(c_win, *errorcode);


}
int mpi_win_call_errhandler__(MPI_Win* win, int* errorcode, int *ierror)
{
	mpi_win_call_errhandler_(win, errorcode, ierror);
}
int mpi_win_complete_(MPI_Fint* win, int *ierror)
{
/* MPI_Win_complete */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_complete(c_win);


}
int mpi_win_complete__(MPI_Win* win, int *ierror)
{
	mpi_win_complete_(win, ierror);
}
int mpi_win_create_(void *base, MPI_Aint* size, int* disp_unit, MPI_Fint* info, MPI_Fint* comm, MPI_Fint *win, int *ierror)
{
/* MPI_Win_create */
if( buffer_is_bottom((void *)base) )
	base = MPI_BOTTOM;
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Win c_win;

*ierror = MPI_Win_create(base, *size, *disp_unit, c_info, c_comm, &c_win);
*win = PMPI_Win_c2f(c_win);


}
int mpi_win_create__(void *base, MPI_Aint* size, int* disp_unit, MPI_Info* info, MPI_Comm* comm, MPI_Win *win, int *ierror)
{
	mpi_win_create_(base, size, disp_unit, info, comm, win, ierror);
}
int mpi_win_create_dynamic_(MPI_Fint* info, MPI_Fint* comm, MPI_Fint *win, int *ierror)
{
/* MPI_Win_create_dynamic */
MPI_Info c_info = PMPI_Info_f2c(*info);
MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
MPI_Win c_win;

*ierror = MPI_Win_create_dynamic(c_info, c_comm, &c_win);
*win = PMPI_Win_c2f(c_win);


}
int mpi_win_create_dynamic__(MPI_Info* info, MPI_Comm* comm, MPI_Win *win, int *ierror)
{
	mpi_win_create_dynamic_(info, comm, win, ierror);
}
int mpi_win_create_errhandler_(MPI_Win_errhandler_function *win_errhandler_fn, MPI_Fint *errhandler, int *ierror)
{
/* MPI_Win_create_errhandler */
MPI_Errhandler c_errhandler;

*ierror = MPI_Win_create_errhandler(win_errhandler_fn, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_win_create_errhandler__(MPI_Win_errhandler_function *win_errhandler_fn, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_win_create_errhandler_(win_errhandler_fn, errhandler, ierror);
}
int mpi_win_create_keyval_(MPI_Win_copy_attr_function *win_copy_attr_fn, MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval, void *extra_state, int *ierror)
{
/* MPI_Win_create_keyval */

*ierror = MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state);


}
int mpi_win_create_keyval__(MPI_Win_copy_attr_function *win_copy_attr_fn, MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval, void *extra_state, int *ierror)
{
	mpi_win_create_keyval_(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierror);
}
int mpi_win_delete_attr_(MPI_Fint* win, int* win_keyval, int *ierror)
{
/* MPI_Win_delete_attr */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_delete_attr(c_win, *win_keyval);


}
int mpi_win_delete_attr__(MPI_Win* win, int* win_keyval, int *ierror)
{
	mpi_win_delete_attr_(win, win_keyval, ierror);
}
int mpi_win_detach_(MPI_Fint* win, const void *base, int *ierror)
{
/* MPI_Win_detach */
MPI_Win c_win = PMPI_Win_f2c(*win);
if( buffer_is_bottom((void *)base) )
	base = MPI_BOTTOM;

*ierror = MPI_Win_detach(c_win, base);


}
int mpi_win_detach__(MPI_Win* win, const void *base, int *ierror)
{
	mpi_win_detach_(win, base, ierror);
}
int mpi_win_fence_(int* assert, MPI_Fint* win, int *ierror)
{
/* MPI_Win_fence */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_fence(*assert, c_win);


}
int mpi_win_fence__(int* assert, MPI_Win* win, int *ierror)
{
	mpi_win_fence_(assert, win, ierror);
}
int mpi_win_flush_(int* rank, MPI_Fint* win, int *ierror)
{
/* MPI_Win_flush */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_flush(*rank, c_win);


}
int mpi_win_flush__(int* rank, MPI_Win* win, int *ierror)
{
	mpi_win_flush_(rank, win, ierror);
}
int mpi_win_flush_all_(MPI_Fint* win, int *ierror)
{
/* MPI_Win_flush_all */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_flush_all(c_win);


}
int mpi_win_flush_all__(MPI_Win* win, int *ierror)
{
	mpi_win_flush_all_(win, ierror);
}
int mpi_win_flush_local_(int* rank, MPI_Fint* win, int *ierror)
{
/* MPI_Win_flush_local */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_flush_local(*rank, c_win);


}
int mpi_win_flush_local__(int* rank, MPI_Win* win, int *ierror)
{
	mpi_win_flush_local_(rank, win, ierror);
}
int mpi_win_flush_local_all_(MPI_Fint* win, int *ierror)
{
/* MPI_Win_flush_local_all */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_flush_local_all(c_win);


}
int mpi_win_flush_local_all__(MPI_Win* win, int *ierror)
{
	mpi_win_flush_local_all_(win, ierror);
}
int mpi_win_free_(MPI_Fint *win, int *ierror)
{
/* MPI_Win_free */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_free(&c_win);
*win = PMPI_Win_c2f(c_win);


}
int mpi_win_free__(MPI_Win *win, int *ierror)
{
	mpi_win_free_(win, ierror);
}
int mpi_win_free_keyval_(int *win_keyval, int *ierror)
{
/* MPI_Win_free_keyval */

*ierror = MPI_Win_free_keyval(win_keyval);


}
int mpi_win_free_keyval__(int *win_keyval, int *ierror)
{
	mpi_win_free_keyval_(win_keyval, ierror);
}
int mpi_win_get_attr_(MPI_Fint* win, int* win_keyval, void *attribute_val, int *flag, int *ierror)
{
/* MPI_Win_get_attr */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_get_attr(c_win, *win_keyval, attribute_val, flag);


}
int mpi_win_get_attr__(MPI_Win* win, int* win_keyval, void *attribute_val, int *flag, int *ierror)
{
	mpi_win_get_attr_(win, win_keyval, attribute_val, flag, ierror);
}
int mpi_win_get_errhandler_(MPI_Fint* win, MPI_Fint *errhandler, int *ierror)
{
/* MPI_Win_get_errhandler */
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Errhandler c_errhandler;

*ierror = MPI_Win_get_errhandler(c_win, &c_errhandler);
*errhandler = PMPI_Errhandler_c2f(c_errhandler);


}
int mpi_win_get_errhandler__(MPI_Win* win, MPI_Errhandler *errhandler, int *ierror)
{
	mpi_win_get_errhandler_(win, errhandler, ierror);
}
int mpi_win_get_group_(MPI_Fint* win, MPI_Fint *group, int *ierror)
{
/* MPI_Win_get_group */
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Group c_group;

*ierror = MPI_Win_get_group(c_win, &c_group);
*group = PMPI_Group_c2f(c_group);


}
int mpi_win_get_group__(MPI_Win* win, MPI_Group *group, int *ierror)
{
	mpi_win_get_group_(win, group, ierror);
}
int mpi_win_get_info_(MPI_Fint* win, MPI_Fint *info_used, int *ierror)
{
/* MPI_Win_get_info */
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Info c_info_used;

*ierror = MPI_Win_get_info(c_win, &c_info_used);
*info_used = PMPI_Info_c2f(c_info_used);


}
int mpi_win_get_info__(MPI_Win* win, MPI_Info *info_used, int *ierror)
{
	mpi_win_get_info_(win, info_used, ierror);
}
int mpi_win_get_name_(MPI_Fint* win, char *win_name CHAR_MIXED(size_win_name), int *resultlen, int *ierror CHAR_END(size_win_name))
{
/* MPI_Win_get_name */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_get_name(c_win, win_name, resultlen);
char_c_to_fortran(win_name,size_win_name);


}
int mpi_win_get_name__(MPI_Win* win, char *win_name CHAR_MIXED(size_win_name), int *resultlen, int *ierror CHAR_END(size_win_name))
{
	mpi_win_get_name_(win, win_name, resultlen, ierror);
}
int mpi_win_lock_(int* lock_type, int* rank, int* assert, MPI_Fint* win, int *ierror)
{
/* MPI_Win_lock */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_lock(*lock_type, *rank, *assert, c_win);


}
int mpi_win_lock__(int* lock_type, int* rank, int* assert, MPI_Win* win, int *ierror)
{
	mpi_win_lock_(lock_type, rank, assert, win, ierror);
}
int mpi_win_lock_all_(int* assert, MPI_Fint* win, int *ierror)
{
/* MPI_Win_lock_all */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_lock_all(*assert, c_win);


}
int mpi_win_lock_all__(int* assert, MPI_Win* win, int *ierror)
{
	mpi_win_lock_all_(assert, win, ierror);
}
int mpi_win_post_(MPI_Fint* group, int* assert, MPI_Fint* win, int *ierror)
{
/* MPI_Win_post */
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_post(c_group, *assert, c_win);


}
int mpi_win_post__(MPI_Group* group, int* assert, MPI_Win* win, int *ierror)
{
	mpi_win_post_(group, assert, win, ierror);
}
int mpi_win_set_attr_(MPI_Fint* win, int* win_keyval, void *attribute_val, int *ierror)
{
/* MPI_Win_set_attr */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_set_attr(c_win, *win_keyval, attribute_val);


}
int mpi_win_set_attr__(MPI_Win* win, int* win_keyval, void *attribute_val, int *ierror)
{
	mpi_win_set_attr_(win, win_keyval, attribute_val, ierror);
}
int mpi_win_set_errhandler_(MPI_Fint* win, MPI_Fint* errhandler, int *ierror)
{
/* MPI_Win_set_errhandler */
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Errhandler c_errhandler = PMPI_Errhandler_f2c(*errhandler);

*ierror = MPI_Win_set_errhandler(c_win, c_errhandler);


}
int mpi_win_set_errhandler__(MPI_Win* win, MPI_Errhandler* errhandler, int *ierror)
{
	mpi_win_set_errhandler_(win, errhandler, ierror);
}
int mpi_win_set_info_(MPI_Fint* win, MPI_Fint* info, int *ierror)
{
/* MPI_Win_set_info */
MPI_Win c_win = PMPI_Win_f2c(*win);
MPI_Info c_info = PMPI_Info_f2c(*info);

*ierror = MPI_Win_set_info(c_win, c_info);


}
int mpi_win_set_info__(MPI_Win* win, MPI_Info* info, int *ierror)
{
	mpi_win_set_info_(win, info, ierror);
}
int mpi_win_set_name_(MPI_Fint* win, const char *win_name CHAR_MIXED(size_win_name), int *ierror CHAR_END(size_win_name))
{
/* MPI_Win_set_name */
MPI_Win c_win = PMPI_Win_f2c(*win);
char *tmp_win_name = NULL, *ptr_win_name = NULL;
tmp_win_name = char_fortran_to_c((char *)win_name, size_win_name, &ptr_win_name);

*ierror = MPI_Win_set_name(c_win, tmp_win_name);
sctk_free(ptr_win_name);


}
int mpi_win_set_name__(MPI_Win* win, const char *win_name CHAR_MIXED(size_win_name), int *ierror CHAR_END(size_win_name))
{
	mpi_win_set_name_(win, win_name, ierror);
}
int mpi_win_shared_query_(MPI_Fint* win, int* rank, MPI_Aint *size, int *disp_unit, void *baseptr, int *ierror)
{
/* MPI_Win_shared_query */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_shared_query(c_win, *rank, size, disp_unit, baseptr);


}
int mpi_win_shared_query__(MPI_Win* win, int* rank, MPI_Aint *size, int *disp_unit, void *baseptr, int *ierror)
{
	mpi_win_shared_query_(win, rank, size, disp_unit, baseptr, ierror);
}
int mpi_win_start_(MPI_Fint* group, int* assert, MPI_Fint* win, int *ierror)
{
/* MPI_Win_start */
MPI_Group c_group = PMPI_Group_f2c(*group);
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_start(c_group, *assert, c_win);


}
int mpi_win_start__(MPI_Group* group, int* assert, MPI_Win* win, int *ierror)
{
	mpi_win_start_(group, assert, win, ierror);
}
int mpi_win_sync_(MPI_Fint* win, int *ierror)
{
/* MPI_Win_sync */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_sync(c_win);


}
int mpi_win_sync__(MPI_Win* win, int *ierror)
{
	mpi_win_sync_(win, ierror);
}
int mpi_win_test_(MPI_Fint* win, int *flag, int *ierror)
{
/* MPI_Win_test */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_test(c_win, flag);


}
int mpi_win_test__(MPI_Win* win, int *flag, int *ierror)
{
	mpi_win_test_(win, flag, ierror);
}
int mpi_win_unlock_(int* rank, MPI_Fint* win, int *ierror)
{
/* MPI_Win_unlock */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_unlock(*rank, c_win);


}
int mpi_win_unlock__(int* rank, MPI_Win* win, int *ierror)
{
	mpi_win_unlock_(rank, win, ierror);
}
int mpi_win_unlock_all_(MPI_Fint* win, int *ierror)
{
/* MPI_Win_unlock_all */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_unlock_all(c_win);


}
int mpi_win_unlock_all__(MPI_Win* win, int *ierror)
{
	mpi_win_unlock_all_(win, ierror);
}
int mpi_win_wait_(MPI_Fint* win, int *ierror)
{
/* MPI_Win_wait */
MPI_Win c_win = PMPI_Win_f2c(*win);

*ierror = MPI_Win_wait(c_win);


}
int mpi_win_wait__(MPI_Win* win, int *ierror)
{
	mpi_win_wait_(win, ierror);
}
double mpi_wtick_()
{
/* MPI_Wtick */

double ret = MPI_Wtick();


}
double mpi_wtick__()
{
	mpi_wtick_(, ierror);
}
double mpi_wtime_()
{
/* MPI_Wtime */

double ret = MPI_Wtime();


}
double mpi_wtime__()
{
return mpi_wtime_();
}
