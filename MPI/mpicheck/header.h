

/*MPI_Abort*/

/**
 * @brief MPI function MPI_Abort
 * 
 * @param comm communicator of \MPI/ processes to abort
 * @param errorcode error code to return to invoking environment
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Abort(MPI_Comm comm, int errorcode);
int PMPI_Abort(MPI_Comm comm, int errorcode);


/*MPI_Accumulate*/

/**
 * @brief MPI function MPI_Accumulate
 * 
 * @param origin_addr initial address of buffer
 * @param origin_count number of entries in buffer
 * @param origin_datatype datatype of each entry
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to beginning of target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param op reduce operation
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Accumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win);
int PMPI_Accumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win);


/*MPI_Add_error_class*/

/**
 * @brief MPI function MPI_Add_error_class
 * 
 * @param errorclass value for the new error class
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Add_error_class(int *errorclass);
int PMPI_Add_error_class(int *errorclass);


/*MPI_Add_error_code*/

/**
 * @brief MPI function MPI_Add_error_code
 * 
 * @param errorclass 
 * @param errorcode new error code to be associated with \mpiarg{errorclass}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Add_error_code(int errorclass, int *errorcode);
int PMPI_Add_error_code(int errorclass, int *errorcode);


/*MPI_Add_error_string*/

/**
 * @brief MPI function MPI_Add_error_string
 * 
 * @param errorcode error code or class
 * @param string text corresponding to \mpiarg{errorcode}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Add_error_string(int errorcode, const char *string);
int PMPI_Add_error_string(int errorcode, const char *string);


/*MPI_Aint_add*/

/**
 * @brief MPI function MPI_Aint_add
 * 
 * @param base base address
 * @param disp displacement
 *
 * @return MPI_Aint 
 
 */
MPI_Aint MPI_Aint_add(MPI_Aint base, MPI_Aint disp);
MPI_Aint PMPI_Aint_add(MPI_Aint base, MPI_Aint disp);


/*MPI_Aint_diff*/

/**
 * @brief MPI function MPI_Aint_diff
 * 
 * @param addr1 minuend address
 * @param addr2 subtrahend address
 *
 * @return MPI_Aint 
 
 */
MPI_Aint MPI_Aint_diff(MPI_Aint addr1, MPI_Aint addr2);
MPI_Aint PMPI_Aint_diff(MPI_Aint addr1, MPI_Aint addr2);


/*MPI_Allgather*/

/**
 * @brief MPI function MPI_Allgather
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements received from any process
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Allgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Allgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Allgather_init*/

/**
 * @brief MPI function MPI_Allgather_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements received from any process
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Allgather_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Allgather_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Allgatherv*/

/**
 * @brief MPI function MPI_Allgatherv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) containing the number of elements that are received from each process
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Allgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Allgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Allgatherv_init*/

/**
 * @brief MPI function MPI_Allgatherv_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) containing the number of elements that are received from each process
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Allgatherv_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Allgatherv_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Alloc_mem*/

/**
 * @brief MPI function MPI_Alloc_mem
 * 
 * @param size size of memory segment in bytes
 * @param info 
 * @param baseptr pointer to beginning of memory segment allocated
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr);
int PMPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr);


/*MPI_Allreduce*/

/**
 * @brief MPI function MPI_Allreduce
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of elements of send buffer
 * @param op operation
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Allreduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int PMPI_Allreduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);


/*MPI_Allreduce_init*/

/**
 * @brief MPI function MPI_Allreduce_init
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of elements of send buffer
 * @param op operation
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Allreduce_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Allreduce_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Alltoall*/

/**
 * @brief MPI function MPI_Alltoall
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each process
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements received from any process
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Alltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Alltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Alltoall_init*/

/**
 * @brief MPI function MPI_Alltoall_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each process
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements received from any process
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Alltoall_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Alltoall_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Alltoallv*/

/**
 * @brief MPI function MPI_Alltoallv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length group size) specifying the number of elements to send to each rank
 * @param sdispls integer array (of length group size). Entry \mpicode{j} specifies the displacement (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for process \mpicode{j}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) specifying the number of elements that can be received from each rank
 * @param rdispls integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Alltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Alltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Alltoallv_init*/

/**
 * @brief MPI function MPI_Alltoallv_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length group size) specifying the number of elements to send to each rank
 * @param sdispls Integer array (of length group size). Entry \mpicode{j} specifies the displacement (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for process \mpicode{j}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) specifying the number of elements that can be received from each rank
 * @param rdispls integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Alltoallv_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Alltoallv_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Alltoallw*/

/**
 * @brief MPI function MPI_Alltoallw
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length group size) specifying the number of elements to send to each rank
 * @param sdispls integer array (of length group size). Entry \mpicode{j} specifies the displacement in bytes (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for process \mpicode{j}
 * @param sendtypes array of datatypes (of length group size). Entry \mpicode{j} specifies the type of data to send to process \mpicode{j}
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) specifying the number of elements that can be received from each rank
 * @param rdispls integer array (of length group size). Entry \mpicode{i} specifies the displacement in bytes (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtypes array of datatypes (of length group size). Entry \mpicode{i} specifies the type of data received from process \mpicode{i}
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Alltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm);
int PMPI_Alltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm);


/*MPI_Alltoallw_init*/

/**
 * @brief MPI function MPI_Alltoallw_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts integer array (of length group size) specifying the number of elements to send to each rank
 * @param sdispls integer array (of length group size). Entry \mpicode{j} specifies the displacement in bytes (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for process \mpicode{j}
 * @param sendtypes Array of datatypes (of length group size). Entry \mpicode{j} specifies the type of data to send to process \mpicode{j}
 * @param recvbuf address of receive buffer
 * @param recvcounts integer array (of length group size) specifying the number of elements that can be received from each rank
 * @param rdispls integer array (of length group size). Entry \mpicode{i} specifies the displacement in bytes (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtypes array of datatypes (of length group size). Entry \mpicode{i} specifies the type of data received from process \mpicode{i}
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Alltoallw_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Alltoallw_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Attr_delete*/

/**
 * @brief MPI function MPI_Attr_delete
 * 
 * @param comm communicator to which attribute is attached
 * @param keyval The key value of the deleted attribute
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Attr_delete(MPI_Comm comm, int keyval);
int PMPI_Attr_delete(MPI_Comm comm, int keyval);


/*MPI_Attr_get*/

/**
 * @brief MPI function MPI_Attr_get
 * 
 * @param comm communicator to which attribute is attached
 * @param keyval key value
 * @param attribute_val attribute value, unless \mpiarg{flag} \mpicode{= false}
 * @param flag \mpicode{true} if an attribute value was extracted; \mpicode{false} if no attribute is associated with the key
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag);
int PMPI_Attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag);


/*MPI_Attr_put*/

/**
 * @brief MPI function MPI_Attr_put
 * 
 * @param comm communicator to which attribute will be attached
 * @param keyval key value, as returned by \mpifunc{MPI_KEYVAL_CREATE}
 * @param attribute_val attribute value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val);
int PMPI_Attr_put(MPI_Comm comm, int keyval, void *attribute_val);


/*MPI_Barrier*/

/**
 * @brief MPI function MPI_Barrier
 * 
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Barrier(MPI_Comm comm);
int PMPI_Barrier(MPI_Comm comm);


/*MPI_Barrier_init*/

/**
 * @brief MPI function MPI_Barrier_init
 * 
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Barrier_init(MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Barrier_init(MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Bcast*/

/**
 * @brief MPI function MPI_Bcast
 * 
 * @param buffer starting address of buffer
 * @param count number of entries in buffer
 * @param datatype datatype of buffer
 * @param root rank of broadcast root
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Bcast(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm);
int PMPI_Bcast(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm);


/*MPI_Bcast_init*/

/**
 * @brief MPI function MPI_Bcast_init
 * 
 * @param buffer starting address of buffer
 * @param count number of entries in buffer
 * @param datatype datatype of buffer
 * @param root rank of broadcast root
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Bcast_init(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Bcast_init(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Bsend*/

/**
 * @brief MPI function MPI_Bsend
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Bsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
int PMPI_Bsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);


/*MPI_Bsend_init*/

/**
 * @brief MPI function MPI_Bsend_init
 * 
 * @param buf initial address of send buffer
 * @param count number of elements sent
 * @param datatype type of each element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Bsend_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Bsend_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Buffer_attach*/

/**
 * @brief MPI function MPI_Buffer_attach
 * 
 * @param buffer initial buffer address
 * @param size buffer size, in bytes
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Buffer_attach(void *buffer, MPI_Count size);
int PMPI_Buffer_attach(void *buffer, MPI_Count size);


/*MPI_Buffer_detach*/

/**
 * @brief MPI function MPI_Buffer_detach
 * 
 * @param buffer_addr initial buffer address
 * @param size buffer size, in bytes
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Buffer_detach(void *buffer_addr, MPI_Count *size);
int PMPI_Buffer_detach(void *buffer_addr, MPI_Count *size);


/*MPI_Cancel*/

/**
 * @brief MPI function MPI_Cancel
 * 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cancel(MPI_Request *request);
int PMPI_Cancel(MPI_Request *request);


/*MPI_Cart_coords*/

/**
 * @brief MPI function MPI_Cart_coords
 * 
 * @param comm communicator with Cartesian structure
 * @param rank rank of a process within group of \mpiarg{comm}
 * @param maxdims length of vector  \mpiarg{coords} in the calling program
 * @param coords integer array (of size \mpiarg{maxdims}) containing the Cartesian coordinates of specified process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[]);
int PMPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[]);


/*MPI_Cart_create*/

/**
 * @brief MPI function MPI_Cart_create
 * 
 * @param comm_old input communicator
 * @param ndims number of dimensions of Cartesian grid
 * @param dims integer array of size \mpiarg{ndims} specifying the number of processes in each dimension
 * @param periods logical array of size \mpiarg{ndims} specifying whether the grid is periodic (\mpicode{true}) or not (\mpicode{false}) in each dimension
 * @param reorder ranking may be reordered (\mpicode{true}) or not (\mpicode{false})
 * @param comm_cart communicator with new Cartesian topology
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cart_create(MPI_Comm comm_old, int ndims, const int dims[], const int periods[], int reorder, MPI_Comm *comm_cart);
int PMPI_Cart_create(MPI_Comm comm_old, int ndims, const int dims[], const int periods[], int reorder, MPI_Comm *comm_cart);


/*MPI_Cart_get*/

/**
 * @brief MPI function MPI_Cart_get
 * 
 * @param comm communicator with Cartesian structure
 * @param maxdims length of vectors \mpiarg{dims}, \mpiarg{periods}, and \mpiarg{coords} in the calling program
 * @param dims number of processes for each Cartesian dimension
 * @param periods periodicity (\mpicode{true}/\mpicode{false}) for each Cartesian dimension
 * @param coords coordinates of calling process in Cartesian structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[], int coords[]);
int PMPI_Cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[], int coords[]);


/*MPI_Cart_map*/

/**
 * @brief MPI function MPI_Cart_map
 * 
 * @param comm input communicator
 * @param ndims number of dimensions of Cartesian structure
 * @param dims integer array of size \mpiarg{ndims} specifying the number of processes in each coordinate direction
 * @param periods logical array of size \mpiarg{ndims} specifying the periodicity specification in each coordinate direction
 * @param newrank reordered rank of the calling process; \mpiconst{MPI_UNDEFINED} if calling process does not belong to grid
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cart_map(MPI_Comm comm, int ndims, const int dims[], const int periods[], int *newrank);
int PMPI_Cart_map(MPI_Comm comm, int ndims, const int dims[], const int periods[], int *newrank);


/*MPI_Cart_rank*/

/**
 * @brief MPI function MPI_Cart_rank
 * 
 * @param comm communicator with Cartesian structure
 * @param coords integer array (of size \mpiarg{ndims}) specifying the Cartesian coordinates of a process
 * @param rank rank of specified process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cart_rank(MPI_Comm comm, const int coords[], int *rank);
int PMPI_Cart_rank(MPI_Comm comm, const int coords[], int *rank);


/*MPI_Cart_shift*/

/**
 * @brief MPI function MPI_Cart_shift
 * 
 * @param comm communicator with Cartesian structure
 * @param direction coordinate dimension of shift
 * @param disp displacement ($> 0$: upwards shift, $< 0$: downwards shift)
 * @param rank_source rank of source process
 * @param rank_dest rank of destination process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source, int *rank_dest);
int PMPI_Cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source, int *rank_dest);


/*MPI_Cart_sub*/

/**
 * @brief MPI function MPI_Cart_sub
 * 
 * @param comm communicator with Cartesian structure
 * @param remain_dims the \mpicode{i}-th entry of \mpiarg{remain_dims} specifies whether the \mpicode{i}-th dimension is kept in the subgrid (\mpicode{true}) or is dropped (\mpicode{false})
 * @param newcomm communicator containing the subgrid that includes the calling process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cart_sub(MPI_Comm comm, const int remain_dims[], MPI_Comm *newcomm);
int PMPI_Cart_sub(MPI_Comm comm, const int remain_dims[], MPI_Comm *newcomm);


/*MPI_Cartdim_get*/

/**
 * @brief MPI function MPI_Cartdim_get
 * 
 * @param comm communicator with Cartesian structure
 * @param ndims number of dimensions of the Cartesian structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Cartdim_get(MPI_Comm comm, int *ndims);
int PMPI_Cartdim_get(MPI_Comm comm, int *ndims);


/*MPI_Close_port*/

/**
 * @brief MPI function MPI_Close_port
 * 
 * @param port_name a port
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Close_port(const char *port_name);
int PMPI_Close_port(const char *port_name);


/*MPI_Comm_accept*/

/**
 * @brief MPI function MPI_Comm_accept
 * 
 * @param port_name port name
 * @param info implementation-dependent information
 * @param root rank in \mpiarg{comm} of root node
 * @param comm intra-communicator over which call is collective
 * @param newcomm inter-communicator with client as remote group
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_accept(const char *port_name, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *newcomm);
int PMPI_Comm_accept(const char *port_name, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *newcomm);


/*MPI_Comm_call_errhandler*/

/**
 * @brief MPI function MPI_Comm_call_errhandler
 * 
 * @param comm communicator with error handler
 * @param errorcode 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_call_errhandler(MPI_Comm comm, int errorcode);
int PMPI_Comm_call_errhandler(MPI_Comm comm, int errorcode);


/*MPI_Comm_compare*/

/**
 * @brief MPI function MPI_Comm_compare
 * 
 * @param comm1 first communicator
 * @param comm2 second communicator
 * @param result result
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result);
int PMPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result);


/*MPI_Comm_connect*/

/**
 * @brief MPI function MPI_Comm_connect
 * 
 * @param port_name network address
 * @param info implementation-dependent information
 * @param root rank in \mpiarg{comm} of root node
 * @param comm intra-communicator over which call is collective
 * @param newcomm inter-communicator with server as remote group
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_connect(const char *port_name, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *newcomm);
int PMPI_Comm_connect(const char *port_name, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *newcomm);


/*MPI_Comm_create*/

/**
 * @brief MPI function MPI_Comm_create
 * 
 * @param comm communicator
 * @param group group, which is a subset of the group of \mpiarg{comm}
 * @param newcomm new communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm);
int PMPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm);


/*MPI_Comm_create_errhandler*/

/**
 * @brief MPI function MPI_Comm_create_errhandler
 * 
 * @param comm_errhandler_fn user defined error handling procedure
 * @param errhandler 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_create_errhandler(MPI_Comm_errhandler_function *comm_errhandler_fn, MPI_Errhandler *errhandler);
int PMPI_Comm_create_errhandler(MPI_Comm_errhandler_function *comm_errhandler_fn, MPI_Errhandler *errhandler);


/*MPI_Comm_create_from_group*/

/**
 * @brief MPI function MPI_Comm_create_from_group
 * 
 * @param group group
 * @param stringtag unique identifier for this operation
 * @param info info object
 * @param errhandler error handler to be attached to new intra-communicator
 * @param newcomm new communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_create_from_group(MPI_Group group, const char *stringtag, MPI_Info info, MPI_Errhandler errhandler, MPI_Comm *newcomm);
int PMPI_Comm_create_from_group(MPI_Group group, const char *stringtag, MPI_Info info, MPI_Errhandler errhandler, MPI_Comm *newcomm);


/*MPI_Comm_create_group*/

/**
 * @brief MPI function MPI_Comm_create_group
 * 
 * @param comm intra-communicator
 * @param group group, which is a subset of the group of \mpiarg{comm}
 * @param tag tag
 * @param newcomm new communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm *newcomm);
int PMPI_Comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm *newcomm);


/*MPI_Comm_create_keyval*/

/**
 * @brief MPI function MPI_Comm_create_keyval
 * 
 * @param comm_copy_attr_fn copy callback function for \mpiarg{comm_keyval}
 * @param comm_delete_attr_fn delete callback function for \mpiarg{comm_keyval}
 * @param comm_keyval key value for future access
 * @param extra_state extra state for callback function
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn, MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval, void *extra_state);
int PMPI_Comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn, MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval, void *extra_state);


/*MPI_Comm_delete_attr*/

/**
 * @brief MPI function MPI_Comm_delete_attr
 * 
 * @param comm communicator from which the attribute is deleted
 * @param comm_keyval key value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval);
int PMPI_Comm_delete_attr(MPI_Comm comm, int comm_keyval);


/*MPI_Comm_disconnect*/

/**
 * @brief MPI function MPI_Comm_disconnect
 * 
 * @param comm 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_disconnect(MPI_Comm *comm);
int PMPI_Comm_disconnect(MPI_Comm *comm);


/*MPI_Comm_dup*/

/**
 * @brief MPI function MPI_Comm_dup
 * 
 * @param comm communicator
 * @param newcomm copy of \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm);
int PMPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm);


/*MPI_Comm_dup_with_info*/

/**
 * @brief MPI function MPI_Comm_dup_with_info
 * 
 * @param comm communicator
 * @param info info object
 * @param newcomm copy of \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_dup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm);
int PMPI_Comm_dup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm);


/*MPI_Comm_free*/

/**
 * @brief MPI function MPI_Comm_free
 * 
 * @param comm communicator to be destroyed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_free(MPI_Comm *comm);
int PMPI_Comm_free(MPI_Comm *comm);


/*MPI_Comm_free_keyval*/

/**
 * @brief MPI function MPI_Comm_free_keyval
 * 
 * @param comm_keyval key value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_free_keyval(int *comm_keyval);
int PMPI_Comm_free_keyval(int *comm_keyval);


/*MPI_Comm_get_attr*/

/**
 * @brief MPI function MPI_Comm_get_attr
 * 
 * @param comm communicator to which the attribute is attached
 * @param comm_keyval key value
 * @param attribute_val attribute value, unless \mpiarg{flag}\mpicode{ = false}
 * @param flag \mpicode{false} if no attribute is associated with the key
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_get_attr(MPI_Comm comm, int comm_keyval, void *attribute_val, int *flag);
int PMPI_Comm_get_attr(MPI_Comm comm, int comm_keyval, void *attribute_val, int *flag);


/*MPI_Comm_get_errhandler*/

/**
 * @brief MPI function MPI_Comm_get_errhandler
 * 
 * @param comm 
 * @param errhandler error handler currently associated with communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler);
int PMPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler);


/*MPI_Comm_get_info*/

/**
 * @brief MPI function MPI_Comm_get_info
 * 
 * @param comm communicator object
 * @param info_used new info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_get_info(MPI_Comm comm, MPI_Info *info_used);
int PMPI_Comm_get_info(MPI_Comm comm, MPI_Info *info_used);


/*MPI_Comm_get_name*/

/**
 * @brief MPI function MPI_Comm_get_name
 * 
 * @param comm communicator whose name is to be returned
 * @param comm_name the name previously stored on the communicator, or an empty string if no such name exists
 * @param resultlen length of returned name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen);
int PMPI_Comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen);


/*MPI_Comm_get_parent*/

/**
 * @brief MPI function MPI_Comm_get_parent
 * 
 * @param parent the parent communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_get_parent(MPI_Comm *parent);
int PMPI_Comm_get_parent(MPI_Comm *parent);


/*MPI_Comm_group*/

/**
 * @brief MPI function MPI_Comm_group
 * 
 * @param comm communicator
 * @param group group corresponding to \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_group(MPI_Comm comm, MPI_Group *group);
int PMPI_Comm_group(MPI_Comm comm, MPI_Group *group);


/*MPI_Comm_idup*/

/**
 * @brief MPI function MPI_Comm_idup
 * 
 * @param comm communicator
 * @param newcomm copy of \mpiarg{comm}
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_idup(MPI_Comm comm, MPI_Comm *newcomm, MPI_Request *request);
int PMPI_Comm_idup(MPI_Comm comm, MPI_Comm *newcomm, MPI_Request *request);


/*MPI_Comm_idup_with_info*/

/**
 * @brief MPI function MPI_Comm_idup_with_info
 * 
 * @param comm communicator
 * @param info info object
 * @param newcomm copy of \mpiarg{comm}
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_idup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm, MPI_Request *request);
int PMPI_Comm_idup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm, MPI_Request *request);


/*MPI_Comm_join*/

/**
 * @brief MPI function MPI_Comm_join
 * 
 * @param fd socket file descriptor
 * @param intercomm new inter-communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_join(int fd, MPI_Comm *intercomm);
int PMPI_Comm_join(int fd, MPI_Comm *intercomm);


/*MPI_Comm_rank*/

/**
 * @brief MPI function MPI_Comm_rank
 * 
 * @param comm communicator
 * @param rank rank of the calling process in group of \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_rank(MPI_Comm comm, int *rank);
int PMPI_Comm_rank(MPI_Comm comm, int *rank);


/*MPI_Comm_remote_group*/

/**
 * @brief MPI function MPI_Comm_remote_group
 * 
 * @param comm inter-communicator
 * @param group remote group corresponding to \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group);
int PMPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group);


/*MPI_Comm_remote_size*/

/**
 * @brief MPI function MPI_Comm_remote_size
 * 
 * @param comm inter-communicator
 * @param size number of processes in the remote group of \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_remote_size(MPI_Comm comm, int *size);
int PMPI_Comm_remote_size(MPI_Comm comm, int *size);


/*MPI_Comm_set_attr*/

/**
 * @brief MPI function MPI_Comm_set_attr
 * 
 * @param comm communicator to which attribute will be attached
 * @param comm_keyval key value
 * @param attribute_val attribute value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val);
int PMPI_Comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val);


/*MPI_Comm_set_errhandler*/

/**
 * @brief MPI function MPI_Comm_set_errhandler
 * 
 * @param comm 
 * @param errhandler new error handler for communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler);
int PMPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler);


/*MPI_Comm_set_info*/

/**
 * @brief MPI function MPI_Comm_set_info
 * 
 * @param comm communicator
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_set_info(MPI_Comm comm, MPI_Info info);
int PMPI_Comm_set_info(MPI_Comm comm, MPI_Info info);


/*MPI_Comm_set_name*/

/**
 * @brief MPI function MPI_Comm_set_name
 * 
 * @param comm communicator whose identifier is to be set
 * @param comm_name the character string that is remembered as the name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_set_name(MPI_Comm comm, const char *comm_name);
int PMPI_Comm_set_name(MPI_Comm comm, const char *comm_name);


/*MPI_Comm_size*/

/**
 * @brief MPI function MPI_Comm_size
 * 
 * @param comm communicator
 * @param size number of processes in the group of \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_size(MPI_Comm comm, int *size);
int PMPI_Comm_size(MPI_Comm comm, int *size);


/*MPI_Comm_spawn*/

/**
 * @brief MPI function MPI_Comm_spawn
 * 
 * @param command name of program to be spawned
 * @param argv arguments to \mpiarg{command}
 * @param maxprocs maximum number of processes to start
 * @param info a set of key-value pairs telling the runtime system where and how to start the processes
 * @param root rank of process in which previous arguments are examined
 * @param comm intra-communicator containing group of spawning processes
 * @param intercomm inter-communicator between original group and the newly spawned group
 * @param array_of_errcodes one code per process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_spawn(const char *command, char *argv[], int maxprocs, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]);
int PMPI_Comm_spawn(const char *command, char *argv[], int maxprocs, MPI_Info info, int root, MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]);


/*MPI_Comm_spawn_multiple*/

/**
 * @brief MPI function MPI_Comm_spawn_multiple
 * 
 * @param count number of commands
 * @param array_of_commands programs to be executed
 * @param array_of_argv arguments for \mpiarg{commands}
 * @param array_of_maxprocs maximum number of processes to start for each command
 * @param array_of_info info objects telling the runtime system where and how to start processes
 * @param root rank of process in which previous arguments are examined
 * @param comm intra-communicator containing group of spawning processes
 * @param intercomm inter-communicator between original group and the newly spawned group
 * @param array_of_errcodes one error code per process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_spawn_multiple(int count, char *array_of_commands[], char **array_of_argv[], const int array_of_maxprocs[], const MPI_Info array_of_info[], int root, MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]);
int PMPI_Comm_spawn_multiple(int count, char *array_of_commands[], char **array_of_argv[], const int array_of_maxprocs[], const MPI_Info array_of_info[], int root, MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]);


/*MPI_Comm_split*/

/**
 * @brief MPI function MPI_Comm_split
 * 
 * @param comm communicator
 * @param color control of subset assignment
 * @param key control of rank assignment
 * @param newcomm new communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm);
int PMPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm);


/*MPI_Comm_split_type*/

/**
 * @brief MPI function MPI_Comm_split_type
 * 
 * @param comm communicator
 * @param split_type type of processes to be grouped together
 * @param key control of rank assignment
 * @param info info argument
 * @param newcomm new communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_split_type(MPI_Comm comm, int split_type, int key, MPI_Info info, MPI_Comm *newcomm);
int PMPI_Comm_split_type(MPI_Comm comm, int split_type, int key, MPI_Info info, MPI_Comm *newcomm);


/*MPI_Comm_test_inter*/

/**
 * @brief MPI function MPI_Comm_test_inter
 * 
 * @param comm communicator
 * @param flag \mpicode{true} if \mpiarg{comm} is an inter-communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Comm_test_inter(MPI_Comm comm, int *flag);
int PMPI_Comm_test_inter(MPI_Comm comm, int *flag);


/*MPI_Compare_and_swap*/

/**
 * @brief MPI function MPI_Compare_and_swap
 * 
 * @param origin_addr initial address of buffer
 * @param compare_addr initial address of compare buffer
 * @param result_addr initial address of result buffer
 * @param datatype datatype of the element in all buffers
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to beginning of target buffer
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Compare_and_swap(const void *origin_addr, const void *compare_addr, void *result_addr, MPI_Datatype datatype, int target_rank, MPI_Aint target_disp, MPI_Win win);
int PMPI_Compare_and_swap(const void *origin_addr, const void *compare_addr, void *result_addr, MPI_Datatype datatype, int target_rank, MPI_Aint target_disp, MPI_Win win);


/*MPI_Dims_create*/

/**
 * @brief MPI function MPI_Dims_create
 * 
 * @param nnodes number of nodes in a grid
 * @param ndims number of Cartesian dimensions
 * @param dims integer array of size \mpiarg{ndims} specifying the number of nodes in each dimension
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Dims_create(int nnodes, int ndims, int dims[]);
int PMPI_Dims_create(int nnodes, int ndims, int dims[]);


/*MPI_Dist_graph_create*/

/**
 * @brief MPI function MPI_Dist_graph_create
 * 
 * @param comm_old input communicator
 * @param n number of source nodes for which this process specifies edges
 * @param sources array containing the \mpiarg{n} source nodes for which this process specifies edges
 * @param degrees array specifying the number of destinations for each source node in the source node array
 * @param destinations destination nodes for the source nodes in the source node array
 * @param weights weights for source to destination edges
 * @param info hints on optimization and interpretation of weights
 * @param reorder the ranks may be reordered (\mpicode{true}) or not~(\mbox{\mpicode{false}})
 * @param comm_dist_graph communicator with distributed graph topology added
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Dist_graph_create(MPI_Comm comm_old, int n, const int sources[], const int degrees[], const int destinations[], const int weights[], MPI_Info info, int reorder, MPI_Comm *comm_dist_graph);
int PMPI_Dist_graph_create(MPI_Comm comm_old, int n, const int sources[], const int degrees[], const int destinations[], const int weights[], MPI_Info info, int reorder, MPI_Comm *comm_dist_graph);


/*MPI_Dist_graph_create_adjacent*/

/**
 * @brief MPI function MPI_Dist_graph_create_adjacent
 * 
 * @param comm_old input communicator
 * @param indegree size of \mpiarg{sources} and \mpiarg{sourceweights} arrays
 * @param sources ranks of processes for which the calling process is a destination
 * @param sourceweights weights of the edges into the calling process
 * @param outdegree size of \mpiarg{destinations} and \mpiarg{destweights} arrays
 * @param destinations ranks of processes for which the calling process is a source
 * @param destweights weights of the edges out of the calling process
 * @param info hints on optimization and interpretation of weights
 * @param reorder the ranks may be reordered (\mpicode{true}) or not~(\mbox{\mpicode{false}})
 * @param comm_dist_graph communicator with distributed graph topology
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Dist_graph_create_adjacent(MPI_Comm comm_old, int indegree, const int sources[], const int sourceweights[], int outdegree, const int destinations[], const int destweights[], MPI_Info info, int reorder, MPI_Comm *comm_dist_graph);
int PMPI_Dist_graph_create_adjacent(MPI_Comm comm_old, int indegree, const int sources[], const int sourceweights[], int outdegree, const int destinations[], const int destweights[], MPI_Info info, int reorder, MPI_Comm *comm_dist_graph);


/*MPI_Dist_graph_neighbors*/

/**
 * @brief MPI function MPI_Dist_graph_neighbors
 * 
 * @param comm communicator with distributed graph topology
 * @param maxindegree size of sources and sourceweights arrays
 * @param sources processes for which the calling process is a destination
 * @param sourceweights weights of the edges into the calling process
 * @param maxoutdegree size of destinations and destweights arrays
 * @param destinations processes for which the calling process is a source
 * @param destweights weights of the edges out of the calling process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[], int maxoutdegree, int destinations[], int destweights[]);
int PMPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[], int maxoutdegree, int destinations[], int destweights[]);


/*MPI_Dist_graph_neighbors_count*/

/**
 * @brief MPI function MPI_Dist_graph_neighbors_count
 * 
 * @param comm communicator with distributed graph topology
 * @param indegree number of edges into this process
 * @param outdegree number of edges out of this process
 * @param weighted \mpicode{false} if \mpiconst{MPI_UNWEIGHTED} was supplied during creation, \mpicode{true} otherwise
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Dist_graph_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted);
int PMPI_Dist_graph_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted);


/*MPI_Errhandler_free*/

/**
 * @brief MPI function MPI_Errhandler_free
 * 
 * @param errhandler 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Errhandler_free(MPI_Errhandler *errhandler);
int PMPI_Errhandler_free(MPI_Errhandler *errhandler);


/*MPI_Error_class*/

/**
 * @brief MPI function MPI_Error_class
 * 
 * @param errorcode Error code returned by an \MPI/ routine
 * @param errorclass Error class associated with \mpiarg{errorcode}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Error_class(int errorcode, int *errorclass);
int PMPI_Error_class(int errorcode, int *errorclass);


/*MPI_Error_string*/

/**
 * @brief MPI function MPI_Error_string
 * 
 * @param errorcode Error code returned by an \MPI/ routine
 * @param string Text that corresponds to the \mpiarg{errorcode}
 * @param resultlen Length (in printable characters) of the result returned in \mpiarg{string}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Error_string(int errorcode, char *string, int *resultlen);
int PMPI_Error_string(int errorcode, char *string, int *resultlen);


/*MPI_Exscan*/

/**
 * @brief MPI function MPI_Exscan
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in input buffer
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm intra-communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Exscan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int PMPI_Exscan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);


/*MPI_Exscan_init*/

/**
 * @brief MPI function MPI_Exscan_init
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in input buffer
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm intra-communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Exscan_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Exscan_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Fetch_and_op*/

/**
 * @brief MPI function MPI_Fetch_and_op
 * 
 * @param origin_addr initial address of buffer
 * @param result_addr initial address of result buffer
 * @param datatype datatype of the entry in origin, result, and target buffers
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to beginning of target buffer
 * @param op reduce operation
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Fetch_and_op(const void *origin_addr, void *result_addr, MPI_Datatype datatype, int target_rank, MPI_Aint target_disp, MPI_Op op, MPI_Win win);
int PMPI_Fetch_and_op(const void *origin_addr, void *result_addr, MPI_Datatype datatype, int target_rank, MPI_Aint target_disp, MPI_Op op, MPI_Win win);


/*MPI_File_call_errhandler*/

/**
 * @brief MPI function MPI_File_call_errhandler
 * 
 * @param fh file with error handler
 * @param errorcode 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_call_errhandler(MPI_File fh, int errorcode);
int PMPI_File_call_errhandler(MPI_File fh, int errorcode);


/*MPI_File_close*/

/**
 * @brief MPI function MPI_File_close
 * 
 * @param fh file handle
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_close(MPI_File *fh);
int PMPI_File_close(MPI_File *fh);


/*MPI_File_create_errhandler*/

/**
 * @brief MPI function MPI_File_create_errhandler
 * 
 * @param file_errhandler_fn user defined error handling procedure
 * @param errhandler 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_create_errhandler(MPI_File_errhandler_function *file_errhandler_fn, MPI_Errhandler *errhandler);
int PMPI_File_create_errhandler(MPI_File_errhandler_function *file_errhandler_fn, MPI_Errhandler *errhandler);


/*MPI_File_delete*/

/**
 * @brief MPI function MPI_File_delete
 * 
 * @param filename name of file to delete
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_delete(const char *filename, MPI_Info info);
int PMPI_File_delete(const char *filename, MPI_Info info);


/*MPI_File_get_amode*/

/**
 * @brief MPI function MPI_File_get_amode
 * 
 * @param fh file handle
 * @param amode file access mode used to open the file
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_amode(MPI_File fh, int *amode);
int PMPI_File_get_amode(MPI_File fh, int *amode);


/*MPI_File_get_atomicity*/

/**
 * @brief MPI function MPI_File_get_atomicity
 * 
 * @param fh file handle
 * @param flag \mpicode{true} if atomic mode, \mpicode{false} if nonatomic mode
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_atomicity(MPI_File fh, int *flag);
int PMPI_File_get_atomicity(MPI_File fh, int *flag);


/*MPI_File_get_byte_offset*/

/**
 * @brief MPI function MPI_File_get_byte_offset
 * 
 * @param fh file handle
 * @param offset offset
 * @param disp absolute byte position of offset
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset, MPI_Offset *disp);
int PMPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset, MPI_Offset *disp);


/*MPI_File_get_errhandler*/

/**
 * @brief MPI function MPI_File_get_errhandler
 * 
 * @param file 
 * @param errhandler error handler currently associated with file
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_errhandler(MPI_File file, MPI_Errhandler *errhandler);
int PMPI_File_get_errhandler(MPI_File file, MPI_Errhandler *errhandler);


/*MPI_File_get_group*/

/**
 * @brief MPI function MPI_File_get_group
 * 
 * @param fh file handle
 * @param group group that opened the file
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_group(MPI_File fh, MPI_Group *group);
int PMPI_File_get_group(MPI_File fh, MPI_Group *group);


/*MPI_File_get_info*/

/**
 * @brief MPI function MPI_File_get_info
 * 
 * @param fh file handle
 * @param info_used new info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_info(MPI_File fh, MPI_Info *info_used);
int PMPI_File_get_info(MPI_File fh, MPI_Info *info_used);


/*MPI_File_get_position*/

/**
 * @brief MPI function MPI_File_get_position
 * 
 * @param fh file handle
 * @param offset offset of individual pointer
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_position(MPI_File fh, MPI_Offset *offset);
int PMPI_File_get_position(MPI_File fh, MPI_Offset *offset);


/*MPI_File_get_position_shared*/

/**
 * @brief MPI function MPI_File_get_position_shared
 * 
 * @param fh file handle
 * @param offset offset of shared pointer
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_position_shared(MPI_File fh, MPI_Offset *offset);
int PMPI_File_get_position_shared(MPI_File fh, MPI_Offset *offset);


/*MPI_File_get_size*/

/**
 * @brief MPI function MPI_File_get_size
 * 
 * @param fh file handle
 * @param size size of the file in bytes
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_size(MPI_File fh, MPI_Offset *size);
int PMPI_File_get_size(MPI_File fh, MPI_Offset *size);


/*MPI_File_get_type_extent*/

/**
 * @brief MPI function MPI_File_get_type_extent
 * 
 * @param fh file handle
 * @param datatype datatype
 * @param extent datatype extent
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Count *extent);
int PMPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Count *extent);


/*MPI_File_get_view*/

/**
 * @brief MPI function MPI_File_get_view
 * 
 * @param fh file handle
 * @param disp displacement
 * @param etype elementary datatype
 * @param filetype filetype
 * @param datarep data representation
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_get_view(MPI_File fh, MPI_Offset *disp, MPI_Datatype *etype, MPI_Datatype *filetype, char *datarep);
int PMPI_File_get_view(MPI_File fh, MPI_Offset *disp, MPI_Datatype *etype, MPI_Datatype *filetype, char *datarep);


/*MPI_File_iread*/

/**
 * @brief MPI function MPI_File_iread
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iread(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iread(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iread_all*/

/**
 * @brief MPI function MPI_File_iread_all
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iread_all(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iread_all(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iread_at*/

/**
 * @brief MPI function MPI_File_iread_at
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iread_at(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iread_at(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iread_at_all*/

/**
 * @brief MPI function MPI_File_iread_at_all
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iread_at_all(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iread_at_all(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iread_shared*/

/**
 * @brief MPI function MPI_File_iread_shared
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iread_shared(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iread_shared(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iwrite*/

/**
 * @brief MPI function MPI_File_iwrite
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iwrite(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iwrite(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iwrite_all*/

/**
 * @brief MPI function MPI_File_iwrite_all
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iwrite_all(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iwrite_all(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iwrite_at*/

/**
 * @brief MPI function MPI_File_iwrite_at
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iwrite_at_all*/

/**
 * @brief MPI function MPI_File_iwrite_at_all
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iwrite_at_all(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iwrite_at_all(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_iwrite_shared*/

/**
 * @brief MPI function MPI_File_iwrite_shared
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param request request object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_iwrite_shared(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);
int PMPI_File_iwrite_shared(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Request *request);


/*MPI_File_open*/

/**
 * @brief MPI function MPI_File_open
 * 
 * @param comm communicator
 * @param filename name of file to open
 * @param amode file access mode
 * @param info info object
 * @param fh new file handle
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_open(MPI_Comm comm, const char *filename, int amode, MPI_Info info, MPI_File *fh);
int PMPI_File_open(MPI_Comm comm, const char *filename, int amode, MPI_Info info, MPI_File *fh);


/*MPI_File_preallocate*/

/**
 * @brief MPI function MPI_File_preallocate
 * 
 * @param fh file handle
 * @param size size to preallocate file
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_preallocate(MPI_File fh, MPI_Offset size);
int PMPI_File_preallocate(MPI_File fh, MPI_Offset size);


/*MPI_File_read*/

/**
 * @brief MPI function MPI_File_read
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_read(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_read_all*/

/**
 * @brief MPI function MPI_File_read_all
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_all(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_read_all(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_read_all_begin*/

/**
 * @brief MPI function MPI_File_read_all_begin
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_all_begin(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype);
int PMPI_File_read_all_begin(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype);


/*MPI_File_read_all_end*/

/**
 * @brief MPI function MPI_File_read_all_end
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_all_end(MPI_File fh, void *buf, MPI_Status *status);
int PMPI_File_read_all_end(MPI_File fh, void *buf, MPI_Status *status);


/*MPI_File_read_at*/

/**
 * @brief MPI function MPI_File_read_at
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_at(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_read_at(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_read_at_all*/

/**
 * @brief MPI function MPI_File_read_at_all
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_read_at_all_begin*/

/**
 * @brief MPI function MPI_File_read_at_all_begin
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_at_all_begin(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype);
int PMPI_File_read_at_all_begin(MPI_File fh, MPI_Offset offset, void *buf, MPI_Count count, MPI_Datatype datatype);


/*MPI_File_read_at_all_end*/

/**
 * @brief MPI function MPI_File_read_at_all_end
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_at_all_end(MPI_File fh, void *buf, MPI_Status *status);
int PMPI_File_read_at_all_end(MPI_File fh, void *buf, MPI_Status *status);


/*MPI_File_read_ordered*/

/**
 * @brief MPI function MPI_File_read_ordered
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_ordered(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_read_ordered(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_read_ordered_begin*/

/**
 * @brief MPI function MPI_File_read_ordered_begin
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_ordered_begin(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype);
int PMPI_File_read_ordered_begin(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype);


/*MPI_File_read_ordered_end*/

/**
 * @brief MPI function MPI_File_read_ordered_end
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_ordered_end(MPI_File fh, void *buf, MPI_Status *status);
int PMPI_File_read_ordered_end(MPI_File fh, void *buf, MPI_Status *status);


/*MPI_File_read_shared*/

/**
 * @brief MPI function MPI_File_read_shared
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_read_shared(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_read_shared(MPI_File fh, void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_seek*/

/**
 * @brief MPI function MPI_File_seek
 * 
 * @param fh file handle
 * @param offset file offset
 * @param whence update mode
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_seek(MPI_File fh, MPI_Offset offset, int whence);
int PMPI_File_seek(MPI_File fh, MPI_Offset offset, int whence);


/*MPI_File_seek_shared*/

/**
 * @brief MPI function MPI_File_seek_shared
 * 
 * @param fh file handle
 * @param offset file offset
 * @param whence update mode
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence);
int PMPI_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence);


/*MPI_File_set_atomicity*/

/**
 * @brief MPI function MPI_File_set_atomicity
 * 
 * @param fh file handle
 * @param flag \mpicode{true} to set atomic mode, \mpicode{false} to set nonatomic mode
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_set_atomicity(MPI_File fh, int flag);
int PMPI_File_set_atomicity(MPI_File fh, int flag);


/*MPI_File_set_errhandler*/

/**
 * @brief MPI function MPI_File_set_errhandler
 * 
 * @param file 
 * @param errhandler new error handler for file
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_set_errhandler(MPI_File file, MPI_Errhandler errhandler);
int PMPI_File_set_errhandler(MPI_File file, MPI_Errhandler errhandler);


/*MPI_File_set_info*/

/**
 * @brief MPI function MPI_File_set_info
 * 
 * @param fh file handle
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_set_info(MPI_File fh, MPI_Info info);
int PMPI_File_set_info(MPI_File fh, MPI_Info info);


/*MPI_File_set_size*/

/**
 * @brief MPI function MPI_File_set_size
 * 
 * @param fh file handle
 * @param size size to truncate or expand file
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_set_size(MPI_File fh, MPI_Offset size);
int PMPI_File_set_size(MPI_File fh, MPI_Offset size);


/*MPI_File_set_view*/

/**
 * @brief MPI function MPI_File_set_view
 * 
 * @param fh file handle
 * @param disp displacement
 * @param etype elementary datatype
 * @param filetype filetype
 * @param datarep data representation
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, MPI_Datatype filetype, const char *datarep, MPI_Info info);
int PMPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, MPI_Datatype filetype, const char *datarep, MPI_Info info);


/*MPI_File_sync*/

/**
 * @brief MPI function MPI_File_sync
 * 
 * @param fh file handle
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_sync(MPI_File fh);
int PMPI_File_sync(MPI_File fh);


/*MPI_File_write*/

/**
 * @brief MPI function MPI_File_write
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_write(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_write_all*/

/**
 * @brief MPI function MPI_File_write_all
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_all(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_write_all(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_write_all_begin*/

/**
 * @brief MPI function MPI_File_write_all_begin
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_all_begin(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype);
int PMPI_File_write_all_begin(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype);


/*MPI_File_write_all_end*/

/**
 * @brief MPI function MPI_File_write_all_end
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_all_end(MPI_File fh, const void *buf, MPI_Status *status);
int PMPI_File_write_all_end(MPI_File fh, const void *buf, MPI_Status *status);


/*MPI_File_write_at*/

/**
 * @brief MPI function MPI_File_write_at
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_at(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_write_at(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_write_at_all*/

/**
 * @brief MPI function MPI_File_write_at_all
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_at_all(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_write_at_all(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_write_at_all_begin*/

/**
 * @brief MPI function MPI_File_write_at_all_begin
 * 
 * @param fh file handle
 * @param offset file offset
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype);
int PMPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, const void *buf, MPI_Count count, MPI_Datatype datatype);


/*MPI_File_write_at_all_end*/

/**
 * @brief MPI function MPI_File_write_at_all_end
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_at_all_end(MPI_File fh, const void *buf, MPI_Status *status);
int PMPI_File_write_at_all_end(MPI_File fh, const void *buf, MPI_Status *status);


/*MPI_File_write_ordered*/

/**
 * @brief MPI function MPI_File_write_ordered
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_ordered(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_write_ordered(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_File_write_ordered_begin*/

/**
 * @brief MPI function MPI_File_write_ordered_begin
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_ordered_begin(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype);
int PMPI_File_write_ordered_begin(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype);


/*MPI_File_write_ordered_end*/

/**
 * @brief MPI function MPI_File_write_ordered_end
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_ordered_end(MPI_File fh, const void *buf, MPI_Status *status);
int PMPI_File_write_ordered_end(MPI_File fh, const void *buf, MPI_Status *status);


/*MPI_File_write_shared*/

/**
 * @brief MPI function MPI_File_write_shared
 * 
 * @param fh file handle
 * @param buf initial address of buffer
 * @param count number of elements in buffer
 * @param datatype datatype of each buffer element
 * @param status status object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_File_write_shared(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);
int PMPI_File_write_shared(MPI_File fh, const void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Status *status);


/*MPI_Finalize*/

/**
 * @brief MPI function MPI_Finalize
 * 
 *
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Finalize();
int PMPI_Finalize();


/*MPI_Finalized*/

/**
 * @brief MPI function MPI_Finalized
 * 
 * @param flag true if \mpi/ was finalized
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Finalized(int *flag);
int PMPI_Finalized(int *flag);


/*MPI_Free_mem*/

/**
 * @brief MPI function MPI_Free_mem
 * 
 * @param base initial address of memory segment allocated by \mpifunc{MPI_ALLOC_MEM}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Free_mem(void *base);
int PMPI_Free_mem(void *base);


/*MPI_Gather*/

/**
 * @brief MPI function MPI_Gather
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements for any single receive
 * @param recvtype datatype of recv buffer elements
 * @param root rank of receiving process
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Gather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);
int PMPI_Gather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);


/*MPI_Gather_init*/

/**
 * @brief MPI function MPI_Gather_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements for any single receive
 * @param recvtype datatype of recv buffer elements
 * @param root rank of receiving process
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Gather_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Gather_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Gatherv*/

/**
 * @brief MPI function MPI_Gatherv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) containing the number of elements that are received from each process
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement relative to \mpiarg{recvbuf} at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of recv buffer elements
 * @param root rank of receiving process
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Gatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, int root, MPI_Comm comm);
int PMPI_Gatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, int root, MPI_Comm comm);


/*MPI_Gatherv_init*/

/**
 * @brief MPI function MPI_Gatherv_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) containing the number of elements that are received from each process
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement relative to \mpiarg{recvbuf} at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of recv buffer elements
 * @param root rank of receiving process
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Gatherv_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Gatherv_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Get*/

/**
 * @brief MPI function MPI_Get
 * 
 * @param origin_addr initial address of origin buffer
 * @param origin_count number of entries in origin buffer
 * @param origin_datatype datatype of each entry in origin buffer
 * @param target_rank rank of target
 * @param target_disp displacement from window start to the beginning of the target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param win window object used for communication
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get(void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win);
int PMPI_Get(void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win);


/*MPI_Get_accumulate*/

/**
 * @brief MPI function MPI_Get_accumulate
 * 
 * @param origin_addr initial address of buffer
 * @param origin_count number of entries in origin buffer
 * @param origin_datatype datatype of each entry in origin buffer
 * @param result_addr initial address of result buffer
 * @param result_count number of entries in result buffer
 * @param result_datatype datatype of each entry in result buffer
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to beginning of target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param op reduce operation
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_accumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, void *result_addr, MPI_Count result_count, MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win);
int PMPI_Get_accumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, void *result_addr, MPI_Count result_count, MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win);


/*MPI_Get_address*/

/**
 * @brief MPI function MPI_Get_address
 * 
 * @param location location in caller memory
 * @param address address of location
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_address(const void *location, MPI_Aint *address);
int PMPI_Get_address(const void *location, MPI_Aint *address);


/*MPI_Get_count*/

/**
 * @brief MPI function MPI_Get_count
 * 
 * @param status return status of receive operation
 * @param datatype datatype of each receive buffer entry
 * @param count number of received entries
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_count(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count);
int PMPI_Get_count(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count);


/*MPI_Get_elements*/

/**
 * @brief MPI function MPI_Get_elements
 * 
 * @param status return status of receive operation
 * @param datatype datatype used by receive operation
 * @param count number of received basic elements
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_elements(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count);
int PMPI_Get_elements(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count);


/*MPI_Get_elements_x*/

/**
 * @brief MPI function MPI_Get_elements_x
 * 
 * @param status return status of receive operation
 * @param datatype datatype used by receive operation
 * @param count number of received basic elements
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_elements_x(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count);
int PMPI_Get_elements_x(const MPI_Status *status, MPI_Datatype datatype, MPI_Count *count);


/*MPI_Get_library_version*/

/**
 * @brief MPI function MPI_Get_library_version
 * 
 * @param version version number
 * @param resultlen Length (in printable characters) of the result returned in \mpiarg{version}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_library_version(char *version, int *resultlen);
int PMPI_Get_library_version(char *version, int *resultlen);


/*MPI_Get_processor_name*/

/**
 * @brief MPI function MPI_Get_processor_name
 * 
 * @param name A unique specifier for the actual (as opposed to virtual) node.
 * @param resultlen Length (in printable characters) of the result returned in \mpiarg{name}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_processor_name(char *name, int *resultlen);
int PMPI_Get_processor_name(char *name, int *resultlen);


/*MPI_Get_version*/

/**
 * @brief MPI function MPI_Get_version
 * 
 * @param version version number
 * @param subversion subversion number
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Get_version(int *version, int *subversion);
int PMPI_Get_version(int *version, int *subversion);


/*MPI_Graph_create*/

/**
 * @brief MPI function MPI_Graph_create
 * 
 * @param comm_old input communicator
 * @param nnodes number of nodes in graph
 * @param index array of integers describing node degrees (see below)
 * @param edges array of integers describing graph edges (see below)
 * @param reorder ranking may be reordered (\mpicode{true}) or not (\mpicode{false})
 * @param comm_graph communicator with graph topology added
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Graph_create(MPI_Comm comm_old, int nnodes, const int index[], const int edges[], int reorder, MPI_Comm *comm_graph);
int PMPI_Graph_create(MPI_Comm comm_old, int nnodes, const int index[], const int edges[], int reorder, MPI_Comm *comm_graph);


/*MPI_Graph_get*/

/**
 * @brief MPI function MPI_Graph_get
 * 
 * @param comm communicator with graph structure
 * @param maxindex length of vector \mpiarg{index} in the calling program
 * @param maxedges length of vector \mpiarg{edges} in the calling program
 * @param index array of integers containing the graph structure (for details see the definition of \mpifunc{MPI_GRAPH_CREATE})
 * @param edges array of integers containing the graph structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges, int index[], int edges[]);
int PMPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges, int index[], int edges[]);


/*MPI_Graph_map*/

/**
 * @brief MPI function MPI_Graph_map
 * 
 * @param comm input communicator
 * @param nnodes number of graph nodes
 * @param index integer array specifying the graph structure, see \mpifunc{MPI_GRAPH_CREATE}
 * @param edges integer array specifying the graph structure
 * @param newrank reordered rank of the calling process; \mpiconst{MPI_UNDEFINED} if the calling process does not belong to graph
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Graph_map(MPI_Comm comm, int nnodes, const int index[], const int edges[], int *newrank);
int PMPI_Graph_map(MPI_Comm comm, int nnodes, const int index[], const int edges[], int *newrank);


/*MPI_Graph_neighbors*/

/**
 * @brief MPI function MPI_Graph_neighbors
 * 
 * @param comm communicator with graph topology
 * @param rank rank of process in group of \mpiarg{comm}
 * @param maxneighbors size of array \mpiarg{neighbors}
 * @param neighbors ranks of processes that are neighbors to specified process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors, int neighbors[]);
int PMPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors, int neighbors[]);


/*MPI_Graph_neighbors_count*/

/**
 * @brief MPI function MPI_Graph_neighbors_count
 * 
 * @param comm communicator with graph topology
 * @param rank rank of process in group of \mpiarg{comm}
 * @param nneighbors number of neighbors of specified process
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors);
int PMPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors);


/*MPI_Graphdims_get*/

/**
 * @brief MPI function MPI_Graphdims_get
 * 
 * @param comm communicator for group with graph structure
 * @param nnodes number of nodes in graph (same as number of processes in the group)
 * @param nedges number of edges in graph
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges);
int PMPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges);


/*MPI_Grequest_complete*/

/**
 * @brief MPI function MPI_Grequest_complete
 * 
 * @param request generalized request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Grequest_complete(MPI_Request request);
int PMPI_Grequest_complete(MPI_Request request);


/*MPI_Grequest_start*/

/**
 * @brief MPI function MPI_Grequest_start
 * 
 * @param query_fn callback function invoked when request status is queried
 * @param free_fn callback function invoked when request is freed
 * @param cancel_fn callback function invoked when request is cancelled
 * @param extra_state extra state
 * @param request generalized request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Grequest_start(MPI_Grequest_query_function *query_fn, MPI_Grequest_free_function *free_fn, MPI_Grequest_cancel_function *cancel_fn, void *extra_state, MPI_Request *request);
int PMPI_Grequest_start(MPI_Grequest_query_function *query_fn, MPI_Grequest_free_function *free_fn, MPI_Grequest_cancel_function *cancel_fn, void *extra_state, MPI_Request *request);


/*MPI_Group_compare*/

/**
 * @brief MPI function MPI_Group_compare
 * 
 * @param group1 first group
 * @param group2 second group
 * @param result result
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_compare(MPI_Group group1, MPI_Group group2, int *result);
int PMPI_Group_compare(MPI_Group group1, MPI_Group group2, int *result);


/*MPI_Group_difference*/

/**
 * @brief MPI function MPI_Group_difference
 * 
 * @param group1 first group
 * @param group2 second group
 * @param newgroup difference group
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_difference(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);
int PMPI_Group_difference(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);


/*MPI_Group_excl*/

/**
 * @brief MPI function MPI_Group_excl
 * 
 * @param group group
 * @param n number of elements in array \mpiarg{ranks}
 * @param ranks array of integer ranks of processes in \mpiarg{group} not to appear in \mpiarg{newgroup}
 * @param newgroup new group derived from above, preserving the order defined by \mpiarg{group}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_excl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup);
int PMPI_Group_excl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup);


/*MPI_Group_free*/

/**
 * @brief MPI function MPI_Group_free
 * 
 * @param group group
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_free(MPI_Group *group);
int PMPI_Group_free(MPI_Group *group);


/*MPI_Group_from_session_pset*/

/**
 * @brief MPI function MPI_Group_from_session_pset
 * 
 * @param session session
 * @param pset_name name of process set to use to create the new group
 * @param newgroup new group derived from supplied session and process set
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_from_session_pset(MPI_Session session, const char *pset_name, MPI_Group *newgroup);
int PMPI_Group_from_session_pset(MPI_Session session, const char *pset_name, MPI_Group *newgroup);


/*MPI_Group_incl*/

/**
 * @brief MPI function MPI_Group_incl
 * 
 * @param group group
 * @param n number of elements in array \mpiarg{ranks} (and size of \mpiarg{newgroup})
 * @param ranks ranks of processes in \mpiarg{group} to appear in \mpiarg{newgroup}
 * @param newgroup new group derived from above, in the order defined by \mpiarg{ranks}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_incl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup);
int PMPI_Group_incl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup);


/*MPI_Group_intersection*/

/**
 * @brief MPI function MPI_Group_intersection
 * 
 * @param group1 first group
 * @param group2 second group
 * @param newgroup intersection group
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_intersection(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);
int PMPI_Group_intersection(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);


/*MPI_Group_range_excl*/

/**
 * @brief MPI function MPI_Group_range_excl
 * 
 * @param group group
 * @param n number of triplets in array \mpiarg{ranges}
 * @param ranges a one-dimensional array of integer triplets, of the form (first rank, last rank, stride) indicating ranks in \mpiarg{group} of processes to be excluded from the output group \mpiarg{newgroup}
 * @param newgroup new group derived from above, preserving the order in \mpiarg{group}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_range_excl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup);
int PMPI_Group_range_excl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup);


/*MPI_Group_range_incl*/

/**
 * @brief MPI function MPI_Group_range_incl
 * 
 * @param group group
 * @param n number of triplets in array \mpiarg{ranges}
 * @param ranges a one-dimensional array of integer triplets, of the form (first rank, last rank, stride) indicating ranks in \mpiarg{group} of processes to be included in \mpiarg{newgroup}
 * @param newgroup new group derived from above, in the order defined by \mpiarg{ranges}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_range_incl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup);
int PMPI_Group_range_incl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup);


/*MPI_Group_rank*/

/**
 * @brief MPI function MPI_Group_rank
 * 
 * @param group group
 * @param rank rank of the calling process in group, or \mpiconst{MPI_UNDEFINED} if the process is not a member
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_rank(MPI_Group group, int *rank);
int PMPI_Group_rank(MPI_Group group, int *rank);


/*MPI_Group_size*/

/**
 * @brief MPI function MPI_Group_size
 * 
 * @param group group
 * @param size number of processes in the group
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_size(MPI_Group group, int *size);
int PMPI_Group_size(MPI_Group group, int *size);


/*MPI_Group_translate_ranks*/

/**
 * @brief MPI function MPI_Group_translate_ranks
 * 
 * @param group1 group1
 * @param n number of ranks in \mpiarg{ranks1} and \mpiarg{ranks2} arrays
 * @param ranks1 array of zero or more valid ranks in group1
 * @param group2 group2
 * @param ranks2 array of corresponding ranks in group2, \mpiconst{MPI_UNDEFINED} when no correspondence exists.
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_translate_ranks(MPI_Group group1, int n, const int ranks1[], MPI_Group group2, int ranks2[]);
int PMPI_Group_translate_ranks(MPI_Group group1, int n, const int ranks1[], MPI_Group group2, int ranks2[]);


/*MPI_Group_union*/

/**
 * @brief MPI function MPI_Group_union
 * 
 * @param group1 first group
 * @param group2 second group
 * @param newgroup union group
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Group_union(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);
int PMPI_Group_union(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);


/*MPI_Iallgather*/

/**
 * @brief MPI function MPI_Iallgather
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements received from any process
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iallgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Iallgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Iallgatherv*/

/**
 * @brief MPI function MPI_Iallgatherv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) containing the number of elements that are received from each process
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iallgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Iallgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Iallreduce*/

/**
 * @brief MPI function MPI_Iallreduce
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of elements of send buffer
 * @param op operation
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iallreduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);
int PMPI_Iallreduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);


/*MPI_Ialltoall*/

/**
 * @brief MPI function MPI_Ialltoall
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each process
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements received from any process
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ialltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Ialltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Ialltoallv*/

/**
 * @brief MPI function MPI_Ialltoallv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length group size) specifying the number of elements to send to each rank
 * @param sdispls integer array (of length group size). Entry \mpicode{j} specifies the displacement (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for process \mpicode{j}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) specifying the number of elements that can be received from each rank
 * @param rdispls integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ialltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Ialltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Ialltoallw*/

/**
 * @brief MPI function MPI_Ialltoallw
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts integer array (of length group size) specifying the number of elements to send to each rank
 * @param sdispls integer array (of length group size). Entry \mpicode{j} specifies the displacement in bytes (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for process \mpicode{j}
 * @param sendtypes array of datatypes (of length group size). Entry \mpicode{j} specifies the type of data to send to process \mpicode{j}
 * @param recvbuf address of receive buffer
 * @param recvcounts integer array (of length group size) specifying the number of elements that can be received from each rank
 * @param rdispls integer array (of length group size). Entry \mpicode{i} specifies the displacement in bytes (relative to \mpiarg{recvbuf}) at which to place the incoming data from process \mpicode{i}
 * @param recvtypes array of datatypes (of length group size). Entry \mpicode{i} specifies the type of data received from process \mpicode{i}
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ialltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Request *request);
int PMPI_Ialltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Request *request);


/*MPI_Ibarrier*/

/**
 * @brief MPI function MPI_Ibarrier
 * 
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ibarrier(MPI_Comm comm, MPI_Request *request);
int PMPI_Ibarrier(MPI_Comm comm, MPI_Request *request);


/*MPI_Ibcast*/

/**
 * @brief MPI function MPI_Ibcast
 * 
 * @param buffer starting address of buffer
 * @param count number of entries in buffer
 * @param datatype datatype of buffer
 * @param root rank of broadcast root
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ibcast(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm, MPI_Request *request);
int PMPI_Ibcast(void *buffer, MPI_Count count, MPI_Datatype datatype, int root, MPI_Comm comm, MPI_Request *request);


/*MPI_Ibsend*/

/**
 * @brief MPI function MPI_Ibsend
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ibsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Ibsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Iexscan*/

/**
 * @brief MPI function MPI_Iexscan
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in input buffer
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm intra-communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iexscan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);
int PMPI_Iexscan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);


/*MPI_Igather*/

/**
 * @brief MPI function MPI_Igather
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements for any single receive
 * @param recvtype datatype of recv buffer elements
 * @param root rank of receiving process
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Igather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);
int PMPI_Igather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);


/*MPI_Igatherv*/

/**
 * @brief MPI function MPI_Igatherv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) containing the number of elements that are received from each process
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement relative to \mpiarg{recvbuf} at which to place the incoming data from process \mpicode{i}
 * @param recvtype datatype of recv buffer elements
 * @param root rank of receiving process
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Igatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);
int PMPI_Igatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);


/*MPI_Improbe*/

/**
 * @brief MPI function MPI_Improbe
 * 
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param tag message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param flag \mpicode{true} if there is a matching message that can be received
 * @param message returned message
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Improbe(int source, int tag, MPI_Comm comm, int *flag, MPI_Message *message, MPI_Status *status);
int PMPI_Improbe(int source, int tag, MPI_Comm comm, int *flag, MPI_Message *message, MPI_Status *status);


/*MPI_Imrecv*/

/**
 * @brief MPI function MPI_Imrecv
 * 
 * @param buf initial address of receive buffer
 * @param count number of elements in receive buffer
 * @param datatype datatype of each receive buffer element
 * @param message message
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Imrecv(void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Message *message, MPI_Request *request);
int PMPI_Imrecv(void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Message *message, MPI_Request *request);


/*MPI_Ineighbor_allgather*/

/**
 * @brief MPI function MPI_Ineighbor_allgather
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcount number of elements received from each neighbor
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ineighbor_allgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Ineighbor_allgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Ineighbor_allgatherv*/

/**
 * @brief MPI function MPI_Ineighbor_allgatherv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) containing the number of elements that are received from each neighbor
 * @param displs integer array (of length indegree). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ineighbor_allgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Ineighbor_allgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Ineighbor_alltoall*/

/**
 * @brief MPI function MPI_Ineighbor_alltoall
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcount number of elements received from each neighbor
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ineighbor_alltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Ineighbor_alltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Ineighbor_alltoallv*/

/**
 * @brief MPI function MPI_Ineighbor_alltoallv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length outdegree) specifying the number of elements to send to each neighbor
 * @param sdispls integer array (of length outdegree). Entry \mpicode{j} specifies the displacement (relative to \mpiarg{sendbuf}) from which send the outgoing data to neighbor \mpicode{j}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) specifying the number of elements that are received from each neighbor
 * @param rdispls integer array (of length indegree). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ineighbor_alltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int PMPI_Ineighbor_alltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);


/*MPI_Ineighbor_alltoallw*/

/**
 * @brief MPI function MPI_Ineighbor_alltoallw
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length outdegree) specifying the number of elements to send to each neighbor
 * @param sdispls integer array (of length outdegree). Entry \mpicode{j} specifies the displacement in bytes (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for neighbor \mpicode{j}
 * @param sendtypes array of datatypes (of length outdegree). Entry \mpicode{j} specifies the type of data to send to neighbor \mpicode{j}
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) specifying the number of elements that are received from each neighbor
 * @param rdispls integer array (of length indegree). Entry \mpicode{i} specifies the displacement in bytes (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtypes array of datatypes (of length indegree). Entry \mpicode{i} specifies the type of data received from neighbor \mpicode{i}
 * @param comm communicator with topology structure
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ineighbor_alltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Request *request);
int PMPI_Ineighbor_alltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Request *request);


/*MPI_Info_create*/

/**
 * @brief MPI function MPI_Info_create
 * 
 * @param info info object created
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_create(MPI_Info *info);
int PMPI_Info_create(MPI_Info *info);


/*MPI_Info_create_env*/

/**
 * @brief MPI function MPI_Info_create_env
 * 
 * @param argc 
 * @param argv 
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_create_env(int argc, char argv[], MPI_Info *info);
int PMPI_Info_create_env(int argc, char argv[], MPI_Info *info);


/*MPI_Info_delete*/

/**
 * @brief MPI function MPI_Info_delete
 * 
 * @param info info object
 * @param key key
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_delete(MPI_Info info, const char *key);
int PMPI_Info_delete(MPI_Info info, const char *key);


/*MPI_Info_dup*/

/**
 * @brief MPI function MPI_Info_dup
 * 
 * @param info info object
 * @param newinfo info object created
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo);
int PMPI_Info_dup(MPI_Info info, MPI_Info *newinfo);


/*MPI_Info_free*/

/**
 * @brief MPI function MPI_Info_free
 * 
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_free(MPI_Info *info);
int PMPI_Info_free(MPI_Info *info);


/*MPI_Info_get*/

/**
 * @brief MPI function MPI_Info_get
 * 
 * @param info info object
 * @param key key
 * @param valuelen length of value associated with \mpiarg{key}
 * @param value value
 * @param flag \mpicode{true} if \mpiarg{key} defined, \mpicode{false} if not
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_get(MPI_Info info, const char *key, int valuelen, char *value, int *flag);
int PMPI_Info_get(MPI_Info info, const char *key, int valuelen, char *value, int *flag);


/*MPI_Info_get_nkeys*/

/**
 * @brief MPI function MPI_Info_get_nkeys
 * 
 * @param info info object
 * @param nkeys number of defined keys
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_get_nkeys(MPI_Info info, int *nkeys);
int PMPI_Info_get_nkeys(MPI_Info info, int *nkeys);


/*MPI_Info_get_nthkey*/

/**
 * @brief MPI function MPI_Info_get_nthkey
 * 
 * @param info info object
 * @param n key number
 * @param key key
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_get_nthkey(MPI_Info info, int n, char *key);
int PMPI_Info_get_nthkey(MPI_Info info, int n, char *key);


/*MPI_Info_get_string*/

/**
 * @brief MPI function MPI_Info_get_string
 * 
 * @param info info object
 * @param key key
 * @param buflen length of buffer
 * @param value value
 * @param flag \mpicode{true} if \mpiarg{key} defined, \mpicode{false} if not
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_get_string(MPI_Info info, const char *key, int *buflen, char *value, int *flag);
int PMPI_Info_get_string(MPI_Info info, const char *key, int *buflen, char *value, int *flag);


/*MPI_Info_get_valuelen*/

/**
 * @brief MPI function MPI_Info_get_valuelen
 * 
 * @param info info object
 * @param key key
 * @param valuelen length of value associated with \mpiarg{key}
 * @param flag \mpicode{true} if \mpiarg{key} defined, \mpicode{false} if not
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_get_valuelen(MPI_Info info, const char *key, int *valuelen, int *flag);
int PMPI_Info_get_valuelen(MPI_Info info, const char *key, int *valuelen, int *flag);


/*MPI_Info_set*/

/**
 * @brief MPI function MPI_Info_set
 * 
 * @param info info object
 * @param key key
 * @param value value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Info_set(MPI_Info info, const char *key, const char *value);
int PMPI_Info_set(MPI_Info info, const char *key, const char *value);


/*MPI_Init*/

/**
 * @brief MPI function MPI_Init
 * 
 * @param argc 
 * @param argv 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Init(int *argc, char ***argv);
int PMPI_Init(int *argc, char ***argv);


/*MPI_Init_thread*/

/**
 * @brief MPI function MPI_Init_thread
 * 
 * @param argc 
 * @param argv 
 * @param required desired level of thread support
 * @param provided provided level of thread support
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Init_thread(int *argc, char ***argv, int required, int *provided);
int PMPI_Init_thread(int *argc, char ***argv, int required, int *provided);


/*MPI_Initialized*/

/**
 * @brief MPI function MPI_Initialized
 * 
 * @param flag Flag is true if \mpifunc{MPI_INIT} has been called and false otherwise
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Initialized(int *flag);
int PMPI_Initialized(int *flag);


/*MPI_Intercomm_create*/

/**
 * @brief MPI function MPI_Intercomm_create
 * 
 * @param local_comm local intra-communicator
 * @param local_leader rank of local group leader in \mpiarg{local_comm}
 * @param peer_comm ``peer'' communicator; significant only at the \mpiarg{local_leader}
 * @param remote_leader rank of remote group leader in \mpiarg{peer_comm}; significant only at the \mpiarg{local_leader}
 * @param tag tag
 * @param newintercomm new inter-communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Intercomm_create(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm);
int PMPI_Intercomm_create(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm);


/*MPI_Intercomm_create_from_groups*/

/**
 * @brief MPI function MPI_Intercomm_create_from_groups
 * 
 * @param local_group local group
 * @param local_leader rank of local group leader in \mpiarg{local\_group}
 * @param remote_group remote group, significant only at \mpiarg{local\_leader}
 * @param remote_leader rank of remote group leader in \mpiarg{remote\_group}, significant only at \mpiarg{local\_leader}
 * @param stringtag unique idenitifier for this operation
 * @param info info object
 * @param errhandler error handler to be attached to new inter-communicator
 * @param newintercomm new inter-communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Intercomm_create_from_groups(MPI_Group local_group, int local_leader, MPI_Group remote_group, int remote_leader, const char *stringtag, MPI_Info info, MPI_Errhandler errhandler, MPI_Comm *newintercomm);
int PMPI_Intercomm_create_from_groups(MPI_Group local_group, int local_leader, MPI_Group remote_group, int remote_leader, const char *stringtag, MPI_Info info, MPI_Errhandler errhandler, MPI_Comm *newintercomm);


/*MPI_Intercomm_merge*/

/**
 * @brief MPI function MPI_Intercomm_merge
 * 
 * @param intercomm inter-communicator
 * @param high ordering of the local and remote groups in the new intra-communicator
 * @param newintracomm new intra-communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Intercomm_merge(MPI_Comm intercomm, int high, MPI_Comm *newintracomm);
int PMPI_Intercomm_merge(MPI_Comm intercomm, int high, MPI_Comm *newintracomm);


/*MPI_Iprobe*/

/**
 * @brief MPI function MPI_Iprobe
 * 
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param tag message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param flag \mpicode{true} if there is a matching message that can be received
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status);
int PMPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status);


/*MPI_Irecv*/

/**
 * @brief MPI function MPI_Irecv
 * 
 * @param buf initial address of receive buffer
 * @param count number of elements in receive buffer
 * @param datatype datatype of each receive buffer element
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param tag message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Irecv(void *buf, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Irecv(void *buf, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Ireduce*/

/**
 * @brief MPI function MPI_Ireduce
 * 
 * @param sendbuf address of send buffer
 * @param recvbuf address of receive buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of elements of send buffer
 * @param op reduce operation
 * @param root rank of root process
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ireduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm, MPI_Request *request);
int PMPI_Ireduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm, MPI_Request *request);


/*MPI_Ireduce_scatter*/

/**
 * @brief MPI function MPI_Ireduce_scatter
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array specifying the number of elements in result distributed to each process. This array must be identical on all calling processes.
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ireduce_scatter(const void *sendbuf, void *recvbuf, const MPI_Count recvcounts[], MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);
int PMPI_Ireduce_scatter(const void *sendbuf, void *recvbuf, const MPI_Count recvcounts[], MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);


/*MPI_Ireduce_scatter_block*/

/**
 * @brief MPI function MPI_Ireduce_scatter_block
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param recvcount element count per block
 * @param datatype datatype of elements of send and receive buffers
 * @param op operation
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ireduce_scatter_block(const void *sendbuf, void *recvbuf, MPI_Count recvcount, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);
int PMPI_Ireduce_scatter_block(const void *sendbuf, void *recvbuf, MPI_Count recvcount, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);


/*MPI_Irsend*/

/**
 * @brief MPI function MPI_Irsend
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Irsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Irsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Is_thread_main*/

/**
 * @brief MPI function MPI_Is_thread_main
 * 
 * @param flag true if calling thread is main thread, false otherwise
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Is_thread_main(int *flag);
int PMPI_Is_thread_main(int *flag);


/*MPI_Iscan*/

/**
 * @brief MPI function MPI_Iscan
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in input buffer
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iscan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);
int PMPI_Iscan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);


/*MPI_Iscatter*/

/**
 * @brief MPI function MPI_Iscatter
 * 
 * @param sendbuf address of send buffer
 * @param sendcount number of elements sent to each process
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype datatype of receive buffer elements
 * @param root rank of sending process
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iscatter(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);
int PMPI_Iscatter(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);


/*MPI_Iscatterv*/

/**
 * @brief MPI function MPI_Iscatterv
 * 
 * @param sendbuf address of send buffer
 * @param sendcounts nonnegative integer array (of length group size) specifying the number of elements to send to each rank
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{sendbuf}) from which to take the outgoing data to process \mpicode{i}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype datatype of receive buffer elements
 * @param root rank of sending process
 * @param comm communicator
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Iscatterv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint displs[], MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);
int PMPI_Iscatterv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint displs[], MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Request *request);


/*MPI_Isend*/

/**
 * @brief MPI function MPI_Isend
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Isend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Isend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Isendrecv*/

/**
 * @brief MPI function MPI_Isendrecv
 * 
 * @param sendbuf initial address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype datatype of each send buffer element
 * @param dest rank of destination
 * @param sendtag send tag
 * @param recvbuf initial address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype datatype of each receive buffer element
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param recvtag receive tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Isendrecv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, int dest, int sendtag, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm, MPI_Request *request);
int PMPI_Isendrecv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, int dest, int sendtag, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm, MPI_Request *request);


/*MPI_Isendrecv_replace*/

/**
 * @brief MPI function MPI_Isendrecv_replace
 * 
 * @param buf initial address of send and receive buffer
 * @param count number of elements in send and receive buffer
 * @param datatype type of elements in send and receive buffer
 * @param dest rank of destination
 * @param sendtag send message tag
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param recvtag receive message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Isendrecv_replace(void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int sendtag, int source, int recvtag, MPI_Comm comm, MPI_Request *request);
int PMPI_Isendrecv_replace(void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int sendtag, int source, int recvtag, MPI_Comm comm, MPI_Request *request);


/*MPI_Issend*/

/**
 * @brief MPI function MPI_Issend
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Issend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Issend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Keyval_create*/

/**
 * @brief MPI function MPI_Keyval_create
 * 
 * @param copy_fn Copy callback function for \mpiarg{keyval}
 * @param delete_fn Delete callback function for \mpiarg{keyval}
 * @param keyval key value for future access
 * @param extra_state Extra state for callback functions
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Keyval_create(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn, int *keyval, void *extra_state);
int PMPI_Keyval_create(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn, int *keyval, void *extra_state);


/*MPI_Keyval_free*/

/**
 * @brief MPI function MPI_Keyval_free
 * 
 * @param keyval Frees the integer key value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Keyval_free(int *keyval);
int PMPI_Keyval_free(int *keyval);


/*MPI_Lookup_name*/

/**
 * @brief MPI function MPI_Lookup_name
 * 
 * @param service_name a service name
 * @param info implementation-specific information
 * @param port_name a port name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Lookup_name(const char *service_name, MPI_Info info, char *port_name);
int PMPI_Lookup_name(const char *service_name, MPI_Info info, char *port_name);


/*MPI_Mprobe*/

/**
 * @brief MPI function MPI_Mprobe
 * 
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param tag message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param message returned message
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Mprobe(int source, int tag, MPI_Comm comm, MPI_Message *message, MPI_Status *status);
int PMPI_Mprobe(int source, int tag, MPI_Comm comm, MPI_Message *message, MPI_Status *status);


/*MPI_Mrecv*/

/**
 * @brief MPI function MPI_Mrecv
 * 
 * @param buf initial address of receive buffer
 * @param count number of elements in receive buffer
 * @param datatype datatype of each receive buffer element
 * @param message message
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Mrecv(void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Message *message, MPI_Status *status);
int PMPI_Mrecv(void *buf, MPI_Count count, MPI_Datatype datatype, MPI_Message *message, MPI_Status *status);


/*MPI_Neighbor_allgather*/

/**
 * @brief MPI function MPI_Neighbor_allgather
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcount number of elements received from each neighbor
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_allgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Neighbor_allgather(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Neighbor_allgather_init*/

/**
 * @brief MPI function MPI_Neighbor_allgather_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcount number of elements received from each neighbor
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_allgather_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Neighbor_allgather_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Neighbor_allgatherv*/

/**
 * @brief MPI function MPI_Neighbor_allgatherv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) containing the number of elements that are received from each neighbor
 * @param displs integer array (of length indegree). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_allgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Neighbor_allgatherv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Neighbor_allgatherv_init*/

/**
 * @brief MPI function MPI_Neighbor_allgatherv_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) containing the number of elements that are received from each neighbor
 * @param displs integer array (of length indegree). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_allgatherv_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Neighbor_allgatherv_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint displs[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Neighbor_alltoall*/

/**
 * @brief MPI function MPI_Neighbor_alltoall
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcount number of elements received from each neighbor
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_alltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Neighbor_alltoall(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Neighbor_alltoall_init*/

/**
 * @brief MPI function MPI_Neighbor_alltoall_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcount number of elements sent to each neighbor
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcount number of elements received from each neighbor
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_alltoall_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Neighbor_alltoall_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Neighbor_alltoallv*/

/**
 * @brief MPI function MPI_Neighbor_alltoallv
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length outdegree) specifying the number of elements to send to each neighbor
 * @param sdispls integer array (of length outdegree). Entry \mpicode{j} specifies the displacement (relative to \mpiarg{sendbuf}) from which to send the outgoing data to neighbor \mpicode{j}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) specifying the number of elements that are received from each neighbor
 * @param rdispls integer array (of length indegree). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_alltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm);
int PMPI_Neighbor_alltoallv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm);


/*MPI_Neighbor_alltoallv_init*/

/**
 * @brief MPI function MPI_Neighbor_alltoallv_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length outdegree) specifying the number of elements to send to each neighbor
 * @param sdispls integer array (of length outdegree). Entry \mpicode{j} specifies the displacement (relative to \mpiarg{sendbuf}) from which send the outgoing data to neighbor \mpicode{j}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) specifying the number of elements that are received from each neighbor
 * @param rdispls integer array (of length indegree). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtype datatype of receive buffer elements
 * @param comm communicator with topology structure
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_alltoallv_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Neighbor_alltoallv_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], MPI_Datatype sendtype, void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], MPI_Datatype recvtype, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Neighbor_alltoallw*/

/**
 * @brief MPI function MPI_Neighbor_alltoallw
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length outdegree) specifying the number of elements to send to each neighbor
 * @param sdispls integer array (of length outdegree). Entry \mpicode{j} specifies the displacement in bytes (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for neighbor \mpicode{j}
 * @param sendtypes array of datatypes (of length outdegree). Entry \mpicode{j} specifies the type of data to send to neighbor \mpicode{j}
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) specifying the number of elements that are received from each neighbor
 * @param rdispls integer array (of length indegree). Entry \mpicode{i} specifies the displacement in bytes (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtypes array of datatypes (of length indegree). Entry \mpicode{i} specifies the type of data received from neighbor \mpicode{i}
 * @param comm communicator with topology structure
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_alltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm);
int PMPI_Neighbor_alltoallw(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm);


/*MPI_Neighbor_alltoallw_init*/

/**
 * @brief MPI function MPI_Neighbor_alltoallw_init
 * 
 * @param sendbuf starting address of send buffer
 * @param sendcounts nonnegative integer array (of length outdegree) specifying the number of elements to send to each neighbor
 * @param sdispls integer array (of length outdegree). Entry \mpicode{j} specifies the displacement in bytes (relative to \mpiarg{sendbuf}) from which to take the outgoing data destined for neighbor \mpicode{j}
 * @param sendtypes array of datatypes (of length outdegree). Entry \mpicode{j} specifies the type of data to send to neighbor \mpicode{j}
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length indegree) specifying the number of elements that are received from each neighbor
 * @param rdispls integer array (of length indegree). Entry \mpicode{i} specifies the displacement in bytes (relative to \mpiarg{recvbuf}) at which to place the incoming data from neighbor \mpicode{i}
 * @param recvtypes array of datatypes (of length indegree). Entry \mpicode{i} specifies the type of data received from neighbor \mpicode{i}
 * @param comm communicator with topology structure
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Neighbor_alltoallw_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Neighbor_alltoallw_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint sdispls[], const MPI_Datatype sendtypes[], void *recvbuf, const MPI_Count recvcounts[], const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Op_commutative*/

/**
 * @brief MPI function MPI_Op_commutative
 * 
 * @param op operation
 * @param commute \mpicode{true} if \mpiarg{op} is commutative, \mpicode{false} otherwise
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Op_commutative(MPI_Op op, int *commute);
int PMPI_Op_commutative(MPI_Op op, int *commute);


/*MPI_Op_create*/

/**
 * @brief MPI function MPI_Op_create
 * 
 * @param user_fn user defined function
 * @param commute \mpicode{true} if commutative; \mpicode{false} otherwise.
 * @param op operation
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Op_create(MPI_User_function *user_fn, int commute, MPI_Op *op);
int PMPI_Op_create(MPI_User_function *user_fn, int commute, MPI_Op *op);


/*MPI_Op_free*/

/**
 * @brief MPI function MPI_Op_free
 * 
 * @param op operation
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Op_free(MPI_Op *op);
int PMPI_Op_free(MPI_Op *op);


/*MPI_Open_port*/

/**
 * @brief MPI function MPI_Open_port
 * 
 * @param info implementation-specific information on how to establish an address
 * @param port_name newly established port
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Open_port(MPI_Info info, char *port_name);
int PMPI_Open_port(MPI_Info info, char *port_name);


/*MPI_Pack*/

/**
 * @brief MPI function MPI_Pack
 * 
 * @param inbuf input buffer start
 * @param incount number of input data items
 * @param datatype datatype of each input data item
 * @param outbuf output buffer start
 * @param outsize output buffer size, in bytes
 * @param position current position in buffer, in bytes
 * @param comm communicator for packed message
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pack(const void *inbuf, MPI_Count incount, MPI_Datatype datatype, void *outbuf, MPI_Count outsize, MPI_Count *position, MPI_Comm comm);
int PMPI_Pack(const void *inbuf, MPI_Count incount, MPI_Datatype datatype, void *outbuf, MPI_Count outsize, MPI_Count *position, MPI_Comm comm);


/*MPI_Pack_external*/

/**
 * @brief MPI function MPI_Pack_external
 * 
 * @param datarep data representation
 * @param inbuf input buffer start
 * @param incount number of input data items
 * @param datatype datatype of each input data item
 * @param outbuf output buffer start
 * @param outsize output buffer size, in bytes
 * @param position current position in buffer, in bytes
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pack_external(const char datarep[], const void *inbuf, MPI_Count incount, MPI_Datatype datatype, void *outbuf, MPI_Count outsize, MPI_Count *position);
int PMPI_Pack_external(const char datarep[], const void *inbuf, MPI_Count incount, MPI_Datatype datatype, void *outbuf, MPI_Count outsize, MPI_Count *position);


/*MPI_Pack_external_size*/

/**
 * @brief MPI function MPI_Pack_external_size
 * 
 * @param datarep data representation
 * @param incount number of input data items
 * @param datatype datatype of each input data item
 * @param size output buffer size, in bytes
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pack_external_size(const char datarep[], MPI_Count incount, MPI_Datatype datatype, MPI_Count *size);
int PMPI_Pack_external_size(const char datarep[], MPI_Count incount, MPI_Datatype datatype, MPI_Count *size);


/*MPI_Pack_size*/

/**
 * @brief MPI function MPI_Pack_size
 * 
 * @param incount count argument to packing call
 * @param datatype datatype argument to packing call
 * @param comm communicator argument to packing call
 * @param size upper bound on size of packed message, in bytes
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pack_size(MPI_Count incount, MPI_Datatype datatype, MPI_Comm comm, MPI_Count *size);
int PMPI_Pack_size(MPI_Count incount, MPI_Datatype datatype, MPI_Comm comm, MPI_Count *size);


/*MPI_Parrived*/

/**
 * @brief MPI function MPI_Parrived
 * 
 * @param request partitioned communication request
 * @param partition partition to be tested
 * @param flag \mpicode{true} if operation completed on the specified partition, \mpicode{false} if not
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Parrived(MPI_Request request, int partition, int *flag);
int PMPI_Parrived(MPI_Request request, int partition, int *flag);


/*MPI_Pcontrol*/

/**
 * @brief MPI function MPI_Pcontrol
 * 
 * @param level Profiling level
 * @param  
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pcontrol(const int level, ... );
int PMPI_Pcontrol(const int level, ... );


/*MPI_Pready*/

/**
 * @brief MPI function MPI_Pready
 * 
 * @param partition partition to mark ready for transfer
 * @param request partitioned communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pready(int partition, MPI_Request request);
int PMPI_Pready(int partition, MPI_Request request);


/*MPI_Pready_list*/

/**
 * @brief MPI function MPI_Pready_list
 * 
 * @param length list length
 * @param array_of_partitions array of partitions
 * @param request partitioned communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pready_list(int length, const int array_of_partitions[], MPI_Request request);
int PMPI_Pready_list(int length, const int array_of_partitions[], MPI_Request request);


/*MPI_Pready_range*/

/**
 * @brief MPI function MPI_Pready_range
 * 
 * @param partition_low lowest partition ready for transfer
 * @param partition_high highest partition ready for transfer
 * @param request partitioned communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Pready_range(int partition_low, int partition_high, MPI_Request request);
int PMPI_Pready_range(int partition_low, int partition_high, MPI_Request request);


/*MPI_Precv_init*/

/**
 * @brief MPI function MPI_Precv_init
 * 
 * @param buf initial address of recv buffer
 * @param partitions number of partitions
 * @param count number of elements received per partition
 * @param datatype type of each element
 * @param source rank of source
 * @param tag message tag
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Precv_init(void *buf, int partitions, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Precv_init(void *buf, int partitions, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Probe*/

/**
 * @brief MPI function MPI_Probe
 * 
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param tag message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status);
int PMPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status);


/*MPI_Psend_init*/

/**
 * @brief MPI function MPI_Psend_init
 * 
 * @param buf initial address of send buffer
 * @param partitions number of partitions
 * @param count number of elements sent per partition
 * @param datatype type of each element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Psend_init(const void *buf, int partitions, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Psend_init(const void *buf, int partitions, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Publish_name*/

/**
 * @brief MPI function MPI_Publish_name
 * 
 * @param service_name a service name to associate with the port
 * @param info implementation-specific information
 * @param port_name a port name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Publish_name(const char *service_name, MPI_Info info, const char *port_name);
int PMPI_Publish_name(const char *service_name, MPI_Info info, const char *port_name);


/*MPI_Put*/

/**
 * @brief MPI function MPI_Put
 * 
 * @param origin_addr initial address of origin buffer
 * @param origin_count number of entries in origin buffer
 * @param origin_datatype datatype of each entry in origin buffer
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param win window object used for communication
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Put(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win);
int PMPI_Put(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win);


/*MPI_Query_thread*/

/**
 * @brief MPI function MPI_Query_thread
 * 
 * @param provided provided level of thread support
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Query_thread(int *provided);
int PMPI_Query_thread(int *provided);


/*MPI_Raccumulate*/

/**
 * @brief MPI function MPI_Raccumulate
 * 
 * @param origin_addr initial address of buffer
 * @param origin_count number of entries in buffer
 * @param origin_datatype datatype of each entry in origin buffer
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to beginning of target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param op reduce operation
 * @param win 
 * @param request \RMA/ request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Raccumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win, MPI_Request *request);
int PMPI_Raccumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win, MPI_Request *request);


/*MPI_Recv*/

/**
 * @brief MPI function MPI_Recv
 * 
 * @param buf initial address of receive buffer
 * @param count number of elements in receive buffer
 * @param datatype datatype of each receive buffer element
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param tag message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Recv(void *buf, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status);
int PMPI_Recv(void *buf, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status);


/*MPI_Recv_init*/

/**
 * @brief MPI function MPI_Recv_init
 * 
 * @param buf initial address of receive buffer
 * @param count number of elements received
 * @param datatype type of each element
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param tag message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Recv_init(void *buf, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Recv_init(void *buf, MPI_Count count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Reduce*/

/**
 * @brief MPI function MPI_Reduce
 * 
 * @param sendbuf address of send buffer
 * @param recvbuf address of receive buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of elements of send buffer
 * @param op reduce operation
 * @param root rank of root process
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Reduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm);
int PMPI_Reduce(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm);


/*MPI_Reduce_init*/

/**
 * @brief MPI function MPI_Reduce_init
 * 
 * @param sendbuf address of send buffer
 * @param recvbuf address of receive buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of elements of send buffer
 * @param op reduce operation
 * @param root rank of root process
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Reduce_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Reduce_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Reduce_local*/

/**
 * @brief MPI function MPI_Reduce_local
 * 
 * @param inbuf input buffer
 * @param inoutbuf combined input and output buffer
 * @param count number of elements in \mpiarg{inbuf} and \mpiarg{inoutbuf} buffers
 * @param datatype datatype of elements of \mpiarg{inbuf} and \mpiarg{inoutbuf} buffers
 * @param op operation
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Reduce_local(const void *inbuf, void *inoutbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op);
int PMPI_Reduce_local(const void *inbuf, void *inoutbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op);


/*MPI_Reduce_scatter*/

/**
 * @brief MPI function MPI_Reduce_scatter
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array (of length group size) specifying the number of elements of the result distributed to each process.
 * @param datatype datatype of elements of send and receive buffers
 * @param op operation
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Reduce_scatter(const void *sendbuf, void *recvbuf, const MPI_Count recvcounts[], MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int PMPI_Reduce_scatter(const void *sendbuf, void *recvbuf, const MPI_Count recvcounts[], MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);


/*MPI_Reduce_scatter_block*/

/**
 * @brief MPI function MPI_Reduce_scatter_block
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param recvcount element count per block
 * @param datatype datatype of elements of send and receive buffers
 * @param op operation
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Reduce_scatter_block(const void *sendbuf, void *recvbuf, MPI_Count recvcount, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int PMPI_Reduce_scatter_block(const void *sendbuf, void *recvbuf, MPI_Count recvcount, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);


/*MPI_Reduce_scatter_block_init*/

/**
 * @brief MPI function MPI_Reduce_scatter_block_init
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param recvcount element count per block
 * @param datatype datatype of elements of send and receive buffers
 * @param op operation
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Reduce_scatter_block_init(const void *sendbuf, void *recvbuf, MPI_Count recvcount, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Reduce_scatter_block_init(const void *sendbuf, void *recvbuf, MPI_Count recvcount, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Reduce_scatter_init*/

/**
 * @brief MPI function MPI_Reduce_scatter_init
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param recvcounts nonnegative integer array specifying the number of elements in result distributed to each process. This array must be identical on all calling processes.
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Reduce_scatter_init(const void *sendbuf, void *recvbuf, const MPI_Count recvcounts[], MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Reduce_scatter_init(const void *sendbuf, void *recvbuf, const MPI_Count recvcounts[], MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Register_datarep*/

/**
 * @brief MPI function MPI_Register_datarep
 * 
 * @param datarep data representation identifier
 * @param read_conversion_fn function invoked to convert from file representation to native representation
 * @param write_conversion_fn function invoked to convert from native representation to file representation
 * @param dtype_file_extent_fn function invoked to get the extent of a datatype as represented in the file
 * @param extra_state extra state
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Register_datarep(const char *datarep, MPI_Datarep_conversion_function *read_conversion_fn, MPI_Datarep_conversion_function *write_conversion_fn, MPI_Datarep_extent_function *dtype_file_extent_fn, void *extra_state);
int PMPI_Register_datarep(const char *datarep, MPI_Datarep_conversion_function *read_conversion_fn, MPI_Datarep_conversion_function *write_conversion_fn, MPI_Datarep_extent_function *dtype_file_extent_fn, void *extra_state);


/*MPI_Request_free*/

/**
 * @brief MPI function MPI_Request_free
 * 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Request_free(MPI_Request *request);
int PMPI_Request_free(MPI_Request *request);


/*MPI_Request_get_status*/

/**
 * @brief MPI function MPI_Request_get_status
 * 
 * @param request request
 * @param flag boolean flag, same as from \mpifunc{MPI_TEST}
 * @param status status object if flag is true
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Request_get_status(MPI_Request request, int *flag, MPI_Status *status);
int PMPI_Request_get_status(MPI_Request request, int *flag, MPI_Status *status);


/*MPI_Rget*/

/**
 * @brief MPI function MPI_Rget
 * 
 * @param origin_addr initial address of origin buffer
 * @param origin_count number of entries in origin buffer
 * @param origin_datatype datatype of each entry in origin buffer
 * @param target_rank rank of target
 * @param target_disp displacement from window start to the beginning of the target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param win window object used for communication
 * @param request \RMA/ request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Rget(void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win, MPI_Request *request);
int PMPI_Rget(void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win, MPI_Request *request);


/*MPI_Rget_accumulate*/

/**
 * @brief MPI function MPI_Rget_accumulate
 * 
 * @param origin_addr initial address of buffer
 * @param origin_count number of entries in origin buffer
 * @param origin_datatype datatype of each entry in origin buffer
 * @param result_addr initial address of result buffer
 * @param result_count number of entries in result buffer
 * @param result_datatype datatype of entries in result buffer
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to beginning of target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param op reduce operation
 * @param win 
 * @param request \RMA/ request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Rget_accumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, void *result_addr, MPI_Count result_count, MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win, MPI_Request *request);
int PMPI_Rget_accumulate(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, void *result_addr, MPI_Count result_count, MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win, MPI_Request *request);


/*MPI_Rput*/

/**
 * @brief MPI function MPI_Rput
 * 
 * @param origin_addr initial address of origin buffer
 * @param origin_count number of entries in origin buffer
 * @param origin_datatype datatype of each entry in origin buffer
 * @param target_rank rank of target
 * @param target_disp displacement from start of window to target buffer
 * @param target_count number of entries in target buffer
 * @param target_datatype datatype of each entry in target buffer
 * @param win window object used for communication
 * @param request \RMA/ request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Rput(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win, MPI_Request *request);
int PMPI_Rput(const void *origin_addr, MPI_Count origin_count, MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp, MPI_Count target_count, MPI_Datatype target_datatype, MPI_Win win, MPI_Request *request);


/*MPI_Rsend*/

/**
 * @brief MPI function MPI_Rsend
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Rsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
int PMPI_Rsend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);


/*MPI_Rsend_init*/

/**
 * @brief MPI function MPI_Rsend_init
 * 
 * @param buf initial address of send buffer
 * @param count number of elements sent
 * @param datatype type of each element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Rsend_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Rsend_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Scan*/

/**
 * @brief MPI function MPI_Scan
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in input buffer
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Scan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int PMPI_Scan(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);


/*MPI_Scan_init*/

/**
 * @brief MPI function MPI_Scan_init
 * 
 * @param sendbuf starting address of send buffer
 * @param recvbuf starting address of receive buffer
 * @param count number of elements in input buffer
 * @param datatype datatype of elements of input buffer
 * @param op operation
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Scan_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Scan_init(const void *sendbuf, void *recvbuf, MPI_Count count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Scatter*/

/**
 * @brief MPI function MPI_Scatter
 * 
 * @param sendbuf address of send buffer
 * @param sendcount number of elements sent to each process
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype datatype of receive buffer elements
 * @param root rank of sending process
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Scatter(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);
int PMPI_Scatter(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);


/*MPI_Scatter_init*/

/**
 * @brief MPI function MPI_Scatter_init
 * 
 * @param sendbuf address of send buffer
 * @param sendcount number of elements sent to each process
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype datatype of receive buffer elements
 * @param root rank of sending process
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Scatter_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Scatter_init(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Scatterv*/

/**
 * @brief MPI function MPI_Scatterv
 * 
 * @param sendbuf address of send buffer
 * @param sendcounts nonnegative integer array (of length group size) specifying the number of elements to send to each rank
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{sendbuf}) from which to take the outgoing data to process \mpicode{i}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype datatype of receive buffer elements
 * @param root rank of sending process
 * @param comm communicator
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Scatterv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint displs[], MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);
int PMPI_Scatterv(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint displs[], MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);


/*MPI_Scatterv_init*/

/**
 * @brief MPI function MPI_Scatterv_init
 * 
 * @param sendbuf address of send buffer
 * @param sendcounts nonnegative integer array (of length group size) specifying the number of elements to send to each rank
 * @param displs integer array (of length group size). Entry \mpicode{i} specifies the displacement (relative to \mpiarg{sendbuf}) from which to take the outgoing data to process \mpicode{i}
 * @param sendtype datatype of send buffer elements
 * @param recvbuf address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype datatype of receive buffer elements
 * @param root rank of sending process
 * @param comm communicator
 * @param info info argument
 * @param request communication request
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Scatterv_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint displs[], MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);
int PMPI_Scatterv_init(const void *sendbuf, const MPI_Count sendcounts[], const MPI_Aint displs[], MPI_Datatype sendtype, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm, MPI_Info info, MPI_Request *request);


/*MPI_Send*/

/**
 * @brief MPI function MPI_Send
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Send(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
int PMPI_Send(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);


/*MPI_Send_init*/

/**
 * @brief MPI function MPI_Send_init
 * 
 * @param buf initial address of send buffer
 * @param count number of elements sent
 * @param datatype type of each element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Send_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Send_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Sendrecv*/

/**
 * @brief MPI function MPI_Sendrecv
 * 
 * @param sendbuf initial address of send buffer
 * @param sendcount number of elements in send buffer
 * @param sendtype type of elements in send buffer
 * @param dest rank of destination
 * @param sendtag send tag
 * @param recvbuf initial address of receive buffer
 * @param recvcount number of elements in receive buffer
 * @param recvtype type of elements receive buffer element
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param recvtag receive tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Sendrecv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, int dest, int sendtag, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm, MPI_Status *status);
int PMPI_Sendrecv(const void *sendbuf, MPI_Count sendcount, MPI_Datatype sendtype, int dest, int sendtag, void *recvbuf, MPI_Count recvcount, MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm, MPI_Status *status);


/*MPI_Sendrecv_replace*/

/**
 * @brief MPI function MPI_Sendrecv_replace
 * 
 * @param buf initial address of send and receive buffer
 * @param count number of elements in send and receive buffer
 * @param datatype type of elements in send and receive buffer
 * @param dest rank of destination
 * @param sendtag send message tag
 * @param source rank of source or \mpiconst{MPI_ANY_SOURCE}
 * @param recvtag receive message tag or \mpiconst{MPI_ANY_TAG}
 * @param comm 
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Sendrecv_replace(void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int sendtag, int source, int recvtag, MPI_Comm comm, MPI_Status *status);
int PMPI_Sendrecv_replace(void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int sendtag, int source, int recvtag, MPI_Comm comm, MPI_Status *status);


/*MPI_Session_call_errhandler*/

/**
 * @brief MPI function MPI_Session_call_errhandler
 * 
 * @param session session with error handler
 * @param errorcode 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_call_errhandler(MPI_Session session, int errorcode);
int PMPI_Session_call_errhandler(MPI_Session session, int errorcode);


/*MPI_Session_create_errhandler*/

/**
 * @brief MPI function MPI_Session_create_errhandler
 * 
 * @param session_errhandler_fn user defined error handling procedure
 * @param errhandler 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_create_errhandler(MPI_Session_errhandler_function *session_errhandler_fn, MPI_Errhandler *errhandler);
int PMPI_Session_create_errhandler(MPI_Session_errhandler_function *session_errhandler_fn, MPI_Errhandler *errhandler);


/*MPI_Session_finalize*/

/**
 * @brief MPI function MPI_Session_finalize
 * 
 * @param session session to be finalized
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_finalize(MPI_Session *session);
int PMPI_Session_finalize(MPI_Session *session);


/*MPI_Session_get_errhandler*/

/**
 * @brief MPI function MPI_Session_get_errhandler
 * 
 * @param session session
 * @param errhandler error handler currently associated with session
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_get_errhandler(MPI_Session session, MPI_Errhandler *errhandler);
int PMPI_Session_get_errhandler(MPI_Session session, MPI_Errhandler *errhandler);


/*MPI_Session_get_info*/

/**
 * @brief MPI function MPI_Session_get_info
 * 
 * @param session session
 * @param info_used see explanation below
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_get_info(MPI_Session session, MPI_Info *info_used);
int PMPI_Session_get_info(MPI_Session session, MPI_Info *info_used);


/*MPI_Session_get_nth_pset*/

/**
 * @brief MPI function MPI_Session_get_nth_pset
 * 
 * @param session session
 * @param info info object
 * @param n index of the desired process set name
 * @param pset_len length of the pset\_name argument
 * @param pset_name name of the \mpiarg{n}th process set
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_get_nth_pset(MPI_Session session, MPI_Info info, int n, int *pset_len, char *pset_name);
int PMPI_Session_get_nth_pset(MPI_Session session, MPI_Info info, int n, int *pset_len, char *pset_name);


/*MPI_Session_get_num_psets*/

/**
 * @brief MPI function MPI_Session_get_num_psets
 * 
 * @param session session
 * @param info info object
 * @param npset_names number of available process sets
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_get_num_psets(MPI_Session session, MPI_Info info, int *npset_names);
int PMPI_Session_get_num_psets(MPI_Session session, MPI_Info info, int *npset_names);


/*MPI_Session_get_pset_info*/

/**
 * @brief MPI function MPI_Session_get_pset_info
 * 
 * @param session session
 * @param pset_name name of process set
 * @param info info object containing information about the given process set
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_get_pset_info(MPI_Session session, const char *pset_name, MPI_Info *info);
int PMPI_Session_get_pset_info(MPI_Session session, const char *pset_name, MPI_Info *info);


/*MPI_Session_init*/

/**
 * @brief MPI function MPI_Session_init
 * 
 * @param info info object to specify thread support level and \MPI/ implementation specific resources
 * @param errhandler error handler to invoke in the event that an error is encountered during this function call
 * @param session new session
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_init(MPI_Info info, MPI_Errhandler errhandler, MPI_Session *session);
int PMPI_Session_init(MPI_Info info, MPI_Errhandler errhandler, MPI_Session *session);


/*MPI_Session_set_errhandler*/

/**
 * @brief MPI function MPI_Session_set_errhandler
 * 
 * @param session session
 * @param errhandler new error handler for session
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Session_set_errhandler(MPI_Session session, MPI_Errhandler errhandler);
int PMPI_Session_set_errhandler(MPI_Session session, MPI_Errhandler errhandler);


/*MPI_Ssend*/

/**
 * @brief MPI function MPI_Ssend
 * 
 * @param buf initial address of send buffer
 * @param count number of elements in send buffer
 * @param datatype datatype of each send buffer element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ssend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);
int PMPI_Ssend(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm);


/*MPI_Ssend_init*/

/**
 * @brief MPI function MPI_Ssend_init
 * 
 * @param buf initial address of send buffer
 * @param count number of elements sent
 * @param datatype type of each element
 * @param dest rank of destination
 * @param tag message tag
 * @param comm 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Ssend_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);
int PMPI_Ssend_init(const void *buf, MPI_Count count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request);


/*MPI_Start*/

/**
 * @brief MPI function MPI_Start
 * 
 * @param request 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Start(MPI_Request *request);
int PMPI_Start(MPI_Request *request);


/*MPI_Startall*/

/**
 * @brief MPI function MPI_Startall
 * 
 * @param count list length
 * @param array_of_requests array of requests
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Startall(int count, MPI_Request array_of_requests[]);
int PMPI_Startall(int count, MPI_Request array_of_requests[]);


/*MPI_Status_set_cancelled*/

/**
 * @brief MPI function MPI_Status_set_cancelled
 * 
 * @param status status with which to associate cancel flag
 * @param flag if true, indicates request was cancelled
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Status_set_cancelled(MPI_Status *status, int flag);
int PMPI_Status_set_cancelled(MPI_Status *status, int flag);


/*MPI_Status_set_elements*/

/**
 * @brief MPI function MPI_Status_set_elements
 * 
 * @param status status with which to associate count
 * @param datatype datatype associated with count
 * @param count number of elements to associate with status
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Status_set_elements(MPI_Status *status, MPI_Datatype datatype, MPI_Count count);
int PMPI_Status_set_elements(MPI_Status *status, MPI_Datatype datatype, MPI_Count count);


/*MPI_Status_set_elements_x*/

/**
 * @brief MPI function MPI_Status_set_elements_x
 * 
 * @param status status with which to associate count
 * @param datatype datatype associated with count
 * @param count number of elements to associate with status
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Status_set_elements_x(MPI_Status *status, MPI_Datatype datatype, MPI_Count count);
int PMPI_Status_set_elements_x(MPI_Status *status, MPI_Datatype datatype, MPI_Count count);


/*MPI_T_category_changed*/

/**
 * @brief MPI function MPI_T_category_changed
 * 
 * @param update_number update number
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_changed(int *update_number);
int PMPI_T_category_changed(int *update_number);


/*MPI_T_category_get_categories*/

/**
 * @brief MPI function MPI_T_category_get_categories
 * 
 * @param cat_index index of the category to be queried, in the range from $0$ to $\mpiarg{num_cat}-1$
 * @param len the length of the indices array
 * @param indices an integer array of size \mpiarg{len}, indicating category indices
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_categories(int cat_index, int len, int indices[]);
int PMPI_T_category_get_categories(int cat_index, int len, int indices[]);


/*MPI_T_category_get_cvars*/

/**
 * @brief MPI function MPI_T_category_get_cvars
 * 
 * @param cat_index index of the category to be queried, in the range from $0$ to $\mpiarg{num_cat}-1$
 * @param len the length of the indices array
 * @param indices an integer array of size \mpiarg{len}, indicating control variable indices
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_cvars(int cat_index, int len, int indices[]);
int PMPI_T_category_get_cvars(int cat_index, int len, int indices[]);


/*MPI_T_category_get_events*/

/**
 * @brief MPI function MPI_T_category_get_events
 * 
 * @param cat_index index of the category to be queried, in the range from $0$ to $\mpiarg{num_cat}-1$
 * @param len the length of the indices array
 * @param indices an integer array of size \mpiarg{len}, indicating event type indices
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_events(int cat_index, int len, int indices[]);
int PMPI_T_category_get_events(int cat_index, int len, int indices[]);


/*MPI_T_category_get_index*/

/**
 * @brief MPI function MPI_T_category_get_index
 * 
 * @param name the name of the category
 * @param cat_index the index of the category
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_index(const char *name, int *cat_index);
int PMPI_T_category_get_index(const char *name, int *cat_index);


/*MPI_T_category_get_info*/

/**
 * @brief MPI function MPI_T_category_get_info
 * 
 * @param cat_index index of the category to be queried
 * @param name buffer to return the string containing the name of the category
 * @param name_len length of the string and/or buffer for \mpiarg{name}
 * @param desc buffer to return the string containing the description of the category
 * @param desc_len length of the string and/or buffer for \mpiarg{desc}
 * @param num_cvars number of control variables in the category
 * @param num_pvars number of performance variables in the category
 * @param num_categories number of  categories contained in the category
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_info(int cat_index, char *name, int *name_len, char *desc, int *desc_len, int *num_cvars, int *num_pvars, int *num_categories);
int PMPI_T_category_get_info(int cat_index, char *name, int *name_len, char *desc, int *desc_len, int *num_cvars, int *num_pvars, int *num_categories);


/*MPI_T_category_get_num*/

/**
 * @brief MPI function MPI_T_category_get_num
 * 
 * @param num_cat current number of categories
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_num(int *num_cat);
int PMPI_T_category_get_num(int *num_cat);


/*MPI_T_category_get_num_events*/

/**
 * @brief MPI function MPI_T_category_get_num_events
 * 
 * @param cat_index index of the category to be queried
 * @param num_events number of event types in the category
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_num_events(int cat_index, int *num_events);
int PMPI_T_category_get_num_events(int cat_index, int *num_events);


/*MPI_T_category_get_pvars*/

/**
 * @brief MPI function MPI_T_category_get_pvars
 * 
 * @param cat_index index of the category to be queried, in the range from $0$ to $\mpiarg{num_cat}-1$
 * @param len the length of the indices array
 * @param indices an integer array of size \mpiarg{len}, indicating performance variable indices
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_category_get_pvars(int cat_index, int len, int indices[]);
int PMPI_T_category_get_pvars(int cat_index, int len, int indices[]);


/*MPI_T_cvar_get_index*/

/**
 * @brief MPI function MPI_T_cvar_get_index
 * 
 * @param name name of the control variable
 * @param cvar_index index of the control variable
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_cvar_get_index(const char *name, int *cvar_index);
int PMPI_T_cvar_get_index(const char *name, int *cvar_index);


/*MPI_T_cvar_get_info*/

/**
 * @brief MPI function MPI_T_cvar_get_info
 * 
 * @param cvar_index index of the control variable to be queried, value between $0$ and $\mpiarg{num_cvar}-1$
 * @param name buffer to return the string containing the name of the control variable
 * @param name_len length of the string and/or buffer for \mpiarg{name}
 * @param verbosity verbosity level of this variable
 * @param datatype \mpi/ datatype of the information stored in the control variable
 * @param enumtype optional descriptor for enumeration information
 * @param desc buffer to return the string containing a description of the control variable
 * @param desc_len length of the string and/or buffer for \mpiarg{desc}
 * @param bind type of \mpi/ object to which this variable must be bound
 * @param scope scope of when changes to this variable are possible
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_cvar_get_info(int cvar_index, char *name, int *name_len, int *verbosity, MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len, int *bind, int *scope);
int PMPI_T_cvar_get_info(int cvar_index, char *name, int *name_len, int *verbosity, MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len, int *bind, int *scope);


/*MPI_T_cvar_get_num*/

/**
 * @brief MPI function MPI_T_cvar_get_num
 * 
 * @param num_cvar returns number of control variables
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_cvar_get_num(int *num_cvar);
int PMPI_T_cvar_get_num(int *num_cvar);


/*MPI_T_cvar_handle_alloc*/

/**
 * @brief MPI function MPI_T_cvar_handle_alloc
 * 
 * @param cvar_index index of control variable for which handle is to be allocated
 * @param obj_handle reference to a handle of the \mpi/ object to which this variable is supposed to be bound
 * @param handle allocated handle
 * @param count number of elements used to represent this variable
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_cvar_handle_alloc(int cvar_index, void *obj_handle, MPI_T_cvar_handle *handle, int *count);
int PMPI_T_cvar_handle_alloc(int cvar_index, void *obj_handle, MPI_T_cvar_handle *handle, int *count);


/*MPI_T_cvar_handle_free*/

/**
 * @brief MPI function MPI_T_cvar_handle_free
 * 
 * @param handle handle to be freed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_cvar_handle_free(MPI_T_cvar_handle *handle);
int PMPI_T_cvar_handle_free(MPI_T_cvar_handle *handle);


/*MPI_T_cvar_read*/

/**
 * @brief MPI function MPI_T_cvar_read
 * 
 * @param handle handle to the control variable to be read
 * @param buf initial address of storage location for variable value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_cvar_read(MPI_T_cvar_handle handle, void *buf);
int PMPI_T_cvar_read(MPI_T_cvar_handle handle, void *buf);


/*MPI_T_cvar_write*/

/**
 * @brief MPI function MPI_T_cvar_write
 * 
 * @param handle handle to the control variable to be written
 * @param buf initial address of storage location for variable value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_cvar_write(MPI_T_cvar_handle handle, const void *buf);
int PMPI_T_cvar_write(MPI_T_cvar_handle handle, const void *buf);


/*MPI_T_enum_get_info*/

/**
 * @brief MPI function MPI_T_enum_get_info
 * 
 * @param enumtype enumeration to be queried
 * @param num number of discrete values represented by this enumeration
 * @param name buffer to return the string containing the name of the enumeration item
 * @param name_len length of the string and/or buffer for \mpiarg{name}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len);
int PMPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len);


/*MPI_T_enum_get_item*/

/**
 * @brief MPI function MPI_T_enum_get_item
 * 
 * @param enumtype enumeration to be queried
 * @param index number of the value to be queried in this enumeration
 * @param value variable value
 * @param name buffer to return the string containing the name of the enumeration item
 * @param name_len length of the string and/or buffer for \mpiarg{name}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_enum_get_item(MPI_T_enum enumtype, int index, int *value, char *name, int *name_len);
int PMPI_T_enum_get_item(MPI_T_enum enumtype, int index, int *value, char *name, int *name_len);


/*MPI_T_event_callback_get_info*/

/**
 * @brief MPI function MPI_T_event_callback_get_info
 * 
 * @param event_registration event registration
 * @param cb_safety callback safety level
 * @param info_used info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_callback_get_info(MPI_T_event_registration event_registration, MPI_T_cb_safety cb_safety, MPI_Info *info_used);
int PMPI_T_event_callback_get_info(MPI_T_event_registration event_registration, MPI_T_cb_safety cb_safety, MPI_Info *info_used);


/*MPI_T_event_callback_set_info*/

/**
 * @brief MPI function MPI_T_event_callback_set_info
 * 
 * @param event_registration event registration
 * @param cb_safety callback safety level
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_callback_set_info(MPI_T_event_registration event_registration, MPI_T_cb_safety cb_safety, MPI_Info info);
int PMPI_T_event_callback_set_info(MPI_T_event_registration event_registration, MPI_T_cb_safety cb_safety, MPI_Info info);


/*MPI_T_event_copy*/

/**
 * @brief MPI function MPI_T_event_copy
 * 
 * @param event_instance event instance provided to the callback function
 * @param buffer user-allocated buffer for event data
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_copy(MPI_T_event_instance event_instance, void *buffer);
int PMPI_T_event_copy(MPI_T_event_instance event_instance, void *buffer);


/*MPI_T_event_get_index*/

/**
 * @brief MPI function MPI_T_event_get_index
 * 
 * @param name name of the event type
 * @param event_index index of the event type
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_get_index(const char *name, int *event_index);
int PMPI_T_event_get_index(const char *name, int *event_index);


/*MPI_T_event_get_info*/

/**
 * @brief MPI function MPI_T_event_get_info
 * 
 * @param event_index index of the event type to be queried between $0$ and $\mpiarg{num_events}-1$
 * @param name buffer to return the string containing the name of the event type
 * @param name_len length of the string and/or buffer for \mpiarg{name}
 * @param verbosity verbosity level of this event type
 * @param array_of_datatypes array of \mpi/ basic datatypes used to encode the event data
 * @param array_of_displacements array of byte displacements of the elements in the event buffer
 * @param num_elements length of \mpiarg{array\_of\_datatypes} and \mpiarg{array\_of\_displacements} arrays
 * @param enumtype optional descriptor for enumeration information
 * @param info optional info object
 * @param desc buffer to return the string containing a description of the event type
 * @param desc_len length of the string and/or buffer for \mpiarg{desc}
 * @param bind type of \mpi/ object to which an event of this type must be bound
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_get_info(int event_index, char *name, int *name_len, int *verbosity, MPI_Datatype array_of_datatypes[], MPI_Aint array_of_displacements[], int *num_elements, MPI_T_enum *enumtype, MPI_Info *info, char *desc, int *desc_len, int *bind);
int PMPI_T_event_get_info(int event_index, char *name, int *name_len, int *verbosity, MPI_Datatype array_of_datatypes[], MPI_Aint array_of_displacements[], int *num_elements, MPI_T_enum *enumtype, MPI_Info *info, char *desc, int *desc_len, int *bind);


/*MPI_T_event_get_num*/

/**
 * @brief MPI function MPI_T_event_get_num
 * 
 * @param num_events returns number of event types
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_get_num(int *num_events);
int PMPI_T_event_get_num(int *num_events);


/*MPI_T_event_get_source*/

/**
 * @brief MPI function MPI_T_event_get_source
 * 
 * @param event_instance event instance provided to the callback function
 * @param source_index index identifying the source
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_get_source(MPI_T_event_instance event_instance, int *source_index);
int PMPI_T_event_get_source(MPI_T_event_instance event_instance, int *source_index);


/*MPI_T_event_get_timestamp*/

/**
 * @brief MPI function MPI_T_event_get_timestamp
 * 
 * @param event_instance event instance provided to the callback function
 * @param event_timestamp timestamp the event was observed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_get_timestamp(MPI_T_event_instance event_instance, MPI_Count *event_timestamp);
int PMPI_T_event_get_timestamp(MPI_T_event_instance event_instance, MPI_Count *event_timestamp);


/*MPI_T_event_handle_alloc*/

/**
 * @brief MPI function MPI_T_event_handle_alloc
 * 
 * @param event_index index of event type for which the registration handle is to be allocated
 * @param obj_handle reference to a handle of the \mpi/ object to which this event is supposed to be bound
 * @param info info object
 * @param event_registration event registration
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_handle_alloc(int event_index, void *obj_handle, MPI_Info info, MPI_T_event_registration *event_registration);
int PMPI_T_event_handle_alloc(int event_index, void *obj_handle, MPI_Info info, MPI_T_event_registration *event_registration);


/*MPI_T_event_handle_free*/

/**
 * @brief MPI function MPI_T_event_handle_free
 * 
 * @param event_registration event registration
 * @param user_data pointer to a user-controlled buffer
 * @param free_cb_function pointer to user-defined callback function
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_handle_free(MPI_T_event_registration event_registration, void *user_data, MPI_T_event_free_cb_function free_cb_function);
int PMPI_T_event_handle_free(MPI_T_event_registration event_registration, void *user_data, MPI_T_event_free_cb_function free_cb_function);


/*MPI_T_event_handle_get_info*/

/**
 * @brief MPI function MPI_T_event_handle_get_info
 * 
 * @param event_registration event registration
 * @param info_used info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_handle_get_info(MPI_T_event_registration event_registration, MPI_Info *info_used);
int PMPI_T_event_handle_get_info(MPI_T_event_registration event_registration, MPI_Info *info_used);


/*MPI_T_event_handle_set_info*/

/**
 * @brief MPI function MPI_T_event_handle_set_info
 * 
 * @param event_registration event registration
 * @param info info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_handle_set_info(MPI_T_event_registration event_registration, MPI_Info info);
int PMPI_T_event_handle_set_info(MPI_T_event_registration event_registration, MPI_Info info);


/*MPI_T_event_read*/

/**
 * @brief MPI function MPI_T_event_read
 * 
 * @param event_instance event-instance handle provided to the callback function
 * @param element_index index into the array of datatypes of the item to be queried
 * @param buffer pointer to a memory location to store the item data
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_read(MPI_T_event_instance event_instance, int element_index, void *buffer);
int PMPI_T_event_read(MPI_T_event_instance event_instance, int element_index, void *buffer);


/*MPI_T_event_register_callback*/

/**
 * @brief MPI function MPI_T_event_register_callback
 * 
 * @param event_registration event registration
 * @param cb_safety maximum callback safety level
 * @param info info object
 * @param user_data pointer to a user-controlled buffer
 * @param event_cb_function pointer to user-defined callback function
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_register_callback(MPI_T_event_registration event_registration, MPI_T_cb_safety cb_safety, MPI_Info info, void *user_data, MPI_T_event_cb_function event_cb_function);
int PMPI_T_event_register_callback(MPI_T_event_registration event_registration, MPI_T_cb_safety cb_safety, MPI_Info info, void *user_data, MPI_T_event_cb_function event_cb_function);


/*MPI_T_event_set_dropped_handler*/

/**
 * @brief MPI function MPI_T_event_set_dropped_handler
 * 
 * @param event_registration valid event registration
 * @param dropped_cb_function pointer to user-defined callback function
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_event_set_dropped_handler(MPI_T_event_registration event_registration, MPI_T_event_dropped_cb_function dropped_cb_function);
int PMPI_T_event_set_dropped_handler(MPI_T_event_registration event_registration, MPI_T_event_dropped_cb_function dropped_cb_function);


/*MPI_T_finalize*/

/**
 * @brief MPI function MPI_T_finalize
 * 
 *
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_finalize();
int PMPI_T_finalize();


/*MPI_T_init_thread*/

/**
 * @brief MPI function MPI_T_init_thread
 * 
 * @param required desired level of thread support
 * @param provided provided level of thread support
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_init_thread(int required, int *provided);
int PMPI_T_init_thread(int required, int *provided);


/*MPI_T_pvar_get_index*/

/**
 * @brief MPI function MPI_T_pvar_get_index
 * 
 * @param name the name of the performance variable
 * @param var_class the class of the performance variable
 * @param pvar_index the index of the performance variable
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_get_index(const char *name, int var_class, int *pvar_index);
int PMPI_T_pvar_get_index(const char *name, int var_class, int *pvar_index);


/*MPI_T_pvar_get_info*/

/**
 * @brief MPI function MPI_T_pvar_get_info
 * 
 * @param pvar_index index of the performance variable to be queried between $0$ and $\mpiarg{num_pvar}-1$
 * @param name buffer to return the string containing the name of the performance variable
 * @param name_len length of the string and/or buffer for \mpiarg{name}
 * @param verbosity verbosity level of this variable
 * @param var_class class of performance variable
 * @param datatype \mpi/ datatype of the information stored in the performance variable
 * @param enumtype optional descriptor for enumeration information
 * @param desc buffer to return the string containing a description of the performance variable
 * @param desc_len length of the string and/or buffer for \mpiarg{desc}
 * @param bind type of \mpi/ object to which this variable must be bound
 * @param readonly flag indicating whether the variable can be written/reset
 * @param continuous flag indicating whether the variable can be started and stopped or is continuously active
 * @param atomic flag indicating whether the variable can be atomically read and reset
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_get_info(int pvar_index, char *name, int *name_len, int *verbosity, int *var_class, MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len, int *bind, int *readonly, int *continuous, int *atomic);
int PMPI_T_pvar_get_info(int pvar_index, char *name, int *name_len, int *verbosity, int *var_class, MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc, int *desc_len, int *bind, int *readonly, int *continuous, int *atomic);


/*MPI_T_pvar_get_num*/

/**
 * @brief MPI function MPI_T_pvar_get_num
 * 
 * @param num_pvar returns number of performance variables
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_get_num(int *num_pvar);
int PMPI_T_pvar_get_num(int *num_pvar);


/*MPI_T_pvar_handle_alloc*/

/**
 * @brief MPI function MPI_T_pvar_handle_alloc
 * 
 * @param pe_session identifier of performance experiment session
 * @param pvar_index index of performance variable for which handle is to be allocated
 * @param obj_handle reference to a handle of the \mpi/ object to which this variable is supposed to be bound
 * @param handle allocated handle
 * @param count number of elements used to represent this variable
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_handle_alloc(MPI_T_pvar_session pe_session, int pvar_index, void *obj_handle, MPI_T_pvar_handle *handle, int *count);
int PMPI_T_pvar_handle_alloc(MPI_T_pvar_session pe_session, int pvar_index, void *obj_handle, MPI_T_pvar_handle *handle, int *count);


/*MPI_T_pvar_handle_free*/

/**
 * @brief MPI function MPI_T_pvar_handle_free
 * 
 * @param pe_session identifier of performance experiment session
 * @param handle handle to be freed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_handle_free(MPI_T_pvar_session pe_session, MPI_T_pvar_handle *handle);
int PMPI_T_pvar_handle_free(MPI_T_pvar_session pe_session, MPI_T_pvar_handle *handle);


/*MPI_T_pvar_read*/

/**
 * @brief MPI function MPI_T_pvar_read
 * 
 * @param pe_session identifier of performance experiment session
 * @param handle handle of a performance variable
 * @param buf initial address of storage location for variable value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_read(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle, void *buf);
int PMPI_T_pvar_read(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle, void *buf);


/*MPI_T_pvar_readreset*/

/**
 * @brief MPI function MPI_T_pvar_readreset
 * 
 * @param pe_session identifier of performance experiment session
 * @param handle handle of a performance variable
 * @param buf initial address of storage location for variable value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_readreset(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle, void *buf);
int PMPI_T_pvar_readreset(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle, void *buf);


/*MPI_T_pvar_reset*/

/**
 * @brief MPI function MPI_T_pvar_reset
 * 
 * @param pe_session identifier of performance experiment session
 * @param handle handle of a performance variable
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_reset(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle);
int PMPI_T_pvar_reset(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle);


/*MPI_T_pvar_session_create*/

/**
 * @brief MPI function MPI_T_pvar_session_create
 * 
 * @param pe_session identifier of performance experiment session
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_session_create(MPI_T_pvar_session *pe_session);
int PMPI_T_pvar_session_create(MPI_T_pvar_session *pe_session);


/*MPI_T_pvar_session_free*/

/**
 * @brief MPI function MPI_T_pvar_session_free
 * 
 * @param pe_session identifier of performance experiment session
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_session_free(MPI_T_pvar_session *pe_session);
int PMPI_T_pvar_session_free(MPI_T_pvar_session *pe_session);


/*MPI_T_pvar_start*/

/**
 * @brief MPI function MPI_T_pvar_start
 * 
 * @param pe_session identifier of performance experiment session
 * @param handle handle of a performance variable
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_start(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle);
int PMPI_T_pvar_start(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle);


/*MPI_T_pvar_stop*/

/**
 * @brief MPI function MPI_T_pvar_stop
 * 
 * @param pe_session identifier of performance experiment session
 * @param handle handle of a performance variable
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_stop(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle);
int PMPI_T_pvar_stop(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle);


/*MPI_T_pvar_write*/

/**
 * @brief MPI function MPI_T_pvar_write
 * 
 * @param pe_session identifier of performance experiment session
 * @param handle handle of a performance variable
 * @param buf initial address of storage location for variable value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_pvar_write(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle, const void *buf);
int PMPI_T_pvar_write(MPI_T_pvar_session pe_session, MPI_T_pvar_handle handle, const void *buf);


/*MPI_T_source_get_info*/

/**
 * @brief MPI function MPI_T_source_get_info
 * 
 * @param source_index index of the source to be queried between $0$ and $\mpiarg{num_sources}-1$
 * @param name buffer to return the string containing the name of the source
 * @param name_len length of the string and/or buffer for \mpiarg{name}
 * @param desc buffer to return the string containing the description of the source
 * @param desc_len length of the string and/or buffer for \mpiarg{desc}
 * @param ordering flag indicating chronological ordering guarantees given by the source
 * @param ticks_per_second the number of ticks per second for the timer of this source
 * @param max_ticks the maximum count of ticks reported by this source before overflow occurs
 * @param info optional info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_source_get_info(int source_index, char *name, int *name_len, char *desc, int *desc_len, MPI_T_source_order *ordering, MPI_Count *ticks_per_second, MPI_Count *max_ticks, MPI_Info *info);
int PMPI_T_source_get_info(int source_index, char *name, int *name_len, char *desc, int *desc_len, MPI_T_source_order *ordering, MPI_Count *ticks_per_second, MPI_Count *max_ticks, MPI_Info *info);


/*MPI_T_source_get_num*/

/**
 * @brief MPI function MPI_T_source_get_num
 * 
 * @param num_sources returns number of event sources
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_source_get_num(int *num_sources);
int PMPI_T_source_get_num(int *num_sources);


/*MPI_T_source_get_timestamp*/

/**
 * @brief MPI function MPI_T_source_get_timestamp
 * 
 * @param source_index index of the source
 * @param timestamp current timestamp from specified source
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_T_source_get_timestamp(int source_index, MPI_Count *timestamp);
int PMPI_T_source_get_timestamp(int source_index, MPI_Count *timestamp);


/*MPI_Test*/

/**
 * @brief MPI function MPI_Test
 * 
 * @param request 
 * @param flag \mpicode{true} if operation completed
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status);
int PMPI_Test(MPI_Request *request, int *flag, MPI_Status *status);


/*MPI_Test_cancelled*/

/**
 * @brief MPI function MPI_Test_cancelled
 * 
 * @param status 
 * @param flag \mpicode{true} if the operation has been cancelled
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Test_cancelled(const MPI_Status *status, int *flag);
int PMPI_Test_cancelled(const MPI_Status *status, int *flag);


/*MPI_Testall*/

/**
 * @brief MPI function MPI_Testall
 * 
 * @param count list length
 * @param array_of_requests array of requests
 * @param flag \mpicode{true} if all of the operations are complete
 * @param array_of_statuses array of status objects
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Testall(int count, MPI_Request array_of_requests[], int *flag, MPI_Status array_of_statuses[]);
int PMPI_Testall(int count, MPI_Request array_of_requests[], int *flag, MPI_Status array_of_statuses[]);


/*MPI_Testany*/

/**
 * @brief MPI function MPI_Testany
 * 
 * @param count list length
 * @param array_of_requests array of requests
 * @param index index of operation that completed or \mpiconst{MPI_UNDEFINED} if none completed
 * @param flag \mpicode{true} if one of the operations is complete
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Testany(int count, MPI_Request array_of_requests[], int *index, int *flag, MPI_Status *status);
int PMPI_Testany(int count, MPI_Request array_of_requests[], int *index, int *flag, MPI_Status *status);


/*MPI_Testsome*/

/**
 * @brief MPI function MPI_Testsome
 * 
 * @param incount length of array_of_requests
 * @param array_of_requests array of requests
 * @param outcount number of completed requests
 * @param array_of_indices array of indices of operations that completed
 * @param array_of_statuses array of status objects for operations that completed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Testsome(int incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[]);
int PMPI_Testsome(int incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[]);


/*MPI_Topo_test*/

/**
 * @brief MPI function MPI_Topo_test
 * 
 * @param comm communicator
 * @param status topology type of communicator \mpiarg{comm}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Topo_test(MPI_Comm comm, int *status);
int PMPI_Topo_test(MPI_Comm comm, int *status);


/*MPI_Type_commit*/

/**
 * @brief MPI function MPI_Type_commit
 * 
 * @param datatype datatype that is committed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_commit(MPI_Datatype *datatype);
int PMPI_Type_commit(MPI_Datatype *datatype);


/*MPI_Type_contiguous*/

/**
 * @brief MPI function MPI_Type_contiguous
 * 
 * @param count replication count
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_contiguous(MPI_Count count, MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_contiguous(MPI_Count count, MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_create_darray*/

/**
 * @brief MPI function MPI_Type_create_darray
 * 
 * @param size size of process group
 * @param rank rank in process group
 * @param ndims number of array dimensions as well as process grid dimensions
 * @param array_of_gsizes number of elements of type \mpiarg{oldtype} in each dimension of global array
 * @param array_of_distribs distribution of array in each dimension
 * @param array_of_dargs distribution argument in each dimension
 * @param array_of_psizes size of process grid in each dimension
 * @param order array storage order flag
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_darray(int size, int rank, int ndims, const MPI_Count array_of_gsizes[], const int array_of_distribs[], const int array_of_dargs[], const int array_of_psizes[], int order, MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_create_darray(int size, int rank, int ndims, const MPI_Count array_of_gsizes[], const int array_of_distribs[], const int array_of_dargs[], const int array_of_psizes[], int order, MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_create_f90_complex*/

/**
 * @brief MPI function MPI_Type_create_f90_complex
 * 
 * @param p precision, in decimal digits
 * @param r decimal exponent range
 * @param newtype the requested \MPI/ datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_f90_complex(int p, int r, MPI_Datatype *newtype);
int PMPI_Type_create_f90_complex(int p, int r, MPI_Datatype *newtype);


/*MPI_Type_create_f90_integer*/

/**
 * @brief MPI function MPI_Type_create_f90_integer
 * 
 * @param r decimal exponent range, i.e., number of decimal digits
 * @param newtype the requested \MPI/ datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_f90_integer(int r, MPI_Datatype *newtype);
int PMPI_Type_create_f90_integer(int r, MPI_Datatype *newtype);


/*MPI_Type_create_f90_real*/

/**
 * @brief MPI function MPI_Type_create_f90_real
 * 
 * @param p precision, in decimal digits
 * @param r decimal exponent range
 * @param newtype the requested \MPI/ datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype);
int PMPI_Type_create_f90_real(int p, int r, MPI_Datatype *newtype);


/*MPI_Type_create_hindexed*/

/**
 * @brief MPI function MPI_Type_create_hindexed
 * 
 * @param count number of blocks---also number of entries in \mpiarg{array_of_displacements} and \mpiarg{array_of_blocklengths}
 * @param array_of_blocklengths number of elements in each block
 * @param array_of_displacements byte displacement of each block
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_hindexed(MPI_Count count, const MPI_Count array_of_blocklengths[], const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_create_hindexed(MPI_Count count, const MPI_Count array_of_blocklengths[], const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_create_hindexed_block*/

/**
 * @brief MPI function MPI_Type_create_hindexed_block
 * 
 * @param count number of blocks---also number of entries in \mpiarg{array_of_displacements}
 * @param blocklength number of elements in each block
 * @param array_of_displacements byte displacement of each block
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_hindexed_block(MPI_Count count, MPI_Count blocklength, const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_create_hindexed_block(MPI_Count count, MPI_Count blocklength, const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_create_hvector*/

/**
 * @brief MPI function MPI_Type_create_hvector
 * 
 * @param count number of blocks
 * @param blocklength number of elements in each block
 * @param stride number of bytes between start of each block
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_hvector(MPI_Count count, MPI_Count blocklength, MPI_Count stride, MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_create_hvector(MPI_Count count, MPI_Count blocklength, MPI_Count stride, MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_create_indexed_block*/

/**
 * @brief MPI function MPI_Type_create_indexed_block
 * 
 * @param count number of blocks---also number of entries in \mpiarg{array_of_displacements}
 * @param blocklength number of elements in each block
 * @param array_of_displacements array of displacements, in multiples of \mpiarg{oldtype}
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_indexed_block(MPI_Count count, MPI_Count blocklength, const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_create_indexed_block(MPI_Count count, MPI_Count blocklength, const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_create_keyval*/

/**
 * @brief MPI function MPI_Type_create_keyval
 * 
 * @param type_copy_attr_fn copy callback function for \mpiarg{type_keyval}
 * @param type_delete_attr_fn delete callback function for \mpiarg{type_keyval}
 * @param type_keyval key value for future access
 * @param extra_state extra state for callback function
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn, MPI_Type_delete_attr_function *type_delete_attr_fn, int *type_keyval, void *extra_state);
int PMPI_Type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn, MPI_Type_delete_attr_function *type_delete_attr_fn, int *type_keyval, void *extra_state);


/*MPI_Type_create_resized*/

/**
 * @brief MPI function MPI_Type_create_resized
 * 
 * @param oldtype input datatype
 * @param lb new lower bound of datatype
 * @param extent new extent of datatype
 * @param newtype output datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Count lb, MPI_Count extent, MPI_Datatype *newtype);
int PMPI_Type_create_resized(MPI_Datatype oldtype, MPI_Count lb, MPI_Count extent, MPI_Datatype *newtype);


/*MPI_Type_create_struct*/

/**
 * @brief MPI function MPI_Type_create_struct
 * 
 * @param count number of blocks---also number of entries in arrays \mpiarg{array_of_types}, \mpiarg{array_of_displacements}, and \mpiarg{array_of_blocklengths}
 * @param array_of_blocklengths number of elements in each block
 * @param array_of_displacements byte displacement of each block
 * @param array_of_types type of elements in each block
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_struct(MPI_Count count, const MPI_Count array_of_blocklengths[], const MPI_Count array_of_displacements[], const MPI_Datatype array_of_types[], MPI_Datatype *newtype);
int PMPI_Type_create_struct(MPI_Count count, const MPI_Count array_of_blocklengths[], const MPI_Count array_of_displacements[], const MPI_Datatype array_of_types[], MPI_Datatype *newtype);


/*MPI_Type_create_subarray*/

/**
 * @brief MPI function MPI_Type_create_subarray
 * 
 * @param ndims number of array dimensions
 * @param array_of_sizes number of elements of type \mpiarg{oldtype} in each dimension of the full array
 * @param array_of_subsizes number of elements of type \mpiarg{oldtype} in each dimension of the subarray
 * @param array_of_starts starting coordinates of the subarray in each dimension
 * @param order array storage order flag
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_create_subarray(int ndims, const MPI_Count array_of_sizes[], const MPI_Count array_of_subsizes[], const MPI_Count array_of_starts[], int order, MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_create_subarray(int ndims, const MPI_Count array_of_sizes[], const MPI_Count array_of_subsizes[], const MPI_Count array_of_starts[], int order, MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_delete_attr*/

/**
 * @brief MPI function MPI_Type_delete_attr
 * 
 * @param datatype datatype from which the attribute is deleted
 * @param type_keyval key value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_delete_attr(MPI_Datatype datatype, int type_keyval);
int PMPI_Type_delete_attr(MPI_Datatype datatype, int type_keyval);


/*MPI_Type_dup*/

/**
 * @brief MPI function MPI_Type_dup
 * 
 * @param oldtype datatype
 * @param newtype copy of \mpiarg{oldtype}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_dup(MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_dup(MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_free*/

/**
 * @brief MPI function MPI_Type_free
 * 
 * @param datatype datatype that is freed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_free(MPI_Datatype *datatype);
int PMPI_Type_free(MPI_Datatype *datatype);


/*MPI_Type_free_keyval*/

/**
 * @brief MPI function MPI_Type_free_keyval
 * 
 * @param type_keyval key value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_free_keyval(int *type_keyval);
int PMPI_Type_free_keyval(int *type_keyval);


/*MPI_Type_get_attr*/

/**
 * @brief MPI function MPI_Type_get_attr
 * 
 * @param datatype datatype to which the attribute is attached
 * @param type_keyval key value
 * @param attribute_val attribute value, unless \mpiarg{flag}\mpicode{ = false}
 * @param flag \mpicode{false} if no attribute is associated with the key
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val, int *flag);
int PMPI_Type_get_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val, int *flag);


/*MPI_Type_get_contents*/

/**
 * @brief MPI function MPI_Type_get_contents
 * 
 * @param datatype datatype to decode
 * @param max_integers number of elements in \mpiarg{array_of_integers}
 * @param max_addresses number of elements in \mpiarg{array_of_addresses}
 * @param max_large_counts number of elements in \mpiarg{array_of_large_counts}
 * @param max_datatypes number of elements in \mpiarg{array_of_datatypes}
 * @param array_of_integers contains integer arguments used in constructing \mpiarg{datatype}
 * @param array_of_addresses contains address arguments used in constructing \mpiarg{datatype}
 * @param array_of_large_counts contains large count arguments used in constructing \mpiarg{datatype}
 * @param array_of_datatypes contains datatype arguments used in constructing \mpiarg{datatype}
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_contents(MPI_Datatype datatype, MPI_Count max_integers, MPI_Count max_addresses, MPI_Count max_large_counts, MPI_Count max_datatypes, int array_of_integers[], MPI_Aint array_of_addresses[], MPI_Count array_of_large_counts[], MPI_Datatype array_of_datatypes[]);
int PMPI_Type_get_contents(MPI_Datatype datatype, MPI_Count max_integers, MPI_Count max_addresses, MPI_Count max_large_counts, MPI_Count max_datatypes, int array_of_integers[], MPI_Aint array_of_addresses[], MPI_Count array_of_large_counts[], MPI_Datatype array_of_datatypes[]);


/*MPI_Type_get_envelope*/

/**
 * @brief MPI function MPI_Type_get_envelope
 * 
 * @param datatype datatype to decode
 * @param num_integers number of input integers used in call constructing \mpiarg{combiner}
 * @param num_addresses number of input addresses used in call constructing \mpiarg{combiner}
 * @param num_large_counts number of input large counts used in call constructing \mpiarg{combiner}
 * @param num_datatypes number of input datatypes used in call constructing \mpiarg{combiner}
 * @param combiner combiner
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_envelope(MPI_Datatype datatype, MPI_Count *num_integers, MPI_Count *num_addresses, MPI_Count *num_large_counts, MPI_Count *num_datatypes, int *combiner);
int PMPI_Type_get_envelope(MPI_Datatype datatype, MPI_Count *num_integers, MPI_Count *num_addresses, MPI_Count *num_large_counts, MPI_Count *num_datatypes, int *combiner);


/*MPI_Type_get_extent*/

/**
 * @brief MPI function MPI_Type_get_extent
 * 
 * @param datatype datatype to get information on
 * @param lb lower bound of datatype
 * @param extent extent of datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_extent(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent);
int PMPI_Type_get_extent(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent);


/*MPI_Type_get_extent_x*/

/**
 * @brief MPI function MPI_Type_get_extent_x
 * 
 * @param datatype datatype to get information on
 * @param lb lower bound of datatype
 * @param extent extent of datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_extent_x(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent);
int PMPI_Type_get_extent_x(MPI_Datatype datatype, MPI_Count *lb, MPI_Count *extent);


/*MPI_Type_get_name*/

/**
 * @brief MPI function MPI_Type_get_name
 * 
 * @param datatype datatype whose name is to be returned
 * @param type_name the name previously stored on the datatype, or an empty string if no such name exists
 * @param resultlen length of returned name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_name(MPI_Datatype datatype, char *type_name, int *resultlen);
int PMPI_Type_get_name(MPI_Datatype datatype, char *type_name, int *resultlen);


/*MPI_Type_get_true_extent*/

/**
 * @brief MPI function MPI_Type_get_true_extent
 * 
 * @param datatype datatype to get information on
 * @param true_lb true lower bound of datatype
 * @param true_extent true extent of datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Count *true_lb, MPI_Count *true_extent);
int PMPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Count *true_lb, MPI_Count *true_extent);


/*MPI_Type_get_true_extent_x*/

/**
 * @brief MPI function MPI_Type_get_true_extent_x
 * 
 * @param datatype datatype to get information on
 * @param true_lb true lower bound of datatype
 * @param true_extent true extent of datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_get_true_extent_x(MPI_Datatype datatype, MPI_Count *true_lb, MPI_Count *true_extent);
int PMPI_Type_get_true_extent_x(MPI_Datatype datatype, MPI_Count *true_lb, MPI_Count *true_extent);


/*MPI_Type_indexed*/

/**
 * @brief MPI function MPI_Type_indexed
 * 
 * @param count number of blocks---also number of entries in \mpiarg{array_of_displacements} and \mpiarg{array_of_blocklengths}
 * @param array_of_blocklengths number of elements per block
 * @param array_of_displacements displacement for each block, in multiples of \mpiarg{oldtype}
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_indexed(MPI_Count count, const MPI_Count array_of_blocklengths[], const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_indexed(MPI_Count count, const MPI_Count array_of_blocklengths[], const MPI_Count array_of_displacements[], MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Type_match_size*/

/**
 * @brief MPI function MPI_Type_match_size
 * 
 * @param typeclass generic type specifier
 * @param size size, in bytes, of representation
 * @param datatype datatype with correct type, size
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_match_size(int typeclass, int size, MPI_Datatype *datatype);
int PMPI_Type_match_size(int typeclass, int size, MPI_Datatype *datatype);


/*MPI_Type_set_attr*/

/**
 * @brief MPI function MPI_Type_set_attr
 * 
 * @param datatype datatype to which attribute will be attached
 * @param type_keyval key value
 * @param attribute_val attribute value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_set_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val);
int PMPI_Type_set_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val);


/*MPI_Type_set_name*/

/**
 * @brief MPI function MPI_Type_set_name
 * 
 * @param datatype datatype whose identifier is to be set
 * @param type_name the character string that is remembered as the name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_set_name(MPI_Datatype datatype, const char *type_name);
int PMPI_Type_set_name(MPI_Datatype datatype, const char *type_name);


/*MPI_Type_size*/

/**
 * @brief MPI function MPI_Type_size
 * 
 * @param datatype datatype to get information on
 * @param size datatype size
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_size(MPI_Datatype datatype, MPI_Count *size);
int PMPI_Type_size(MPI_Datatype datatype, MPI_Count *size);


/*MPI_Type_size_x*/

/**
 * @brief MPI function MPI_Type_size_x
 * 
 * @param datatype datatype to get information on
 * @param size datatype size
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_size_x(MPI_Datatype datatype, MPI_Count *size);
int PMPI_Type_size_x(MPI_Datatype datatype, MPI_Count *size);


/*MPI_Type_vector*/

/**
 * @brief MPI function MPI_Type_vector
 * 
 * @param count number of blocks
 * @param blocklength number of elements in each block
 * @param stride number of elements between start of each block
 * @param oldtype old datatype
 * @param newtype new datatype
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Type_vector(MPI_Count count, MPI_Count blocklength, MPI_Count stride, MPI_Datatype oldtype, MPI_Datatype *newtype);
int PMPI_Type_vector(MPI_Count count, MPI_Count blocklength, MPI_Count stride, MPI_Datatype oldtype, MPI_Datatype *newtype);


/*MPI_Unpack*/

/**
 * @brief MPI function MPI_Unpack
 * 
 * @param inbuf input buffer start
 * @param insize size of input buffer, in bytes
 * @param position current position in bytes
 * @param outbuf output buffer start
 * @param outcount number of items to be unpacked
 * @param datatype datatype of each output data item
 * @param comm communicator for packed message
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Unpack(const void *inbuf, MPI_Count insize, MPI_Count *position, void *outbuf, MPI_Count outcount, MPI_Datatype datatype, MPI_Comm comm);
int PMPI_Unpack(const void *inbuf, MPI_Count insize, MPI_Count *position, void *outbuf, MPI_Count outcount, MPI_Datatype datatype, MPI_Comm comm);


/*MPI_Unpack_external*/

/**
 * @brief MPI function MPI_Unpack_external
 * 
 * @param datarep data representation
 * @param inbuf input buffer start
 * @param insize input buffer size, in bytes
 * @param position current position in buffer, in bytes
 * @param outbuf output buffer start
 * @param outcount number of output data items
 * @param datatype datatype of output data item
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Unpack_external(const char datarep[], const void *inbuf, MPI_Count insize, MPI_Count *position, void *outbuf, MPI_Count outcount, MPI_Datatype datatype);
int PMPI_Unpack_external(const char datarep[], const void *inbuf, MPI_Count insize, MPI_Count *position, void *outbuf, MPI_Count outcount, MPI_Datatype datatype);


/*MPI_Unpublish_name*/

/**
 * @brief MPI function MPI_Unpublish_name
 * 
 * @param service_name a service name
 * @param info implementation-specific information
 * @param port_name a port name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Unpublish_name(const char *service_name, MPI_Info info, const char *port_name);
int PMPI_Unpublish_name(const char *service_name, MPI_Info info, const char *port_name);


/*MPI_Wait*/

/**
 * @brief MPI function MPI_Wait
 * 
 * @param request request
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Wait(MPI_Request *request, MPI_Status *status);
int PMPI_Wait(MPI_Request *request, MPI_Status *status);


/*MPI_Waitall*/

/**
 * @brief MPI function MPI_Waitall
 * 
 * @param count list length
 * @param array_of_requests array of requests
 * @param array_of_statuses array of status objects
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Waitall(int count, MPI_Request array_of_requests[], MPI_Status array_of_statuses[]);
int PMPI_Waitall(int count, MPI_Request array_of_requests[], MPI_Status array_of_statuses[]);


/*MPI_Waitany*/

/**
 * @brief MPI function MPI_Waitany
 * 
 * @param count list length
 * @param array_of_requests array of requests
 * @param index index of handle for operation that completed
 * @param status 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Waitany(int count, MPI_Request array_of_requests[], int *index, MPI_Status *status);
int PMPI_Waitany(int count, MPI_Request array_of_requests[], int *index, MPI_Status *status);


/*MPI_Waitsome*/

/**
 * @brief MPI function MPI_Waitsome
 * 
 * @param incount length of array_of_requests
 * @param array_of_requests array of requests
 * @param outcount number of completed requests
 * @param array_of_indices array of indices of operations that completed
 * @param array_of_statuses array of status objects for operations that completed
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Waitsome(int incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[]);
int PMPI_Waitsome(int incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[]);


/*MPI_Win_allocate*/

/**
 * @brief MPI function MPI_Win_allocate
 * 
 * @param size size of window in bytes
 * @param disp_unit local unit size for displacements, in bytes
 * @param info 
 * @param comm intra-communicator
 * @param baseptr initial address of window
 * @param win window object returned by call
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_allocate(MPI_Aint size, MPI_Aint disp_unit, MPI_Info info, MPI_Comm comm, void *baseptr, MPI_Win *win);
int PMPI_Win_allocate(MPI_Aint size, MPI_Aint disp_unit, MPI_Info info, MPI_Comm comm, void *baseptr, MPI_Win *win);


/*MPI_Win_allocate_shared*/

/**
 * @brief MPI function MPI_Win_allocate_shared
 * 
 * @param size size of local window in bytes
 * @param disp_unit local unit size for displacements, in bytes
 * @param info 
 * @param comm intra-communicator
 * @param baseptr address of local allocated window segment
 * @param win window object returned by the call
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_allocate_shared(MPI_Aint size, MPI_Aint disp_unit, MPI_Info info, MPI_Comm comm, void *baseptr, MPI_Win *win);
int PMPI_Win_allocate_shared(MPI_Aint size, MPI_Aint disp_unit, MPI_Info info, MPI_Comm comm, void *baseptr, MPI_Win *win);


/*MPI_Win_attach*/

/**
 * @brief MPI function MPI_Win_attach
 * 
 * @param win 
 * @param base initial address of memory to be attached
 * @param size size of memory to be attached in bytes
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_attach(MPI_Win win, void *base, MPI_Aint size);
int PMPI_Win_attach(MPI_Win win, void *base, MPI_Aint size);


/*MPI_Win_call_errhandler*/

/**
 * @brief MPI function MPI_Win_call_errhandler
 * 
 * @param win window with error handler
 * @param errorcode 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_call_errhandler(MPI_Win win, int errorcode);
int PMPI_Win_call_errhandler(MPI_Win win, int errorcode);


/*MPI_Win_complete*/

/**
 * @brief MPI function MPI_Win_complete
 * 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_complete(MPI_Win win);
int PMPI_Win_complete(MPI_Win win);


/*MPI_Win_create*/

/**
 * @brief MPI function MPI_Win_create
 * 
 * @param base initial address of window
 * @param size size of window in bytes
 * @param disp_unit local unit size for displacements, in bytes
 * @param info 
 * @param comm intra-communicator
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_create(void *base, MPI_Aint size, MPI_Aint disp_unit, MPI_Info info, MPI_Comm comm, MPI_Win *win);
int PMPI_Win_create(void *base, MPI_Aint size, MPI_Aint disp_unit, MPI_Info info, MPI_Comm comm, MPI_Win *win);


/*MPI_Win_create_dynamic*/

/**
 * @brief MPI function MPI_Win_create_dynamic
 * 
 * @param info 
 * @param comm intra-communicator
 * @param win window object returned by the call
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_create_dynamic(MPI_Info info, MPI_Comm comm, MPI_Win *win);
int PMPI_Win_create_dynamic(MPI_Info info, MPI_Comm comm, MPI_Win *win);


/*MPI_Win_create_errhandler*/

/**
 * @brief MPI function MPI_Win_create_errhandler
 * 
 * @param win_errhandler_fn user defined error handling procedure
 * @param errhandler 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_create_errhandler(MPI_Win_errhandler_function *win_errhandler_fn, MPI_Errhandler *errhandler);
int PMPI_Win_create_errhandler(MPI_Win_errhandler_function *win_errhandler_fn, MPI_Errhandler *errhandler);


/*MPI_Win_create_keyval*/

/**
 * @brief MPI function MPI_Win_create_keyval
 * 
 * @param win_copy_attr_fn copy callback function for \mpiarg{win_keyval}
 * @param win_delete_attr_fn delete callback function for \mpiarg{win_keyval}
 * @param win_keyval key value for future access
 * @param extra_state extra state for callback function
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn, MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval, void *extra_state);
int PMPI_Win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn, MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval, void *extra_state);


/*MPI_Win_delete_attr*/

/**
 * @brief MPI function MPI_Win_delete_attr
 * 
 * @param win window from which the attribute is deleted
 * @param win_keyval key value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_delete_attr(MPI_Win win, int win_keyval);
int PMPI_Win_delete_attr(MPI_Win win, int win_keyval);


/*MPI_Win_detach*/

/**
 * @brief MPI function MPI_Win_detach
 * 
 * @param win 
 * @param base initial address of memory to be detached
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_detach(MPI_Win win, const void *base);
int PMPI_Win_detach(MPI_Win win, const void *base);


/*MPI_Win_fence*/

/**
 * @brief MPI function MPI_Win_fence
 * 
 * @param assert 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_fence(int assert, MPI_Win win);
int PMPI_Win_fence(int assert, MPI_Win win);


/*MPI_Win_flush*/

/**
 * @brief MPI function MPI_Win_flush
 * 
 * @param rank rank of target window
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_flush(int rank, MPI_Win win);
int PMPI_Win_flush(int rank, MPI_Win win);


/*MPI_Win_flush_all*/

/**
 * @brief MPI function MPI_Win_flush_all
 * 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_flush_all(MPI_Win win);
int PMPI_Win_flush_all(MPI_Win win);


/*MPI_Win_flush_local*/

/**
 * @brief MPI function MPI_Win_flush_local
 * 
 * @param rank rank of target window
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_flush_local(int rank, MPI_Win win);
int PMPI_Win_flush_local(int rank, MPI_Win win);


/*MPI_Win_flush_local_all*/

/**
 * @brief MPI function MPI_Win_flush_local_all
 * 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_flush_local_all(MPI_Win win);
int PMPI_Win_flush_local_all(MPI_Win win);


/*MPI_Win_free*/

/**
 * @brief MPI function MPI_Win_free
 * 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_free(MPI_Win *win);
int PMPI_Win_free(MPI_Win *win);


/*MPI_Win_free_keyval*/

/**
 * @brief MPI function MPI_Win_free_keyval
 * 
 * @param win_keyval key value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_free_keyval(int *win_keyval);
int PMPI_Win_free_keyval(int *win_keyval);


/*MPI_Win_get_attr*/

/**
 * @brief MPI function MPI_Win_get_attr
 * 
 * @param win window to which the attribute is attached
 * @param win_keyval key value
 * @param attribute_val attribute value, unless \mpiarg{flag}\mpicode{ = false}
 * @param flag \mpicode{false} if no attribute is associated with the key
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_get_attr(MPI_Win win, int win_keyval, void *attribute_val, int *flag);
int PMPI_Win_get_attr(MPI_Win win, int win_keyval, void *attribute_val, int *flag);


/*MPI_Win_get_errhandler*/

/**
 * @brief MPI function MPI_Win_get_errhandler
 * 
 * @param win 
 * @param errhandler error handler currently associated with window
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler);
int PMPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler);


/*MPI_Win_get_group*/

/**
 * @brief MPI function MPI_Win_get_group
 * 
 * @param win 
 * @param group group of processes that share access to the window
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_get_group(MPI_Win win, MPI_Group *group);
int PMPI_Win_get_group(MPI_Win win, MPI_Group *group);


/*MPI_Win_get_info*/

/**
 * @brief MPI function MPI_Win_get_info
 * 
 * @param win 
 * @param info_used new info object
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_get_info(MPI_Win win, MPI_Info *info_used);
int PMPI_Win_get_info(MPI_Win win, MPI_Info *info_used);


/*MPI_Win_get_name*/

/**
 * @brief MPI function MPI_Win_get_name
 * 
 * @param win window whose name is to be returned
 * @param win_name the name previously stored on the window, or an empty string if no such name exists
 * @param resultlen length of returned name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_get_name(MPI_Win win, char *win_name, int *resultlen);
int PMPI_Win_get_name(MPI_Win win, char *win_name, int *resultlen);


/*MPI_Win_lock*/

/**
 * @brief MPI function MPI_Win_lock
 * 
 * @param lock_type either \mpiconst{MPI_LOCK_EXCLUSIVE} or \mpiconst{MPI_LOCK_SHARED}
 * @param rank rank of locked window
 * @param assert 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_lock(int lock_type, int rank, int assert, MPI_Win win);
int PMPI_Win_lock(int lock_type, int rank, int assert, MPI_Win win);


/*MPI_Win_lock_all*/

/**
 * @brief MPI function MPI_Win_lock_all
 * 
 * @param assert 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_lock_all(int assert, MPI_Win win);
int PMPI_Win_lock_all(int assert, MPI_Win win);


/*MPI_Win_post*/

/**
 * @brief MPI function MPI_Win_post
 * 
 * @param group group of origin processes
 * @param assert 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_post(MPI_Group group, int assert, MPI_Win win);
int PMPI_Win_post(MPI_Group group, int assert, MPI_Win win);


/*MPI_Win_set_attr*/

/**
 * @brief MPI function MPI_Win_set_attr
 * 
 * @param win window to which attribute will be attached
 * @param win_keyval key value
 * @param attribute_val attribute value
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val);
int PMPI_Win_set_attr(MPI_Win win, int win_keyval, void *attribute_val);


/*MPI_Win_set_errhandler*/

/**
 * @brief MPI function MPI_Win_set_errhandler
 * 
 * @param win 
 * @param errhandler new error handler for window
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler);
int PMPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler);


/*MPI_Win_set_info*/

/**
 * @brief MPI function MPI_Win_set_info
 * 
 * @param win 
 * @param info 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_set_info(MPI_Win win, MPI_Info info);
int PMPI_Win_set_info(MPI_Win win, MPI_Info info);


/*MPI_Win_set_name*/

/**
 * @brief MPI function MPI_Win_set_name
 * 
 * @param win window whose identifier is to be set
 * @param win_name the character string that is remembered as the name
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_set_name(MPI_Win win, const char *win_name);
int PMPI_Win_set_name(MPI_Win win, const char *win_name);


/*MPI_Win_shared_query*/

/**
 * @brief MPI function MPI_Win_shared_query
 * 
 * @param win shared memory window object
 * @param rank rank in the group of window win or \mpiconst{MPI_PROC_NULL}
 * @param size size of the window segment
 * @param disp_unit local unit size for displacements, in bytes
 * @param baseptr address for load/store access to window segment
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_shared_query(MPI_Win win, int rank, MPI_Aint *size, MPI_Aint *disp_unit, void *baseptr);
int PMPI_Win_shared_query(MPI_Win win, int rank, MPI_Aint *size, MPI_Aint *disp_unit, void *baseptr);


/*MPI_Win_start*/

/**
 * @brief MPI function MPI_Win_start
 * 
 * @param group group of target processes
 * @param assert 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_start(MPI_Group group, int assert, MPI_Win win);
int PMPI_Win_start(MPI_Group group, int assert, MPI_Win win);


/*MPI_Win_sync*/

/**
 * @brief MPI function MPI_Win_sync
 * 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_sync(MPI_Win win);
int PMPI_Win_sync(MPI_Win win);


/*MPI_Win_test*/

/**
 * @brief MPI function MPI_Win_test
 * 
 * @param win 
 * @param flag success flag
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_test(MPI_Win win, int *flag);
int PMPI_Win_test(MPI_Win win, int *flag);


/*MPI_Win_unlock*/

/**
 * @brief MPI function MPI_Win_unlock
 * 
 * @param rank rank of window
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_unlock(int rank, MPI_Win win);
int PMPI_Win_unlock(int rank, MPI_Win win);


/*MPI_Win_unlock_all*/

/**
 * @brief MPI function MPI_Win_unlock_all
 * 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_unlock_all(MPI_Win win);
int PMPI_Win_unlock_all(MPI_Win win);


/*MPI_Win_wait*/

/**
 * @brief MPI function MPI_Win_wait
 * 
 * @param win 
 *
 * @return int MPI_SUCCESS on success other MPI_* error code otherwise 
 */
int MPI_Win_wait(MPI_Win win);
int PMPI_Win_wait(MPI_Win win);


/*MPI_Wtick*/

/**
 * @brief MPI function MPI_Wtick
 * 
 *
 *
 * @return double 
 
 */
double MPI_Wtick();
double PMPI_Wtick();


/*MPI_Wtime*/

/**
 * @brief MPI function MPI_Wtime
 * 
 *
 *
 * @return double 
 
 */
double MPI_Wtime();
double PMPI_Wtime();
