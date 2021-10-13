/*
 * Copyright (C) 2002-2018 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Copyright (C) 2019 Bull S. A. S. All rights reserved.
 * Bull, Rue Jean Jaures, B.P.68, 78340, Les Clayes-sous-Bois, France
 * This is not Free or Open Source software.
 * Please contact Bull S. A. S. for details about its license.
 */

#include <sys/time.h>

#include "common.h"
#include "common_output.h"

/*
 * Global variables definition
 */

char const *win_info[N_WIN] = {
    [WIN_CREATE] = "MPI_Win_create",
    [WIN_CREATE_NOTIFY] = "MPI_Win_create_notify",
#if MPI_3
    [WIN_ALLOCATE] = "MPI_Win_allocate",
    [WIN_ALLOCATE_NOTIFY] = "MPI_Win_allocate_notify",
    [WIN_DYNAMIC] = "MPI_Win_create_dynamic",
#endif
};

char const *sync_info[N_SYNC] = {
    [MULTIPLE] = "Multiple synchronizations used",
    [LOCK] = "MPI_Win_lock/unlock",
    [PSCW] = "MPI_Win_post/start/complete/wait",
    [FENCE] = "MPI_Win_fence",
#if MPI_3
    [LOCK_ALL] = "MPI_Win_lock_all/unlock_all",
    [FLUSH] = "MPI_Win_flush",
    [FLUSH_LOCAL] = "MPI_Win_flush_local",
    [NOTIFICATIONS] = "MPI_Win_wait/test_notify",
#endif
};

char const *bench_info[N_BENCH] = {
    [FUNCTIONAL_OK] = "OK",
    [FUNCTIONAL_XFAIL] = "XFAIL",
};

char const * benchmark_header = NULL;
struct options_t options;

/*
 * Memory Management
 */

void win_and_memory_allocate(int rank, void **sbuf, void **win_base,
            size_t size, enum WINDOW type, MPI_Win *win)
{
    int page_size;
#if MPI_3
    MPI_Status  reqstat;
    MPI_Aint disp_remote;
    MPI_Aint disp_local;
#else
    (void) rank;
    (void) type;
#endif // MPI_VERSION

    page_size = getpagesize();
    assert(page_size <= MAX_ALIGNMENT);

    MEM_CHECK(posix_memalign(sbuf, page_size, size));
    memset(*sbuf, 'a', size);

#if MPI_3
    switch (type) {
        case WIN_CREATE:
            MEM_CHECK(posix_memalign(win_base, page_size, size));
            memset(*win_base, 'b', size);

            MPI_CHECK(MPI_Win_create(*win_base, size, 1, MPI_INFO_NULL,
                            MPI_COMM_WORLD, win));
            break;
        case WIN_CREATE_NOTIFY:
            MEM_CHECK(posix_memalign(win_base, page_size, size));
            memset(*win_base, 'b', size);

            MPI_CHECK(MPIX_Win_create_notify(*win_base, size, 1, MPI_INFO_NULL,
                            MPI_COMM_WORLD, win));
            break;
        /* FIXME: this allocation style requires exactly two processes to work
         * for now. */
        case WIN_DYNAMIC:
            MEM_CHECK(posix_memalign(win_base, page_size, size));
            memset(*win_base, 'b', size);

            MPI_CHECK(MPI_Win_create_dynamic(MPI_INFO_NULL, MPI_COMM_WORLD,
                            win));
            MPI_CHECK(MPI_Win_attach(*win, (void *)*win_base, size));
            MPI_CHECK(MPI_Get_address(*win_base, &disp_local));
            if(rank == 0) {
                MPI_CHECK(MPI_Send(&disp_local, 1, MPI_AINT, 1, 1,
                                MPI_COMM_WORLD));
                MPI_CHECK(MPI_Recv(&disp_remote, 1, MPI_AINT, 1, 1,
                                MPI_COMM_WORLD, &reqstat));
            } else {
                MPI_CHECK(MPI_Recv(&disp_remote, 1, MPI_AINT, 0, 1,
                                MPI_COMM_WORLD, &reqstat));
                MPI_CHECK(MPI_Send(&disp_local, 1, MPI_AINT, 0, 1,
                                MPI_COMM_WORLD));
            }
            break;
        case WIN_ALLOCATE_NOTIFY:
            MPI_CHECK(MPIX_Win_allocate_notify(size, 1, MPI_INFO_NULL,
                            MPI_COMM_WORLD, win_base, win));
            break;
        default:
            MPI_CHECK(MPI_Win_allocate(size, 1, MPI_INFO_NULL, MPI_COMM_SELF,
                            win_base, win));
            break;
    }
#else
    MEM_CHECK(posix_memalign(win_base, page_size, size));
    memset(*win_base, 'b', size);

    MPI_CHECK(MPI_Win_create(*win_base, size, 1, MPI_INFO_NULL, MPI_COMM_WORLD,
                    win));
#endif // MPI_VERSION
}

void win_and_memory_free (void *sbuf, void *win_base, enum WINDOW type, MPI_Win win)
{
    MPI_CHECK(MPI_Win_free(&win));
    free(sbuf);

    /* If the window was allocated with Allocate flavors, the MPI_Win_free call
     * automatically frees the win_base buffer. */
    if (WIN_ALLOCATE != type && WIN_ALLOCATE_NOTIFY != type) {
        free(win_base);
    }
}

/*
 * Set Benchmark Properties
 */

void set_header (const char *header)
{
    benchmark_header = header;
}

int get_max_notification_id(){
        int cvars_idx, csize, cvars_val;
        int t_lvl;
        int err;
        MPI_T_cvar_handle chandle;
        MPI_T_init_thread(1, &t_lvl);

        err= MPI_T_cvar_get_index("mpi_max_notification_idx",&cvars_idx);
        if (err != MPI_SUCCESS){
             fprintf(stderr, "Index not found for 'mpi_max_notification_idx'\n");
             MPI_Abort(MPI_COMM_WORLD, err);
        }

        err= MPI_T_cvar_handle_alloc(cvars_idx, NULL, &chandle, &csize);
        if (err != MPI_SUCCESS){
             fprintf(stderr, "Cannot allocate handle for a cvar pointing to 'mpi_max_notification_idx'\n");
             MPI_Abort(MPI_COMM_WORLD, err);
        }

        MPI_T_cvar_read(chandle, &cvars_val);

        MPI_T_cvar_handle_free(&chandle);
        MPI_T_finalize();

        return cvars_val;
}

void compute(int duration){
    struct timeval start, end;
    long long  expected, time;

    gettimeofday(&start, NULL);
    expected = start.tv_sec*1000000 + start.tv_usec + duration;

    do{
        gettimeofday(&end, NULL);
        time = end.tv_sec*1000000 + end.tv_usec;
    } while (time < expected);
    return;
}
