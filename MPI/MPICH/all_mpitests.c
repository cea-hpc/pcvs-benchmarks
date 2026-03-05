/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */
/* This file is generated. Do not edit. */

#include "mpitest.h"

#ifdef MULTI_TESTS

int attr_attrt(const char *args);
int attr_attrend(const char *args);
int attr_attric(const char *args);
int attr_attrerr(const char *args);
int attr_attrerrcomm(const char *args);
int attr_attrerrtype(const char *args);
int attr_attrdeleteget(const char *args);
int attr_attr2type(const char *args);
int attr_attrorder(const char *args);
int attr_attrordercomm(const char *args);
int attr_attrordertype(const char *args);
int attr_baseattr2(const char *args);
int attr_baseattrcomm(const char *args);
int attr_fkeyval(const char *args);
int attr_fkeyvalcomm(const char *args);
int attr_keyval_double_free(const char *args);
int attr_keyval_double_free_comm(const char *args);
int attr_keyval_double_free_type(const char *args);
int attr_keyval_double_free_win(const char *args);
int attr_fkeyvaltype(const char *args);
int coll_allgather2(const char *args);
int coll_allgather3(const char *args);
int coll_allgatherv2(const char *args);
int coll_allgatherv3(const char *args);
int coll_allgatherv4(const char *args);
int coll_allred(const char *args);
int coll_allred2(const char *args);
int coll_allred3(const char *args);
int coll_allred5(const char *args);
int coll_allred6(const char *args);
int coll_allredmany(const char *args);
int coll_alltoall1(const char *args);
int coll_alltoallv(const char *args);
int coll_alltoallv0(const char *args);
int coll_alltoallw1(const char *args);
int coll_alltoallw2(const char *args);
int coll_alltoallw_zeros(const char *args);
int coll_bcasttest(const char *args);
int coll_bcastzerotype(const char *args);
int coll_gather(const char *args);
int coll_gather2(const char *args);
int coll_gatherv(const char *args);
int coll_neighb_allgather(const char *args);
int coll_neighb_allgatherv(const char *args);
int coll_neighb_alltoall(const char *args);
int coll_neighb_alltoallv(const char *args);
int coll_neighb_alltoallw(const char *args);
int coll_op_coll(const char *args);
int coll_p_allgather(const char *args);
int coll_p_allgatherv(const char *args);
int coll_p_allred(const char *args);
int coll_p_alltoall(const char *args);
int coll_p_alltoallv(const char *args);
int coll_p_alltoallw(const char *args);
int coll_p_bcast(const char *args);
int coll_p_bcast2(const char *args);
int coll_p_gather(const char *args);
int coll_p_gatherv(const char *args);
int coll_p_neighb_allgather(const char *args);
int coll_p_neighb_allgatherv(const char *args);
int coll_p_neighb_alltoall(const char *args);
int coll_p_neighb_alltoallv(const char *args);
int coll_p_neighb_alltoallw(const char *args);
int coll_p_red(const char *args);
int coll_p_red_scat_block(const char *args);
int coll_p_redscat(const char *args);
int coll_p_scan(const char *args);
int coll_p_scatter(const char *args);
int coll_p_scatterv(const char *args);
int coll_red3(const char *args);
int coll_red4(const char *args);
int coll_red_scat_block(const char *args);
int coll_red_scat_block2(const char *args);
int coll_redscat(const char *args);
int coll_redscat2(const char *args);
int coll_redscat3(const char *args);
int coll_redscatblk3(const char *args);
int coll_reduce(const char *args);
int coll_scantst(const char *args);
int coll_scatter2(const char *args);
int coll_scatter3(const char *args);
int coll_scattern(const char *args);
int coll_scatterv(const char *args);

struct mpitest alltests[] = {
    {"attr/attrt", attr_attrt},
    {"attr/attrend", attr_attrend},
    {"attr/attric", attr_attric},
    {"attr/attrerr", attr_attrerr},
    {"attr/attrerrcomm", attr_attrerrcomm},
    {"attr/attrerrtype", attr_attrerrtype},
    {"attr/attrdeleteget", attr_attrdeleteget},
    {"attr/attr2type", attr_attr2type},
    {"attr/attrorder", attr_attrorder},
    {"attr/attrordercomm", attr_attrordercomm},
    {"attr/attrordertype", attr_attrordertype},
    {"attr/baseattr2", attr_baseattr2},
    {"attr/baseattrcomm", attr_baseattrcomm},
    {"attr/fkeyval", attr_fkeyval},
    {"attr/fkeyvalcomm", attr_fkeyvalcomm},
    {"attr/keyval_double_free", attr_keyval_double_free},
    {"attr/keyval_double_free_comm", attr_keyval_double_free_comm},
    {"attr/keyval_double_free_type", attr_keyval_double_free_type},
    {"attr/keyval_double_free_win", attr_keyval_double_free_win},
    {"attr/fkeyvaltype", attr_fkeyvaltype},
    {"coll/allgather2", coll_allgather2},
    {"coll/allgather3", coll_allgather3},
    {"coll/allgatherv2", coll_allgatherv2},
    {"coll/allgatherv3", coll_allgatherv3},
    {"coll/allgatherv4", coll_allgatherv4},
    {"coll/allred", coll_allred},
    {"coll/allred2", coll_allred2},
    {"coll/allred3", coll_allred3},
    {"coll/allred5", coll_allred5},
    {"coll/allred6", coll_allred6},
    {"coll/allredmany", coll_allredmany},
    {"coll/alltoall1", coll_alltoall1},
    {"coll/alltoallv", coll_alltoallv},
    {"coll/alltoallv0", coll_alltoallv0},
    {"coll/alltoallw1", coll_alltoallw1},
    {"coll/alltoallw2", coll_alltoallw2},
    {"coll/alltoallw_zeros", coll_alltoallw_zeros},
    {"coll/bcasttest", coll_bcasttest},
    {"coll/bcastzerotype", coll_bcastzerotype},
    {"coll/gather", coll_gather},
    {"coll/gather2", coll_gather2},
    {"coll/gatherv", coll_gatherv},
    {"coll/neighb_allgather", coll_neighb_allgather},
    {"coll/neighb_allgatherv", coll_neighb_allgatherv},
    {"coll/neighb_alltoall", coll_neighb_alltoall},
    {"coll/neighb_alltoallv", coll_neighb_alltoallv},
    {"coll/neighb_alltoallw", coll_neighb_alltoallw},
    {"coll/op_coll", coll_op_coll},
    {"coll/p_allgather", coll_p_allgather},
    {"coll/p_allgatherv", coll_p_allgatherv},
    {"coll/p_allred", coll_p_allred},
    {"coll/p_alltoall", coll_p_alltoall},
    {"coll/p_alltoallv", coll_p_alltoallv},
    {"coll/p_alltoallw", coll_p_alltoallw},
    {"coll/p_bcast", coll_p_bcast},
    {"coll/p_bcast2", coll_p_bcast2},
    {"coll/p_gather", coll_p_gather},
    {"coll/p_gatherv", coll_p_gatherv},
    {"coll/p_neighb_allgather", coll_p_neighb_allgather},
    {"coll/p_neighb_allgatherv", coll_p_neighb_allgatherv},
    {"coll/p_neighb_alltoall", coll_p_neighb_alltoall},
    {"coll/p_neighb_alltoallv", coll_p_neighb_alltoallv},
    {"coll/p_neighb_alltoallw", coll_p_neighb_alltoallw},
    {"coll/p_red", coll_p_red},
    {"coll/p_red_scat_block", coll_p_red_scat_block},
    {"coll/p_redscat", coll_p_redscat},
    {"coll/p_scan", coll_p_scan},
    {"coll/p_scatter", coll_p_scatter},
    {"coll/p_scatterv", coll_p_scatterv},
    {"coll/red3", coll_red3},
    {"coll/red4", coll_red4},
    {"coll/red_scat_block", coll_red_scat_block},
    {"coll/red_scat_block2", coll_red_scat_block2},
    {"coll/redscat", coll_redscat},
    {"coll/redscat2", coll_redscat2},
    {"coll/redscat3", coll_redscat3},
    {"coll/redscatblk3", coll_redscatblk3},
    {"coll/reduce", coll_reduce},
    {"coll/scantst", coll_scantst},
    {"coll/scatter2", coll_scatter2},
    {"coll/scatter3", coll_scatter3},
    {"coll/scattern", coll_scattern},
    {"coll/scatterv", coll_scatterv},
    {NULL, NULL}
};

#endif /* MULTI_TESTS */
