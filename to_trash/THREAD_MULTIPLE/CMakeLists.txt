
set(CMAKE_C_COMPILER ${MPC_CC})
foreach(name bw bw_th collcomp_noth conc_allred conc_allred_th latency
latency_th message_rate message_rate_th no_overlap overlap shortlong
shortlong_th)

add_executable (${name} ${name}.c)
target_link_libraries(${name} ${MPC_LDFLAGS_VAL})

foreach(tasknb ${MPC_TASK_NB_LIST})
foreach(procnb ${MPC_PROC_NB_LIST})
foreach(nettype ${MPC_NET_LIST})
foreach(threadtype ${MPC_THREAD_LIST})

if(  ( ${procnb} LESS ${tasknb} ) OR ( ${procnb} EQUAL ${tasknb} ) )
    add_test(${WORKDIR}_${name}_${threadtype}_${nettype}_proc_nb_${procnb}_task_${tasknb} ${MPCRUN} --clean --autokill=${MPC_AUTO_KILL_TIMEOUT} -n=${tasknb}  --share-node -p=${procnb} -net=${nettype} -m=${threadtype}  ${MPCRUN_ARGS} ./${name})
endif(  ( ${procnb} LESS ${tasknb} ) OR ( ${procnb} EQUAL ${tasknb} ) )


endforeach(threadtype)
endforeach(nettype)
endforeach(procnb)
endforeach(tasknb)
endforeach(name)


