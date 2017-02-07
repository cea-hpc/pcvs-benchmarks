#!/bin/sh
############################# MPC License ##############################
# Wed Nov 19 15:19:19 CET 2008                                         #
# Copyright or (C) or Copr. Commissariat a l'Energie Atomique          #
#                                                                      #
# IDDN.FR.001.230040.000.S.P.2007.000.10000                            #
# This file is part of the MPC Runtime.                                #
#                                                                      #
# This software is governed by the CeCILL-C license under French law   #
# and abiding by the rules of distribution of free software.  You can  #
# use, modify and/ or redistribute the software under the terms of     #
# the CeCILL-C license as circulated by CEA, CNRS and INRIA at the     #
# following URL http://www.cecill.info.                                #
#                                                                      #
# The fact that you are presently reading this means that you have     #
# had knowledge of the CeCILL-C license and that you accept its        #
# terms.                                                               #
#                                                                      #
# Authors:                                                             #
#   - VALAT Sebastien sebastien.valat@cea.fr                           #
#                                                                      #
########################################################################

################## LOAD CONFIG FILE #####################
#default
. ${MPC_TEST_SOURCE_DIR}/config/default.cfg
#the current one
. ${MPC_TEST_WORK_DIR}/config.cfg

################# ARGUEMENT COMPLETION ##################
#apply some links between arguments and complete some otheres
#prefix MPCRUN with MPCRUN_WRAPPER
#if [ ! -z "${MPCRUN_WRAPPER}" ]; then
#	launcher="${MPC_TEST_SOURCE_DIR}/build_scripts/launchers/${MPCRUN_WRAPPER}"
#   MPCRUN="${launcher} ${MPCRUN}"
#	if [ ! -f "${launcher}" ]; then common_print_error "Invalid launcher : ${launcher}"; fi
#fi
