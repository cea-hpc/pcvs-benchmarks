############################################################################
#                                                                          #
#                         Copyright or (C) or Copr.                        #
#       Commissariat a l'Energie Atomique et aux Energies Alternatives     #
#                                                                          #
# Version : 1.2                                                            #
# Date    : Tue Jul 22 13:28:10 CEST 2014                                  #
# Ref ID  : IDDN.FR.001.160040.000.S.P.2015.000.10800                      #
# Author  : Julien Adam <julien.adam@cea.fr>                               #
#           Marc Perache <marc.perache@cea.fr>                             #
#                                                                          #
# This file is part of JCHRONOSS software.                                 #
#                                                                          #
# This software is governed by the CeCILL-C license under French law and   #
# abiding by the rules of distribution of free software.  You can  use,    #
# modify and/or redistribute the software under the terms of the CeCILL-C  #
# license as circulated by CEA, CNRS and INRIA at the following URL        #
# "http://www.cecill.info".                                                #
#                                                                          #
# As a counterpart to the access to the source code and  rights to copy,   #
# modify and redistribute granted by the license, users are provided only  #
# with a limited warranty  and the software's author,  the holder of the   #
# economic rights,  and the successive licensors  have only  limited       #
# liability.                                                               #
#                                                                          #
# In this respect, the user's attention is drawn to the risks associated   #
# with loading,  using,  modifying and/or developing or reproducing the    #
# software by the user in light of its specific status of free software,   #
# that may mean  that it is complicated to manipulate,  and  that  also    #
# therefore means  that it is reserved for developers  and  experienced    #
# professionals having in-depth computer knowledge. Users are therefore    #
# encouraged to load and test the software's suitability as regards their  #
# requirements in conditions enabling the security of their systems and/or #
# data to be ensured and,  more generally, to use and operate it in the    #
# same conditions as regards security.                                     #
#                                                                          #
# The fact that you are presently reading this means that you have had     #
# knowledge of the CeCILL-C license and that you accept its terms.         #
#                                                                          #
############################################################################
#set(XML_TESTS testSimple testWithDeps testFailed testMultiJobs)

#pre actions : dirty hook...
set(GENERIC_CALL ${CMAKE_BINARY_DIR}/src/jchronoss --master)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testSimple.xml ${CMAKE_CURRENT_BINARY_DIR}/testSimple.xml)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testWithDeps.xml ${CMAKE_CURRENT_BINARY_DIR}/testWithDeps.xml)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testFailed.xml ${CMAKE_CURRENT_BINARY_DIR}/testFailed.xml)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testMultiJobs.xml ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/testBlocking.xml ${CMAKE_CURRENT_BINARY_DIR}/testBlocking.xml)

#good execution
add_test(Integration_noTest ${GENERIC_CALL} --nb-resources=1)
add_test(Integration_testSimple ${GENERIC_CALL} --nb-resources=10 ${CMAKE_CURRENT_BINARY_DIR}/testSimple.xml)
add_test(Integration_testWithDeps ${GENERIC_CALL} --nb-resources=10 ${CMAKE_CURRENT_BINARY_DIR}/testWithDeps.xml)
add_test(Integration_testFailed ${GENERIC_CALL} --nb-resources=10 ${CMAKE_CURRENT_BINARY_DIR}/testFailed.xml)
add_test(Integration_testMultiJobs ${GENERIC_CALL} --nb-resources=10 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_blocking ${GENERIC_CALL} --nb-resources=10 ${CMAKE_CURRENT_BINARY_DIR}/testBlocking.xml)
add_test(Integration_policy0 ${GENERIC_CALL} --nb-resources=10 --policy=0 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_policy1 ${GENERIC_CALL} --nb-resources=10 --policy=1 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_policy2 ${GENERIC_CALL} --nb-resources=10 --policy=2 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
#add_test(Integration_policy3 ${GENERIC_CALL} --nb-resources=10 --policy=3 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_wrapper ${GENERIC_CALL} --nb-resources=10 --launcher=${CMAKE_CURRENT_SOURCE_DIR}/wrapper.sh ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
set_tests_properties(Integration_wrapper PROPERTIES PASS_REGULAR_EXPRESSION "STARTED BY WRAPPER")
add_test(Integration_autokill ${GENERIC_CALL} --nb-resources=10 --autokill=2 ${CMAKE_CURRENT_BINARY_DIR}/testBlocking.xml)
set_tests_properties(Integration_autokill PROPERTIES PASS_REGULAR_EXPRESSION "Interrupted by Autokill")
add_test(Integration_withConfig ${GENERIC_CALL} --config-file=${CMAKE_CURRENT_SOURCE_DIR}/configSimple.xml ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_result1 ${GENERIC_CALL} --nb-resources=10 --verbose ${CMAKE_CURRENT_BINARY_DIR}/testFailed.xml)
set_tests_properties(Integration_result1 PROPERTIES PASS_REGULAR_EXPRESSION "This is the output of the simple failed test !")
add_test(Integration_result2 ${GENERIC_CALL} --nb-resources=10 --very-verbose ${CMAKE_CURRENT_BINARY_DIR}/testSimple.xml)
set_tests_properties(Integration_result2 PROPERTIES PASS_REGULAR_EXPRESSION "This is the output of the simple succeeded test !")

#bad execution
add_test(Integration_resources_error ${GENERIC_CALL} --nb-resources=1 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_policy_error1 ${GENERIC_CALL} --nb-resources=10 --policy=4 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_policy_error2 ${GENERIC_CALL} --nb-resources=10 --policy=-1 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_min_max_error ${GENERIC_CALL} --nb-resources=10 --policy=2 --maxt-slave=10 --mint-slave=20 ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_wrapper_error ${GENERIC_CALL} --nb-resources=10 --launcher=${CMAKE_CURRENT_SOURCE_DIR}/wrapper_error.sh ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_withConfig_error1 ${GENERIC_CALL} --config-file=${CMAKE_CURRENT_SOURCE_DIR}/DontExists.xml ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)
add_test(Integration_withConfig_error2 ${GENERIC_CALL} --config-file=${CMAKE_CURRENT_SOURCE_DIR}/configError.xml ${CMAKE_CURRENT_BINARY_DIR}/testMultiJobs.xml)

set(WILL_FAIL_TESTS Integration_resources_error Integration_policy_error1 Integration_policy_error2 Integration_min_max_error Integration_wrapper_error Integration_withConfig_error1 Integration_withConfig_error2)
set_tests_properties(${WILL_FAIL_TESTS} PROPERTIES WILL_FAIL true)
