
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

########################################################
#init CMakeLists.txt
cmake_minimum_required(VERSION 2.6)
project(JCHRONOSS)


########################################################
#Globals vars
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_VERBOSE_MAKEFILE false)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)
set(ADD_FLAGS "-Wall -pedantic -ansi -Wextra -Wno-long-long" CACHE STRING "Define main flags to use at compilation time")
set(JCHRONOSS_VERSION "1.2")

########################################################
#Possible vars set by user
#NDEBUG            --> activate debug mode
#ENABLE_COLOR      --> output color
#ENABLE_TRACE      --> Jsloc input file generation
#ENABLE_TEST_SUITE --> activate test-suite compilation
#ENABLE_MEASURE    --> activate Time measurement (verbose !)
#ENABLE_ALL        --> activate ENABLE_*
#DISABLE_OPENMP    --> disable OpenMP even if found

########################################################
#dependances packages finding
Find_Package(LibXml2 REQUIRED)
Find_package(Threads REQUIRED)
Find_Package(OpenMP)
Find_package(Doxygen)

########################################################
#add found directories and libraries to path inclusion
include_directories(${LIBXML2_INCLUDE_DIR})
link_directories(${LIBXML2_LIBS_DIR})
set(LIBS_TO_LINK ${LIBXML2_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

########################################################
#include subdirectories
add_subdirectory(src)
add_subdirectory(tools)

if(ENABLE_TEST_SUITE OR ENABLE_ALL)
	enable_testing()
	add_subdirectory(tests)
endif(ENABLE_TEST_SUITE OR ENABLE_ALL)

########################################################
#Generate Documentation if DOxygen is found
if(DOXYGEN_FOUND)
	set(DOXYFILE ${CMAKE_BINARY_DIR}/doc/Doxyfile)
	#move Doxyfile
	configure_file(${CMAKE_SOURCE_DIR}/doc/Doxyfile.in ${DOXYFILE})
	add_custom_target(doc COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYFILE} WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/doc)
endif()

########################################################
#packaging generation
include(InstallRequiredSystemLibraries)
set(CPACK_PACKAGE_NAME "JCHRONOSS")
Set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A Job Checker under Resources constraint with Optimized and Scalable Scheduler")
set(CPACK_PACKAGE_VENDOR "CEA, DAM, DIF")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/README.md")
set(CPACK_RESOURCES_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYRIGHT.EN")
set(CPACK_PACKAGE_VERSION "${JCHRONOSS_VERSION}")
set(CPACK_GENERATOR "ZIP;TGZ;TBZ2;DEB;RPM")
set(CPACK_PACKAGE_CONTACT "Julien Adam <julien.adam@cea.fr")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")
include(CPack)
