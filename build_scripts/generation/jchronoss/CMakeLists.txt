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
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

########################################################
#Globals vars
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_VERBOSE_MAKEFILE OFF)
set(JCHRONOSS_VERSION "1.2")

########################################################
#dependances packages finding
Find_package(Threads REQUIRED)
Find_Package(OpenMP)
Find_package(Doxygen)
	Find_Package(LibXml2 REQUIRED)
	include_directories(${LIBXML2_INCLUDE_DIR})
########################################################
#JCHRONOSS-specific CFLAGS

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
#add found directories and libraries to path inclusion
set(CMAKE_CXX_FLAGS "-Wall -pedantic -ansi -Wextra")
set(EXTRA_LIBS ${LIBXML2_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

option(ENABLE_OPENMP "Enabling OpenMP for JCHRONOSS" ON)
option(ENABLE_TESTS "Building the validation suite with 'make test'" OFF)
option(ENABLE_TRACE "Enabling Trace generation (JsLoc input)" OFF)
option(ENABLE_PROFILING "Enabling time report for different code sections (for debug)" OFF)
option(ENABLE_COLOR "Enabling the color for JCHRONOSS" OFF)
option(ENABLE_ALL "Enabling all options above" OFF)
option(ENABLE_SSL "Enabling SSL options for log server (implies ENALBE_PLUGIN_SERVER)" ON)
option(SPACK_DRIVEN "Dependencies are handled by Spack this way" OFF)

option(ENABLE_PLUGIN_SERVER "Enabling real-time through dedicated log server" OFF)
option(ENABLE_PLUGIN_CURSES "Enabling ncurses-based frontend" OFF)
option(ENABLE_PLUGIN_JENKINS "Compiling JCHRONOSS-oriented Jenkins plugin" OFF)
option(ENABLE_PLUGIN_ALL "Enabling all plugin modules above" OFF)

#defining "_ALL" options
if(ENABLE_ALL)
	set(ENABLE_OPENMP ON)
	set(ENABLE_COLOR ON)
	set(ENABLE_TESTS ON)
	set(ENABLE_PROFILING ON)
	set(ENABLE_TRACE ON)
endif()

if(ENABLE_PLUGIN_ALL)
	set(ENABLE_PLUGIN_SERVER ON)
	set(ENABLE_PLUGIN_CURSES ON)
	set(ENABLE_PLUGIN_JENKINS ON)
endif()

if(ENABLE_SSL AND OPENSSL_FOUND)
	set(ENABLE_PLUGIN_SERVER ON)
endif()

#Special cases to make JCHRONOSS supporting plugins/ dir
if(ENABLE_PLUGIN_SERVER)
	find_package(LibEV REQUIRED)
	include_directories(${LIBEV_INCLUDE_DIRS})
	list(APPEND EXTRA_LIBS -lev)
	add_definitions("-DENABLE_PLUGIN_SERVER")

	if(SPACK_DRIVEN)
		list(APPEND EXTRA_LIBS -lwebsockets)	
	else(SPACK_DRIVEN)
		#libwebsockets-specific building
		#set(LWS_WITHOUT_TESTAPPS ON)
		#set(LWS_WITH_SHARED OFF)
		#if(NOT ENABLE_SSL)
		#set(LWS_WITH_SSL OFF)
		#endif()
		add_subdirectory(deps/libwebsockets)
	endif(SPACK_DRIVEN)

endif(ENABLE_PLUGIN_SERVER)

########################################################
#include subdirectories
add_subdirectory(doc)
add_subdirectory(plugins)
add_subdirectory(src)
add_subdirectory(tools)

if(ENABLE_TESTS)
	enable_testing()
	add_subdirectory(tests)
endif(ENABLE_TESTS)

message(STATUS "Summary for the current JCHRONOSS build:")
message(STATUS "Install Prefix   = ${CMAKE_INSTALL_PREFIX}")
message(STATUS "LibXML Prefix    = ${LIBXML2_LIBRARIES}")
if(ENABLE_PLUGIN_SERVER)
message(STATUS "LibEV prefix     = ${LIBEV_LIBRARIES}")
endif(ENABLE_PLUGIN_SERVER)
message(STATUS "OpenMP Support   = ${OPENMP_FOUND}")
message(STATUS "Doxygen Detect   = ${DOXYGEN_FOUND}")
message(STATUS "OpenSSL Support  = ${OPENSSL_FOUND}")
message(STATUS "")

message(STATUS "Enabled options:")
message(STATUS "ENABLE_OPENMP    = ${ENABLE_OPENMP}")
message(STATUS "ENABLE_COLOR     = ${ENABLE_COLOR}")
message(STATUS "ENABLE_TESTS     = ${ENABLE_TESTS}")
message(STATUS "ENABLE_PROFILING = ${ENABLE_PROFILING}")
message(STATUS "ENABLE_TRACE     = ${ENABLE_TRACE}")
message(STATUS "ENABLE_ALL       = ${ENABLE_ALL}")
message(STATUS "ENABLE_SSL       = ${ENABLE_SSL}")
message(STATUS "")

message(STATUS "Enabled plugins for this build:")
message(STATUS "ENABLE_PLUGIN_SERVER  = ${ENABLE_PLUGIN_SERVER}")
message(STATUS "ENABLE_PLUGIN_CURSES  = ${ENABLE_PLUGIN_CURSES}")
message(STATUS "ENABLE_PLUGIN_JENKINS = ${ENABLE_PLUGIN_JENKINS}")
message(STATUS "ENABLE_PLUGIN_ALL     = ${ENABLE_PLUGIN_ALL}")
message(STATUS "")
########################################################
#packaging generation
include(InstallRequiredSystemLibraries)
set(PACKAGE "jchronoss")
set(CPACK_GENERATOR "ZIP;TGZ;TBZ2;DEB;RPM")
set(CPACK_PACKAGE_NAME "JCHRONOSS")
set(CPACK_PACKAGE_FILE_NAME "JCHRONOSS")
set(CPACK_PACKAGE_VERSION "${JCHRONOSS_VERSION}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")
set(CPACK_PACKAGE_CONTACT "Julien Adam <adamj@paratools.com>")
set(CPACK_PACKAGE_VENDOR "CEA, DAM, DIF")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/README.md")
Set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A Parallel and Resilient Frontend for High Performance Validation Suites")
set(CPACK_RESOURCES_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYRIGHT.EN")
include(CPack)
