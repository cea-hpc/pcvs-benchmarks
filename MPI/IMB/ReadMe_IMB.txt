--------------------------------------------------
Intel(R) MPI Benchmarks 2017
README
--------------------------------------------------

--------
Contents
--------

- Introduction
- Product Directories
- What's New
- Command-Line Control
- Building Instructions for Linux* OS for Intel(R) Many Integrated Core 
  Architecture (Intel(R) MIC Architecture)
- Building Instructions for Windows* OS
- Copyright and License Information
- Legal Information


------------
Introduction
------------
Intel(R) MPI Benchmarks provides a set of elementary benchmarks that conform
to MPI-1, MPI-2, and MPI-3 standard.
You can run all of the supported benchmarks, or a subset specified in the
command line using one executable file. Use command-line parameters to specify
various settings, such as time measurement, message lengths, and selection of 
communicators. For details, see the Intel(R) MPI Benchmarks User's Guide 
located in the <install-dir>/doc directory.

By default, Intel(R) MPI Benchmarks is installed at:
  - C:\Program Files (x86)\IntelSWTools\imb on Windows* OS 
  - /opt/intel/imb on Linux* OS

Before using the Intel(R) MPI Benchmarks, please read the license agreements 
located in the imb/license directory.

-------------------
Product Directories
-------------------
After a successful installation of Intel(R) MPI Benchmarks, the following 
files and folders appear on your system:

    +-- \imb            Intel(R) MPI Benchmarks product directory
         |
         +-- \doc            Documentation directory. 
         |                   Includes the Intel(R) MPI Benchmarks User's
         |                   Guide, in HTML and PDF formats.
         |
         +-- \license           Product license files.
         |    |              
         |    +--license.txt    Source code license granted to you.
         |    |                             
         |    +--use-of-trademark-license.txt    License file describing the 
         |                                       use of the Intel(R) MPI 
         |                                       Benchmarks name and trademark.
         |
         +-- \src                  Product source code and Makefiles. 
         |
         +-- \WINDOWS              Microsoft* Visual Studio* project files. 
         |
         +-- Readme_IMB.txt        Readme file providing the basic information
                                   about the product (this file).

----------
What's New
----------
New in Intel(R) MPI Benchmarks 2017
-------------------------------------------
- Changed default values for the -sync and -root_shift options.
- Support for the Microsoft* Visual Studio* 2015. Microsoft* Visual Studio* 2010
support is removed.
- Bug fixes.

New in Intel(R) MPI Benchmarks 4.1 Update 1
-------------------------------------------
This release includes the following updates as compared to the Intel(R) MPI 
Benchmarks 4.1:
- Bug fixes.

New in Intel(R) MPI Benchmarks 4.1
-------------------------------------------
This release includes the following updates as compared to the Intel(R) MPI 
Benchmarks 4.0 Update 2: 
- Introduced two new benchmarks: uniband and biband.
- Introduced two new command-line options for collective benchmarks: -sync and -root_shift.

New in Intel(R) MPI Benchmarks 4.0 Update 2
-------------------------------------------
This release includes the following updates as compared to the Intel(R) MPI 
Benchmarks 4.0 Update 1: 
- Fix of a bug where benchmarking was failing on certain message lengths with -DCHECK.

New in Intel(R) MPI Benchmarks 4.0 Update 1
-------------------------------------------
This release includes the following updates as compared to the Intel(R) MPI 
Benchmarks 4.0: 
- Fix of a bug where benchmarking could continue after the time limit is exceeded.

New in Intel(R) MPI Benchmarks 4.0
-------------------------------------------
This release includes the following updates as compared to the Intel(R) MPI 
Benchmarks 3.2.4: 
- Introduced new components IMB-NBC and IMB-RMA that conform to the MPI-3.0 
standard.
  Note: These components can only be built and used with MPI libraries that conform 
  to the MPI-3 standard.
- Added new targets to the Linux* OS Makefiles:
    - NBC for building IMB-NBC
    - RMA for building IMB-RMA 
- Updated Microsoft* Visual Studio* solutions to include the IMB-NBC and 
IMB-RMA targets.
- Consolidated all first-use documents in ReadMe_IMB.txt to improve usability.
- Introduced a new feature to set the appropriate algorithm for automatic calculation
of iterations. The algorithm can be set through the -iter and -iter_policy options.
- Support for the Microsoft* Visual Studio* 2013. Microsoft* Visual Studio* 2008
support is removed.

New in Intel(R) MPI Benchmarks 3.2 Update 4
-------------------------------------------
This release includes the following update compared to 
Intel(R) MPI Benchmarks 3.2 Update 3 (see product documentation for details):
- Support for Intel(R) Xeon Phi(TM) coprocessor (codename: Knights Corner)
based on Intel(R) Many Integrated Core Architecture (Intel(R) MIC Architecture)

New in Intel(R) MPI Benchmarks 3.2 Update 3
-------------------------------------------
This release includes the following updates compared to 
Intel(R) MPI Benchmarks 3.2 Update 2:
- New command-line option -msglog to control the maximum allocated message 
  length
- New option -thread_level to specify the desired thread level support for 
  MPI_Init_thread
- Thread safety support in the MPI initialization phase

New in Intel(R) MPI Benchmarks 3.2 Update 2
-------------------------------------------
- Support for large buffers greater than 2 GB for some MPI benchmarks
- New benchmarks PingPongSpecificSource and PingPingSpecificSource. The exact
  destination rank is used for these tests instead of MPI_ANY_SOURCE as in 
  PingPong and PingPing tests.
  Use the include option to enable new benchmarks. For instance, include 
  PingPongSpecificSource PingPingSpecificSource
- New options -include/-exclude to better control benchmarks list
    
New in Intel(R) MPI Benchmarks 3.2 Update 1
-------------------------------------------
Compared to Intel(R) MPI Benchmanrks 3.2, Intel(R) MPI Benchmarks 3.2.1 
includes the following updates:
- Fix of memory corruption when the -msglen command-line option is used
  with the Intel(R) MPI Benchmark executables
- Fix in Accumulate benchmark related to using the CHECK conditional
  compilation macro
- Fix for integer overflow in dynamic calculations on the number of iterations
- Recipes for building IA-32 executables within Microsoft* Visual Studio* 2005
  and Microsoft* Visual Studio* 2008 project folders associated with the
  Intel(R) MPI Benchmarks

New in Intel(R) MPI Benchmarks 3.2
----------------------------------
Compared to its predecessors, Intel(R) MPI Benchmarks 3.2 has one other default 
setting:
- Runtime control

   Intel(R) MPI Benchmarks dynamically computes a repetition count for each 
   experiment so that the sum of the resulting repetitions (roughly) does not 
   exceed a given run time. The run time for each sample (= 1 message size with 
   repetitions) is set in => IMB_settings.h, IMB_settings_io.h 
   (current value: 10 seconds, preprocessor variable SECS_PER_SAMPLE)

   To override this behavior, use the flag -time

--------------------
Command-Line Control
--------------------

You can get help on the Intel(R) MPI Benchmarks from the command line using 
the component name and the -help parameter. For example, for the IMB-MPI1 
component, run:
IMB-MPI1 -help

You can see the Intel(R) MPI Benchmarks User's Guide for details on the 
command-line parameters.

---------------------------------------------------------------------
Building Instructions for Linux* OS for Intel(R) Many Integrated Core 
Architecture (Intel MIC Architecture)
---------------------------------------------------------------------

To build Intel(R) MPI Benchmarks for Linux* OS for Intel(R) Many Integrated 
Core Architecture (Intel(R) MIC Architecture), follow these steps:

1. Build the Intel MPI Benchmarks for the host system:
   host$ source <path to composerxe_mic directory>/composerxe_mic/bin/compilervars.sh intel64
   host$ source <path to Intel MPI Library directory>/intel64/bin/mpivars.sh
   host$ cd <path to Intel MPI Benchmarks directory>/src
   host$ make -f make_ict

2. Build the Intel MPI Benchmarks for the target system with the 
   Intel(R) MIC Architecture. Note that the corresponding make_ict_mic 
   makefile uses the -mmic option that may be specific to Intel(R) compilers.
   host$ source <path to composerxe_mic directory>/composerxe_mic/bin/compilervars.sh intel64
   host$ source <path to Intel MPI Library directory>/intel64/bin/mpivars.sh
   host$ cd <path to Intel MPI Benchmarks directory>/src
   host$ make -f make_ict_mic

For details on running the resulting executable files, see the Intel(R) MPI 
Library documentation.

-----------------------------------------
Building Instructions for Windows* OS
-----------------------------------------
Use the enclosed solution files located in the component-specific 
subdirectories under the imb/WINDOWS directory. Click on the respective 
".vcproj" or ".vcxproj" project file and use the Microsoft* Visual Studio* 
menu to run the associated benchmark application.

Building "x64" Executable Files 
-------------------------------
1) Check that the Include, Lib, and Path environment variables are set as follows:
    %I_MPI_ROOT%\intel64\include
    %I_MPI_ROOT%\intel64\lib
    %I_MPI_ROOT%\mpi\intel64\bin
The %I_MPI_ROOT% environment variable is set to the Intel(R) MPI Library 
installation directory. The default installation directory is
C:\Program Files (x86)\Intel\mpi\<version>, where <version> is the product version.
2) Open the ".vcproj" or ".vcxproj" file for the component you would like to 
   build. From the Visual Studio Project panel:
   a) Change the "Solution Platforms" dialog box to "x64".
   b) Change the "Solution Configurations" dialog box to "Release".
   c) Check other settings as required, for example:
    General > Project Defaults
       - Set "Character Set" to "Use Multi-Byte Character Set"
    C/C++ > General 
       - Set "Additional Include Directories" to 
           "$(I_MPI_ROOT)\intel64\include"
       - Set "Warning Level" to "Level 1 (/W1)"
    C/C++ > Preprocessor
       - For the "Preprocessor definitions" within the Visual Studio 
         projects, add the conditional compilation macros WIN_IMB and 
         _CRT_SECURE_NO_DEPRECATE. Depending on the components you intend to 
         use, add one or more of the following macros: 
         MPI1, EXT, MPIIO, NBC, RMA.
    Linker > Input
       - Set "Additional Dependencies" to "$(I_MPI_ROOT)\intel64\lib\impi.lib". 
         Make sure to add quotes.

3) Use F7 or Build > Build Solution to create an executable.

Building "Win32" Executables
----------------------------

1) Change the Include, Lib, and Path environment variables to the 
   following settings, respectively: 

  %I_MPI_ROOT%\ia32\include
  %I_MPI_ROOT%\ia32\lib
  %I_MPI_ROOT%\ia32\bin

The %I_MPI_ROOT% environment variable is set to the Intel(R) MPI Library 
installation directory. The default installation directory is
C:\Program Files (x86)\Intel\mpi\<version>, where <version> is the product version.
2)Open the relevant Visual Studio project under the WINDOWS subfolder.
  From the Visual Studio Project panel:
  a) Change the "Solution Platforms" dialog box to "Win32".
  b) Change the "Solution Configurations" dialog box to "Release".
  c) Check other settings in the usual style, for example
      General > Project Defaults 
       - Set "Character Set" to "Use Multi-Byte Character Set"
      C/C++ > General 
       - Set "Additional Include Directories" to 
         "$(I_MPI_ROOT)\ia32\include".
       - Set "Warning Level" to "Level 1 (/W1)"
      C/C++ > Preprocessor
       - For the "Preprocessor definitions" within the Visual Studio 
         projects, add the conditional compilation macros WIN_IMB and 
         _CRT_SECURE_NO_DEPRECATE. Depending on the components you intend to 
         use, add one or more of the following macros: 
         MPI1, EXT, MPIIO, NBC, RMA.
      Linker > Input
         Set "Additional Dependencies" to "$(I_MPI_ROOT)\ia32\lib\impi.lib". 
         Make sure to add quotes.

4) Use F7 or Build > Build Solution to create an executable.

----------------------
Copyright and Licenses
----------------------

See the license files in the imb/license directory.

--------------------------------
Legal Information
--------------------------------
No license (express or implied, by estoppel or otherwise) to any intellectual
property rights is granted by this document.

Intel disclaims all express and implied warranties, including without limitation,
the implied warranties of merchantability, fitness for a particular purpose, and
non-infringement, as well as any warranty arising from course of performance,
course of dealing, or usage in trade.

This document contains information on products, services and/or processes in
development. All information provided here is subject to change without notice.
Contact your Intel representative to obtain the latest forecast, schedule,
specifications and roadmaps.

The products and services described may contain defects or errors known as
errata which may cause deviations from published specifications. Current
characterized errata are available on request.

Intel, Intel Core, Xeon, Xeon Phi and the Intel logo are trademarks of Intel
Corporation in the U.S. and/or other countries.

* Other names and brands may be claimed as the property of others.

(C) 2016 Intel Corporation.
