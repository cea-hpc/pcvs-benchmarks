--------------------------------------------------
Intel(R) MPI Benchmarks 4.0
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
communicators. For details, see the Intel(R) MPI Benchmarks User's guide 
located in the <install-dir>/doc directory.

By default, Intel(R) MPI Benchmarks is installed at:
  - C:\Program Files (x86)\Intel\imb on Windows* OS 
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
         |                   guide, in HTML and PDF formats.
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
New in Intel(R) MPI Benchmarks 4.0
-------------------------------------------

This release includes the following updates as compared to the Intel(R) MPI 
Benchmarks 3.2.4: 
- Introduced new components IMB-NBC and IMB-RMA that conform to the MPI-3.0 
  standard.
  Note: 
  These components can only be built and used with MPI libraries that conform 
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

Intel(R) MPI Benchmarks 3.2 Update 4 is an update release of  
Intel(R) MPI Benchmarks 3.2 Update 3.

This release includes the following update compared to 
Intel(R) MPI Benchmarks 3.2 Update 3 (see product documentation for details):

- Support for Intel(R) Xeon Phi(TM) coprocessor (codename: Knights Corner)
based on Intel(R) Many Integrated Core Architecture (Intel(R) MIC Architecture)


New in Intel(R) MPI Benchmarks 3.2 Update 3
-------------------------------------------

Intel(R) MPI Benchmarks 3.2 Update 3 is an update release of 
Intel(R) MPI Benchmarks 3.2 Update 2.

This release includes the following updates compared to 
Intel(R) MPI Benchmarks 3.2 Update 2 (see product documentation for details):

- New command-line option -msglog to control the maximum allocated message 
  length
- New option -thread_level to specify the desired thread level support for 
  MPI_Init_thread
- Thread safety support in the MPI initialization phase


New in Intel(R) MPI Benchmarks 3.2 Update 2
-------------------------------------------

Intel(R) MPI Benchmarks 3.2 Update 2 is an update release of 
Intel(R) MPI Benchmarks 3.2 Update 1.

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

-Run time control

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
2)Open the relevant Visual Studio 2010*, Visual Studio 2012*, or Visual 
  Studio 2013* projects under the WINDOWS subfolder for Intel(R) MPI Benchmarks.
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
INFORMATION IN THIS DOCUMENT IS PROVIDED IN CONNECTION WITH INTEL PRODUCTS. 
NO LICENSE, EXPRESS OR IMPLIED, BY ESTOPPEL OR OTHERWISE, TO ANY INTELLECTUAL 
PROPERTY RIGHTS IS GRANTED BY THIS DOCUMENT. EXCEPT AS PROVIDED IN INTEL'S TERMS 
AND CONDITIONS OF SALE FOR SUCH PRODUCTS, INTEL ASSUMES NO LIABILITY WHATSOEVER
AND INTEL DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY, RELATING TO SALE AND/OR USE 
OF INTEL PRODUCTS INCLUDING LIABILITY OR WARRANTIES RELATING TO FITNESS FOR A 
PARTICULAR PURPOSE, MERCHANTABILITY, OR INFRINGEMENT OF ANY PATENT, COPYRIGHT OR 
OTHER INTELLECTUAL PROPERTY RIGHT. 

A "Mission Critical Application" is any application in which failure of the 
Intel Product could result, directly or indirectly, in personal injury or death. 
SHOULD YOU PURCHASE OR USE INTEL'S PRODUCTS FOR ANY SUCH MISSION CRITICAL 
APPLICATION, YOU SHALL INDEMNIFY AND HOLD INTEL AND ITS SUBSIDIARIES, 
SUBCONTRACTORS AND AFFILIATES, AND THE DIRECTORS, OFFICERS, AND EMPLOYEES OF 
EACH, HARMLESS AGAINST ALL CLAIMS COSTS, DAMAGES, AND EXPENSES AND REASONABLE 
ATTORNEYS' FEES ARISING OUT OF, DIRECTLY OR INDIRECTLY, ANY CLAIM OF PRODUCT 
LIABILITY, PERSONAL INJURY, OR DEATH ARISING IN ANY WAY OUT OF SUCH MISSION 
CRITICAL APPLICATION, WHETHER OR NOT INTEL OR ITS SUBCONTRACTOR WAS NEGLIGENT 
IN THE DESIGN, MANUFACTURE, OR WARNING OF THE INTEL PRODUCT OR ANY OF ITS PARTS. 

Intel may make changes to specifications and product descriptions at any time, 
without notice. Designers must not rely on the absence or characteristics of any 
features or instructions marked "reserved" or "undefined". Intel reserves these 
for future definition and shall have no responsibility whatsoever for conflicts 
or incompatibilities arising from future changes to them. The information here 
is subject to change without notice. Do not finalize a design with this 
information. 

The products described in this document may contain design defects or errors 
known as errata which may cause the product to deviate from published 
specifications. Current characterized errata are available on request. 

Contact your local Intel sales office or your distributor to obtain the latest 
specifications and before placing your product order. 

Copies of documents which have an order number and are referenced in this 
document, or other Intel literature, may be obtained by calling 1-800-548-4725, 
or go to: http://www.intel.com/design/literature.htm

Intel processor numbers are not a measure of performance. Processor numbers 
differentiate features within each processor family, not across different 
processor families. Go to: 
http://www.intel.com/products/processor_number/

Software and workloads used in performance tests may have been optimized for performance
only on Intel microprocessors. Performance tests, such as SYSmark and MobileMark,
are measured using specific computer systems, components, software, operations and
functions. Any change to any of those factors may cause the results to vary. You
should consult other information and performance tests to assist you in fully evaluating
your contemplated purchases, including the performance of that product when combined
with other products. 

MPEG-1, MPEG-2, MPEG-4, H.261, H.263, H.264, MP3, DV, VC-1, MJPEG, AC3, AAC, 
G.711, G.722, G.722.1, G.722.2, AMRWB, Extended AMRWB (AMRWB+), G.167, G.168, 
G.169, G.723.1, G.726, G.728, G.729, G.729.1, GSM AMR, GSM FR are international 
standards promoted by ISO, IEC, ITU, ETSI, 3GPP and other organizations. 
Implementations of these standards, or the standard enabled platforms may 
require licenses from various entities, including Intel Corporation.

BlueMoon, BunnyPeople, Celeron, Celeron Inside, Centrino, Centrino Inside, Cilk, 
Core Inside, E-GOLD, Flexpipe, i960, Intel, the Intel logo, Intel AppUp, Intel 
Atom, Intel Atom Inside, Intel CoFluent, Intel Core, Intel Inside, 
Intel Insider, the Intel Inside logo, Intel NetBurst, Intel NetMerge, Intel 
NetStructure, Intel SingleDriver, Intel SpeedStep, Intel Sponsors of Tomorrow., 
the Intel Sponsors of Tomorrow. logo, Intel StrataFlash, Intel vPro, 
Intel Xeon Phi, Intel XScale, InTru, the InTru logo, the InTru Inside logo, 
InTru soundmark, Itanium, Itanium Inside, MCS, MMX, Pentium, Pentium Inside, 
Puma, skoool, the skoool logo, SMARTi, Sound Mark, Stay With It, The Creators 
Project, The Journey Inside, Thunderbolt, Ultrabook, vPro Inside, VTune, Xeon, 
Xeon Inside, X-GOLD, XMM, X-PMU and XPOSYS are trademarks of Intel Corporation 
in the U.S. and/or other countries.

* Other names and brands may be claimed as the property of others.

Microsoft, Windows, Visual Studio, Visual C++, and the Windows logo are 
trademarks, or registered trademarks of Microsoft Corporation in the 
United States and/or other countries.

Java is a registered trademark of Oracle and/or its affiliates.



Optimization Notice
-------------------
Intel's compilers may or may not optimize to the same degree for non-Intel 
microprocessors for optimizations that are not unique to Intel microprocessors.
These optimizations include SSE2, SSE3, and SSSE3 instruction sets and other 
optimizations. Intel does not guarantee the availability, functionality, 
or effectiveness of any optimization on microprocessors not manufactured 
by Intel. Microprocessor-dependent optimizations in this product are intended 
for use with Intel microprocessors. Certain optimizations not specific to Intel
microarchitecture are reserved for Intel microprocessors. Please refer to the 
applicable product User and Reference Guides for more information regarding the
specific instruction sets covered by this notice.

Notice revision #20110804

Copyright (C) 2004-2014, Intel Corporation. All rights reserved.
