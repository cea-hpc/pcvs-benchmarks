gmake -C CMG_CLEAN
gmake[1]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/CMG_CLEAN'
gmake -C src
gmake[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/CMG_CLEAN/src'
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o meshAndInputData.o meshAndInputData.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o CMGDomainQuery.o CMGDomainQuery.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o CMGIO.o CMGIO.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o CMGGenerator.o CMGGenerator.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o CMGMeshQuery.o CMGMeshQuery.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o CMGGlobalMeshQuery.o CMGGlobalMeshQuery.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o CMGTagQuery.o CMGTagQuery.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o CMGMeshTopology.o CMGMeshTopology.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o dataTypes.o dataTypes.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o fortranUtilities.o fortranUtilities.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o subdivision.o subdivision.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o lex.yy.o lex.yy.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o cmgparse.tab.o cmgparse.tab.c
mpiicpc -shared -fPIC -o libcmgp.so meshAndInputData.o CMGDomainQuery.o CMGIO.o CMGGenerator.o CMGMeshQuery.o CMGGlobalMeshQuery.o CMGTagQuery.o CMGMeshTopology.o dataTypes.o fortranUtilities.o subdivision.o lex.yy.o cmgparse.tab.o
gmake[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/CMG_CLEAN/src'
gmake[1]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/CMG_CLEAN'
gmake -C cmg2Kull
gmake[1]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/cmg2Kull'
gmake -C sources
gmake[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/cmg2Kull/sources'
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-CMG.o C2K-CMG.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_Geom.o C2K-KC_Geom.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_API.o C2K-KC_API.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-Lists.o C2K-Lists.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-Storage.o C2K-Storage.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_Alter.o C2K-KC_Alter.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_Create.o C2K-KC_Create.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_SubDivide.o C2K-KC_SubDivide.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_Check.o C2K-KC_Check.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_CutZone.o C2K-KC_CutZone.c
mpiicc   -I/usr/local/tools/mvapich-intel/include -I../../CMG_CLEAN/src -I. -g -O3 -w -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX   -c -o C2K-KC_Info.o C2K-KC_Info.c
mpiicpc -shared -fPIC -o libc2k.so  C2K-CMG.o C2K-KC_Geom.o C2K-KC_API.o C2K-Lists.o C2K-Storage.o C2K-KC_Alter.o C2K-KC_Create.o C2K-KC_SubDivide.o C2K-KC_Check.o C2K-KC_CutZone.o C2K-KC_Info.o    
gmake[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/cmg2Kull/sources'
gmake[1]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/cmg2Kull'
gmake -C Teton
gmake[1]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton'
gmake -C geom
gmake[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom'
gmake -C CMI
gmake[3]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/CMI'
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  MeshBase.cc -o MeshBase.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  ZoneBase.cc -o ZoneBase.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  CornerBase.cc -o CornerBase.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  FaceBase.cc -o FaceBase.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  SideBase.cc -o SideBase.o
gmake[3]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/CMI'
gmake -C Field
gmake[3]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Field'
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  FieldInst.cc -o FieldInst.o
gmake[3]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Field'
gmake -C Region
gmake[3]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Region'
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  RegionInst.cc -o RegionInst.o
gmake[3]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Region'
gmake[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom'
gmake -C communication
gmake[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/communication'
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  DomainNeighborMapInst.cc -o DomainNeighborMapInst.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  CommAgent.cc -o CommAgent.o
gmake[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/communication'
gmake -C part
gmake[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/part'
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  OpacityBase.cc -o OpacityBase.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  PartInst.cc -o PartInst.o
gmake[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/part'
gmake -C transport
gmake[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport'
gmake -C Teton
gmake[3]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/Teton'
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/kind_mod.F90 > mods/kind_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/kind_mod.f90 -o mods/kind_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/constant_mod.F90 > mods/constant_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/constant_mod.f90 -o mods/constant_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Size_mod.F90 > mods/Size_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Size_mod.f90 -o mods/Size_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include misc/mpi_param_mod.F90 > misc/mpi_param_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c misc/mpi_param_mod.f90 -o misc/mpi_param_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include misc/mpif90_mod.F90 > misc/mpif90_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c misc/mpif90_mod.f90 -o misc/mpif90_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Communicator_mod.F90 > mods/Communicator_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Communicator_mod.f90 -o mods/Communicator_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Quadrature_mod.F90 > mods/Quadrature_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Quadrature_mod.f90 -o mods/Quadrature_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/QuadratureList_mod.F90 > mods/QuadratureList_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/QuadratureList_mod.f90 -o mods/QuadratureList_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Boundary_mod.F90 > mods/Boundary_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Boundary_mod.f90 -o mods/Boundary_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/BoundaryList_mod.F90 > mods/BoundaryList_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/BoundaryList_mod.f90 -o mods/BoundaryList_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Editor_mod.F90 > mods/Editor_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Editor_mod.f90 -o mods/Editor_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/ZoneData_mod.F90 > mods/ZoneData_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/ZoneData_mod.f90 -o mods/ZoneData_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Geometry_mod.F90 > mods/Geometry_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Geometry_mod.f90 -o mods/Geometry_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Material_mod.F90 > mods/Material_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Material_mod.f90 -o mods/Material_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/Profile_mod.F90 > mods/Profile_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/Profile_mod.f90 -o mods/Profile_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/ProfileList_mod.F90 > mods/ProfileList_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/ProfileList_mod.f90 -o mods/ProfileList_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/QuadratureData_mod.F90 > mods/QuadratureData_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/QuadratureData_mod.f90 -o mods/QuadratureData_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/TimeStepControls_mod.F90 > mods/TimeStepControls_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/TimeStepControls_mod.f90 -o mods/TimeStepControls_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/io_mod.F90 > mods/io_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/io_mod.f90 -o mods/io_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/iter_control_mod.F90 > mods/iter_control_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/iter_control_mod.f90 -o mods/iter_control_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/iter_control_list_mod.F90 > mods/iter_control_list_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/iter_control_list_mod.f90 -o mods/iter_control_list_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include mods/radconstant_mod.F90 > mods/radconstant_mod.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c mods/radconstant_mod.f90 -o mods/radconstant_mod.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include misc/assert.F90 > misc/assert.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c misc/assert.f90 -o misc/assert.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include misc/f90advise.F90 > misc/f90advise.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c misc/f90advise.f90 -o misc/f90advise.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include misc/f90fatal.F90 > misc/f90fatal.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c misc/f90fatal.f90 -o misc/f90fatal.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructBoundary.F90 > aux/ConstructBoundary.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructBoundary.f90 -o aux/ConstructBoundary.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructDtControls.F90 > aux/ConstructDtControls.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructDtControls.f90 -o aux/ConstructDtControls.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructEditor.F90 > aux/ConstructEditor.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructEditor.f90 -o aux/ConstructEditor.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructGeometry.F90 > aux/ConstructGeometry.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructGeometry.f90 -o aux/ConstructGeometry.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructIterControls.F90 > aux/ConstructIterControls.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructIterControls.f90 -o aux/ConstructIterControls.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructMaterial.F90 > aux/ConstructMaterial.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructMaterial.f90 -o aux/ConstructMaterial.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructProfile.F90 > aux/ConstructProfile.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructProfile.f90 -o aux/ConstructProfile.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructQuadrature.F90 > aux/ConstructQuadrature.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructQuadrature.f90 -o aux/ConstructQuadrature.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ConstructSize.F90 > aux/ConstructSize.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ConstructSize.f90 -o aux/ConstructSize.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/ResetSize.F90 > aux/ResetSize.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/ResetSize.f90 -o aux/ResetSize.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/addBoundary.F90 > aux/addBoundary.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/addBoundary.f90 -o aux/addBoundary.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/addProfile.F90 > aux/addProfile.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/addProfile.f90 -o aux/addProfile.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/getEdits.F90 > aux/getEdits.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/getEdits.f90 -o aux/getEdits.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/getGeometry.F90 > aux/getGeometry.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/getGeometry.f90 -o aux/getGeometry.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/getRunStats.F90 > aux/getRunStats.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/getRunStats.f90 -o aux/getRunStats.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/setEditorModule.F90 > aux/setEditorModule.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/setEditorModule.f90 -o aux/setEditorModule.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/setEnergyEdits.F90 > aux/setEnergyEdits.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/setEnergyEdits.f90 -o aux/setEnergyEdits.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/setGeometry.F90 > aux/setGeometry.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/setGeometry.f90 -o aux/setGeometry.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/setMaterialModule.F90 > aux/setMaterialModule.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/setMaterialModule.f90 -o aux/setMaterialModule.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/setSnOrder.F90 > aux/setSnOrder.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/setSnOrder.f90 -o aux/setSnOrder.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/setTimeStep.F90 > aux/setTimeStep.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/setTimeStep.f90 -o aux/setTimeStep.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include aux/setZone.F90 > aux/setZone.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c aux/setZone.f90 -o aux/setZone.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/RadMoments.F90 > control/RadMoments.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/RadMoments.f90 -o control/RadMoments.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/advanceRT.F90 > control/advanceRT.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/advanceRT.f90 -o control/advanceRT.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/dtnew.F90 > control/dtnew.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/dtnew.f90 -o control/dtnew.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/newenergy.F90 > control/newenergy.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/newenergy.f90 -o control/newenergy.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/profint.F90 > control/profint.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/profint.f90 -o control/profint.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/radtr.F90 > control/radtr.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/radtr.f90 -o control/radtr.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/rtbatch.F90 > control/rtbatch.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/rtbatch.f90 -o control/rtbatch.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/rtbdry.F90 > control/rtbdry.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/rtbdry.f90 -o control/rtbdry.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/rtedit.F90 > control/rtedit.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/rtedit.f90 -o control/rtedit.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/rtinit.F90 > control/rtinit.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/rtinit.f90 -o control/rtinit.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include control/rtvsrc.F90 > control/rtvsrc.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c control/rtvsrc.f90 -o control/rtvsrc.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/InitExchange.F90 > rt/InitExchange.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/InitExchange.f90 -o rt/InitExchange.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/SweepScheduler.F90 > rt/SweepScheduler.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/SweepScheduler.f90 -o rt/SweepScheduler.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/UpdateMaterialCoupling.F90 > rt/UpdateMaterialCoupling.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/UpdateMaterialCoupling.f90 -o rt/UpdateMaterialCoupling.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/bdyedt.F90 > rt/bdyedt.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/bdyedt.f90 -o rt/bdyedt.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/exchange.F90 > rt/exchange.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/exchange.f90 -o rt/exchange.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/face_coords.F90 > rt/face_coords.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/face_coords.f90 -o rt/face_coords.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/findReflectedAngles.F90 > rt/findReflectedAngles.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/findReflectedAngles.f90 -o rt/findReflectedAngles.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/findexit.F90 > rt/findexit.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/findexit.f90 -o rt/findexit.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/getAbsorptionRate.F90 > rt/getAbsorptionRate.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/getAbsorptionRate.f90 -o rt/getAbsorptionRate.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/initcomm.F90 > rt/initcomm.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/initcomm.f90 -o rt/initcomm.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/quadLobatto.F90 > rt/quadLobatto.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/quadLobatto.f90 -o rt/quadLobatto.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/quadProduct.F90 > rt/quadProduct.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/quadProduct.f90 -o rt/quadProduct.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/quadxyz.F90 > rt/quadxyz.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/quadxyz.f90 -o rt/quadxyz.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rswpmd.F90 > rt/rswpmd.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rswpmd.f90 -o rt/rswpmd.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtave.F90 > rt/rtave.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtave.f90 -o rt/rtave.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtcompton.F90 > rt/rtcompton.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtcompton.f90 -o rt/rtcompton.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtconi.F90 > rt/rtconi.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtconi.f90 -o rt/rtconi.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtconv.F90 > rt/rtconv.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtconv.f90 -o rt/rtconv.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtgeom3.F90 > rt/rtgeom3.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtgeom3.f90 -o rt/rtgeom3.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtmainsn.F90 > rt/rtmainsn.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtmainsn.f90 -o rt/rtmainsn.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtorder.F90 > rt/rtorder.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtorder.f90 -o rt/rtorder.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtplnk.F90 > rt/rtplnk.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtplnk.f90 -o rt/rtplnk.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtquad.F90 > rt/rtquad.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtquad.f90 -o rt/rtquad.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/rtstrtsn.F90 > rt/rtstrtsn.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/rtstrtsn.f90 -o rt/rtstrtsn.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/setIncidentFlux.F90 > rt/setIncidentFlux.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/setIncidentFlux.f90 -o rt/setIncidentFlux.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/setbdy.F90 > rt/setbdy.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/setbdy.f90 -o rt/setbdy.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/testFluxConv.F90 > rt/testFluxConv.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/testFluxConv.f90 -o rt/testFluxConv.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include rt/zone_coords.F90 > rt/zone_coords.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c rt/zone_coords.f90 -o rt/zone_coords.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/GaussLegendreLobattoWgts.F90 > snac/GaussLegendreLobattoWgts.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/GaussLegendreLobattoWgts.f90 -o snac/GaussLegendreLobattoWgts.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/Jacobi.F90 > snac/Jacobi.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/Jacobi.f90 -o snac/Jacobi.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/JacobiLobattoPts.F90 > snac/JacobiLobattoPts.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/JacobiLobattoPts.f90 -o snac/JacobiLobattoPts.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/cyclebreaker.F90 > snac/cyclebreaker.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/cyclebreaker.f90 -o snac/cyclebreaker.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/findseeds.F90 > snac/findseeds.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/findseeds.f90 -o snac/findseeds.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/fixZone.F90 > snac/fixZone.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/fixZone.f90 -o snac/fixZone.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/getDownStreamData.F90 > snac/getDownStreamData.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/getDownStreamData.f90 -o snac/getDownStreamData.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/sccsearch.F90 > snac/sccsearch.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/sccsearch.f90 -o snac/sccsearch.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snflwxyz.F90 > snac/snflwxyz.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snflwxyz.f90 -o snac/snflwxyz.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snmoments.F90 > snac/snmoments.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snmoments.f90 -o snac/snmoments.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snmref.F90 > snac/snmref.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snmref.f90 -o snac/snmref.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snneed.F90 > snac/snneed.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snneed.f90 -o snac/snneed.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snnext.F90 > snac/snnext.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snnext.f90 -o snac/snnext.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snpnmset.F90 > snac/snpnmset.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snpnmset.f90 -o snac/snpnmset.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snreflect.F90 > snac/snreflect.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snreflect.f90 -o snac/snreflect.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snswp3d.F90 > snac/snswp3d.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snswp3d.f90 -o snac/snswp3d.o
fpp -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -I/usr/local/tools/mvapich-intel/include -I./include snac/snynmset.F90 > snac/snynmset.f90
fpp: warning: bad option: -openmp , ignored.
ifort -I/usr/local/tools/mvapich-intel/include -I./include -g -O3 -no-prec-div  -fPIC -openmp   -DLINUX -DLinux -Dmpi -DMPI -DUSE_MPI -DMPICH_SKIP_MPICXX -openmp   -c snac/snynmset.f90 -o snac/snynmset.o
gmake[3]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/Teton'
gmake -C TetonInterface
gmake[3]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/TetonInterface'
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  TetonInst.cc -o TetonInst.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I../.. -I../../../cmg2Kull/sources -I../../../CMG_CLEAN/src -I../../utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  TetonNT.cc -o TetonNT.o
gmake[3]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/TetonInterface'
gmake[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport'
gmake -C utilities
gmake[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/utilities'
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  DBC.cc -o DBC.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  VERIFY.cc -o VERIFY.o
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I.. -I../../cmg2Kull/sources -I../../CMG_CLEAN/src -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  Process.cc -o Process.o
gmake[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/utilities'
mpiicpc -shared -fPIC -o libInfrastructure.so communication/DomainNeighborMapInst.o communication/CommAgent.o geom/Region/RegionInst.o geom/Field/FieldInst.o geom/CMI/MeshBase.o geom/CMI/ZoneBase.o geom/CMI/CornerBase.o geom/CMI/FaceBase.o geom/CMI/SideBase.o part/OpacityBase.o part/PartInst.o transport/TetonInterface/TetonNT.o transport/TetonInterface/TetonInst.o transport/Teton/mods/BoundaryList_mod.o transport/Teton/mods/Boundary_mod.o transport/Teton/mods/Communicator_mod.o transport/Teton/mods/Editor_mod.o transport/Teton/mods/Geometry_mod.o transport/Teton/mods/Material_mod.o transport/Teton/mods/ProfileList_mod.o transport/Teton/mods/Profile_mod.o transport/Teton/mods/QuadratureData_mod.o transport/Teton/mods/QuadratureList_mod.o transport/Teton/mods/Quadrature_mod.o transport/Teton/mods/Size_mod.o transport/Teton/mods/TimeStepControls_mod.o transport/Teton/mods/ZoneData_mod.o transport/Teton/mods/constant_mod.o transport/Teton/mods/io_mod.o transport/Teton/mods/iter_control_list_mod.o transport/Teton/mods/iter_control_mod.o transport/Teton/mods/kind_mod.o transport/Teton/mods/radconstant_mod.o transport/Teton/misc/assert.o transport/Teton/misc/f90advise.o transport/Teton/misc/f90fatal.o transport/Teton/misc/mpi_param_mod.o transport/Teton/misc/mpif90_mod.o transport/Teton/aux/ConstructBoundary.o transport/Teton/aux/ConstructDtControls.o transport/Teton/aux/ConstructEditor.o transport/Teton/aux/ConstructGeometry.o transport/Teton/aux/ConstructIterControls.o transport/Teton/aux/ConstructMaterial.o transport/Teton/aux/ConstructProfile.o transport/Teton/aux/ConstructQuadrature.o transport/Teton/aux/ConstructSize.o transport/Teton/aux/ResetSize.o transport/Teton/aux/addBoundary.o transport/Teton/aux/addProfile.o transport/Teton/aux/getEdits.o transport/Teton/aux/getGeometry.o transport/Teton/aux/getRunStats.o transport/Teton/aux/setEditorModule.o transport/Teton/aux/setEnergyEdits.o transport/Teton/aux/setGeometry.o transport/Teton/aux/setMaterialModule.o transport/Teton/aux/setSnOrder.o transport/Teton/aux/setTimeStep.o transport/Teton/aux/setZone.o transport/Teton/control/RadMoments.o transport/Teton/control/advanceRT.o transport/Teton/control/dtnew.o transport/Teton/control/newenergy.o transport/Teton/control/profint.o transport/Teton/control/radtr.o transport/Teton/control/rtbatch.o transport/Teton/control/rtbdry.o transport/Teton/control/rtedit.o transport/Teton/control/rtinit.o transport/Teton/control/rtvsrc.o transport/Teton/rt/InitExchange.o transport/Teton/rt/SweepScheduler.o transport/Teton/rt/UpdateMaterialCoupling.o transport/Teton/rt/bdyedt.o transport/Teton/rt/exchange.o transport/Teton/rt/face_coords.o transport/Teton/rt/findReflectedAngles.o transport/Teton/rt/findexit.o transport/Teton/rt/getAbsorptionRate.o transport/Teton/rt/initcomm.o transport/Teton/rt/quadLobatto.o transport/Teton/rt/quadProduct.o transport/Teton/rt/quadxyz.o transport/Teton/rt/rswpmd.o transport/Teton/rt/rtave.o transport/Teton/rt/rtcompton.o transport/Teton/rt/rtconi.o transport/Teton/rt/rtconv.o transport/Teton/rt/rtgeom3.o transport/Teton/rt/rtmainsn.o transport/Teton/rt/rtorder.o transport/Teton/rt/rtplnk.o transport/Teton/rt/rtquad.o transport/Teton/rt/rtstrtsn.o transport/Teton/rt/setIncidentFlux.o transport/Teton/rt/setbdy.o transport/Teton/rt/testFluxConv.o transport/Teton/rt/zone_coords.o transport/Teton/snac/GaussLegendreLobattoWgts.o transport/Teton/snac/Jacobi.o transport/Teton/snac/JacobiLobattoPts.o transport/Teton/snac/cyclebreaker.o transport/Teton/snac/findseeds.o transport/Teton/snac/fixZone.o transport/Teton/snac/getDownStreamData.o transport/Teton/snac/sccsearch.o transport/Teton/snac/snflwxyz.o transport/Teton/snac/snmoments.o transport/Teton/snac/snmref.o transport/Teton/snac/snneed.o transport/Teton/snac/snnext.o transport/Teton/snac/snpnmset.o transport/Teton/snac/snreflect.o transport/Teton/snac/snswp3d.o transport/Teton/snac/snynmset.o utilities/VERIFY.o utilities/DBC.o utilities/Process.o -L/usr/local/tools/ic-13.0.079/lib  -openmp  -L/usr/local/tools/mvapich-intel/lib  -lfmpich -lmpichfarg -lmpich  -libverbs -lpthread -libumad 
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../cmg2Kull/sources -I../CMG_CLEAN/src -I/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  TetonUtils.cc -o TetonUtils.o
mpiicpc -shared -fPIC -o libTetonUtils.so TetonUtils.o -L/usr/local/tools/ic-13.0.079/lib  
mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../cmg2Kull/sources -I../CMG_CLEAN/src -I/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  TetonTest.cc -o TetonTest.o
gmake[1]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton'
