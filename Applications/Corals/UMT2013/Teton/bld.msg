mpiicpc -c   -I/usr/local/tools/mvapich-intel/include -I. -I../cmg2Kull/sources -I../CMG_CLEAN/src -I/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   -DLINUX -DLinux -DUSE_MPI -DMPICH_SKIP_MPICXX  SuOlsonTest.cc -o SuOlsonTest.o
make -C geom
make[1]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom'
make -C CMI
make[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/CMI'
make[2]: Nothing to be done for `all'.
make[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/CMI'
make -C Field
make[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Field'
make[2]: Nothing to be done for `all'.
make[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Field'
make -C Region
make[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Region'
make[2]: Nothing to be done for `all'.
make[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom/Region'
make[1]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/geom'
make -C transport
make[1]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport'
make -C Teton
make[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/Teton'
make[2]: Nothing to be done for `all'.
make[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/Teton'
make -C TetonInterface
make[2]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/TetonInterface'
make[2]: Nothing to be done for `all'.
make[2]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport/TetonInterface'
make[1]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/transport'
make -C part
make[1]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/part'
make[1]: Nothing to be done for `all'.
make[1]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/part'
make -C communication
make[1]: Entering directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/communication'
make[1]: Nothing to be done for `all'.
make[1]: Leaving directory `/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/communication'
mpiicpc   -I/usr/local/tools/mvapich-intel/include -I. -I../cmg2Kull/sources -I../CMG_CLEAN/src -I/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/utilities -g -O3 -w  -no-prec-div -fPIC  -openmp   SuOlsonTest.o -o SuOlsonTest -L/usr/local/tools/ic-13.0.079/lib  -openmp  -L../CMG_CLEAN/src -Wl,-rpath,/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/../CMG_CLEAN/src -L../cmg2Kull/sources -Wl,-rpath,/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/../cmg2Kull/sources -L../Teton -Wl,-rpath,/nfs/tmp2/ghosh4/ADC/final/UMT2_SW_Review/Teton/../Teton -L/usr/local/tools/mvapich-intel/lib  -lfmpich -lmpichfarg -lmpich  -libverbs -lpthread -libumad  -ldl   -lInfrastructure -lTetonUtils -lc2k -lcmgp  -lifcore -lm -lc -lsvml  -lfmpich -lmpichfarg -lmpich  -libverbs -lpthread -libumad   
