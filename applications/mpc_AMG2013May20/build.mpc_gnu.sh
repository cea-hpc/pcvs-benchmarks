#!/bin/sh
echo "Clean up:"
find . -name "*.o" -print | xargs /bin/rm -f
find . -name "*.a" -print | xargs /bin/rm -f 
rm -f src/test/amg2013

echo "Building with GNU 4.8+ Compilers and MPC"
cd src
cp Makefile.include.mpc_gnu Makefile.include
make 
cd test/
./run.sh

