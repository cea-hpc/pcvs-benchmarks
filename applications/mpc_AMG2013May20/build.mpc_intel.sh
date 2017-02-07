#!/bin/sh
echo "Clean up:"
find . -name "*.o" -print | xargs /bin/rm -f
find . -name "*.a" -print | xargs /bin/rm -f 
rm -f src/test/amg2013

echo "Building with Intel v14.+ Compilers and MPC"
cd src
cp Makefile.include.mpc_intel Makefile.include
make 
cd test/
./run.sh

