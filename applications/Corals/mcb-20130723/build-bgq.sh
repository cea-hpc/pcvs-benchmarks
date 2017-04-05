
#!/bin/sh
#

export MCBDIR=src

if [ ! -d ./boost_headers ]; then
  tar xzf boost_headers.tgz
fi

cp Makefile-bgq $MCBDIR

cd $MCBDIR

# Use veryclean when switching architectures. Only need clean
# when building on the same architecture as the last build.
make -f Makefile-bgq veryclean

make -f Makefile-bgq
