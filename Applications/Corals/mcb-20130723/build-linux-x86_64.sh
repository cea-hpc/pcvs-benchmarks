
#!/bin/sh
#

export MCBDIR=src

if [ ! -d ./bash_headers ]; then
  tar xzf boost_headers.tgz
fi

cp Makefile-linux-x86_64 $MCBDIR

cd $MCBDIR

make -f Makefile-linux-x86_64 veryclean

make -f Makefile-linux-x86_64
