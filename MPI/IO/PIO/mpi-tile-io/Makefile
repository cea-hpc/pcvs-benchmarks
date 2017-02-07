CC=mpc_cc
CFLAGS=-Wall -g -DHAVE_GETOPT_LONG

all: mpi-tile-io

mpi-tile-io: mpi-tile-io.c
	$(CC) $(CFLAGS) mpi-tile-io.c -o $@

clean::
	rm -f mpi-tile-io *.o *~
