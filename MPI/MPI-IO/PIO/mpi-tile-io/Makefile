CC=$(PCVS_CC)
CFLAGS=-Wall -g -DHAVE_GETOPT_LONG $(PCVS_CFLAGS)

all: mpi-tile-io

mpi-tile-io: mpi-tile-io.c
	$(CC) $(CFLAGS) mpi-tile-io.c -o $@

clean::
	rm -f mpi-tile-io *.o *~
