#
# Makefile for Linux machines 
#

OS := $(shell uname -i | tr "[A-Z]" "[a-z]")

#Cadillac
#MPI_PATH	= 	/usr/local/project/mpich/mpich-1.2.4
#MPI_PATH	= 	/usr/local/project/mpich/mpich-panfs
#MPI_PATH	= 	/home2/swh/mpich-1.2.6
#MPI_PATH	= 	/home2/johnbent/mpi/mpich-1.2.6

# Pink
MPI_PATH	= 	${MPI_ROOT}

CC		=       mpc_cc -D_LARGEFILE64_SOURCE 
CLIBS           =       -L${MPI_PATH}/lib     -lmpi -lromio
CLIBS = ${MPI_INC}
#ifeq "$(findstring x86_64,$(OS))" "x86_64"
#CLIBS           =       -L${MPI_PATH}/lib64   -lmpi
#endif
#CLIBS           =       -L${MPI_PATH}/lib -lmpich -lgm
CFLAGS 		= 	-g     -I. -I${MPI_PATH}/include

AR		=	ar
ARFLAGS		=	-r -c
OBJS		=	rompio_timing.o

all:	rompio 

rompio:	$(OBJS) rompio.o
	$(CC) $(CFLAGS) \
		-o $@.${OS}.x rompio.o $(OBJS) $(CLIBS)


rompio.o: rompio.h

rompio_timing.o: rompio.h

clean:
	- /bin/rm -f *~
	- /bin/rm -f *.o
	- /bin/rm -f *.${OS}.x

