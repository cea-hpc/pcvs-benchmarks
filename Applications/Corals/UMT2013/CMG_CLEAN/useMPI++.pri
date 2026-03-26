#MPI Additions
MPI = /usr/lib/mpi
MPI_LIBS = -L$${MPI}/lib -lmpi 
aix{
MPCC = /usr/local/mpi/bin/mpiCC
}
else{
MPCC = /usr/local/tools/compilers/gnu/mpig++-3.2.3
}
MPI_RPATH = $${MPI}/mpi_gnu/lib

DEFINES += mpi $${MPI_DEFS}

QMAKE_CC  = $${MPCC}
QMAKE_CXX = $${MPCC}

#Append p between the filename and .o
QMAKE_EXT_OBJ = $${QMAKE_EXT_OBJ}p.o

#Link the executable with the MPI compiler
QMAKE_LINK = $${MPCC}
QMAKE_LINK_SHLIB = $${MPCC}


