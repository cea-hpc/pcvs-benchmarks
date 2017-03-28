HACC_CPP_OBJS = RestartIO_GLEAN.o testHACC_IO.o 
HAPP_CPP_HDRS = RestartIO_GLEAN.h

# 	---------------------------
# 	Compiler Flags
# 	---------------------------
MPICC =	mpicc
MPICXX = mpicxx

#Compiler Flags
MPI_CFLAGS = -g -O3 -DGLEAN_PRINT_PERROR -I./
MPI_LDFLAGS = -L. -lstdc++ -lpthread
MPI_CFLAGS += -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -Wall

CXXFLAGS = $(MPI_CFLAGS)
LDFLAGS = $(MPI_LDFLAGS)

all: HACC_IO HACC_OPEN_CLOSE 

RestartIO_GLEAN.o:RestartIO_GLEAN.cxx
	$(MPICXX) $(MPI_CFLAGS) -c RestartIO_GLEAN.cxx 

testHACC_OPEN_CLOSE.o:testHACC_OPEN_CLOSE.cxx
	$(MPICXX) $(MPI_CFLAGS) -c testHACC_OPEN_CLOSE.cxx 

testHACC_IO.o:testHACC_IO.cxx
	$(MPICXX) $(MPI_CFLAGS) -c testHACC_IO.cxx 

#$(HACC_CPP_OBJS): %.o: %.cxx 
#	$(MPICXX) $(MPI_CFLAGS) $< -c -o $@

HACC_IO_FILES=RestartIO_GLEAN.o testHACC_IO.o
HACC_IO:$(HACC_IO_FILES)
	$(MPICXX) $(MPI_CFLAGS) $(HACC_IO_FILES) -o $@


HACC_OC_FILES=RestartIO_GLEAN.o testHACC_OPEN_CLOSE.o 
HACC_OPEN_CLOSE: $(HACC_OC_FILES) 
	$(MPICXX) $(MPI_CFLAGS) $(HACC_OC_FILES) -o $@

clean:
	rm -f *.a *.o a.out core* HACC_IO HACC_OPEN_CLOSE
