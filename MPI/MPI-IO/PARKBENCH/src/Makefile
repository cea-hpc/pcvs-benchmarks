include ../Makevars

SINUNIX_OBJS = lowlevel/sinunix/sinunix.o

SINGLE_OBJS = lowlevel/single/single.o

MULTIPLE_OBJS = lowlevel/multiple/multiple.o

#SINGLEI_OBJS = lowlevel/singleI/singleI.o

#MULTIPLEI_OBJS = lowlevel/multipleI/multipleI.o

MATRIX2D_OBJS = kernel/matrix2D/matrix2D.o

MATRIX3D_OBJS = kernel/matrix3D/matrix3D.o

NONSEQ_OBJS = kernel/nonseq/nonseq.o

SHAREDFP_OBJS = kernel/sharedfp/sharedfp.o

#GATHERSCAT2D_OBJS = kernel/gatherscat2D/gatherscat2D.o

#TRANSPOSE_OBJS = kernel/transpose/transpose.o

# macro for shared module object files
SHARE_OBJS = share/iot_defs.o share/iot_string.o \
	     share/iot_file.o share/iotest.o \
	     share/iot_key.o share/iot_err.o \
	     share/iot_rand.o share/iot_types.o

# macro for lowlevel test object files
LOWLEVEL_OBJS = $(SINGLE_OBJS) $(MULTIPLE_OBJS) \
	        $(SINGLEI_OBJS) $(MULTIPLEI_OBJS) $(SINUNIX_OBJS)

# macro for kernel test object files
KERNEL_OBJS = $(MATRIX2D_OBJS) $(MATRIX3D_OBJS) \
	      $(NONSEQ_OBJS) $(SHAREDFP_OBJS) \
	      $(GATHERSCAT2D_OBJS) $(TRANSPOSE_OBJS)

# macro for wrapper object files
WRAP_OBJS = iot_lowlevel.o iot_kernel.o iot_all.o

# test suite executables
EXECS = iot_lowlevel iot_kernel iot_all

# default target
default: all

# make all test suite executables
all: 
	make $(EXECS)

# rules to compile and link each executable

iot_lowlevel: iot_lowlevel.o $(LOWLEVEL_OBJS) $(SHARE_OBJS)
	$(FLINKER) $(LFLAGS) -o iot_lowlevel iot_lowlevel.o \
	$(LOWLEVEL_OBJS) $(SHARE_OBJS) $(PROFLIBS) $(LIBS)

iot_kernel: iot_kernel.o $(KERNEL_OBJS) $(SHARE_OBJS)
	$(FLINKER) $(LFLAGS) -o iot_kernel iot_kernel.o \
	$(KERNEL_OBJS) $(SHARE_OBJS) $(PROFLIBS) $(LIBS)

iot_all: iot_all.o $(LOWLEVEL_OBJS) $(KERNEL_OBJS) $(SHARE_OBJS)
	$(FLINKER) $(LFLAGS) -o iot_all iot_all.o \
	$(LOWLEVEL_OBJS) $(KERNEL_OBJS) $(SHARE_OBJS) $(PROFLIBS) $(LIBS)

# all object files depend on shared modules
$(WRAP_OBJS) $(LOWLEVEL_OBJS) $(KERNEL_OBJS): $(SHARE_OBJS)

# shared module dependencies
share/iot_defs.o:

share/iot_string.o: share/iot_defs.o

share/iot_key.o: share/iot_defs.o share/iot_string.o

share/iot_err.o: share/iot_defs.o share/iot_string.o

share/iot_types.o: share/iot_defs.o share/iot_err.o

share/iot_rand.o:

share/iot_file.o: share/iot_defs.o share/iot_string.o share/iot_key.o

share/iotest.o: share/iot_defs.o share/iot_string.o share/iot_key.o share/iot_err.o share/iot_rand.o share/iot_file.o share/iot_types.o

# rule for compiling .o files
.f.o:
	$(FC) $(FFLAGS) -o $*.o -c $*.f

# remove all but executables and source files
clean:
	-/bin/rm -f $(WRAP_OBJS) $(LOWLEVEL_OBJS) $(KERNEL_OBJS) 
	-/bin/rm -f $(SHARE_OBJS) *.mod

# remove all but original source files
veryclean:
	-make clean
	-/bin/rm -f $(EXECS)



