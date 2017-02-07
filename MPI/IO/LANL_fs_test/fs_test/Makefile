LARGE_FILE  = -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE=1 \
			  -D__USE_LARGEFILE64=1
#UTILS_DIR   = ../../utilities/src

# PLEASE never edit this file to change MPI_INC, MPI_CC, MY_MPI_HOST, MPI_LD
# these are customizable to allow building easily on different architectures
# the intended mechanism is to set them appropriately in your login scripts
# (e.g. .bashrc)

# get MPI_CC, MPI_INC, MY_MPI_HOST, and MPI_LD from environment
# MY_MPI_HOST is needed to specify cluster name for the database inserts
# MPI_INC and MPI_LD can be used for various different settings such as plfs:
# MPI_INC = -DHAS_PLFS -I/path/to/plfs/include 
# MPI_LD = -L/path/to/plfs/lib -lplfs -lstdc++
ifdef MPI_CC 
	CC		= ${MPI_CC}	
else
	CC      = mpc_cc -lm
endif

GCC = gcc -lm
CPP = mpicxx -lm

TARGET_FS_TEST = fs_test
TARGET_THD = nton_plfs_thread_db
ifdef MY_MPI_HOST

	TARGET_FS_EXE = $(TARGET_FS_TEST).${MY_MPI_HOST}.x
	TARGET_THD_EXE = $(TARGET_THD).${MY_MPI_HOST}.x
else 
	TARGET_FS_EXE = $(TARGET_FS_TEST).x
	TARGET_THD_EXE = $(TARGET_THD).x
endif

# -Wall doesn't work with pgi....
CFLAGS  = -g -I. ${MPI_INC} ${MPI_HOST_32BIT} -DROMIO ${LARGE_FILE}

SRCS_FS_TEST = fs_test.c utilities.c print.c hashtable.c hashtable_itr.c boolean.c
SRCS_THREADS = nton_plfs_thread_db.c utilities.c hashtable.c hashtable_itr.c boolean.c print.c
OBJS_FS_TEST=$(SRCS_FS_TEST:.c=.o)
OBJS_THREADS=$(SRCS_THREADS:.c=.o)

# do not build nton_plfs_thread_db if plfs not configured
ifneq (,$(findstring HAS_PLFS,$(MPI_INC)))
all:    $(TARGET_FS_TEST) $(TARGET_THD)
else
all:    $(TARGET_FS_TEST)
endif

$(TARGET_FS_TEST):	$(OBJS_FS_TEST) 
	$(CC) $(CFLAGS) $(OBJS_FS_TEST) ${MPI_LD} -o $(TARGET_FS_EXE) -lm

$(TARGET_THD):	$(OBJS_THREADS) 
	$(CC) $(CFLAGS) $(OBJS_THREADS) ${MPI_LD} -o $(TARGET_THD_EXE) -lm

%.o : %.c
	$(CC)  $(CFLAGS) -c $? -o $@ -lm

clean:
	- /bin/rm -f *~ $(OBJS_FS_TEST) $(OBJS_THREADS) $(TARGET_FS_EXE) $(TARGET_THD_EXE)

