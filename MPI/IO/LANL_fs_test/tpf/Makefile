#
# Makefile 
#

CC		=	g++ 
CLIBS	=	

# CFLAGS may need to be changed due to location of panfs/include
CFLAGS		=	-g -I. -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64
OBJS		=	


#all:	tpf_q tpf_panfs	# tpf_q is for q machines
all:	tpf_panfs

tpf_q:	$(OBJS)
	$(CC) $(CFLAGS) \
	-o $@.${MY_MPI_HOST}.x tpf_q.c $(OBJS) $(MPI_LD_FLAGS) $(CLIBS)

tpf_panfs:	$(OBJS)
		$(CC) $(CFLAGS) \
		-o $@.${MY_MPI_HOST}.x tpf_panfs.c $(OBJS) $(CLIBS)

clean:
	- /bin/rm -f *~
	- /bin/rm -f *.o
	- /bin/rm -f *.x


