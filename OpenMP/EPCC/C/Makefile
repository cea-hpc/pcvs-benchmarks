include Makefile.defs

.c.o:
	${CC} ${CFLAGS} -c $*.c

SYNCOBJS =  syncbench.o common.o
SCHEDOBJS = schedbench.o common.o
ARRAYOBJS = arraybench_$(IDA).o common.o
TASKOBJS =  taskbench.o common.o

all:      syncbench schedbench taskbench
	$(MAKE) IDA=1 prog
	$(MAKE) IDA=3 prog
	$(MAKE) IDA=9 prog
	$(MAKE) IDA=27 prog
	$(MAKE) IDA=81 prog
	$(MAKE) IDA=243 prog
	$(MAKE) IDA=729 prog
	$(MAKE) IDA=2187 prog
	$(MAKE) IDA=6561 prog
	$(MAKE) IDA=19683 prog
	$(MAKE) IDA=59049 prog

prog: arraybench_$(IDA)

# Multiple header files due to multiple array sizes, makes header file arraybench_*.h
arraybench_$(IDA).h: arraybench.h
	$(CC) -DIDA=$(IDA) $(CPPFLAGS) arraybench.h -o $@

# Multiple object files due to multiple array sizes, makes object file arraybench_*.o
arraybench_$(IDA).o: arraybench_$(IDA).h arraybench.c
	$(CC) $(CFLAGS) -DIDA=$(IDA) arraybench.c -c -o $@

# Multiple executables due to multiple array sizes, makes exe file arraybench_*
arraybench_$(IDA): $(ARRAYOBJS) arraybench.c
	$(CC) $(LDFLAGS) $(ARRAYOBJS) $(LIBS) -o $@

syncbench: $(SYNCOBJS)
	$(CC) -o $@ $(LDFLAGS) $(SYNCOBJS) $(LIBS) 

schedbench: $(SCHEDOBJS)
	$(CC) -o $@ $(LDFLAGS) $(SCHEDOBJS) $(LIBS) 

taskbench: $(TASKOBJS)
	$(CC) -o $@ $(LDFLAGS) $(TASKOBJS) $(LIBS) 

clean:
	-rm  syncbench schedbench taskbench arraybench_* syncbench.o schedbench.o taskbench.o common.o 


