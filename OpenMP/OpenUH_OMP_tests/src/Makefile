CC=${TS_CC}
RM=rm -f
SOURCES=$(wildcard *.c)
OBJS=$(SOURCES:%.c=${PCVS_CWORK_DIR}/OpenMP/OpenUH_OMP_tests/executables/%.o)
CFLAGS=${TS_CFLAGS}
CFLAGS+=-fopenmp
LD_GLAGS=${TS_LDFLAGS}
LDFLAGS+=-lm

all: $(OBJS)

${PCVS_CWORK_DIR}/OpenMP/OpenUH_OMP_tests/executables/%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS) ${PRIVATIZATION}

clean:
	$(RM) $(OBJS)
