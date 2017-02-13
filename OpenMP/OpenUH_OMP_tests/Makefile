CC=${TS_CC}
CFLAGS=${TS_CFLAGS}
CFLAGS+=-fopenmp -O3 -g
LDFLAGS=${TS_LD_FLAGS}
LDFLAGS+=-lm
MAKE=make
SOURCES=$(wildcard src/*.c)
OBJS=$(SOURCES:src/%.c=${PCVS_CWORK_DIR}/OpenMP/OpenUH_OMP_tests/executables/%.o)

all: ${PCVS_CWORK_DIR}/OpenMP/OpenUH_OMP_tests/executables/execTestSuite

subdirs:
	$(MAKE) -C src

${PCVS_CWORK_DIR}/OpenMP/OpenUH_OMP_tests/executables/execTestSuite: subdirs main.c
	$(CC) -o $@ main.c ${OBJS} ${PRIVATIZATION} $(CFLAGS) $(LDFLAGS)

clean:
	$(RM) execTestSuite
	$(MAKE) -C src clean

