CC=mpicc
#CFLAGS=-Wall -g -Wstrict-prototypes -Wmissing-prototypes -Wundef -Wpointer-arith -Wbad-function-cast
CFLAGS=-O

all: bw bw_th latency latency_th shortlong shortlong_th overlap no_overlap \
     conc_allred conc_allred_th collcomp message_rate message_rate_th

bw.o: bw.c
bw_th.o: bw_th.c
latency.o: latency.c
latency_th.o: latency_th.c
shortlong.o: shortlong.c
shortlong_th.o: shortlong_th.c
overlap.o: overlap.c
no_overlap.o: no_overlap.c
conc_allred.o: conc_allred.c
conc_allred_th.o: conc_allred_th.c
collcomp.o: collcomp.c
collcomp_noth.o: collcomp_noth.c
message_rate.o: message_rate.c
message_rate_th.o: message_rate_th.c

bw: bw.o 
	$(CC) -o $@ $^

bw_th: bw_th.o 
	$(CC) -o $@ $^

latency: latency.o 
	$(CC) -o $@ $^

latency_th: latency_th.o 
	$(CC) -o $@ $^

shortlong: shortlong.o 
	$(CC) -o $@ $^

shortlong_th: shortlong_th.o 
	$(CC) -o $@ $^

no_overlap: no_overlap.o 
	$(CC) -o $@ $^

overlap: overlap.o 
	$(CC) -o $@ $^

conc_allred: conc_allred.o 
	$(CC) -o $@ $^

conc_allred_th: conc_allred_th.o 
	$(CC) -o $@ $^

collcomp: collcomp.o 
	$(CC) -o $@ $^

collcomp_noth: collcomp_noth.o 
	$(CC) -o $@ $^

message_rate: message_rate.o 
	$(CC) -o $@ $^

message_rate_th: message_rate_th.o 
	$(CC) -o $@ $^

clean:
	rm -f *.o bw bw_th latency latency_th shortlong shortlong_th overlap no_overlap conc_allred conc_allred_th collcomp collcomp_noth message_rate message_rate_th
