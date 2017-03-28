FC = $(PCVS_FC)
FFLAGS = $(PCVS_CFLAGS)
LDFLAGS =  -lm $(PCVS_CFLAGS)
CPP = cpp
OMPFLAG = -DOMPVER2
CLOCKFLAG = -DOMPCLOCK
LIBS = 

.SUFFIXES:
.SUFFIXES: .f90 .o .mod 
.f90.o:
	${FC} ${FFLAGS}  -c $< 
.f90.mod:
	${FC} ${FFLAGS}  -c $< 

SYNCOBJS = setprecision.o benchdata.o benchstats.o syncbench.o delay.o getclock.o 
SCHEDOBJS = setprecision.o benchdata.o benchstats.o schedbench.o delay.o getclock.o 
ARRAYOBJS = setprecision.o arraydata_$(IDA).o arraybench_$(IDA).o benchdata.o benchstats.o arraydelay.o getclock.o

all:
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

prog:  syncbench schedbench arraybench_$(IDA)

benchdata.o benchdata.mod: benchdata.f90 setprecision.mod
	$(FC) $(FFLAGS) -c benchdata.f90 

syncbench.o: benchstats.mod benchdata.mod 
	$(CPP) $(OMPFLAG) -P  syncbench.f90 tempsync.f90
	$(FC) tempsync.f90 $(FFLAGS) -c -o $@ 

syncbench: $(SYNCOBJS)
	$(FC) $(LDFLAGS) $(SYNCOBJS) $(LIBS) -o $@ 

schedbench: $(SCHEDOBJS) benchdata.mod benchstats.mod
	$(FC) -o schedbench $(LDFLAGS) $(SCHEDOBJS) $(LIBS)  

getclock.o: getclock.f90
	$(CPP) $(CLOCKFLAG) -P getclock.f90 tempclock.f90
	$(FC) tempclock.f90 $(FFLAGS) -c -o $@ 

# Makes separate arraydata_$IDA.o for each array size
arraydata_$(IDA).o arraydata_$(IDA).mod: arraydata.f90 setprecision.mod
	$(CPP) -DIDA=$(IDA) -P arraydata.f90 tempdata.f90
	$(FC) tempdata.f90 $(FFLAGS) -c -o $@ 

# Makes separate arraybench_$IDA.o for each array size
arraybench_$(IDA).o: arraydata_$(IDA).mod benchstats.mod benchdata.mod 
	$(CPP) -DIDA=$(IDA) -P $(OMPFLAG) arraybench.f90 temparray.f90
	$(FC) temparray.f90 $(FFLAGS) -c -o $@ 

# Make benchstats
benchstats.o benchstats.mod: benchstats.f90 benchdata.mod
	$(FC) $(FFLAGS) -c benchstats.f90 

# Make arraybench_$IDA exe for each array size
arraybench_$(IDA): $(ARRAYOBJS)
	$(FC) $(LDFLAGS) -o $@ $(ARRAYOBJS) $(LIBS) 

clean: 
	rm *.o *.mod schedbench syncbench arraybench_* tempsync.f90 tempdata.f90 temparray.f90 tempclock.f90


