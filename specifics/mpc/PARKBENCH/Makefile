include Makevars

EXECS = iot_lowlevel iot_kernel iot_all

default: all

all: 
	(cd src; make all)

$(EXECS):
	(cd src; make $@)

install:
	-(cd src; cp $(EXECS) ../bin)

clean:	
	-(cd src; make clean)

# doesn't clean out data files or qsub files
veryclean:	
	-make clean
	-(cd src; make veryclean)
	-(cd bin; rm -f $(EXECS))
	-(cd data; rm -f core core.*)

tarfile:
	make veryclean
	(cd ..; tar -cvf - ./iotest | compress > iotest.tar.Z)









