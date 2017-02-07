include ../../Makevars

OBJS = iot_defs.o iot_string.o iot_key.o iot_err.o iot_rand.o\
	iot_types.o iot_file.o iotest.o
MODS = iot_defs.mod iot_string.mod iot_key.mod iot_err.mod\
	iot_rand.mod iot_types.mod iot_file.mod iotest.mod

objs: $(OBJS)

clean:
	-/bin/rm -f core $(OBJS) $(MODS)
#	-/bin/rm -f *~ core $(OBJS) $(MODS)

veryclean:
	-make clean

iot_defs.o:

iot_string.o: iot_defs.o

iot_key.o: iot_defs.o iot_string.o

iot_err.o: iot_defs.o iot_string.o

iot_types.o: iot_defs.o iot_err.o

iot_rand.o:

iot_file.o: iot_defs.o iot_string.o iot_key.o

iottest.o: iot_defs.o iot_string.o iot_key.o iot_err.o iot_rand.o iot_types.o iot_file.o

.f.o:
	$(FC) $(FFLAGS) -c $*.f

