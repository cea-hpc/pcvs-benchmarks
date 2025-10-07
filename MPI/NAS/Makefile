SHELL=/bin/sh
CLASS=U
SUBTYPE=
VERSION=
F08=def
MFLAGS=CLASS=$(CLASS) F08=$(F08)
SFILE=config/suite.def

default: header
	@ sys/print_instructions

BT: bt
bt: header
	cd BT; $(MAKE) $(MFLAGS) SUBTYPE=$(SUBTYPE) VERSION=$(VERSION)

SP: sp
sp: header
	cd SP; $(MAKE) $(MFLAGS)

LU: lu
lu: header
	cd LU; $(MAKE) $(MFLAGS) VERSION=$(VERSION)

MG: mg
mg: header
	cd MG; $(MAKE) $(MFLAGS)

FT: ft
ft: header
	cd FT; $(MAKE) $(MFLAGS)

IS: is
is: header
	cd IS; $(MAKE) $(MFLAGS)

CG: cg
cg: header
	cd CG; $(MAKE) $(MFLAGS)

EP: ep
ep: header
	cd EP; $(MAKE) $(MFLAGS)

DT: dt
dt: header
	cd DT; $(MAKE) $(MFLAGS)

# Awk script courtesy cmg@cray.com, modified by Haoqiang Jin
suite:
	@ awk -f sys/suite.awk SMAKE=$(MAKE) $(SFILE) | $(SHELL)


# It would be nice to make clean in each subdirectory (the targets
# are defined) but on a really clean system this will won't work
# because those makefiles need config/make.def
clean:
	- rm -f core *~ */core */*~
	- rm -f */*.o */*.mod */*.obj */*.exe */npbparams.h */mpinpb.*
	- rm -f MPI_dummy/test MPI_dummy/libmpi.a
	- rm -f sys/setparams sys/makesuite sys/setparams.h
	- rm -f btio.*.out*

veryclean: clean
	- rm -f config/make.def config/suite.def 
	- rm -f bin/sp.* bin/lu.* bin/mg.* bin/ft.* bin/bt.* bin/is.* 
	- rm -f bin/ep.* bin/cg.* bin/dt.*

header:
	@ sys/print_header

