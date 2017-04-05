VERSION = 1.0

#-------------- User Defined Makefile Variables ---------------#
BOOST_PREFIX    = ../boost_headers
CXX             = mpixlcxx_r
OPENMPFLAG      = -qsmp=omp
MPI_INCLUDE     = -I/bgsys/drivers/ppcfloor/comm/xl/include 
CXXDEFINES      = -DORTHOXY -DUSE_OPENMP -DUSE_MPI -DNOASSERT # -DUSE_TAU_EVENT_COUNTERS
#CXXFLAGS        = -g5 -O3 $(CXXDEFINES) $(OPENMPFLAG)
CXXFLAGS        = -g5 -O3 -q64 -qhot $(CXXDEFINES) $(OPENMPFLAG)
LDFLAGS         =
LIBPATH         =  -Wl,-zmuldefs
LIBS            =
# ea: 
ifdef HPM
BGPM            = -L/bgsys/drivers/ppcfloor/bgpm/lib -lbgpm
MPITRACE_OMP    = -L/usr/local/tools/mpitrace/lib -lmpihpm_smp $(BGPM)
CXXDEFINES     += -DEA_USE_HPM
LDFLAGS        += $(MPITRACE_OMP)
EXT             = .hpm
endif 
ifdef PROF
BGPM            = -L/bgsys/drivers/ppcfloor/bgpm/lib -lbgpm
HPM_PROF        = -L/usr/local/tools/mpitrace/hpmprof -lhpmprof $(BGPM)
LDFLAGS        += $(HPM_PROF)
EXT             = .prof
endif 
ifdef LOMP
CXX             = /usr/local/tools/compilers/ibm/mpixlcxx_r-lompbeta1
CXXFLAGS       += -qdebug=lompinterface
EXT             = .lomp
endif 
BINEXT          = $(EXT).$(shell uname -m)
#--------------------------------------------------------------#

#BOOST_INCLUDE = -I${BOOST_PREFIX}/include
BOOST_INCLUDE = -I${BOOST_PREFIX}
BOOST_LIB = -L${BOOST_PREFIX}/lib -Wl,-rpath ${BOOST_PREFIX}/lib

include distfiles.txt

########## Setup File Lists ##########
THREAD_TEST_CFILES = \
	./Meshes/Ortho_Cartesian_2D_Mesh.cc \
	./Meshes/zcfInst.cc \
	./Meshes/fcfInst.cc \
	./BoundaryConditions/Domain_BCInst.cc \
	./DomainDecomposition/Domain_organizerInst.cc \
	./DomainDecomposition/Domain_exchangeInst.cc \
	./DomainDecomposition/setDomainInformationInst.cc \
	./DomainDecomposition/sumOverDomainsInst.cc \
	./UtilityFunctions/BoxPartition.cc \
	./UtilityFunctions/LinePartition.cc \
	./UtilityFunctions/Nonblocking_Gather.cc \
	./UtilityFunctions/Nonblocking_Sync.cc \
	./UtilityFunctions/Buffered_MPI_Send.cc \
	./UtilityFunctions/printGlobalInfo.cc \
	./StandAloneMains/threadFieldTest.cc \
	./StandAloneMains/anyOption.cc

THREAD_TEST_OFILES := ${subst .cc,.o,${THREAD_TEST_CFILES}}
THREAD_TEST_DFILES := ${subst .cc,.d,${THREAD_TEST_CFILES}}

CFILES = \
	./ImplicitMonteCarlo/IMCInst.cc \
	./ImplicitMonteCarlo/IMC_4MomentumInst.cc \
	./ImplicitMonteCarlo/photonInst.cc \
	./AbsorptionOpacity/constant_absorp_opac.cc \
	./AbsorptionOpacity/Opacity_data_baseInst.cc \
	./AbsorptionOpacity/Analytic_opacity_data_baseInst.cc \
	./ScatteringOpacity/isotropic_scattering.cc \
	./Sources/Source_data_baseInst.cc \
	./Sources/Domain_photon_sourceInst.cc \
	./Sources/isotropic_sourceInst.cc \
	./Material/Material_data_baseInst.cc \
	./Material/piece.cc \
	./BoundaryConditions/transmitting_BCInst.cc \
	./BoundaryConditions/Domain_BCInst.cc \
	./BoundaryConditions/reflecting_BCInst.cc \
	./BoundaryConditions/BC_listInst.cc \
	./Meshes/Ortho_Cartesian_2D_Mesh.cc \
	./Meshes/zcfInst.cc \
	./Meshes/fcfInst.cc \
	./DomainDecomposition/IMC_Domain_organizerInst.cc \
	./DomainDecomposition/Domain_organizerInst.cc \
	./DomainDecomposition/Domain_exchangeInst.cc \
	./DomainDecomposition/setDomainInformationInst.cc \
	./DomainDecomposition/sumOverDomainsInst.cc \
	./UtilityFunctions/rotate_anglesInst.cc \
	./UtilityFunctions/BoxPartition.cc \
	./UtilityFunctions/LinePartition.cc \
	./UtilityFunctions/Nonblocking_Gather.cc \
	./UtilityFunctions/Nonblocking_Sync.cc \
	./UtilityFunctions/Buffered_MPI_Send.cc \
	./UtilityFunctions/mfpsToCollision.cc \
	./UtilityFunctions/printGlobalInfo.cc \
	./UtilityFunctions/MCAnswer.cc \
	./RandomNumberGenerators/rng.cc \
	./StandAloneMains/MCBenchmark.cc \
	./StandAloneMains/anyOption.cc

OFILES := ${subst .cc,.o,${CFILES}}
DFILES := ${subst .cc,.d,${CFILES}}

#---------------------- Compilation Flags ----------------------#

INCLUDES = -I. -I.. -I./Meshes -I./ImplicitMonteCarlo \
	 -I./AbsorptionOpacity -I./ScatteringOpacity \
	 -I./Sources -I./Material \
	 -I./BoundaryConditions -I./Meshes \
	 -I./UtilityFunctions -I./RandomNumberGenerators -I./RandomWalker \
	 -I./StandAloneMains -I./TypeInformation -I./DomainDecomposition \
	 $(MPI_INCLUDE) $(BOOST_INCLUDE)

##########  creating executable  ##########

MCBenchmark.exe: $(OFILES)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $@ $(OFILES) $(LDFLAGS) $(LIBPATH) $(LIBS)
	cp $@ ../bin/MCBenchmark-instr$(BINEXT)

threadFieldTest: $(THREAD_TEST_OFILES)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $@ $(THREAD_TEST_OFILES) $(LDFLAGS) $(LIBPATH) $(LIBS)

%.o : %.cc
	${CXX} $(CXXFLAGS) $(INCLUDES) -c $< -o $@

%.d: %.cc
	@set -e; rm -f $@; \
	g++ -MM -MT $*.o -MT $@ $(CXXDEFINES) $(INCLUDES) $< > $@

clean:
	rm -rf *~ core *.o $(OFILES) MCBenchmark threadFieldTest StandAloneMains/threadFieldTest.o

veryclean: clean
	rm -f $(DFILES)

DISTDIR = mcb-$(VERSION)
dist: $(DISTFILES)
	rm -rf $(DISTDIR) 
	rm -f $(DISTDIR).tar.gz
	mkdir $(DISTDIR)
	list='$(DISTFILES)'; for f in $$list; do \
		cp --parents $$f $(DISTDIR); \
	done
	-find $(DISTDIR) -type d -exec chmod 777 {} \;
	-find $(DISTDIR) -type f -exec chmod a+wr {} \;
	tar cf $(DISTDIR).tar $(DISTDIR)
	gzip $(DISTDIR).tar
	rm -rf $(DISTDIR)

-include $(DFILES)


