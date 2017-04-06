topsrcdir=..

include ../make.defs

INCPATH += -I. -I../cmg2Kull/sources -I../CMG_CLEAN/src -I$(CWD)/utilities


# Our locally built libs that need to be linked in


EXE_LIBS +=  $(PLATFORM_EXE_EXTRAS)  -lInfrastructure -lTetonUtils -lc2k -lcmgp $(FLIBS) $(LIBS) 

LIBPATH += $(PLATFORM_LIBPATH_EXTRAS)


UTILS_SOURCES = TetonUtils.cc
UTILS_OBJECTS = $(UTILS_SOURCES:.cc=.o)

INFRASTRUCTURE_OBJECTS=communication/DomainNeighborMapInst.o communication/CommAgent.o \
	geom/Region/RegionInst.o geom/Field/FieldInst.o geom/CMI/MeshBase.o geom/CMI/ZoneBase.o \
	geom/CMI/CornerBase.o geom/CMI/FaceBase.o geom/CMI/SideBase.o part/OpacityBase.o part/PartInst.o \
	transport/TetonInterface/TetonNT.o transport/TetonInterface/TetonInst.o \
	transport/Teton/mods/BoundaryList_mod.o \
	transport/Teton/mods/Boundary_mod.o \
	transport/Teton/mods/Communicator_mod.o \
	transport/Teton/mods/Editor_mod.o \
	transport/Teton/mods/Geometry_mod.o \
	transport/Teton/mods/Material_mod.o \
	transport/Teton/mods/ProfileList_mod.o \
	transport/Teton/mods/Profile_mod.o \
	transport/Teton/mods/QuadratureData_mod.o \
	transport/Teton/mods/QuadratureList_mod.o \
	transport/Teton/mods/Quadrature_mod.o \
	transport/Teton/mods/Size_mod.o \
	transport/Teton/mods/TimeStepControls_mod.o \
	transport/Teton/mods/ZoneData_mod.o \
	transport/Teton/mods/constant_mod.o \
	transport/Teton/mods/io_mod.o \
	transport/Teton/mods/iter_control_list_mod.o \
	transport/Teton/mods/iter_control_mod.o \
	transport/Teton/mods/kind_mod.o \
	transport/Teton/mods/radconstant_mod.o \
	transport/Teton/misc/assert.o \
	transport/Teton/misc/f90advise.o \
	transport/Teton/misc/f90fatal.o \
	transport/Teton/misc/mpi_param_mod.o \
	transport/Teton/misc/mpif90_mod.o \
	transport/Teton/aux/ConstructBoundary.o \
	transport/Teton/aux/ConstructDtControls.o \
	transport/Teton/aux/ConstructEditor.o \
	transport/Teton/aux/ConstructGeometry.o \
	transport/Teton/aux/ConstructIterControls.o \
	transport/Teton/aux/ConstructMaterial.o \
	transport/Teton/aux/ConstructProfile.o \
	transport/Teton/aux/ConstructQuadrature.o \
	transport/Teton/aux/ConstructSize.o \
	transport/Teton/aux/ResetSize.o \
	transport/Teton/aux/addBoundary.o \
	transport/Teton/aux/addProfile.o \
	transport/Teton/aux/getEdits.o \
	transport/Teton/aux/getGeometry.o \
	transport/Teton/aux/getRunStats.o \
	transport/Teton/aux/setEditorModule.o \
	transport/Teton/aux/setEnergyEdits.o	\
	transport/Teton/aux/setGeometry.o \
	transport/Teton/aux/setMaterialModule.o \
	transport/Teton/aux/setSnOrder.o \
	transport/Teton/aux/setTimeStep.o \
	transport/Teton/aux/setZone.o \
	transport/Teton/control/RadMoments.o \
	transport/Teton/control/advanceRT.o \
	transport/Teton/control/dtnew.o \
	transport/Teton/control/newenergy.o \
	transport/Teton/control/profint.o \
	transport/Teton/control/radtr.o \
	transport/Teton/control/rtbatch.o \
	transport/Teton/control/rtbdry.o \
	transport/Teton/control/rtedit.o \
	transport/Teton/control/rtinit.o \
	transport/Teton/control/rtvsrc.o \
	transport/Teton/rt/InitExchange.o \
	transport/Teton/rt/SweepScheduler.o \
	transport/Teton/rt/UpdateMaterialCoupling.o \
	transport/Teton/rt/bdyedt.o \
	transport/Teton/rt/exchange.o \
	transport/Teton/rt/face_coords.o \
	transport/Teton/rt/findReflectedAngles.o \
	transport/Teton/rt/findexit.o \
	transport/Teton/rt/getAbsorptionRate.o \
	transport/Teton/rt/initcomm.o \
	transport/Teton/rt/quadLobatto.o \
	transport/Teton/rt/quadProduct.o \
	transport/Teton/rt/quadxyz.o \
	transport/Teton/rt/rswpmd.o \
	transport/Teton/rt/rtave.o \
	transport/Teton/rt/rtcompton.o \
	transport/Teton/rt/rtconi.o \
	transport/Teton/rt/rtconv.o \
	transport/Teton/rt/rtgeom3.o \
	transport/Teton/rt/rtmainsn.o \
	transport/Teton/rt/rtorder.o \
	transport/Teton/rt/rtplnk.o \
	transport/Teton/rt/rtquad.o \
	transport/Teton/rt/rtstrtsn.o \
	transport/Teton/rt/setIncidentFlux.o \
	transport/Teton/rt/setbdy.o \
	transport/Teton/rt/testFluxConv.o \
	transport/Teton/rt/zone_coords.o \
	transport/Teton/snac/GaussLegendreLobattoWgts.o \
	transport/Teton/snac/Jacobi.o \
	transport/Teton/snac/JacobiLobattoPts.o \
	transport/Teton/snac/cyclebreaker.o \
	transport/Teton/snac/findseeds.o \
	transport/Teton/snac/fixZone.o \
	transport/Teton/snac/getDownStreamData.o \
	transport/Teton/snac/sccsearch.o \
	transport/Teton/snac/snflwxyz.o \
	transport/Teton/snac/snmoments.o \
	transport/Teton/snac/snmref.o \
	transport/Teton/snac/snneed.o \
	transport/Teton/snac/snnext.o \
	transport/Teton/snac/snpnmset.o \
	transport/Teton/snac/snreflect.o \
	transport/Teton/snac/snswp3d.o \
	transport/Teton/snac/snynmset.o\
	utilities/VERIFY.o\
	utilities/DBC.o\
	utilities/Process.o



all: subdirs libInfrastructure.$(LIB_EXT) libTetonUtils.$(LIB_EXT) TetonTest.o



libInfrastructure.$(LIB_EXT):$(INFRASTRUCTURE_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBPATH) $(PLATFORM_Infrastructure_EXTRAS)

libTetonUtils.$(LIB_EXT):$(UTILS_OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBPATH) $(PLATFORM_TetonUtils_EXTRAS)


# SuOlsonTest target only for internal testing purposes
SuOlsonTest:SuOlsonTest.o transport part communication libInfrastructure.$(LIB_EXT) libTetonUtils.$(LIB_EXT) 
	$(LINK) $(LINKFLAGS) $< -o $@ $(LIBPATH) $(EXE_LIBS)

SUBDIRS = geom communication part transport utilities

deps: TetonTest.d SuOlsonTest.d


.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

communication: geom
part: geom
transport: geom

-include TetonTest.d
-include SuOlsonTest.d

clean:
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) clean); done
	rm -f *.o  *.pdb *.inst.* TetonTest SuOlsonTest libInfrastructure.* libTetonUtils.*

veryclean: clean
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) veryclean); done
	rm -f *.d
