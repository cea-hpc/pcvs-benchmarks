topsrcdir=../../..

include ../../../make.defs


INCPATH += -I./include

SOURCES=mods/BoundaryList_mod.F90 \
        mods/Boundary_mod.F90 \
        mods/Communicator_mod.F90 \
        mods/Editor_mod.F90 \
        mods/Geometry_mod.F90 \
        mods/Material_mod.F90 \
        mods/ProfileList_mod.F90 \
        mods/Profile_mod.F90 \
        mods/QuadratureData_mod.F90 \
        mods/QuadratureList_mod.F90 \
        mods/Quadrature_mod.F90 \
        mods/Size_mod.F90 \
        mods/TimeStepControls_mod.F90 \
        mods/ZoneData_mod.F90 \
        mods/constant_mod.F90 \
        mods/io_mod.F90 \
        mods/iter_control_list_mod.F90 \
        mods/iter_control_mod.F90 \
        mods/kind_mod.F90 \
        mods/radconstant_mod.F90 \
        misc/assert.F90 \
        misc/f90advise.F90 \
        misc/f90fatal.F90 \
        misc/mpi_param_mod.F90 \
        misc/mpif90_mod.F90 \
        aux/ConstructBoundary.F90 \
        aux/ConstructDtControls.F90 \
        aux/ConstructEditor.F90 \
        aux/ConstructGeometry.F90 \
        aux/ConstructIterControls.F90 \
        aux/ConstructMaterial.F90 \
        aux/ConstructProfile.F90 \
        aux/ConstructQuadrature.F90 \
        aux/ConstructSize.F90 \
        aux/ResetSize.F90  \
        aux/addBoundary.F90  \
        aux/addProfile.F90 \
        aux/getEdits.F90 \
        aux/getGeometry.F90 \
        aux/getRunStats.F90 \
        aux/setEditorModule.F90 \
        aux/setEnergyEdits.F90 \
        aux/setGeometry.F90 \
        aux/setMaterialModule.F90 \
        aux/setSnOrder.F90 \
        aux/setTimeStep.F90 \
        aux/setZone.F90 \
        control/RadMoments.F90 \
        control/advanceRT.F90 \
        control/dtnew.F90 \
        control/newenergy.F90 \
        control/profint.F90 \
        control/radtr.F90 \
        control/rtbatch.F90 \
        control/rtbdry.F90 \
        control/rtedit.F90 \
        control/rtinit.F90 \
        control/rtvsrc.F90 \
        rt/InitExchange.F90\
        rt/SweepScheduler.F90 \
        rt/UpdateMaterialCoupling.F90 \
        rt/bdyedt.F90 \
        rt/exchange.F90 \
        rt/face_coords.F90 \
        rt/findReflectedAngles.F90 \
        rt/findexit.F90 \
        rt/getAbsorptionRate.F90 \
        rt/initcomm.F90 \
        rt/quadLobatto.F90 \
        rt/quadProduct.F90 \
        rt/quadxyz.F90 \
        rt/rswpmd.F90 \
        rt/rtave.F90 \
        rt/rtcompton.F90 \
        rt/rtconi.F90 \
        rt/rtconv.F90 \
        rt/rtgeom3.F90 \
        rt/rtmainsn.F90 \
        rt/rtorder.F90 \
        rt/rtplnk.F90 \
        rt/rtquad.F90 \
        rt/rtstrtsn.F90 \
        rt/setIncidentFlux.F90 \
        rt/setbdy.F90 \
        rt/testFluxConv.F90 \
        rt/zone_coords.F90 \
        snac/GaussLegendreLobattoWgts.F90 \
        snac/Jacobi.F90 \
        snac/JacobiLobattoPts.F90 \
        snac/cyclebreaker.F90 \
        snac/findseeds.F90 \
        snac/fixZone.F90 \
        snac/getDownStreamData.F90 \
        snac/sccsearch.F90 \
        snac/snflwxyz.F90 \
        snac/snmoments.F90 \
        snac/snmref.F90 \
        snac/snneed.F90 \
        snac/snnext.F90 \
        snac/snpnmset.F90 \
        snac/snreflect.F90 \
        snac/snswp3d.F90 \
        snac/snynmset.F90

OBJECTS = ${patsubst %.F90,%.o,$(SOURCES)}

all: $(OBJECTS)

# don't delete intermediate .f90 files
.PRECIOUS: %.f90

include deps

ifeq ($(USE_TAU),on)
snac/snflwxyz.o:snac/snflwxyz.F90
	$(F90) -WF,-DPROFILING_ON $(F90FLAGS) -c $< -o $@
endif

clean:
	rm -f *.mod aux/*.o misc/*.o mods/*.o control/*.o rt/*.o snac/*.o *.mod 
	rm -f aux/*.pdb misc/*.pdb mods/*.pdb control/*.pdb rt/*.pdb snac/*.pdb 
	rm -f aux/*-inst.* misc/*-inst.* mods/*-inst.* control/*-inst.* rt/*-inst.* snac/*-inst.* 

veryclean: clean
	rm -f aux/*.f90 misc/*.f90 mods/*.f90 control/*.f90 rt/*.f90 snac/*.f90
