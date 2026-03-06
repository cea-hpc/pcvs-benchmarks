//---------------------------------*-C++-*----------------------------------//
// Copyright 1998 The Regents of the University of California.
// All rights reserved.
//
// Created on: May 1998
// Created by:  Pat Miller
// Also maintained by: Michael Nemanic
//--------------------------------------------------------------------------//

#ifndef __TETON_TETON_HH__
#define __TETON_TETON_HH__

#include "utilities/KullTypes/CharStar8.hh"
#include "transport/TetonInterface/TetonFreq.hh"
#include "transport/TetonInterface/TetonBoundary.hh"
#include "transport/TetonInterface/TetonVolumeSource.hh"
#include "transport/TetonInterface/TetonProfile.hh"
#include <vector>

#include "geom/Field/Field.hh"
#include "part/PartList.hh"
#include "geom/Region/PolyhedralRegion.hh"
#include "geom/Region/PolygonalRZRegion.hh"
#include "part/Part.hh"

using namespace Geometry;

template <typename Mesh>
class Teton;


template <typename Mesh>
class Teton {
//--------------------------------------------------------------------------//
//  Teton Sn Radiation Transport Module
//    The class provides the methods, variables, and interfaces to support
//   the SN module for KULL.
//------------------------------------------------------------------------ -//
public:
   // Just 3D arbitraries for now....
   typedef  Mesh       MeshType;
   typedef  typename MeshType::SideHandle   Side;
   typedef  typename MeshType::FaceHandle   Face;
   typedef  typename MeshType::ZoneHandle   Zone;
   typedef  typename MeshType::CornerHandle Corner;
   typedef  typename MeshType::EdgeHandle   Edge;
   typedef  typename MeshType::NodeHandle   Node;
   typedef  typename MeshType::ScalarType   Scalar;

   typedef Field<MeshType, Zone, Scalar> MeshZCSF;
   typedef Field<MeshType, Face, int>    MeshFCiF;
   typedef Field<MeshType, Zone, typename MeshType::VectorType> MeshZCVF;

   typedef Region<Mesh>   RegionType;

   typedef Geometry::Field<RegionType, Zone, Scalar> RZCSF;

   typedef std::map<size_t, std::vector<Face> > CommMapType;

   Teton();
   ~Teton() {
   }

   void resize();

   // routines wrapped to call fortran.  Radtr is interface
   // into Teton.
   double cxxRadtr();

   // used to setup Teton region, group and boundary condition vars.
   void setBCs( std::vector<TetonBoundary<MeshType> > &);
   void setVSs( std::vector<TetonVolumeSource> &);
   void setAProfile( TetonProfile &, int);
   void setGroups(const std::vector<TetonFreq> &);

   // set Teton node positions 
   void setMeshSizeAndPositions(MeshType &,
                                std::vector<int> &);

   // communication
   void setCommunication(MeshType &,
                         std::vector<int> &,
                         std::vector<int> &,
                         std::vector<int> &,
                         std::vector<int> &);

   // set up the mesh connectivity arrays used in Teton.
   void linkKull(MeshType &,
                 const std::vector<TetonFreq> &,
                 std::vector<TetonBoundary<MeshType> > &,
                 std::vector<TetonVolumeSource> &);

   // Initializes material properties used by Teton at the beginning
   // of a run or after a restart.
   void CInitMaterial(PartList<Mesh>&);

   // update opacity, density etc prior to SN step.
   void CupdateSn();

   // update Hydro var as a result of the Teton step
   void CupdateSpecificEnergy(std::vector<double> &,
                              MeshType &,
                              PartList<Mesh> &);

   // adjust Teton node points due to Hydro motion.
   void CrelinkMesh();

   // create IterControl/DtControl object used in Teton for
   // iteration/time-step control etc.  called from shadowteton.py
   void CsetControls();

   // boundary edits
   void getBoundaryEdits();

   // a member function to update opacity
   void UpdateOpacity();

   // ------------------------------------------------------------
   // Computed from other inputs (in resize)
   // ------------------------------------------------------------
   int          ndim;           // number of spatial dimensions (1,2, or 3)
   // ------------------------------------------------------------
   // Need these to get going
   // ------------------------------------------------------------
   int          nbc;            // number of boundary condition profiles
   int          nvs;            // number of volume source profiles
   // ------------------------------------------------------------
   int          my_node;        // Node number (set to 0 for a serial)
   int          n_Procs;        // number of MPI processes (set to 1 for a serial)
   // ------------------------------------------------------------
   int          ncycle;         // radiation cycle number (host increment)
   int          nbelem;         // number of boundary elements 
   int          nzones;         // number of spatial zones
   int          ncornr;         // number of corners
   int          npnts;          // number of points
   int          nfaces;         // number of faces
   int          nbedit;         // number of boundary edits
   int          ngr;            // number of radiation groups
   int          nangsn;         //
   int          noutmx;         // max number of outer (temperature) iterations
   int          ninmx;          // max number of inner (intensity) per outer iter
   int          ngdamx;         // max number of grey-acceleration iterations per inner iter
   // ------------------------------------------------------------
   int          nprof;          // total number of profiles
   int          GTAorder;       // quadrature order used for grey transport acceleration (def=2 for s2 acc)
   int          npsi;           // second dimension of psir (nangsn*ngr if storing angle dependent intensity, else 1)
   int          noutrt;         // Number of temperature (outer) iterations performed this cycle
   int          ninrt;          // Number of radiation intensity (inner) iterations performed this cycle
   int          ngdart;         // Number of grey-acceleration iterations performed this cycle
   int          TrMaxZone;      // Zone with the largest radiation temperature
   int          TeMaxZone;      // Zone with the largest electron temperature
   int          TiMaxZone;      // Zone with the largest ion temperature
   int          TrMaxNode;      // NodeID with the largest radiation temperature
   int          TeMaxNode;      // NodeID with the largest electron temperature
   int          TiMaxNode;      // NodeID with the largest ion temperature
   // ------------------------------------------------------------
   int          ConvControlNode;   // NodeID controlling convergence
   int          ConvControlZone;   // Zone controlling convergence
   int          DtControlNode;     // NodeID controlling the time step
   int          DtControlZoneTr4;  // Zone with max Tr4 change on DtControlNode
   int          DtControlZoneTe;   // Zone with max Te change on DtControlNode
   // ------------------------------------------------------------
   double       ConvControlError;   // Max relative error
   double       ConvControlTr;      // Radiation temperature of controlling zone 
   double       ConvControlTe;      // Electron temperature of controlling zone
   double       ConvControlRho;     // Density of controlling zone
   double       ConvControlCve;     // Specific heat of controlling zone
   double       ConvControlEdep;    // External energy deposition rate
   double       DtControlChangeTr4; // Max change in Tr4 of controlling zone
   double       DtControlChangeTe;  // Max change in Te of controlling zone
   double       DtControlTr;        // Radiation temperature of controlling zone
   double       DtControlTe;        // Electron temperature of controlling zone
   double       DtControlTrOld;     // Previous Radiation temperature of controlling zone
   double       DtControlTeOld;     // Previous Electron temperature of controlling zone
   // ------------------------------------------------------------
   double       EnergyRadiation;    // (E) Total energy in the radiation field
   double       EnergyMaterial;     // (E) Total energy in the material
   double       EnergyIncident;     // (E) Total energy incident on the problem
   double       EnergyEscaped;      // (E) Total energy that escaped the problem
   double       EnergyExtSources;   // (E) External energy source
   double       EnergyCheck;        // (E) Energy error
   double       deltaEsrc;          // (E) Change in energy due to external sources
   double       deltaEhyd;          // (E) Change in energy due to hydrodynamic work
   double       TrMax;              // (T) Maximum zonal radiation temperature
   double       TeMax;              // (T) Maximum zonal electron temperature
   double       TiMax;              // (T) Maximum zonal ion temperature
   double       tfloor;             // (T) Temperature floor
   double       tmin;               // (T) Minimum temperature for time step/convergence control
   double       dtrad;              // (t) Radiation timestep
   double       dtused;             // (t) Radiation timestep used during the last rad step
   double       CommTimeCycle;
   double       CommTimeTotal;
   // ------------------------------------------------------------
   double       epstmp;         // Convergence criterion for the temperature (outer) iteration
   double       epsinr;         // Convergence criterion for the intensity (inner) iteration
   double       epsgda;         // Convergence criterion for the grey-acceleration calculation
   double       delte;          // Maximum fractional change allowed per cycle in the material temperature
   double       deltr;          // Maximum fractional change allowed per cycle in the radiation energy density
   double       timerad;        // (t) Problem time
   double       angleLoopTime;  // (t) time spent in loop over angles in snflwxyz.F90
   double       dtrmn;          // (t) Minimum allowed radiation timestep
   double       dtrmx;          // (t) Maximum allowed radiation timestep
   CharStar8    igeom;          // Geometry flag (1D--slab, sphere, cylinder; 2D--rz; 3D--xyz)
   CharStar8    ittyp;          // Time-dependence flag (timedep, stdyst)
   CharStar8    iaccel;         // Iterative acceleration flag (gda,off)
   CharStar8    iscat;          // Scattering treatment flag (on,off)
   CharStar8    itimsrc;        // Treatment of the time-dependent source (exact, moments, zonal)
   CharStar8    decomp_s;       //
   CharStar8    ConvControlReason;  // What's controlling convergence
   CharStar8    DtControlReason;    // What's controlling the time step
   // ------------------------------------------------------------
   std::vector<int>  quaddef;        // [ngr] Sn order for each group
   // ------------------------------------------------------------
   // Dimensioning below is relevant to Fortran, even though square
   // braces are used!  Leftmost indices vary most rapidly as the vector
   // elements are traversed one after the other.  -M. Lambert, 03-27-09
   // ------------------------------------------------------------
   std::vector<double> psir;         // [ngr,ncornr,nangsn] (E/A/t/ster) Angle-dependent flux
   std::vector<double> Phi;          // [ngr,ncornr] (E/A/t) scalar intensity 
   std::vector<double> RadEnergyDensity; // [nzones,ngr] the zone-averaged energy density by Enrgy group
   std::vector<double> RadiationForce; // [ndim,ncornr] radiation force on the matter
   std::vector<double> RadiationFlux; // [ndim,nzones] radiation flux [E/A/t]
   std::vector<double> tec;          // [ncornr] (T) Corner electron temperature
   std::vector<double> RadEnergyEscRate;  // [nbedit+1,ngr] (E/t) instantanious rate of radiation energy escaping
   std::vector<double> RadEnergyIncRate;  // [nbedit+1,ngr] (E/t) instantanious rate of radiation energy incident
   std::vector<double> RadEnergyEscape;   // [nbedit+1,ngr] (E) Time-integrated radiation energy escaping
   std::vector<double> RadEnergyIncident; // [nbedit+1,ngr] (E)Time-integrated radiation energy incident
   std::vector<double> denez;        // [nzones] (E/m) Zone-average specific electron energy change
   std::vector<double> trz;          // [nzones] (T) Zone-average radation temperature
   std::vector<double> tez;          // [nzones] (T) Zone-average energy temperature
   std::vector<double> px;           // [ndim,npnts] (L) Point coordinates 1d-r or x 2d-r,z 3d-x,y,z
   // ------------------------------------------------------------
   std::vector<double> RE_Escape;
   std::vector<double> RE_Incident;
   std::vector<double> RE_EscapeRate;
   std::vector<double> RE_IncidentRate;
   std::vector<double> RE_EscapeSpectrum;
   std::vector<double> RE_IncidentSpectrum;
   std::vector<double> RE_EscapeRateSpectrum;
   std::vector<double> RE_IncidentRateSpectrum;
   // ------------------------------------------------------------
   std::vector<double> siga;         // [nzones,ngr] (1/L) Absorption cross section
   std::vector<double> sigs;         // [nzones,isctp1,ngr] (1/L) Scattering cross section
   std::vector<double> cve;          // [nzones] (E/m/T) Electron specific heat
   std::vector<double> rho;          // [nzones] (m/V) Material density
   std::vector<double> SMatEff;      // [nzones] (E/m/t) Specific energy rate to the matter 
   std::vector<double> gnu;          // [ngr+1] (e) Photon energy group boundaries
   // ------------------------------------------------------------
   int          ncomm;               // number of communication neighbors
   int          nbshare;             // tot num of shared boundary elements.
   int          maxcf;               // maximum number of corner faces
   // ------------------------------------------------------------
   std::vector<int>  PXlookup;       // maps local node ID to Teton node ID
   std::vector<int>  nfpc;           // number of corner faces per corner [ncornr]
   std::vector<int>  ctoface;        // corner to face -  (maxcf,ncornr)
   std::vector<int>  ctozone;        // corner to zone - (ncornr)
   std::vector<int>  ctopoint;       // corner to point - (ncornr)
   std::vector<int>  bdytoc;         // boundary to corner - (2,nbelem)
   std::vector<int>  zonetosrc;      // zone to source - (nzones)

private:
   // ------------------------------------------------------------

   PartList<Mesh>  *mPartListPtr;

};  

#endif                   // __TETON_TETON_HH__
