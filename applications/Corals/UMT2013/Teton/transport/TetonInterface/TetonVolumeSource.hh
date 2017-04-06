//---------------------------------*-C++-*----------------------------------//
// Copyright 1998 The Regents of the University of California.
// All rights reserved.
//--------------------------------------------------------------------------//

#ifndef __TETON_TETONVOLUMESOURCE_HH__
#define __TETON_TETONVOLUMESOURCE_HH__

#include <vector>
#include "utilities/KullTypes/CharStar8.hh"
#include "transport/TetonInterface/TetonProfile.hh"

class TetonVolumeSource {
//------------------------------------------------------------------
//  Teton Radiation Transport Volume Source Module
//------------------------------------------------------------------
public:
   // --------------------------------------------------------------
   // Default constructor needed for std::vector<TetonVolumeSource>
   // --------------------------------------------------------------
   TetonVolumeSource(){
   }
   ~TetonVolumeSource() {}

   // --------------------------------------------------------------
   // Standard VS with all info
   // --------------------------------------------------------------
   TetonVolumeSource(const std::vector<int> &_zoneids,
		     const CharStar8 _vsname,
		     const CharStar8 _vsrce,
		     const CharStar8 _vshape,
		     double _prmult,
		     const std::vector<double>& _timetemps)
      : zoneids(_zoneids),
        vsname(_vsname),
        profile(_vsrce,_vshape,_prmult,_timetemps)
    {
    }

  std::vector<int> zoneids;
  CharStar8 vsname;
  TetonProfile profile;
private:
   // Disable copy and assginment
   //TetonVolumeSource& operator=(const TetonVolumeSource&);
   // Disable copy constructor.
   //TetonVolumeSource(const TetonVolumeSource&);
};

#endif
