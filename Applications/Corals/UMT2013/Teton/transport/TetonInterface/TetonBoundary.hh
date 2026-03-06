//---------------------------------*-C++-*----------------------------------//
// Copyright 1998 The Regents of the University of California.
// All rights reserved.
//--------------------------------------------------------------------------//

#ifndef __TETON_TETONBOUNDARY_HH__
#define __TETON_TETONBOUNDARY_HH__

#include <vector>
#include "utilities/KullTypes/CharStar8.hh"
#include "transport/TetonInterface/TetonProfile.hh"


template<typename Mesh>
class TetonBoundary {
//-------------------------------------------------------------
//  Teton Radiation Transport Boundary Module
//-------------------------------------------------------------
public:
   // ------------------------------------------------------------
   // Default constructor needed for std::vector<TetonBoundary>
   // ------------------------------------------------------------
   TetonBoundary(){}
   ~TetonBoundary() {}

   // ------------------------------------------------------------
   // Standard BC with all info (including shape)
   // ------------------------------------------------------------
   TetonBoundary(const std::vector<int> &_faceids,
                 const CharStar8 _bctype,
                 const CharStar8 _bshape,
                 double _prmult,
                 const std::vector<double>& _timetemps)
      : faceids(_faceids),
        profile(_bctype,_bshape,_prmult,_timetemps)
    {
    }

   // ------------------------------------------------------------
   // Provide a constructor for ``shapeless'' ("none") BCs
   // ------------------------------------------------------------
   TetonBoundary(const std::vector<int> &_faceids,
                 const CharStar8 _bctype)
       : faceids(_faceids),
         profile(_bctype)
    {
    }

   std::vector<int> faceids;
   TetonProfile profile;

private:
   // Disable copy and assginment
   //TetonBoundary& operator=(const TetonBoundary&);
   // Disable copy constructor.
   //TetonBoundary(const TetonBoundary&);
};

#endif
