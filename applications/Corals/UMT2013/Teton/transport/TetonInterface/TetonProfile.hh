//---------------------------------*-C++-*----------------------------------//
// Copyright 1998 The Regents of the University of California.
// All rights reserved.
//--------------------------------------------------------------------------//

#ifndef __TETON_TETONPROFILE_HH__
#define __TETON_TETONPROFILE_HH__

#include "utilities/KullTypes/CharStar8.hh"
#include <cstring>
#include <cstdio>
#include <vector>

class TetonProfile {
//-----------------------------------------------------------
//  Teton Sn Radiation Transport source and boundary profile
//  Module
//-----------------------------------------------------------
public:
   ~TetonProfile() {}

   // ------------------------------------------------------------
   // An empty profile
   // ------------------------------------------------------------
   TetonProfile()
      : 
      prmult(0.0),
      profID(0)
   {
      strncpy(type.data,  "none    ", 8);
      strncpy(pshape.data, "none    ", 8);
      timetemps.resize(0);
   }

   // ------------------------------------------------------------
   // No shape, just a type
   // ------------------------------------------------------------
   TetonProfile(const CharStar8& _type)
       : type(_type),
         prmult(0.0),
         profID(0){
      strncpy(pshape.data, "none    ", 8);
      timetemps.resize(0);
   }

   // ------------------------------------------------------------
   // Add type (refl,vac,temp) and shape (none,iso,fds) info
   // ------------------------------------------------------------
   TetonProfile(const CharStar8& _type,
                const CharStar8& _pshape,
                double _prmult,
                const std::vector<double> &_timetemps)
       : type(_type),
         pshape(_pshape),
         prmult(_prmult),
         timetemps(_timetemps),
         profID(0)
   {
   }

   // ------------------------------------------------------------
   // Does it lack type
   // ------------------------------------------------------------
   int 
   typeless() {
      printf("Check type of %c%c%c%c%c\n",
              type.data[0], type.data[1], type.data[2],
              type.data[3], type.data[4]);
      return ( strncmp(type.data, "none    ", 8) == 0 );
   }

   // ------------------------------------------------------------
   // Does it lack shape
   // ------------------------------------------------------------
   int 
   shapeless() {
      return ( strncmp(pshape.data, "none    ", 8) == 0 );
   }

   // ------------------------------------------------------------
   // How many time fields
   // ------------------------------------------------------------
   int 
   tim(int ngr) {
      int isSourceFds = 0;
      char buf[9];
      strncpy(buf, type.data, 8);
      buf[8] = 0;
      isSourceFds = typeIs("fds");
      if ( isSourceFds ) {
         return (timetemps.size()/(ngr+1));
      } 
      else {
         return (timetemps.size()/2);
      }
   }

   int 
   typeIs(const char *proftype) {
      int l;      
      l = strlen(proftype);
      if ( l >= 8 ) {
         return ( strncmp(proftype, type.data, 8) == 0 );
      } 
      else {
         return ( strncmp(proftype, type.data, l) == 0 
               && type.data[l] == ' ');
      }
   }


   CharStar8            type;
   CharStar8            pshape;
   double               prmult;
   std::vector<double>  timetemps;
   int                  profID;
};

#endif
