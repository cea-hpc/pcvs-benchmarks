//---------------------------------*-C++-*----------------------------------//
// Copyright 1998 The Regents of the University of California.
// All rights reserved.
//
// Created on: May 1998
// Created by:  Pat Miller
// Also maintained by: Michael Nemanic
//--------------------------------------------------------------------------//

#ifndef __TETON_TETONFREQ_HH__
#define __TETON_TETONFREQ_HH__

class TetonFreq {
//-------------------------------------------------------------
// Teton Sn Radiation Transport Energy frequency module
//-------------------------------------------------------------
public:
  TetonFreq() {}
  ~TetonFreq() {}
  TetonFreq(double _bot,
            double _top,
            int _qtype,
            int _qorder,
            int _npolar,
            int _nazimu,
            int _paxis)
     : qtype(_qtype), qorder(_qorder), npolar(_npolar), nazimu(_nazimu), paxis(_paxis), bot(_bot), top(_top)
  {
  }

  int           qtype, qorder, npolar, nazimu, paxis;
  double        bot, top;
};

#endif
