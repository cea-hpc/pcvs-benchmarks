//------------------------------*-C++-*----------------------------------
// Copyright 2001 The Regents of the University of California. 
// All rights reserved.
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// For full text see LICENSE.TXT
//-----------------------------------------------------------------------


#include <algorithm>
#include <set>
#include "geom/CMI/MeshBase.hh"
#include "communication/CommAgent.hh"
#include "communication/DomainNeighborMap.hh"


using namespace std;


// ----------------------------------------------------------------------

namespace Communication
{


//----------------------------------------------------------------------
CommAgent::
CommAgent():
   mTag(1),
   mFaceNeighborMap(new FaceMap())
{
} // end constructor
//----------------------------------------------------------------------


//----------------------------------------------------------------------
CommAgent::
~CommAgent()
{
   delete mFaceNeighborMap;
} // end destructor
//----------------------------------------------------------------------


//----------------------------------------------------------------------
void
CommAgent::
clear()
{
   mFaceNeighborMap->clear();
} // end clear
//----------------------------------------------------------------------


//----------------------------------------------------------------------------
void 
CommAgent::
setFaceNeighborMap(const FaceMap& info)
{
   *mFaceNeighborMap = info;
}
//----------------------------------------------------------------------------


//----------------------------------------------------------------------------
const CommAgent::FaceMap&
CommAgent::
getFaceNeighborMap() const
{
   return *mFaceNeighborMap;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
CommAgent::FaceMap&
CommAgent::
getFaceNeighborMap()
{
   return *mFaceNeighborMap;
}
//----------------------------------------------------------------------------

} // end namespace Communication
