#ifndef __TETON_UTILS_HH__
#define __TETON_UTILS_HH__
#include <vector>
#include <string>
#include "transport/TetonInterface/Teton.hh"
#include "transport/TetonInterface/TetonVolumeSource.hh"
#include "geom/CMI/MeshBase.hh"

void getZoneIDs(char coordAxis, double axisValue, const MeshBase& mesh, std::vector<int>& zoneIDs);
double newDT(const Teton<MeshBase>& theTeton);
double advance(MeshBase& theMesh, Teton<MeshBase>& theTeton, PartList<MeshBase>& myPartList);
void dumpEdit(const Teton<MeshBase>& theTeton,double radtrTime, double cumulativeTime);
void dumpLineout(MeshBase& theMesh, Teton<MeshBase>& theTeton, const std::string &fileName);
void setBoundary( const MeshBase& mesh, std::vector<TetonBoundary<MeshBase> >& theBoundary, 
                  const std::vector<std::string>& tagNames, const std::vector<std::string>& bcNames, 
                  const std::vector<std::string>& srcNames);
void setFrequencies( int numgp, int quadType, int theOrder, int Npolar, int Nazimu, std::vector<TetonFreq>& theFreqs );
void checkAnalyticAnswer(double goalTime, MeshBase& myMesh, PartList<MeshBase> &thePartList);

#endif
