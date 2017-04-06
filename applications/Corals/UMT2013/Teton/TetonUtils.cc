#include <sys/time.h>
#include "TetonUtils.hh"
#include "DBC.hh"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <map>
#include <cmath>
#include <vector>

#ifdef USE_OPENMP
#include <omp.h>
#endif

using std::cout;
using std::endl;
using std::setw;

//Retrieve owned zones IDs that have coordinates matching the specified criteria.
//The x, y, or z coordinate axis can be specified.  This algorithm will return all owned zones IDs
//with zone axis value <= axisValue. (ie: all zones with z <= 0.5)
void getZoneIDs(char coordAxis, double axisValue, const MeshBase& mesh, std::vector<int>& zoneIDs)
{
   MeshBase::ZoneIterator zItEnd = mesh.ownedZoneEnd();
   
   for (MeshBase::ZoneIterator zIt = mesh.ownedZoneBegin(); zIt != zItEnd; ++zIt)
   {
      const std::vector<double>& position = zIt->getPosition();
      if (coordAxis == 'x' && position[0] <= axisValue)
      {
         zoneIDs.push_back(zIt->getLocalID());
      }
      else if (coordAxis == 'y' && position[1] <= axisValue)
      {
         zoneIDs.push_back(zIt->getLocalID());
      }
      else if (coordAxis == 'z' && position[2] <= axisValue) 
      {
         zoneIDs.push_back(zIt->getLocalID());
      }
   }
}


void setBoundary( const MeshBase& mesh, std::vector<TetonBoundary<MeshBase> >& theBoundary, 
                  const std::vector<std::string>& tagNames, const std::vector<std::string>& typeNames, 
                  const std::vector<std::string>& shapeNames )
{
    int numTags = tagNames.size();
    int myRank=0,theNumOmpThreads=-1,myThreadNum=-2;
    
#ifdef USE_OPENMP
    MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
#pragma omp parallel private(myThreadNum)
    {
        theNumOmpThreads = omp_get_num_threads();
        myThreadNum = omp_get_thread_num();
   }//end of OMP section
#endif
    
    assert( typeNames.size() == numTags );
    assert( typeNames.size() == numTags );
    
    std::vector< std::vector<int> > problemBoundaryFaceIDs(numTags);
    
    //
    // loop over faces and add the IDs of the PROBLEM_BOUNDARY_FACE ones
    //
    for( MeshBase::FaceIterator fIt = mesh.ownedFaceBegin();
         fIt != mesh.ownedFaceEnd(); ++fIt)
    {
        if( fIt->getFacePtr()->FType == PROBLEM_BOUNDARY_FACE )
        {
            // get the boundary identifier tag (e.g. xMinFaces)
            std::string theFaceTag = fIt->getTag();
            int i = std::distance(tagNames.begin(),std::find(tagNames.begin(),tagNames.end(),theFaceTag));
            ASSERT( i<numTags );
            
            // add this face id to the vector of boundary faces of BC type 'theBCName'
            problemBoundaryFaceIDs[i].push_back( fIt->getLocalID() );
        }
    }//end of loop over faces

    // loop over list of BC types and create TetonBoundary objects for each 
    for(int i =0;i<typeNames.size();i++)
    {
        CharStar8  btype;
        strncpy(btype.data,typeNames[i].c_str(),8);

        if( shapeNames[i] == "none   ")
        {
            theBoundary.push_back( TetonBoundary<MeshBase>(problemBoundaryFaceIDs[i], btype) );        
        }
        else
        {
            std::vector<double> timeTemps(4);
            timeTemps[0]=0.0;
            timeTemps[1]=199.0;
            timeTemps[2]=150.0;
            timeTemps[3]=199.0;
            CharStar8 bshape;
            strncpy(bshape.data,shapeNames[i].c_str(),8);
            
            theBoundary.push_back( TetonBoundary<MeshBase>(problemBoundaryFaceIDs[i], btype, bshape, 1.0, timeTemps ) );        

        }
        
    }
    
    
}//end of setBoundary

void setFrequencies( int numgp, int quadType, int theOrder, int Npolar, int Nazimu, std::vector<TetonFreq>& theFreqs )
{
    assert ( (numgp>=1) );
    
    //  Quadrature Types
    //  quadType = 1  Level Symmetric 
    //  quadType = 2  Product
    
    // Quadrature Parameters
    int PolarAxis = 3;

    if (numgp == 1)
    {
        theFreqs.push_back(TetonFreq(0.0001, 200, quadType, theOrder, Npolar, Nazimu, PolarAxis));
    }
    else if(numgp > 1)
    {
        theFreqs.push_back(TetonFreq(0.0001, 0.01, quadType, theOrder, Npolar, Nazimu, PolarAxis));
        theFreqs.push_back(TetonFreq(0.01  , 0.05, quadType, theOrder, Npolar, Nazimu, PolarAxis));
        for(int g=2;g<numgp;g++)
        {
            theFreqs.push_back(TetonFreq(.05*(g-1), 0.05*g, quadType, theOrder, Npolar, Nazimu, PolarAxis));
        }
    }
    
}//end of setFrequencies

double newDT(const Teton<MeshBase>& theTeton)
{
    return theTeton.dtrad;
}

double advance(MeshBase& theMesh, Teton<MeshBase>& theTeton, PartList<MeshBase> &thePartList)
{
    struct timeval theStartTime, theEndTime;
    struct timezone theTimeZone;
    double radtrTime = 0.0;
    std::vector<double> addedEnergy( theMesh.getNumberOfOwnedZones(),0.0 );
    
    gettimeofday( &theStartTime, &theTimeZone );
    
    theTeton.CrelinkMesh( );
    
    theTeton.CupdateSn( );

    theTeton.UpdateOpacity( );
    
//     for(int i=0;i<theTeton.nzones;i++)
//         cout<<" trz["<<i<<"]="<<theTeton.trz[i]<<endl;
    
//     for(int i=0;i<theTeton.nzones;i++)
//         cout<<" tez["<<i<<"]="<<theTeton.tez[i]<<endl;
//     for(int i=0;i<theTeton.nzones;i++) 
//         for(int g=0;g<theTeton.ngr;g++)
//             cout<<"RadEnergyDensity["<<i<<","<<g<<"]="<<theTeton.RadEnergyDensity[g+i*theTeton.ngr]<<endl;
    
    theTeton.cxxRadtr();
    
    theTeton.CupdateSpecificEnergy( addedEnergy, theMesh, thePartList );

//     for(int i=0;i<theTeton.nzones;i++)
//         cout<<" trz["<<i<<"]="<<theTeton.trz[i]<<endl;
//     for(int i=0;i<theTeton.nzones;i++)
//         cout<<" tez["<<i<<"]="<<theTeton.tez[i]<<endl;

    gettimeofday( &theEndTime, &theTimeZone );

    radtrTime = (double)(theEndTime.tv_sec - theStartTime.tv_sec )*1000000.0 + (theEndTime.tv_usec- theStartTime.tv_usec);
    
    dumpEdit( theTeton , radtrTime, theTeton.angleLoopTime);

    return radtrTime;
}



void dumpEdit(const Teton<MeshBase>& theTeton, double radtrTime, double angleLoopTime)
{
    int myProc = 0;
    //unused variables
    double timeRadtr=0.0,radtotal=0.0;
    
#ifdef USE_MPI
    MPI_Comm_rank( MPI_COMM_WORLD, &myProc);
#endif

    
    if( myProc == 0 )
    {
        cout.precision(14);
        cout<< "CYCLE "<<theTeton.ncycle<<" timerad = "<<theTeton.timerad<<endl;
        cout<< " "<<endl;
        cout<< "TempIters = "<<theTeton.noutrt<<" FluxIters = "<<theTeton.ninrt<<" GTAIters = "<<theTeton.ngdart<<endl;
        cout<< "TrMax =     "<<theTeton.TrMax<<" in Zone "<<theTeton.TrMaxZone<<" on Node "<<theTeton.TrMaxNode<<endl;
        cout<< "TeMax =     "<<theTeton.TeMax<<" in Zone "<<theTeton.TeMaxZone<<" on Node "<<theTeton.TeMaxNode<<endl;
        cout<< "Recommended time step for next rad cycle = "<<theTeton.dtrad<<endl;
        cout<< " "<<endl;
        cout<< " "<<endl;
        cout<< "********** Run Time Statistics **********"<<endl;
        cout<< "                  Cycle Advance             Accumulated " << endl;
        cout<< "                     Time (sec)         Angle Loop Time (sec)"<<endl;
        cout<< "RADTR              = "<<radtrTime/1.0e6<<"             "<<angleLoopTime<<endl;
        cout<< " "<<endl;
    }
} //end of dumpEdit

void
dumpLineout(MeshBase& mesh, Teton<MeshBase>& theTeton, const std::string &fileName)
{
    //loop over mesh zones and write out rad temp and energy temp to file
    std::ofstream outFile;
    outFile.open(fileName.c_str());
    
    //
    // loop over zones and put rad temp to file
    //
    for( MeshBase::ZoneIterator zIt = mesh.ownedZoneBegin();
         zIt != mesh.ownedZoneEnd(); ++zIt)
    {
        std::vector<double> position = zIt->getPosition();
        outFile<< position[2]<<"  "<<pow(theTeton.trz[zIt->getLocalID()],4.0)
               <<"  "<<pow(theTeton.tez[zIt->getLocalID()],4.0)<<std::endl;
    }
    outFile<<std::endl;
    
    outFile.close();

}//end of dumpLineout

struct CompareZValues
{
    bool operator()(const std::pair<double,double> &v1, const std::pair<double,double> &v2)
        { return v1.first<v2.first; }
};

void checkAnalyticAnswer(double goalTime, MeshBase& myMesh, PartList<MeshBase> &partList)
{
    double  alpha_cv = 0.0548806601399;
    double  a = alpha_cv/4.0;    
    int myProc = 0,numProcs=1;
    
#ifdef USE_MPI
    MPI_Comm_size( MPI_COMM_WORLD, &numProcs);
    MPI_Comm_rank( MPI_COMM_WORLD, &myProc);
#endif

    std::vector<double> zValues(8), exactMatEnergyVals(8);
    if( fabs( goalTime - 0.000334) < 1.0e-5 )
    {
        // case for end time tau=0.10000
    
        zValues[0]= 0.01000; exactMatEnergyVals[0] = .00468;
        zValues[1]= 0.10000; exactMatEnergyVals[1] = .00468;
        zValues[2]= 0.17783; exactMatEnergyVals[2] = .00468;
        zValues[3]= 0.31623; exactMatEnergyVals[3] = .00468;
        zValues[4]= 0.45000; exactMatEnergyVals[4] = .00455;
        zValues[5]= 0.50000; exactMatEnergyVals[5] = .00234;
        zValues[6]= 0.56234; exactMatEnergyVals[6] = .00005;
        zValues[7]= 0.75000; exactMatEnergyVals[7] = .00000;
    }
    if( fabs( goalTime - 0.001055) < 1.0e-5 )
    {
        // case for end time tau=0.31623
    
        zValues[0]= 0.01000; exactMatEnergyVals[0] = .04093;
        zValues[1]= 0.10000; exactMatEnergyVals[1] = .04093;
        zValues[2]= 0.17783; exactMatEnergyVals[2] = .04093;
        zValues[3]= 0.31623; exactMatEnergyVals[3] = .04032;
        zValues[4]= 0.45000; exactMatEnergyVals[4] = .03314;
        zValues[5]= 0.50000; exactMatEnergyVals[5] = .02046;
        zValues[6]= 0.56234; exactMatEnergyVals[6] = .00635;
        zValues[7]= 0.75000; exactMatEnergyVals[7] = .00005;
    }
    else if( fabs( goalTime - 0.003336) < 1.0e-5 )
    {
        // case for end time tau=1.00000
    
        zValues[0]= 0.01000; exactMatEnergyVals[0] = .27126;
        zValues[1]= 0.10000; exactMatEnergyVals[1] = .26839;
        zValues[2]= 0.17783; exactMatEnergyVals[2] = .26261;
        zValues[3]= 0.31623; exactMatEnergyVals[3] = .23978;
        zValues[4]= 0.45000; exactMatEnergyVals[4] = .18826;
        zValues[5]= 0.50000; exactMatEnergyVals[5] = .14187;
        zValues[6]= 0.56234; exactMatEnergyVals[6] = .08838;
        zValues[7]= 0.75000; exactMatEnergyVals[7] = .03014;
    }

    PartList<MeshBase>::Iterator partPtr;

    // first find one face on the z=0 boundary on processor 0
    double theCenters[2];
    theCenters[0] = -1.0;
    theCenters[1] = -1.0;

    if (myProc == 0 )
    {
        for(MeshBase::FaceIterator fidP =myMesh.ownedFaceBegin(); fidP != myMesh.ownedFaceEnd(); ++fidP)
        {
            std::vector<double> faceCentroid = fidP->getPosition();
            if( fabs(faceCentroid[2])<1.0e-8 )
            {
                theCenters[0] = faceCentroid[0];
                theCenters[1] = faceCentroid[1];
                
                std::cout.precision(6);
                cout<<" Computing error check along x="<<setw(9)<<theCenters[0]<<", y="<<setw(9)<<theCenters[1]<<endl;
                break;
            }
        }
    }

    // broadcast this location to all procs
    double xCenter=-1.0,yCenter=-1.0;

#ifdef USE_MPI
    MPI_Bcast(theCenters,2,MPI_DOUBLE,0,MPI_COMM_WORLD);
    xCenter = theCenters[0];
    yCenter = theCenters[1];
#endif

    // record (z, T) data at the given (xCenter,yCenter)
    std::vector< std::pair<double,double> > matTemp;
            
    for(partPtr =  partList.begin(); partPtr !=  partList.end(); partPtr++) 
    {
        // collect the (z,(T(z)^4) pairs if x and y are close to (xCenter,yCenter) 
        const Part<MeshBase>::ZonalScalarFieldType &T  = partPtr->getTemperature();
        for(MeshBase::ZoneIterator zidP =myMesh.ownedZoneBegin(); zidP != myMesh.ownedZoneEnd(); ++zidP)
        {
            double x = (zidP->getPosition())[0];
            double y = (zidP->getPosition())[1];
            
            if( (fabs(x-xCenter)<1.0e-5 ) && (fabs(y-yCenter)<1.0e-5) )
            {
                matTemp.push_back( std::make_pair((zidP->getPosition())[2],pow(T[*zidP],4.0)) );
            }           
        }
    }
    
    // first gather the data sizes to proc 0
    int *arraySizes = new int[numProcs];
    int *displs = new int[numProcs];
    int myLocalArraySize = matTemp.size();
    
//     cout<<" task "<<myProc<<"  local array size:"<<myLocalArraySize<<endl;
    
    MPI_Gather( &myLocalArraySize, 1, MPI_INT, arraySizes, 1, MPI_INT, 0, MPI_COMM_WORLD);

    // gather (z, T) data to proc 0

    double *localZValues, *localTValues;
    localZValues = new double[matTemp.size()];
    localTValues = new double[matTemp.size()];    
    int j=0;
    
    for(std::vector<std::pair<double,double> >::const_iterator it = matTemp.begin(); it != matTemp.end(); j++,++it)
    {
        localZValues[j] = it->first;
        localTValues[j] = it->second;

    }
    
    double *globalZValues;
    double *globalTValues;

    int theGlobalArraySize = 0;
    if( myProc == 0 )
    {        
        for(int p=0;p<numProcs;p++)
        {
            displs[p] = theGlobalArraySize;
            theGlobalArraySize += arraySizes[p];
        }
        
        globalZValues = new double[theGlobalArraySize];
        globalTValues = new double[theGlobalArraySize];
    }

    MPI_Gatherv(localZValues, myLocalArraySize, MPI_DOUBLE, globalZValues, 
                arraySizes, displs, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    MPI_Gatherv(localTValues, myLocalArraySize, MPI_DOUBLE, globalTValues, 
                arraySizes, displs, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    delete[] localTValues;
    delete[] localZValues;
    delete[] displs;
    delete[] arraySizes;
    
    
    // sort and compare with analytic solution

    if( myProc == 0 )
    {
        std::vector< std::pair<double,double> > myGlobalMatTemps;
        for(int i=0;i<theGlobalArraySize;i++)
        {
            myGlobalMatTemps.push_back( std::make_pair(globalZValues[i],globalTValues[i]) );
        }
        
        // sort the vector 
        std::sort(myGlobalMatTemps.begin(),myGlobalMatTemps.end(),CompareZValues());

        int currentExactValueIndex=0;
        double interpolatedT=0.0;
        double zExact = zValues[currentExactValueIndex];
        
        // make sure first sorted z value is below zValues[currentExactValueIndex]
        while( myGlobalMatTemps.begin()->first > zExact )
        {
            zExact = zValues[++currentExactValueIndex];
        }

        for(std::vector<std::pair<double,double> >::const_iterator it = myGlobalMatTemps.begin()+1; 
            it != myGlobalMatTemps.end(); ++it)
        {
       
            double zLow = (it-1)->first;
            double zHigh = it->first;
            
            if( ( zLow <= zExact ) && ( zHigh > zExact ) )
            {
                // found an index (i) s.t. an exact value lies beween i-1 and i of matTemp
                double tLow = (it-1)->second;
                double tHigh = it->second;
           
                // brain dead linear interpolation for now.
                interpolatedT = tLow + (zExact-zLow)/(zHigh-zLow)*(tHigh-tLow);
           
                std::cout<<" z= "<<setw(7)<<zExact;
                std::cout<<" tInterpolated= "<<setw(11)<<interpolatedT;
                std::cout<<" tExact="<<setw(11)<<exactMatEnergyVals[currentExactValueIndex];
                std::cout<<" error="<<setw(11)<<fabs(interpolatedT-exactMatEnergyVals[currentExactValueIndex])<<std::endl;
           
                // next exact value
                currentExactValueIndex++;
                if( currentExactValueIndex == zValues.size() )
                    break;
                
                zExact = zValues[currentExactValueIndex];
            }

        }// end of loop over myGlobalMatTemps looking for exact values to interpolate.

        delete[] globalTValues;
        delete[] globalZValues;

    }//end of myProc == 0 condition

}

