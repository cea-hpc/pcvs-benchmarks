#include <iostream>
#include <ctime>
#include "TetonUtils.hh"
#include "transport/TetonInterface/Teton.hh"
#include "geom/CMI/MeshBase.hh"
#include "mpi.h"

#ifdef PROFILING_ON
#include "TAU.h"
#endif

#ifdef USE_HPM
#include "libhpm.h"
#endif

void initialize(MeshBase& myMesh, Teton<MeshBase>& theTeton, PartList<MeshBase>& myPartList);

using namespace Geometry;
using std::cout;
using std::endl;

int main(int argc, char* argv[])
{
    int myRank, numProcs;
    if( argc <2 )
    {
        cout<<" Must input mesh file name on input line."<<endl;
        exit(1);
    }
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
    MPI_Comm_size(MPI_COMM_WORLD,&numProcs);
    
#ifdef PROFILING_ON
    TAU_PROFILE_SET_NODE(myRank);
#endif

    if( myRank == 0 )
        cout<<"my proc is "<<myRank<<" out of "<<numProcs<<".  Constructing MeshBase object."<<endl;
    
    //
    // build me a MeshBase
    std::string meshFileName(argv[1]);
    MeshBase myMesh( meshFileName.c_str() );

    if( myRank == 0 )
        cout<<" MeshBase object built, building PartList, Region, OpacityBase, and Material."<<endl;
    
    //
    // build a PartList, Part, Region, Opacity, and Material
    PartList<MeshBase> myPartList( myMesh );
    Region<MeshBase> myRegion(myMesh);
    OpacityBase myOpacity;
    Material myMaterial(myOpacity);
    
    Part<MeshBase> myPart(myRegion,myMaterial);

    // shudder....
    myPartList.addPart( myPart );
    
    if( myRank == 0 )
        cout<<" Building Teton object."<<endl;
    //
    // Now build a teton
    Teton<MeshBase> myTetonObject;
    
    initialize(myMesh, myTetonObject, myPartList);   

    int numFluxes = myTetonObject.psir.size();
    int cumulativeIterationCount= 0;
    double cumulativeWorkTime =0.0;
    
    if( myRank == 0 )
    {
        cout<<" number of fluxes in Teton object: "<<numFluxes<<endl;
        cout<<" Teton object initialized, starting time advance loop."<<endl;
    }
    
#ifdef USE_HPM
    hpmInit(myRank,"TetonTest");
#endif
    for(int step = 0 ; step < 4; step++)
    {
        cumulativeWorkTime += advance(myMesh, myTetonObject, myPartList);
        cumulativeIterationCount += myTetonObject.noutrt * myTetonObject.ninrt;
    }
    
    if( myRank == 0 )
    {
        cout<<"cumulativeIterationCount= "<<cumulativeIterationCount<<"  cumulativeWorkTime= "<<cumulativeWorkTime<<" s."<<endl;
        cout<<" flux iteration updates per second = "<<numFluxes*cumulativeIterationCount/cumulativeWorkTime<<endl;
    }
    

#ifdef USE_HPM
    hpmTerminate(myRank);
#endif

    MPI_Finalize();
    return 0;
}

void initialize(MeshBase& myMesh, Teton<MeshBase>& theTeton, PartList<MeshBase>& myPartList)
{
    //
    // ugh.  accessing public data of Teton
    strncpy(theTeton.igeom.data,"xyz     ", 8);
    strncpy(theTeton.iaccel.data,  "off     ", 8);  // gda = acceleration, off = none

    std::vector<TetonFreq> freqData;    
    std::vector<TetonBoundary<MeshBase> > boundaryData;
    std::vector<TetonVolumeSource> volSourceData;
    std::vector<int> PXlookup( myMesh.getNumberOfOwnedNodes() , -1);
    
    std::vector<std::string> tagNames(6),bcNames(6),srcNames(6);

    tagNames[0] = "xMinFaces";
    tagNames[1] = "xMaxFaces";
    tagNames[2] = "yMinFaces";
    tagNames[3] = "yMaxFaces";
    tagNames[4] = "zMinFaces";
    tagNames[5] = "zMaxFaces";
    for(int i=0;i<bcNames.size(); i++)
    {
        bcNames[i] = "Vac     ";
        srcNames[i] = "vac     ";
    }

    int theOrder = 12;
    int quadType = 2;
    int Npolar = 8;
    int Nazimu = 4;
    
    //setFrequencies( 14, theOrder, freqData);
    setFrequencies( 14, quadType, theOrder, Npolar, Nazimu, freqData);
    setBoundary( myMesh, boundaryData, tagNames, bcNames, srcNames);
    
    //
    // ugh.  again accessing public data of Teton
    theTeton.ngr = freqData.size();
    theTeton.GTAorder = 2;
    
    theTeton.dtrad = 0.0001;
    theTeton.noutmx = 20;
    theTeton.ninmx = 1;
    theTeton.ngdamx = 7 ;
    theTeton.epsgda = 1.00e-05;
    theTeton.epstmp = 1.00e-04;
    theTeton.epsinr = 1.00e-04;
    theTeton.deltr = 0.40;
    theTeton.delte = 0.40 ;

    theTeton.dtrmn = 0.00002;
    theTeton.dtrmx = 1.0;

    theTeton.tfloor = 2.5e-5;
    
    theTeton.linkKull(myMesh, freqData, boundaryData, volSourceData);
    
    theTeton.CInitMaterial(myPartList);
    theTeton.CsetControls();
    
}
