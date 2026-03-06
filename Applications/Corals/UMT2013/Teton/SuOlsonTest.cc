#include <iostream>
#include <algorithm>
#include <ctime>
#include <cstring>
#include <cmath>
#include "TetonUtils.hh"
#include "transport/TetonInterface/Teton.hh"
#include "geom/CMI/MeshBase.hh"
#include "mpi.h"
#include "omp.h"

// #ifdef BGP
#if 0
#include <spi/kernel_interface.h>
#include <common/bgp_personality.h>
#include <common/bgp_personality_inlines.h>
#endif

#ifdef PROFILING_ON
#include "TAU.h"
#endif

void initialize(MeshBase& myMesh, Teton<MeshBase>& theTeton, PartList<MeshBase>& myPartList,
                int theNumGroups, int quadType, int theOrder, int Npolar, int Nazimu);

using namespace Geometry;
using std::cout;
using std::endl;

//==========================================================
//  run syntax:
//
// ./SuOlsonTest gridFileName quadratureOrder numGroups
//==========================================================

int main(int argc, char* argv[])
{
    int myRank=0, numProcs, theOrder=16, theNumGroups=16;
    int Npolar=8, Nazimu=4;
    int quadType=2;
    std::string theVersionNumber = "1.0";
    
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
    MPI_Comm_size(MPI_COMM_WORLD,&numProcs);
    
    if( argc <2 )
    {
        if(myRank == 0)
            cout<<" Must input mesh file name on input line."<<endl;
        exit(1);
    }    
    if(myRank == 0)
    {
            cout<<" Executing UMT2013 Number of ranks ="<<numProcs<<endl;
#pragma omp parallel
{
	    int myTID = omp_get_thread_num();
	    int numThreads = omp_get_num_threads();
            if (myTID == 0) 
            {
                 cout<<" and number of OMP threads  ="<< numThreads <<endl;
            }
}
    }

    if( argc >= 3 )
    {
        theNumGroups = atoi(argv[2]);
        if(myRank == 0)
            cout<<" gridFileName: "<< argv[1] <<endl;
        if(myRank == 0)
            cout<<" setting the number of groups="<<theNumGroups<<endl;
    }

    if( argc >= 4 )
    {
        quadType = atoi(argv[3]);
        
        if(quadType == 1) {
           theOrder = atoi(argv[4]);
              if(myRank == 0) {
                 cout<<" Running with Level Symmetric Quadrature "<<endl;
                 cout<<" setting quadrature Order = "<<theOrder<<endl;
              }
        }

        if(quadType == 2) {
           Npolar = atoi(argv[5]);
           Nazimu = atoi(argv[6]);
              if(myRank == 0) {
                 cout<<" Running with Product Quadrature "<<endl;
                 cout<<" setting number of polar angles     = "<<Npolar<<endl;
                 cout<<" setting number of azimuthal angles = "<<Nazimu<<endl;
              }
        }
    }
    
#ifdef PROFILING_ON
    TAU_PROFILE_INIT(argc,argv);
    TAU_PROFILE("main()","",TAU_DEFAULT);
    TAU_PROFILE_SET_NODE(myRank);
#endif
    
    //
    // build a MeshBase
    if(myRank == 0)
        cout<<" Building mesh..."<<endl;
    std::string meshFileName(argv[1]);
    MeshBase myMesh( meshFileName.c_str() );
    if(myRank == 0)
        cout<<" Mesh complete."<<endl;

    //
    // get SMS index for this task's mesh domain
    //
    int sms_i=-1, sms_j=-1, sms_k=-1;
    cgetsms(&sms_i, &sms_j, &sms_k);
    
    
// #ifdef BGP
#if 0
    _BGP_Personality_t personality; 
    Kernel_GetPersonality(&personality, sizeof(personality));
    if (myRank == 0)
    {
        int memory_size_MBytes = personality.DDR_Config.DDRSizeMB;
        printf("Memory size = %d MBytes\n", memory_size_MBytes);
        int node_config = personality.Kernel_Config.ProcessConfig;
        if (node_config == _BGP_PERS_PROCESSCONFIG_SMP) printf("SMP mode\n");
        else if (node_config == _BGP_PERS_PROCESSCONFIG_VNM) printf("Virtual-node mode\n");
        else if (node_config == _BGP_PERS_PROCESSCONFIG_2x2) printf("Dual mode\n");
        else printf("Unknown mode\n");
        int xsize = personality.Network_Config.Xnodes;
        int ysize = personality.Network_Config.Ynodes;
        int zsize = personality.Network_Config.Znodes;
        printf("torus dimensions = <%d,%d,%d>\n", xsize, ysize, zsize); 
    }
    int torus_x = personality.Network_Config.Xcoord;
    int torus_y = personality.Network_Config.Ycoord;
    int torus_z = personality.Network_Config.Zcoord;
    MPI_Barrier(MPI_COMM_WORLD);
    printf("MPI rank %d has torus coords <%d,%d,%d> \n",myRank, torus_x, torus_y, torus_z);
#endif

    //
    // build a PartList, Part, Region, Opacity, and Material
    PartList<MeshBase> myPartList( myMesh );
    Region<MeshBase> myRegion(myMesh);
    OpacityBase myOpacity;
    Material myMaterial(myOpacity);
    
    double T0 = pow(10.0,-2.5);
    double density0 = 1.0;
    
    Part<MeshBase> myPart(myRegion,myMaterial, MARSHAK, density0, T0);

    // shudder....
    myPartList.addPart( myPart );
    
    //
    // Now build a teton
    Teton<MeshBase> myTetonObject;
    
    if(myRank == 0)
        cout<<" Initializing Teton object..."<<endl;
    initialize(myMesh, myTetonObject, myPartList, theNumGroups, quadType, theOrder, Npolar, Nazimu);
    if(myRank == 0)
        cout<<"    done."<<endl;
    
    double goalTime = 0.000334;
//     double goalTime = 0.0000334;

    double time;
    double dt= newDT(myTetonObject);
    int numZones = myMesh.getNumberOfOwnedZones();
    int totMeshZones=0;
    
//     cout<<" SMS index for task "<<myRank<<":("<<sms_i<<","<<sms_j<<","<<sms_k<<").  num zones ="<<numZones<<endl;

    MPI_Reduce(&numZones,&totMeshZones,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD);
    
    int numFluxes = myTetonObject.psir.size();
    int cumulativeIterationCount= 0;
    double cumulativeWorkTime = 0.0;
    
    if(myRank == 0)
        cout<<" Starting time advance..."<<endl;
    for(time=0;time<goalTime; time+= dt)
    {
        dt = newDT( myTetonObject );
        if( time + dt >=goalTime ) 
        {
            dt = goalTime-time;
            myTetonObject.dtrmn = dt;
            if( myRank == 0 )
                cout<<" SuOlson setting last step's dt="<<dt<<endl;
        }
        
        myTetonObject.dtrad  = dt;
    
        cumulativeWorkTime += advance(myMesh, myTetonObject, myPartList);
        cumulativeIterationCount += myTetonObject.ninrt;
    }
    if( myRank == 0 )
        cout<<" SuOlson Test version "<<theVersionNumber<<" completed at time= "<<time<<"  goalTime= "<<goalTime<<endl;

    checkAnalyticAnswer(goalTime,myMesh,myPartList);     

    cumulativeWorkTime/=1.0e6;  // microseconds -> seconds
    if( myRank == 0 )
    {
        double numUnknowns = static_cast<double>(myTetonObject.ngr) * 
            static_cast<double>(myTetonObject.nangsn) * 
            static_cast<double>(totMeshZones) * 
            8.0;   
        cout<<"numUnknowns = "<<numUnknowns<<endl;
        
        dumpLineout(myMesh, myTetonObject,"dump.out");
        cout<<"cumulativeIterationCount= "<<cumulativeIterationCount<<" cumulativeWorkTime="<<cumulativeWorkTime<<" s."<<endl;
        cout<<"figure of merit = "<<numUnknowns/cumulativeWorkTime*cumulativeIterationCount<<endl;
    }

#ifdef PROFILING_ON
    TAU_DB_DUMP_PREFIX("profile");  
#endif

    MPI_Finalize();
    return 0;
}

void initialize(MeshBase& myMesh, Teton<MeshBase>& theTeton, PartList<MeshBase>& myPartList,
                int theNumGroups, int quadType, int theOrder, int Npolar, int Nazimu)
{
    //
    // ugh.  accessing public data of Teton
    
    strncpy(theTeton.igeom.data,"xyz     ", 8);
    strncpy(theTeton.iaccel.data,"off     ", 8);  // iterative acceleration: gda, or off
         
    std::vector<TetonFreq> freqData;    
    std::vector<TetonBoundary<MeshBase> > boundaryData;
    std::vector<TetonVolumeSource> volSourceData(1);

    std::vector<int> theSourceZones;
    
    // define volume source on some subset of zones with the given
    // time history
    getZoneIDs('z',0.5, myMesh, theSourceZones);

    double theSourceTemp = 1.0;
    
    std::vector<double> theTimeTemps(8);
    theTimeTemps[0] = 0.0;
    theTimeTemps[1] = theSourceTemp;
    theTimeTemps[2] = 10.0;
    theTimeTemps[3] = theSourceTemp;
    theTimeTemps[4] = 10.01;
    theTimeTemps[5] = 0.0;
    theTimeTemps[6] = 150.0;
    theTimeTemps[7] = 0.0;
    
    CharStar8 theSrcName, theType, theShape;
    strncpy(theSrcName.data,"Source  ",8);
    strncpy(theType.data, "temp    ",8);
    strncpy(theShape.data,"iso     ",8);
    
    TetonVolumeSource theVolSource(theSourceZones, theSrcName, theType, theShape, 1.0, theTimeTemps);
    volSourceData[0]=theVolSource;

    // set group boundaries
    setFrequencies( theNumGroups, quadType, theOrder, Npolar, Nazimu, freqData);

    // set boundary information
    std::vector<std::string> tagNames(6),typeNames(6),shapeNames(6);

    tagNames[0] = "xMinFaces";
    tagNames[1] = "xMaxFaces";
    tagNames[2] = "yMinFaces";
    tagNames[3] = "yMaxFaces";
    tagNames[4] = "zMinFaces";
    tagNames[5] = "zMaxFaces";
    for(int i=0;i<typeNames.size()-1; i++)
    {
        typeNames[i] = "refl    ";
        shapeNames[i] ="none    ";
    }
    typeNames[typeNames.size()-1] = "vac     ";
    shapeNames[typeNames.size()-1] ="none    ";

    setBoundary( myMesh, boundaryData, tagNames, typeNames, shapeNames);
    
    //
    // ugh.  again accessing public data of Teton
    theTeton.ngr = freqData.size();
    theTeton.GTAorder = 2;
    
    theTeton.dtrad  = 3.0e-06;
    theTeton.noutmx = 20;
    theTeton.ninmx  = 1;
    theTeton.ngdamx = 7 ;
    theTeton.epsgda = 1.00e-07;
    theTeton.epstmp = 1.00e-06;
    theTeton.epsinr = 1.00e-06;
    theTeton.deltr  = 0.40;
    theTeton.delte  = 0.40 ;

    theTeton.dtrmn = 0.00006;
    theTeton.dtrmx = 0.1;

    theTeton.tfloor = 2.5e-5;
    theTeton.tmin   = 5.0e-3;
         
    theTeton.linkKull(myMesh, freqData, boundaryData, volSourceData);
    
    
    theTeton.CInitMaterial(myPartList);
    theTeton.CsetControls();

    for(PartList<MeshBase>::iterator pIt = myPartList.begin();
        pIt != myPartList.end(); ++pIt)
    {
        const std::vector<double>& matTemp = theTeton.tez;
        const std::vector<double>& radTemp = theTeton.trz;
        
        pIt->updateThermodynamicState(matTemp,radTemp);
    }
            
}

