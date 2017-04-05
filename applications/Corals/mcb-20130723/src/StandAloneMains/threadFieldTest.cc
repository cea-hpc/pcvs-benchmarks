//----------------------------------*-C++-*----------------------------------//
// Copyright 2009 Lawrence Livermore National Security, LLC
// All rights reserved.
//---------------------------------------------------------------------------//

// This work performed under the auspices of the U.S. Department of Energy by
// Lawrence Livermore National Laboratory under Contract DE-AC52-07NA27344

//  DISCLAIMER
//  This work was prepared as an account of work sponsored by an agency of the
//  United States Government. Neither the United States Government nor the
//  Lawrence Livermore National Security, LLC, nor any of their employees,
//  makes any warranty, express or implied, including the warranties of
//  merchantability and fitness for a particular purpose, or assumes any
//  legal liability or responsibility for the accuracy, completeness, or
//  usefulness of any information, apparatus, product, or process disclosed,
//  or represents that its use would not infringe privately owned rights.

#include <vector>
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <string>
#include <sstream>
#include <cstring>

#include "mesh_typedef.hh"
#include "mat_typedef.hh"
#include "Ortho_Cartesian_2D_Mesh.hh"
#include "Material_data_base.hh"
#include "Analytic_opacity_data_base.hh"
#include "Boundary_Condition.hh"
#include "reflecting_BC.hh"
#include "IMC.hh"
#include "constant_absorp_opac.hh"
#include "isotropic_scattering.hh"
#include "isotropic_source.hh"
#include "BoxPartition.hh"
#include "MCAnswer.hh"

#include "anyOption.h"

#ifdef USE_MPI
#include "mpi.h"
#include "IMC_Domain_organizer.hh"
#endif

#ifdef USE_OPENMP
#include <omp.h>
#endif


using namespace std;
using namespace IMC_namespace;

template<typename mesh_type>
void touchOMPFields( const mesh_type &inMesh );

void parseArgs(int argc, char *argv[], 
               int &nMpiTaskssX, int &nMpiTaskssY,
               unsigned int &nZonesXGlobal, unsigned int &nZonesYGlobal, 
               double &xMaxGlobal, double &yMaxGlobal,
               double &sigmaAbsorb, double &sigmaScatt, 
               double &xRoot, double &yRoot, 
               double &xInnerLo, double &yInnerLo,
               double &xInnerHi, double &yInnerHi,
               double &ratioRhoInner,
               unsigned long long &N_particles, bool &isWeakScaling);

int main(int argc, char* argv[])
{
    //---------------------------------------------------------------------//
    // Start MPI

    int procID = 0;
    int nProcs = 1;

#ifdef USE_MPI
    MPI_Init( &argc, &argv );

    MPI_Comm_size( MPI_COMM_WORLD, &nProcs );
    MPI_Comm_rank( MPI_COMM_WORLD, &procID );
#endif
    int num_threads =1;
    
#ifdef USE_OPENMP
    num_threads = omp_get_max_threads();
    printf("hello from MPI task: %d, I have %d omp threads.\n",procID,num_threads);
#endif

    cout << "nProcs = " << nProcs << endl;
    //---------------------------------------------------------------------//
    // Set up output precision
    //cout << setiosflags(ios::fixed);
    //cout << setprecision(3);

    //---------------------------------------------------------------------//
    // create a mesh
    unsigned int nZonesXGlobal = 10;
    unsigned int nZonesYGlobal = 10;
    double xMinGlobal = 0.0;
    double yMinGlobal = 0.0;
    double xMaxGlobal = 2.0;
    double yMaxGlobal = 2.0;
    double sigmaAbsorb = 1.0;
    double sigmaScatt = 20.0;
     
    double sourceStrength = 1.0;
    double xRoot = xMaxGlobal/2.0;     
    double yRoot = yMaxGlobal/2.0;
    unsigned long long N_particles = 1000000;

    double xInnerLo = -999999.0;
    double yInnerLo = -999999.0;
    double xInnerHi = -999999.0;
    double yInnerHi = -999999.0;
    double ratioRhoInner = 1.0;

    int nMpiTasksX = 1; 
    int nMpiTasksY = 2; 

    bool isWeakScaling=false;
    
    parseArgs(argc, argv,
              nMpiTasksX, nMpiTasksY,
              nZonesXGlobal, nZonesYGlobal, 
              xMaxGlobal, yMaxGlobal,
              sigmaAbsorb, sigmaScatt, 
              xRoot, yRoot, 
              xInnerLo, yInnerLo, xInnerHi,yInnerHi,
              ratioRhoInner,
              N_particles, isWeakScaling);

    cout<<" After call to parseArgs, "<<endl;
    cout<<"    nMpiTasksX=       "<<nMpiTasksX<<endl;
    cout<<"    nMpiTasksY=       "<<nMpiTasksY<<endl;
    cout<<"    nZonesXGlobal= "<<nZonesXGlobal<<endl;
    cout<<"    nZonesYGlobal= "<<nZonesYGlobal<<endl;
    cout<<"    xMaxGlobal=    "<<xMaxGlobal<<endl;
    cout<<"    yMaxGlobal=    "<<yMaxGlobal<<endl;
    cout<<"    sigmaAbsorb=   "<<sigmaAbsorb<<endl;
    cout<<"    sigmaScatt=    "<<sigmaScatt<<endl;
    cout<<"    xRoot     =    "<<xRoot<<endl;
    cout<<"    yRoot     =    "<<yRoot<<endl;
    cout<<"    numParticles=  "<<N_particles<<endl;
    cout<<"    xInnerLo     = "<<xInnerLo<<endl;
    cout<<"    xInnerHi     = "<<xInnerHi<<endl;
    cout<<"    yInnerLo     = "<<yInnerLo<<endl;
    cout<<"    yInnerHi     = "<<yInnerHi<<endl;
    cout<<"    ratioRatioInner = "<<ratioRatioInner<<endl;
    cout<<"    isWeakScaling= "<<isWeakScaling<<endl;
    

    //---------------------------------------------------------------------//
    // partition zones across procs

    if( nMpiTasksX * nMpiTasksY != nProcs)
    {
        if( procID == 0 )
        {
            cout << "ERROR: Must have nMpiTasksX * nMpiTasksY = nProcs." << '\n';
            cout << "nMpiTasksX = " << nMpiTasksX << endl;
            cout << "nMpiTasksY = " << nMpiTasksY << endl;
            cout << "nMpiTasksX*nMpiTasksY = " << nMpiTasksX*nMpiTasksY << endl;
            cout << "nProcs = " << nProcs << endl;

        }
        
#ifdef USE_MPI
        MPI_Abort( MPI_COMM_WORLD, -1 );
#else
        throw ("Must have nMpiTasksX * nMpiTasksY = nProcs.");
#endif
    }

    unsigned int nZonesXLocal;
    unsigned int nZonesYLocal;
    double xMinLocal;
    double yMinLocal;
    double xMaxLocal;
    double yMaxLocal;

    BoxPartition( nZonesXGlobal, xMinGlobal, xMaxGlobal,
                  nZonesYGlobal, yMinGlobal, yMaxGlobal,
                  nMpiTasksX, nMpiTasksY, procID,
                  nZonesXLocal, xMinLocal, xMaxLocal,
                  nZonesYLocal, yMinLocal, yMaxLocal );

    Ortho_Cartesian_2D_Mesh Mesh( nZonesXLocal,
                                  xMinLocal,
                                  xMaxLocal,
                                  nZonesYLocal,
                                  yMinLocal,
                                  yMaxLocal,
                                  nMpiTasksX,
                                  nMpiTasksY,
                                  procID );

    touchOMPFields<Ortho_Cartesian_2D_Mesh>(Mesh);
//---------------------------------------------------------------------//
// Make a face centered field of doubles
    fcf<Ortho_Cartesian_2D_Mesh,double>** theDoubleFaceFieldArray;
    theDoubleFaceFieldArray= new fcf<Ortho_Cartesian_2D_Mesh,double>*[num_threads];
    
    int tid;
#pragma omp parallel default(shared), private(tid)
    {
        tid = omp_get_thread_num();
        
        theDoubleFaceFieldArray[tid] = new fcf<Ortho_Cartesian_2D_Mesh,double>(Mesh);

    }


    size_t faceID;
#pragma omp parallel default(shared), private(tid,faceID)
    {
        tid = omp_get_thread_num();        
        
        for( faceID = Mesh.first_real_face(); faceID != Mesh.last_real_face(); faceID++ )
        {
            (*theDoubleFaceFieldArray[tid])[faceID] = static_cast<double>(tid+10);
        }// end of loop over mesh faces

        (*theDoubleFaceFieldArray[tid]).threadSum();
        
        printf("mpi rank: %d, thread: %d, (*theDoubleFaceFieldArray[%d])[1]=%f\n",procID,tid,tid,(*theDoubleFaceFieldArray[tid])[1]);

        (*theDoubleFaceFieldArray[tid]).replicaSum();
        
        
    }//end of parallel block
    
    
#pragma omp parallel default(shared), private(tid,faceID)
    {
        tid = omp_get_thread_num();        
        
        delete theDoubleFaceFieldArray[tid];
    }
    delete[] theDoubleFaceFieldArray;
    
#ifdef USE_MPI
    MPI_Finalize();
#endif

    return 0;
}

template<typename mesh_type>
void touchOMPFields(const mesh_type &mesh)
{
    //
    // by creating one of each type of explicitly instantiated 
    // zcf and fcf (see Meshes/fcfInst.cc and Meshes/zcfInst.cc)
    // we force the creation of the static buffer data
    //

    fcf<mesh_type,double> double_fcf(mesh);

    //
    // put an OpenMP barrier here to make sure that all threads
    // have these called before they may be needed
    //
#pragma omp parallel
    {
#pragma omp barrier
    }
    
}

void parseArgs(int argc, char *argv[], 
               int &nMpiTasksX, int &nMpiTasksY,
               unsigned int &nZonesXGlobal, unsigned int &nZonesYGlobal, 
               double &xMaxGlobal, double &yMaxGlobal,
               double &sigmaAbsorb, double &sigmaScatt, 
               double &xRoot, double &yRoot, 
               double &xInnerLo, double &yInnerLo,
               double &xInnerHi, double &yInnerHi,
               double &ratioRhoInner,
               unsigned long long &N_particles, bool &isWeakScaling)
{
    int nProcs= 1;
    int myRank = 0;
    
#ifdef USE_MPI
    MPI_Comm_size( MPI_COMM_WORLD, &nProcs);
    MPI_Comm_rank( MPI_COMM_WORLD, &myRank);
#endif

    AnyOption *opt = new AnyOption();
    
    // set preferences
//     opt->noPOSIX();  // Do not check for POSIX style character options
//     opt->setVerbose(); /* print warnings about unknown options */
//     opt->autoUsagePrint(true); /* print usage for bad options */

    // 3. SET THE USAGE/HELP  
    opt->addUsage( "" );
    opt->addUsage( "Usage: " );
    opt->addUsage( "" );
    opt->addUsage( " -h  --help           Prints this help " );
    opt->addUsage( "     --nMpiTasksX         Number of mpiTaskessors in x" );
    opt->addUsage( "     --nMpiTasksY         Number of mpiTaskessors in y" );
    opt->addUsage( "     --nZonesX        Number of zones in x (per-mpi-task for weak scaling study)" );
    opt->addUsage( "     --nZonesY        Number of zones in y (per-mpi-task for weak scaling study)" );
    opt->addUsage( "     --xDim           Upper boundary x coordinate (domain x length for weak scaling study)" );
    opt->addUsage( "     --yDim           Upper boundary y coordinate (domain y length for weak scaling study)" );
    opt->addUsage( "     --sigmaA         Absorption opacity" );
    opt->addUsage( "     --sigmaS         Scattering opacity" );
    opt->addUsage( "     --xSource        Upper x bound for source (unused in weak scaling study)" );
    opt->addUsage( "     --ySource        Upper y bound for source (unused in weak scaling study)" );
    opt->addUsage( "     --xInnerLo       Lower boundary x coordinate for low density interior" );
    opt->addUsage( "     --xInnerHi       Upper boundary x coordinate for low density interior" );
    opt->addUsage( "     --yInnerLo       Lower boundary y coordinate for low density interior" );
    opt->addUsage( "     --yInnerHi       Upper boundary y coordinate for low density interior" );
    opt->addUsage( "     --ratioRhoInner  rhoInner/rhoOouter" );
    opt->addUsage( "     --numParticles   Number of particles (per-mpi-task in weak scaling study)" );
    opt->addUsage( "     --weakScaling    Turns on options for weak scaling study" );
    opt->addUsage( "" );

    // Set the option stings/characters that can be read from command line or input file
    opt->setFlag("help",'h');  // flag takes no argument, supports both long and short form
    opt->setOption("nMpiTasksX");  // an option takes an argument, but only support long form
    opt->setOption("nMpiTasksY");  // an option takes an argument, but only support long form
    opt->setOption("nZonesX");  // an option takes an argument, but only support long form
    opt->setOption("nZonesY");  // an option takes an argument, but only support long form
    opt->setOption("xDim");  // an option takes an argument, but only support long form
    opt->setOption("yDim");  // an option takes an argument, but only support long form
    opt->setOption("sigmaA");  // an option takes an argument, but only support long form
    opt->setOption("sigmaS");  // an option takes an argument, but only support long form
    opt->setOption("xSource");  // an option takes an argument, but only support long form
    opt->setOption("ySource");  // an option takes an argument, but only support long form
    opt->setOption("xInnerLo");  // an option takes an argument, but only support long form
    opt->setOption("xInnerHi");  // an option takes an argument, but only support long form
    opt->setOption("yInnerLo");  // an option takes an argument, but only support long form
    opt->setOption("yInnerHi");  // an option takes an argument, but only support long form
    opt->setOption("ratioRhoInner");  // an option takes an argument, but only support long form
    opt->setOption("numParticles");  // an option takes an argument, but only support long form
    opt->setFlag("weakScalingStudy");  // a flag takes no argument, supports only long form

    // Process the command line and input file
    opt->processFile("./input.MCB");
    opt->processCommandArgs(argc, argv);
    
    if( ! opt->hasOptions() ) { // print usage if no options given
        opt->printUsage();
        delete opt;
        return;
    }
    
    // get the values
    if( opt->getFlag("weakScalingStudy") )
    {
        isWeakScaling = true;
    }else{
        isWeakScaling = false;
    }
    
    if( opt->getValue("nMpiTasksX") != NULL )
        nMpiTasksX = static_cast<int>(strtol(opt->getValue("nMpiTasksX"), (char**)NULL, 10));
    
    if( opt->getValue("nMpiTasksY") != NULL )
        nMpiTasksY = static_cast<int>(strtol(opt->getValue("nMpiTasksY"), (char**)NULL,10));

    if( opt->getValue("sigmaA") != NULL )
        sigmaAbsorb = strtod(opt->getValue("sigmaA"), (char**)NULL);
    
    if( opt->getValue("sigmaS") != NULL )
        sigmaScatt = strtod(opt->getValue("sigmaS"), (char**)NULL);
    

    // Get the actual Arguments after the options
    for(int i=0;i<opt->getArgc();i++)
    {
        cout<<" arg="<<opt->getArgv(i)<<endl;
    }
    
  
    if( opt->getValue("xInnerLo") != NULL )
    {
      xInnerLo =  strtod(opt->getValue("xInnerLo"), (char**)NULL);
    }
    else
    {
      xInnerLo = InnerRectOff;
    }
  
    if( opt->getValue("xInnerHi") != NULL )
    {
      xInnerHi =  strtod(opt->getValue("xInnerHi"), (char**)NULL);
    }
    else
    {
      xInnerHi = InnerRectOff;
    }
  
    if( opt->getValue("yInnerLo") != NULL )
    {
      yInnerLo =  strtod(opt->getValue("yInnerLo"), (char**)NULL);
    }
    else
    {
      yInnerLo = InnerRectOff;
    }
  
    if( opt->getValue("yInnerHi") != NULL )
    {
      yInnerHi =  strtod(opt->getValue("yInnerHi"), (char**)NULL);
    }
    else
    {
      yInnerHi = InnerRectOff;
    }
  
    if( opt->getValue("ratioRhoInner") != NULL )
    {
      ratioRhoInner = strtod(opt->getValue("ratioRhoInner"), (char**)NULL);
    }
    else
    {
      ratioRhoInner = 1.0;
    }

    if( isWeakScaling )
    {
        // Weak scaling case
        if( opt->getValue("nZonesX") != NULL )
        {
            nZonesXGlobal = nMpiTasksX * static_cast<int>(strtol(opt->getValue("nZonesX"), (char**)NULL,10));
        }
        else
        {
            nZonesXGlobal *= nMpiTasksX;
        }

        if( opt->getValue("nZonesY") != NULL )
        {
            nZonesYGlobal = nMpiTasksY * static_cast<int>(strtol(opt->getValue("nZonesY"), (char**)NULL,10));
        }
        else
        {
            nZonesYGlobal *=nMpiTasksY;
        }
        
        if( opt->getValue("xDim") != NULL )
        {
            xMaxGlobal = nMpiTasksX * strtod(opt->getValue("xDim"), (char**)NULL);
        }
        else
        {
            xMaxGlobal *= nMpiTasksX;
        }
        
        if( opt->getValue("yDim") != NULL )
        {
            yMaxGlobal = nMpiTasksY * strtod(opt->getValue("yDim"), (char**)NULL);
        }
        else
        {
            yMaxGlobal *= nMpiTasksY;
        }
        
        
        // ignore xSource, ySource for weakScalingStudy, force xRoot = xMaxGlobal
        // and yRoot = yMaxGlobal
        xRoot = xMaxGlobal;
        yRoot = yMaxGlobal;

        if(myRank==0)
            cout<<" N_particles = "<<N_particles<<endl;
        
        if( opt->getValue("numParticles") != NULL )
        {
            N_particles = static_cast<long long>(nProcs) * strtol(opt->getValue("numParticles"), (char**)NULL,10);
        }
        else
        {
            N_particles *= nProcs;
        }
        if( myRank==0 )
            cout<<" N_particles = "<<N_particles<<endl;
        
        
    }
    else
    {
        // Strong scaling case
        if( opt->getValue("nZonesX") != NULL )
            nZonesXGlobal = static_cast<int>(strtol(opt->getValue("nZonesX"), (char**)NULL,10));

        if( opt->getValue("nZonesY") != NULL )
            nZonesYGlobal = static_cast<int>(strtol(opt->getValue("nZonesY"), (char**)NULL,10));

        if( opt->getValue("xDim") != NULL )
            xMaxGlobal = strtod(opt->getValue("xDim"), (char**)NULL);

        if( opt->getValue("yDim") != NULL )
            yMaxGlobal = strtod(opt->getValue("yDim"), (char**)NULL);

        if( opt->getValue("xSource") != NULL )
            xRoot = strtod(opt->getValue("xSource"), (char**)NULL);

        if( opt->getValue("ySource") != NULL )
            yRoot = strtod(opt->getValue("ySource"), (char**)NULL);

        if( opt->getValue("numParticles") != NULL )
            N_particles = strtol(opt->getValue("numParticles"), (char**)NULL,10);
        
    }
    
    delete opt;
    

}

