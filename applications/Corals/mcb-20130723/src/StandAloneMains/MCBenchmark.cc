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
#include <ctime>

#ifdef USE_MPI
#include "mpi.h"
#endif

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
#include "IMC_Domain_organizer.hh"
#endif

#ifdef USE_OPENMP
#include <omp.h>
#endif


using namespace std;
using namespace IMC_namespace;

template<typename mesh_type>
void touchOMPFields( const mesh_type &inMesh );

void parseArgs(int argc, char *argv[], int &num_threads,
               int &nMpiTasksX, int &nMpiTasksY, int &nCores, 
               unsigned int &nZonesXGlobal, unsigned int &nZonesYGlobal, 
               double &xMaxGlobal, double &yMaxGlobal,
               double &sigmaAbsorb, double &sigmaScatt, 
               double &xRoot, double &yRoot, bool &isDistribSource,
               bool &isMultiOpacity,
               double &xInnerLo, double &yInnerLo,
               double &xInnerHi, double &yInnerHi,
               double &ratioSigmaInner,
               unsigned long long &N_particles, 
               bool &isWeakScaling, bool &isMirrorBoundary,
               unsigned int &numCycles);

// 
// MPI + thread mode may be one of the following.
// We need MPI_THREAD_FUNNELED for the current
// implementation to function properly.
//
// MPI_THREAD_SINGLE     = 0
// MPI_THREAD_FUNNELED   = 1
// MPI_THREAD_SERIALIZED = 2
// MPI_THREAD_MULTIPLE   = 3

int main(int argc, char* argv[])
{
  //---------------------------------------------------------------------//
  // Start MPI

  int mpiTaskID = 0;
  int nMpiTasks = 1;


#ifdef USE_MPI

#ifdef USE_OPENMP
  int required, provided;
  // set this to desired value 
  required = MPI_THREAD_FUNNELED;
  // set this to minimum possible value
  provided = MPI_THREAD_SINGLE;

  MPI_Init_thread(&argc, &argv, required, &provided );
  MPI_Comm_size( MPI_COMM_WORLD, &nMpiTasks );
  MPI_Comm_rank( MPI_COMM_WORLD, &mpiTaskID );

  if (mpiTaskID == 0) 
    printf ("MPI_threaded provided  = %d \n", provided );
#else

  MPI_Init( &argc, &argv );
  MPI_Comm_size( MPI_COMM_WORLD, &nMpiTasks );
  MPI_Comm_rank( MPI_COMM_WORLD, &mpiTaskID );
#endif // USE_OPENMP

#endif // USE_MPI

  int num_threads = 1;
  
#ifdef USE_OPENMP
  num_threads = omp_get_max_threads();
  if (mpiTaskID == 0) 
    cout<<"max num omp threads= "<<num_threads<<endl;
#endif

  if (mpiTaskID == 0) {
    printf ("\nMCB version 1.0.3, July 22, 2013.\n\n" );
    cout << "nMpiTasks = " << nMpiTasks << endl;
  }
  //---------------------------------------------------------------------//
  // Set maximum time steps.
  unsigned int maxTimesteps = 1000;


  //---------------------------------------------------------------------//
  // create a mesh
  unsigned int nZonesXGlobal = 100;
  unsigned int nZonesYGlobal = 100;
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

  double xInnerLo = 0.1;
  double yInnerLo = 0.1;
  double xInnerHi = 0.9;
  double yInnerHi = 0.9;
  double ratioSigmaInner = 0.05;
  
  int nMpiTasksX = 1; 
  int nMpiTasksY = 1; 
  
  int nCores = 1; 

  bool isWeakScaling=false;
  bool isMirrorBoundary=false;
  bool isDistribSource=false;
  bool isMultiOpacity=false;
    
  parseArgs(argc, argv, num_threads,
            nMpiTasksX, nMpiTasksY, nCores,
            nZonesXGlobal, nZonesYGlobal, 
            xMaxGlobal, yMaxGlobal,
            sigmaAbsorb, sigmaScatt, 
            xRoot, yRoot, isDistribSource, isMultiOpacity,
            xInnerLo, yInnerLo, xInnerHi, yInnerHi,
            ratioSigmaInner,
            N_particles, isWeakScaling, isMirrorBoundary, maxTimesteps);

  if( mpiTaskID == 0)
    cout<<"isWeakScaling="<<(isWeakScaling==1?"True":"False")<<endl;
  if(num_threads > 0) omp_set_num_threads(num_threads);
  if (mpiTaskID == 0) {
    int omp_maxthreads;
    omp_maxthreads= omp_get_max_threads();
    cout<<"number of threads used = "<<num_threads<<endl;
  }

#define OUTPUT_ARGS
#ifdef OUTPUT_ARGS
  if( mpiTaskID == 0) {
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
    cout<<"    ratioSigmaInner = "<<ratioSigmaInner<<endl;
    cout<<"    isDistribSource= "<<isDistribSource<<endl;
    cout<<"    isWeakScaling= "<<isWeakScaling<<endl;
    cout<<"    isMirrorBoundary= "<<isMirrorBoundary<<endl;
    cout<<"    isMultiOpacity= "<<isMultiOpacity<<endl;
  }
#endif

  //---------------------------------------------------------------------//
  // partition zones across mpiTasks

  if( nMpiTasksX * nMpiTasksY != nMpiTasks)
  {
    if( mpiTaskID == 0 )
    {
      cout << "ERROR: Must have nMpiTasksX * nMpiTasksY = nMpiTasks." << '\n';
      cout << "nMpiTasksX = " << nMpiTasksX << endl;
      cout << "nMpiTasksY = " << nMpiTasksY << endl;
      cout << "nMpiTasksX*nMpiTasksY = " << nMpiTasksX*nMpiTasksY << endl;
      cout << "nMpiTasks = " << nMpiTasks << endl;

    }
#ifdef USE_MPI
    MPI_Abort( MPI_COMM_WORLD, -1 );
#else
    throw ("Must have nMpiTasksX * nMpiTasksY = nMpiTasks.");
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
                nMpiTasksX, nMpiTasksY, mpiTaskID,
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
                                mpiTaskID );
    
#ifdef USE_OPENMP
  touchOMPFields<Ortho_Cartesian_2D_Mesh>(Mesh);
#endif

//---------------------------------------------------------------------//
// Make pieces
  vector<unsigned int> InnerPieceZoneList;
  vector<unsigned int> OuterPieceZoneList;

  for( unsigned int zone = Mesh.first_real_zone();
       zone <= Mesh.last_real_zone();
       zone = Mesh.next_real_zone(zone) )
  {
    vector<double> position = Mesh.zone_position( zone );
    double x = position[0];
    double y = position[1];

    // Use a different opacity inside the inner box 
    // IF multi-opacity is enabled
    if( isMultiOpacity && 
        x >= xInnerLo && x <= xInnerHi && 
        y >= yInnerLo && y <= yInnerHi )
    {
        InnerPieceZoneList.push_back(zone);
    }
    else
    {
        OuterPieceZoneList.push_back(zone);
    }
  }

//---------------------------------------------------------------------//
  piece InnerPiece( InnerPieceZoneList );
  piece OuterPiece( OuterPieceZoneList );

//    list of pieces
    vector<piece> pieceList;
    pieceList.push_back( InnerPieceZoneList );
    pieceList.push_back( OuterPieceZoneList );

//---------------------------------------------------------------------//
// Make data bases
  typedef Material_data_base<Ortho_Cartesian_2D_Mesh> MatDBType;
  MatDBType MaterialDB( Mesh, pieceList );
    
  constant_absorp_opac InnerAbsorptionOpacity( ratioSigmaInner*sigmaAbsorb );
  constant_absorp_opac OuterAbsorptionOpacity( sigmaAbsorb );
  isotropic_scattering InnerScatteringOpacity( ratioSigmaInner*sigmaScatt );
  isotropic_scattering OuterScatteringOpacity( sigmaScatt );

  vector<absorption_opacity_base*> analyticAbsorptionOpacities;
  analyticAbsorptionOpacities.push_back( &InnerAbsorptionOpacity );
  analyticAbsorptionOpacities.push_back( &OuterAbsorptionOpacity );

  vector<scattering_opacity_base*> analyticScatteringOpacities;
  analyticScatteringOpacities.push_back( &InnerScatteringOpacity );
  analyticScatteringOpacities.push_back( &OuterScatteringOpacity );

  typedef Analytic_opacity_data_base<mesh_types, mat_types> AnalyticODBType;
  AnalyticODBType AnalyticODB( analyticAbsorptionOpacities,
                               analyticScatteringOpacities,
                               Mesh,
                               MaterialDB );

//---------------------------------------------------------------------//
//    Boundary conditions
//    reflecting BC at x = 0 and on both y boundaries
//    open at x = xMax
//  
//    If we're running a weak scaling study, we set all boundaries
//    to reflecting.

  std::vector<unsigned int> BCFaceList;    
  for( unsigned int face = Mesh.first_real_face(); 
       face <= Mesh.last_real_face();
       face = Mesh.next_real_face(face) )
  {
    unsigned int faceOrientation = Mesh.face_orientation( face );

    if( Mesh.boundary_face(face) )
    {
 
      if( isWeakScaling == false &&
          faceOrientation == Ortho_Cartesian_2D_Mesh:: plus_x &&
          !isMirrorBoundary )
      {
        continue;
      }
            
      BCFaceList.push_back( face );
    }
  }
    
  typedef mesh_types::Mesh_type Mesh_type;
  typedef mesh_types::particle_zone_ID_type particle_zone_ID_type;
  typedef mesh_types::Vector3d Vector3d;
  typedef photon<particle_zone_ID_type, Vector3d> photon_type;

  reflecting_BC<Mesh_type,photon_type> reflection( Mesh, BCFaceList );
    
//    std::vector of pointers to BC object

  std::vector< std::vector<Boundary_Condition<Mesh_type,photon_type>*> > userBCs(num_threads);
    
  int tid=0;
#pragma omp parallel default(shared) private(tid)
  {
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif // USE_OPENMP

    userBCs[tid].push_back( &reflection );
  }//end of openmp parallel block

//---------------------------------------------------------------------//
//    Sources
//    add isotropic_source 
  std::vector<unsigned int> isoSourceZoneList;
  for( unsigned int zone = Mesh.first_real_zone(); 
       zone <= Mesh.last_real_zone();
       zone = Mesh.next_real_zone(zone) )
  {
    isoSourceZoneList.push_back( zone );
  }
    
  typedef isotropic_source<mesh_types, mat_types> isoSourceType;
  typedef photon_source<mesh_types, mat_types> photonSourceType;
    
//    source is sourceStrength * Sx(x) * Sy(y) * T(t) with
//    Sx(x) = (x-xRoot)**3 * (x+xRoot)**3
//    Sy(y) = (y-yRoot)**3 * (y+yRoot)**3
//    T(t) = 0.5 * [1.0 + sin( omega * t + phi )]
//    This holds for x < xRoot, y < yRoot

  double omega = 1.0;
  double phi = 0.5*3.14159265359;

    
  typedef Source_data_base<mesh_types, mat_types> SourceDBType;

  SourceDBType **SourceDBArray;

  SourceDBArray = new SourceDBType*[num_threads];

  vector< vector<photon_source<mesh_types, mat_types>*> > sourceListVector(num_threads);

#pragma omp parallel default(shared) private(tid)
  {
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif // USE_OPENMP

    isoSourceType* pIsoSource = new isoSourceType( sourceStrength,
                                                   xRoot, yRoot,
                                                   omega, phi,
                                                   isDistribSource,
                                                   isoSourceZoneList,
                                                   Mesh, 
                                                   MaterialDB,
                                                   AnalyticODB );
          
    sourceListVector[tid].push_back(pIsoSource);

    SourceDBArray[tid] = new SourceDBType( Mesh,
                                           MaterialDB,
                                           AnalyticODB,
                                           sourceListVector[tid] );
          
  }//end of openmp parallel block

//---------------------------------------------------------------------//
// Set up domain decomposition

#ifdef USE_MPI
  unsigned int buffer_size = 5000;
  typedef IMC_Domain_organizer<mesh_types, mat_types, SourceDBType> IMC_Domain_organizer_type;
    
  IMC_Domain_organizer_type **domain_org = new IMC_Domain_organizer_type*[num_threads];

#pragma omp parallel default(shared) private(tid)
  {
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif
    domain_org[tid] = new IMC_Domain_organizer_type ( Mesh, buffer_size );

    // Only set up if we have more than one processor.
    if( nMpiTasks > 1)
    {
      domain_org[tid]->setUpDomainDecomposition( *SourceDBArray[tid], userBCs[tid] );
    }
  }//end of omp parallel 
#endif
//---------------------------------------------------------------------//
//    make object that computes analytic answer

//    number of terms in Fourier series of analytic answer
  unsigned int nXTerms = 20;
  unsigned int nYTerms = 20;

//    diffusion coefficient
  double D = 1.0/( 3.0*(sigmaScatt + sigmaAbsorb) );

  MCAnswer analyticAnswer( nXTerms, nYTerms,
                           D, sigmaAbsorb,
                           sourceStrength,
                           xRoot, yRoot,
                           omega, phi,
                           isWeakScaling,
                           xMaxGlobal, yMaxGlobal );

//---------------------------------------------------------------------//
// Set other info needed for MC object
  double initial_time = 0.0;
  ostringstream file_name_header;
  file_name_header << "MCBenchmark_" << mpiTaskID << "_" << nMpiTasks;
  double RR_threshold = 0.01;
    
  unsigned int message_check_period = 3000;

  typedef IMC<mesh_types, mat_types> MCType;
  MCType **MonteCarloArray;
  MonteCarloArray = new MCType*[num_threads];

#pragma omp parallel default(shared) private(tid)
  {

#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif // USE_OPENMP

    MonteCarloArray[tid] = new MCType( Mesh,
                                       MaterialDB,
                                       AnalyticODB,
                                       userBCs[tid],
                                       *(SourceDBArray[tid]),
                                       analyticAnswer,
                                       N_particles,
                                       initial_time,
                                       file_name_header.str(),
                                       RR_threshold,
                                       message_check_period );
  }//end of openmp parallel block
      
  //---------------------------------------------------------------------//
  // time step information
  double tEnd = 1.0;
  double dt = 1.0e-1;

  if(mpiTaskID == 0)
  {
    cout << "tEnd = " << tEnd << endl;
    cout << "dt = " << dt << endl;
  }

  //---------------------------------------------------------------------//
  // Do the time steps
  unsigned int iteration = 0;
  double t = 0.0;
  double startTime=0.0, stopTime=0.0, advanceTime=0.0;

  do
  {
    AnalyticODB.setUp( dt );
    MaterialDB.setUp( );

#ifdef USE_MPI    
    startTime = MPI_Wtime();
#else
    startTime = static_cast<double>(clock())/CLOCKS_PER_SEC;
#endif


#pragma omp parallel default(shared) private(tid)
    {
#ifdef USE_OPENMP
      tid = omp_get_thread_num();
#endif
          
      MonteCarloArray[tid]->advance( dt );
    }// end of omp parallel block


 
#ifdef USE_MPI    
    stopTime = MPI_Wtime();
#else
    stopTime = static_cast<double>(clock())/CLOCKS_PER_SEC;
#endif

    advanceTime += stopTime - startTime;
      
    t = t + dt;
    ++iteration;

    const unsigned int interval = 1;
    if( iteration % interval == 0 && mpiTaskID == 0)
    {
      cout << "cycle = " << iteration << " t = " << t << '\n';
      cout << '\n';

    }

  } while( t < (tEnd-dt/2.0) && iteration < maxTimesteps );

//     MonteCarlo.spatialOutput();
    
  unsigned long long localTracks,globalTracks=0;
    
  //
  // add up thread local localTracks into globalTracks which are then
  // MPI_Reduced to task 0
  //
#pragma omp parallel default(shared) private(tid,localTracks)
  {
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif      
    MonteCarloArray[tid]->threadedDiagnosticOutput();

    localTracks=0;
          
    for( unsigned int zone = Mesh.first_real_zone(); 
         zone <= Mesh.last_real_zone();
         zone = Mesh.next_real_zone(zone) )
    {
      localTracks += MonteCarloArray[tid]->nPaths(zone);
    }
          
#pragma omp critical (globalTracks)
    {
      globalTracks += localTracks;
    }//end of critical section
  }//end of parallel section
      
  unsigned long long globalTracksSum = 0;
      
#ifdef USE_MPI
  int result = MPI_Reduce(&globalTracks, &globalTracksSum, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD);
#else
  globalTracksSum = localTracks;
#endif


  //
  // compute max error from analytic solution
  //
  double maxError = MonteCarloArray[0]->computeMaxDeviationFromAnalytic();
    
  if(mpiTaskID == 0)
  {
    cout << '\n';
    cout << "MC advance time ="<<advanceTime<<endl;
    cout <<" MC max error ="<<maxError<<endl;
    cout <<" globalTracks="<<globalTracksSum<<endl;
    cout <<" FOM ="<<globalTracksSum/advanceTime<<" tracks/second"<<endl;
    cout <<" performance per core ="<<globalTracksSum/advanceTime/(nCores*nMpiTasks)<<" tracks/second/core"<<endl;
    cout << "done!" << endl;
  }


  //
  // clean up dynamically allocated memory
  //
#pragma omp parallel default(shared) private(tid,localTracks)
  {
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif
        
    delete MonteCarloArray[tid];
    delete SourceDBArray[tid];
    delete domain_org[tid];
    for(vector<photon_source<mesh_types, mat_types>*>::iterator it = sourceListVector[tid].begin();
        it !=  sourceListVector[tid].end();++it)
    {
      delete *it;
    }
  }//end of openmp block

  delete[] domain_org;
  delete[] MonteCarloArray;
  delete[] SourceDBArray;
    
#ifdef USE_MPI
  MPI_Finalize();
#endif

}

template<typename mesh_type>
void touchOMPFields(const mesh_type &mesh)
{
  //
  // by creating one of each type of explicitly instantiated 
  // zcf and fcf (see Meshes/fcfInst.cc and Meshes/zcfInst.cc)
  // we force the creation of the static buffer data
  //
  typedef mesh_types::particle_zone_ID_type particle_zone_ID_type;
  typedef mesh_types::Vector3d Vector3d;
  typedef photon<particle_zone_ID_type, Vector3d> photon_type;

  fcf<mesh_type,double> double_fcf(mesh);
  zcf<mesh_type,double> double_zcf(mesh);
  zcf<mesh_type,mesh_types::Vector3d> vector_zcf(mesh);
  zcf<mesh_type,unsigned long long> ull_zcf(mesh);
  zcf<mesh_type,long long> ll_zcf(mesh);
  zcf<mesh_type,unsigned int> u_zcf(mesh);

  //
  // put an OpenMP barrier here to make sure that all threads
  // have these called before they may be needed
  //
#pragma omp parallel
  {
#pragma omp barrier
  }
    
}

int splitInt(int nMpiTasks)
{
  int rt, nx, ratio;

  /* This function splits the MPI tasks into the most nearly
     square decomposition.
     Find the largest integer smaller than the square root
     of the number of MPI processes which is a factor of
     the number of MPI processes.
  */
  rt= sqrt(nMpiTasks);
  for(nx= rt; nx >= 1; nx--) {
    ratio= nMpiTasks/nx;
    if(ratio*nx == nMpiTasks) break;
  }
  return nx;
}


void parseArgs(int argc, char *argv[], int &num_threads,
               int &nMpiTasksX, int &nMpiTasksY, int &nCores,
               unsigned int &nZonesXGlobal, unsigned int &nZonesYGlobal, 
               double &xMaxGlobal, double &yMaxGlobal,
               double &sigmaAbsorb, double &sigmaScatt, 
               double &xRoot, double &yRoot, bool &isDistribSource,
               bool &isMultiOpacity,
               double &xInnerLo, double &yInnerLo,
               double &xInnerHi, double &yInnerHi,
               double &ratioSigmaInner,
               unsigned long long &N_particles, 
               bool &isWeakScaling, bool &isMirrorBoundary,
               unsigned int &numCycles)
{
  int nMpiTasks= 1;
  int myRank = 0;
  int nCoresY= 1;
  int nCoresX= 1;
  int nThreadCore= 1;
  double xDimBase= 16.0;
  double yDimBase= 16.0;
  
#ifdef USE_MPI
  MPI_Comm_size( MPI_COMM_WORLD, &nMpiTasks);
  MPI_Comm_rank( MPI_COMM_WORLD, &myRank);
#endif

  AnyOption *opt = new AnyOption();
    
  // set preferences
//     opt->noPOSIX);  // Do not check for POSIX style character options
    opt->setVerbose(); /* print warnings about unknown options */
    opt->autoUsagePrint(true); /* print usage for bad options */

  // 3. SET THE USAGE/HELP  
  opt->addUsage( "" );
  opt->addUsage( "Usage: " );
  opt->addUsage( "" );
  opt->addUsage( " -h  --help           Prints this help " );
  opt->addUsage( "     --nMpiTasksX     Number of mpiTasks in x" );
  opt->addUsage( "     --nMpiTasksY     Number of mpiTasks in y" );
  opt->addUsage( "     --nCores         Number of cores per domain" );
  opt->addUsage( "     --nThreadCore    Number of threads per core" );
  opt->addUsage( "     --nZonesX        Number of zones in x (per-mpi-task for weak scaling study)" );
  opt->addUsage( "     --nZonesY        Number of zones in y (per-mpi-task for weak scaling study)" );
  opt->addUsage( "     --xDim           Upper boundary x coordinate (domain x length for weak scaling study)" );
  opt->addUsage( "     --yDim           Upper boundary y coordinate (domain y length for weak scaling study)" );
  opt->addUsage( "     --sigmaA         Absorption opacity" );
  opt->addUsage( "     --sigmaS         Scattering opacity" );
  opt->addUsage( "     --xSource        Upper x bound for source (unused in weak scaling study)" );
  opt->addUsage( "     --ySource        Upper y bound for source (unused in weak scaling study)" );
  opt->addUsage( "     --distributedSource     Source will be distributed throughout the problem" );
  opt->addUsage( "     --multiSigma   Turns on different densities in the center and periphery" );
  opt->addUsage( "     --xInnerLo       Lower boundary x coordinate fraction for low density interior" );
  opt->addUsage( "     --xInnerHi       Upper boundary x coordinate fraction for low density interior" );
  opt->addUsage( "     --yInnerLo       Lower boundary y coordinate fraction for low density interior" );
  opt->addUsage( "     --yInnerHi       Upper boundary y coordinate fraction for low density interior" );
  opt->addUsage( "     --ratioSigmaInner  sigmaInner/sigmaOuter" );
  opt->addUsage( "     --numParticles   Number of particles (per-mpi-task in weak scaling study)" );
  opt->addUsage( "     --weakScaling    Turns on options for weak scaling study" );
  opt->addUsage( "     --mirrorBoundary    Turns on mirror boundary conditions on all sides" );
  opt->addUsage( "     --numCycles      Total number of timesteps to take" );
  opt->addUsage( "" );

  // Set the option strings/characters that can be read from command line or input file
  opt->setFlag("help",'h');  // flag takes no argument, supports both long and short form
  opt->setOption("nMpiTasksX");  // an option takes an argument, but only support long form
  opt->setOption("nMpiTasksY");  // an option takes an argument, but only support long form
  opt->setOption("nCores");  // an option takes an argument, but only support long form
  opt->setOption("nThreadCore");  // an option takes an argument, but only support long form
  opt->setOption("nZonesX");  // an option takes an argument, but only support long form
  opt->setOption("nZonesY");  // an option takes an argument, but only support long form
  opt->setOption("xDim");  // an option takes an argument, but only support long form
  opt->setOption("yDim");  // an option takes an argument, but only support long form
  opt->setOption("sigmaA");  // an option takes an argument, but only support long form
  opt->setOption("sigmaS");  // an option takes an argument, but only support long form
  opt->setOption("xSource");  // an option takes an argument, but only support long form
  opt->setOption("ySource");  // an option takes an argument, but only support long form
  opt->setFlag("distributedSource");  // an option takes an argument, but only support long form
  opt->setFlag("multiSigma");  // a flag takes no argument, supports only long form
  opt->setOption("xInnerLo");  // an option takes an argument, but only support long form
  opt->setOption("xInnerHi");  // an option takes an argument, but only support long form
  opt->setOption("yInnerLo");  // an option takes an argument, but only support long form
  opt->setOption("yInnerHi");  // an option takes an argument, but only support long form
  opt->setOption("ratioSigmaInner");  // an option takes an argument, but only support long form
  opt->setOption("numParticles");  // an option takes an argument, but only support long form
  opt->setFlag("weakScaling");  // a flag takes no argument, supports only long form
  opt->setFlag("mirrorBoundary");  // a flag takes no argument, supports only long form
  opt->setFlag("numCycles");  // a flag takes no argument, supports only long form

  // Process the command line and input file
  opt->processFile("./input.MCB");
  opt->processCommandArgs(argc, argv);
    
  if( ! opt->hasOptions() ) { // print usage if no options given
    opt->printUsage();
    delete opt;
    return;
  }
    
  // get the values
  if( opt->getFlag("weakScaling") )
  {
    isWeakScaling = true;
  }else{
    isWeakScaling = false;
  }
  if( opt->getFlag("mirrorBoundary") )
  {
    isMirrorBoundary = true;
  }else{
    isMirrorBoundary = false;
  }
  
  // Set default number of MPI domains in X and Y to give
  // a nearly square simulation.
  // This will be overridden by any command line argument.
  nMpiTasksX= splitInt(nMpiTasks);
  nMpiTasksY= nMpiTasks/nMpiTasksX;
  
  if( opt->getValue("nMpiTasksX") != NULL )
    nMpiTasksX = static_cast<int>(strtol(opt->getValue("nMpiTasksX"), (char**)NULL, 10));
    
  if( opt->getValue("nMpiTasksY") != NULL )
    nMpiTasksY = static_cast<int>(strtol(opt->getValue("nMpiTasksY"), (char**)NULL,10));
    
  if( opt->getValue("nCores") != NULL )
    nCores = static_cast<int>(strtol(opt->getValue("nCores"), (char**)NULL, 10));
  /* Turn the cores per process into factors to use in extending the
     X and Y extent of the domain and the number of particles per domain. */
  nCoresY= splitInt(nCores);
  nCoresX= nCores/nCoresY;
  if( myRank==0 ) {
    cout<<"nCoresX = "<<nCoresX<<endl;
    cout<<"nCoresY = "<<nCoresY<<endl;
  }

  if( opt->getValue("nThreadCore") != NULL )
    nThreadCore = static_cast<int>(strtol(opt->getValue("nThreadCore"), (char**)NULL, 10));
  num_threads = nThreadCore * nCores;

  if( opt->getValue("sigmaA") != NULL )
    sigmaAbsorb = strtod(opt->getValue("sigmaA"), (char**)NULL);
    
  if( opt->getValue("sigmaS") != NULL )
    sigmaScatt = strtod(opt->getValue("sigmaS"), (char**)NULL);
    
  if( opt->getValue("numCycles") != NULL )
    numCycles = static_cast<unsigned int>(strtol(opt->getValue("numCycles"),(char**)NULL,10));
 
  if( opt->getFlag("distributedSource") ) {
    isDistribSource = true;
    if( myRank==0 ) cout<<"Using a distributed source"<<endl;
  } else {
    isDistribSource = false;
    if( myRank==0 ) cout<<"Using a point source"<<endl;
  }

  if( isWeakScaling )
  {
    // Weak scaling case
    if( opt->getValue("nZonesX") != NULL )
    {
      nZonesXGlobal = static_cast<int>(strtol(opt->getValue("nZonesX"), (char**)NULL,10));
    }
    nZonesXGlobal *= nCoresX * nMpiTasksX;
        
    if( opt->getValue("nZonesY") != NULL )
    {
      nZonesYGlobal = static_cast<int>(strtol(opt->getValue("nZonesY"), (char**)NULL,10));
    }
    nZonesYGlobal *= nCoresY * nMpiTasksY;
        
    if( opt->getValue("xDim") != NULL )
    {
      // At this point, xMaxGlobal is the extent of one domain
      xMaxGlobal = strtod(opt->getValue("xDim"), (char**)NULL);
    }
    else
    {
      xMaxGlobal = xDimBase;
    }
    // xMaxGlobal *= nCoresX * nMpiTasksX;
        
    if( opt->getValue("yDim") != NULL )
    {
      // At this point, yMaxGlobal is the extent of one domain
      yMaxGlobal = strtod(opt->getValue("yDim"), (char**)NULL);
    }
    else
    {
      yMaxGlobal = yDimBase;
    }
    // yMaxGlobal *= nCoresY * nMpiTasksY;
         
    // ignore xSource, ySource for weakScaling, force xRoot = xMaxGlobal
    // and yRoot = yMaxGlobal
    xRoot = xMaxGlobal/2.0;
    yRoot = yMaxGlobal/2.0;

    if( opt->getValue("numParticles") != NULL )
    {
      N_particles = static_cast<long long>(strtol(opt->getValue("numParticles"), (char**)NULL,10));
    }
    N_particles *= nCores * nMpiTasks;
    if( myRank==0 )
      cout<<"N_particles = "<<N_particles<<endl;
        
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
      N_particles = static_cast<long long>(strtol(opt->getValue("numParticles"), (char**)NULL,10));
        
  }

  // NOTE - this code is deliberately placed after processing of
  // other arguments so that default values can be computed based on
  // the problem x and y extents.
  if( opt->getFlag("multiSigma") )
  {
    isMultiOpacity = true;
    // Process the arguments that set the properties of the inner rectangle
    if( opt->getValue("xInnerLo") != NULL )
    {
      xInnerLo =  strtod(opt->getValue("xInnerLo"), (char**)NULL);
    }
    xInnerLo *= xMaxGlobal;
  
    if( opt->getValue("xInnerHi") != NULL )
    {
      xInnerHi =  strtod(opt->getValue("xInnerHi"), (char**)NULL);
    }
    xInnerHi *= xMaxGlobal;
  
    if( opt->getValue("yInnerLo") != NULL )
    {
      yInnerLo =  strtod(opt->getValue("yInnerLo"), (char**)NULL);
    }
    yInnerLo *= yMaxGlobal;
  
    if( opt->getValue("yInnerHi") != NULL )
    {
      yInnerHi =  strtod(opt->getValue("yInnerHi"), (char**)NULL);
    }
    yInnerHi *= yMaxGlobal;
  
    if( opt->getValue("ratioSigmaInner") != NULL )
    {
      ratioSigmaInner = strtod(opt->getValue("ratioSigmaInner"), (char**)NULL);
    }
  } else {
    isMultiOpacity = false;
    ratioSigmaInner = 1.0;
  }    
    
  delete opt;
    

}

