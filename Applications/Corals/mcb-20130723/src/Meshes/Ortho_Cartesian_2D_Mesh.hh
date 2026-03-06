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

#ifndef __Ortho_Cartesian_2D_Mesh_hh__
#define __Ortho_Cartesian_2D_Mesh_hh__

#include <cmath>
#include <vector>
#include <map>
#include <deque>
#include <iostream>

#ifdef USE_MPI
#include <mpi.h>
#endif

#include "rng.hh"
#include "zcf.hh"
#include "ASSERT.hh"
#include "Vector3d.hh"

namespace IMC_namespace
{

class Ortho_Cartesian_2D_Mesh
{
  public:
    Ortho_Cartesian_2D_Mesh( unsigned int N_x_zones, double x_min, double x_max,
                             unsigned int N_y_zones, double y_min, double y_max,
                             unsigned int nprocs_x_in = 1,
                             unsigned int nprocs_y_in = 1,
                             unsigned int myid_in = 0
                           );

    Ortho_Cartesian_2D_Mesh( std::vector<double> x_in, std::vector<double> y_in,
                             unsigned int nprocs_x_in = 1,
                             unsigned int nprocs_y_in = 1,
                             unsigned int myid_in = 0 );

//    zone and face ID type classes are used because the IMC may track through a different
//    entity than the zones and facesnthat are used by the Mesh class. In the case of
//    this mesh, all types are unsigned int.
    typedef unsigned int mesh_zone_ID_type;
    typedef unsigned int particle_zone_ID_type;

    typedef unsigned int mesh_face_ID_type;
    typedef unsigned int particle_face_ID_type;

    typedef std::vector<unsigned int>::const_iterator ZoneIterator;

    //struct newDomainZoneIDStruct
    struct newDomainZoneIDType
    {
       double x;
       double y;
       //int from_proc; // used for debugging
       //int from_zone; // used for debugging
    };

    //typedef newDomainZoneIDStruct newDomainZoneIDType;

//    functions to give access to the zone centered mesh data
//    like zcf entries and volumes
    unsigned int N_zones() const { return number_of_zones; }
    ZoneIterator PhysicalZoneBegin() const { return zoneIDs.begin(); }
    ZoneIterator PhysicalZoneEnd() const { return zoneIDs.end(); }
    unsigned int first_real_zone() const { return 1; }
    unsigned int last_real_zone() const { return number_of_zones; }
    unsigned int next_real_zone( unsigned int zone ) const { return zone + 1; }
    unsigned int first_zone_type() const { return 1; }
    unsigned int last_zone_type() const { return number_of_zones; }
    unsigned int getGlobalID( unsigned int zoneID ) const { return zoneID; }
    std::vector<double> zone_position( unsigned int zoneID ) const;

//    functions to give access to face information
    unsigned int N_faces() const { return number_of_faces; }
    unsigned int first_real_face() const { return 1; }
    unsigned int last_real_face() const { return number_of_faces; }
    unsigned int next_real_face( unsigned int face ) const { return face + 1; }

    const double& x( unsigned int index ) const { return _x[index]; }
    const double& y( unsigned int index ) const { return _y[index]; }

    double x_zone( unsigned int zone ) const { return 0.5*( x(x_index(zone)) + x(x_index(zone)+1) ); }
    double y_zone( unsigned int zone ) const { return 0.5*( y(y_index(zone)) + y(y_index(zone)+1) ); }

    unsigned int x_index( unsigned int zone ) const { return (zone - 1)%N_x_zones + 1; }
    unsigned int y_index( unsigned int zone ) const { return (zone - 1)/N_x_zones + 1; }
    unsigned int zone( unsigned int xi, unsigned int yi ) const{ return xi + (yi-1)*N_x_zones; }

//    function used in input tests and Assertions
    inline bool zone_is_real( unsigned int zone ) const;
    inline bool face_is_real( unsigned int face ) const;

//    a routine that can only run on a KL mesh might need to use these functions
    unsigned int get_N_x_zones() const { return N_x_zones; }
    unsigned int get_N_y_zones() const { return N_y_zones; }

    double mesh_zone_volume( unsigned int zone ) const { return zone_volume[zone]; }
    double particle_zone_volume( particle_zone_ID_type zone ) const { return zone_volume[zone]; }

    unsigned int N_boundary_faces() const { return number_of_boundary_faces; }

//    zone that a boundary face abuts
    inline unsigned int zone_of_face( unsigned int face ) const;

    inline double mesh_face_area( unsigned int face ) const;
    inline double particle_face_area( unsigned int face ) const;

    inline bool boundary_face( unsigned int face ) const;

//    direction of OUTWARD normal to face
    enum orientation { plus_x = 0, minus_x, plus_y, minus_y };
    inline orientation face_orientation( unsigned int face ) const;

    particle_zone_ID_type particleZoneOfPoint ( const Vector3d_namespace::Vector3d& p,
                                                bool& foundZone ) const;

    particle_zone_ID_type particleZoneOfPoint( const Vector3d_namespace::Vector3d& p,
                                               const particle_zone_ID_type& zone,
                                               bool& foundZone ) const;

    double mesh_zone_minimum_length( const mesh_zone_ID_type& zone ) const;

//    min distance from particle to zone boundary. 
//    implimentation assuming position is a Cartesian 3 std::vector
    double d_min( const Vector3d_namespace::Vector3d& X, unsigned int zone ) const;

    double d_min_to_zone_boundary( const Vector3d_namespace::Vector3d& p,
                                   const particle_zone_ID_type& zone,
                                   double minPath ) const;

//    distance to boundary crossing for particle, and what zone
//    it will be in if it crosses, or if it goes out of bounds
//    d_event and event_zone arguments are put in for consistency with the
//    d_boundary function in PolyMesh. d_event is the min of d_collision and d_census.
//    event_zone = current_zone always in this mesh

    double distanceToBoundary( const Vector3d_namespace::Vector3d& X,
                               const Vector3d_namespace::Vector3d& Omega,
                               unsigned int zone,
                               unsigned int& crossing_face,
                               unsigned int& next_zone,
                               bool& exits_problem,
                               bool& particle_trapped ) const;

//    give cell and random number generator, give a location (x,y,z) for
//    a Monte Carlo particle in the zone.
//    This version, used by the version above,
//    does not weight particle location with slope of T^4
    void randomZoneLocation( rng& rand, mesh_zone_ID_type zone, 
                             Vector3d_namespace::Vector3d& X, 
                             particle_zone_ID_type& particle_zone ) const;

//    give face and random number generator, produces a location (x,y,z) for
//    a Monte Carlo particle, and tells what zone it's in.
    void randomFaceLocation( rng& rand, mesh_face_ID_type boundary_face, 
                             Vector3d_namespace::Vector3d& X, 
                             particle_face_ID_type& particle_face) const;                          

//    return outward normal to a face; used to calculate the angles of
//    particles born on boundaries
//    for this mesh, faces are planes, so x, y, and z are ignorable
    Vector3d_namespace::Vector3d face_normal( unsigned int face, 
                                              const Vector3d_namespace::Vector3d& X) const;

// A consistent normal, no matter which side of the face you're on.  Used for flux tallies.
    Vector3d_namespace::Vector3d natural_face_normal( unsigned int face, 
                                                      const Vector3d_namespace::Vector3d& X) const;

//    function that returns list of the zone_types given a zone index
//    for this Mesh, particle_zone_ID_type = zone
    std::vector<particle_zone_ID_type> particle_zone_IDs( mesh_zone_ID_type zone ) const;

//    function that returns list of the face_types given a face index
//    for this Mesh, particle_face_ID_type = face
    std::vector<particle_face_ID_type> particle_face_IDs( mesh_face_ID_type face ) const;

//    Functions used in debugging, esp. in advance_particle, when particle takes too many steps
    double print_distanceToBoundary( const Vector3d_namespace::Vector3d& X,
                                     const Vector3d_namespace::Vector3d& Omega,
                                     unsigned int current_zone,
                                     unsigned int& crossing_face,
                                     unsigned int& next_zone,
                                     bool& exits_problem,
                                     bool& particle_trapped ) const;

    void derivative( const zcf<Ortho_Cartesian_2D_Mesh, double>& field,
                     zcf<Ortho_Cartesian_2D_Mesh, double>& df_dx,
                     zcf<Ortho_Cartesian_2D_Mesh, double>& df_dy,
                     zcf<Ortho_Cartesian_2D_Mesh, double>& df_dz ) const;

    double getTrackingTolerance() const { return 1.0e-9; }

//    operator << defined for debugging purposes
    friend std::ostream& operator <<( std::ostream& os,
                                      const Ortho_Cartesian_2D_Mesh& Mesh );

    std::vector<mesh_face_ID_type> getBorderFacesFromDomain( unsigned int neighborDomain ) const;
    std::vector<int> getNeighborMPITasks() const;
    std::vector<int> getNeighborMPITasks( int neighborDomain ) const;
    std::vector<int> getNeighborMasterMPITasks() const;
    std::vector<int> getNeighborDomains() const;
    int getDomainID() const { return myid; }
    std::vector<int> getProcessIDsFromDomain( int domainID ) const;
    int getDomainGroupRank() const { return 0; }
#ifdef USE_MPI
    const MPI_Comm& getDomainGroupCommunicator() const;
    const MPI_Comm& getDomainMastersCommunicator() const;
    const MPI_Comm& getWorldCommunicator() const;
    int getRank() const;
    int getNumberOfProcs() const { return nprocs; }
#endif
    bool isDomainMaster() const { return true; }
    int getNumberOfDomains() const { return nprocs; }
    void setupMPI();

    bool pointIsInParticleZone( Vector3d_namespace::Vector3d& X, 
                                unsigned int zone ) const;

    void setNewDomainZoneInfo( const particle_zone_ID_type& current_particle_zone,
                               const particle_zone_ID_type& next_zone,
                               const particle_face_ID_type& face,
                               Vector3d_namespace::Vector3d& X, 
                               newDomainZoneIDType& newDomainZoneInfo) const;
       
    void correctNewDomainZone( const newDomainZoneIDType& newDomainZoneInfo,
                               Vector3d_namespace::Vector3d& X, 
                               particle_zone_ID_type& current_particle_zone ) const;


  private:
//    set face info the same way in each constructor
    void set_face_info();

//    called by public versions of randomZoneLocation function
    void randomZoneLocation( double r1, double r2, double r3, unsigned int zone,
                             Vector3d_namespace::Vector3d& X ) const;

//    vector holding unsigned int IDs of zones in the mesh - used in
//    iterating over zones
    std::vector<unsigned int> zoneIDs;

    const unsigned int N_x_zones, N_y_zones;   //   number of real zones in x and y directions
    const unsigned int number_of_zones;    // number of real zones in mesh = N_x_zones*N_y_zones

    std::vector<double> _x;      //    x and y coordinates of faces for 0:N_x_zones + 1
    std::vector<double> _y;      //    real and ghost mesh points

    std::vector<double> zone_volume;    //    volume of each zone

//    variables and arrays holding face data
    const unsigned int N_x_faces, N_y_faces;    //    faces with normals in x and y respectively
    const unsigned int number_of_faces;
    const unsigned int number_of_boundary_faces;

    std::deque<bool> boundary_face_data;

    std::vector<unsigned int> plus_zone;    //    index of zone with higher index abutted by face
    std::vector<unsigned int> minus_zone;    //    index of zone with lower index abutted by face
//    index of zone a boundary face abuts
    std::vector<unsigned int> boundary_face_zone_list;

//    area of faces
    std::vector<double> face_area_list;
//    direction that faces outward normal points
    std::vector<orientation> face_orientation_list;

//    don't want these called, so make them private
    Ortho_Cartesian_2D_Mesh();
    Ortho_Cartesian_2D_Mesh( const Ortho_Cartesian_2D_Mesh& );
    Ortho_Cartesian_2D_Mesh& operator=( const Ortho_Cartesian_2D_Mesh& );

    // MPI Information  (MPI likes ints, not unsigned ints.)
    const int nprocs_x;
    const int nprocs_y;
    const int nprocs;
    const int myid;
#ifdef USE_MPI
    MPI_Comm master_comm;
    MPI_Comm replica_comm;
#endif
    enum Direction { up=0, down, left, right };

    std::vector<int> DD_neighbors;
    std::vector<int> DD_direction;
    std::vector< std::vector<mesh_face_ID_type> > DD_border_faces;

    void setupDDParallel();

};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

inline std::vector<double> Ortho_Cartesian_2D_Mesh::
zone_position( unsigned int zoneID ) const
{
    ASSERT( zoneID >= first_real_zone() );
    ASSERT( zoneID <= last_real_zone() );

    std::vector<double> xy_zone(2);
    
    xy_zone[0] = x_zone( zoneID );
    xy_zone[1] = y_zone( zoneID );
    
    return xy_zone;
}

//---------------------------------------------------------------------------//

inline bool Ortho_Cartesian_2D_Mesh::
zone_is_real( unsigned int zone ) const
{
    if( zone < first_real_zone() )
        return false;

    if( zone > last_real_zone() )
        return false;

//    if you get here, zone is in real range, so return true
    return true;
}

//---------------------------------------------------------------------------//

inline bool Ortho_Cartesian_2D_Mesh::
face_is_real( unsigned int face ) const
{
    if( face < first_real_face() )
        return false;

    if( face > last_real_face() )
        return false;

//    if you get here, face is in real range, so return true
    return true;
}

//---------------------------------------------------------------------------//

//    zone that a face abuts

inline unsigned int Ortho_Cartesian_2D_Mesh::
zone_of_face( unsigned int face ) const
{
    ASSERT( boundary_face_data[face] );

    return boundary_face_zone_list[face];
}

//---------------------------------------------------------------------------//

inline double Ortho_Cartesian_2D_Mesh::
mesh_face_area( unsigned int face ) const
{
    return face_area_list[face];
}

//---------------------------------------------------------------------------//

inline double Ortho_Cartesian_2D_Mesh::
particle_face_area( unsigned int face ) const
{
    return face_area_list[face];
}

//---------------------------------------------------------------------------//

inline  Ortho_Cartesian_2D_Mesh::orientation
Ortho_Cartesian_2D_Mesh::face_orientation( unsigned int face ) const
{
//    We'll let this return data for non coundary faces also
//    ASSERT( boundary_face_data[b_face] );

    return face_orientation_list[face];
}

//---------------------------------------------------------------------------//

inline bool Ortho_Cartesian_2D_Mesh::
boundary_face( unsigned int face ) const
{
    return boundary_face_data[face];
}

//---------------------------------------------------------------------------//

inline std::vector<int> Ortho_Cartesian_2D_Mesh::
getNeighborMPITasks() const
{
   return DD_neighbors;
}

//---------------------------------------------------------------------------//
inline double
Ortho_Cartesian_2D_Mesh::
mesh_zone_minimum_length( const mesh_zone_ID_type& zone ) const
{
    double xm = x(x_index(zone));
    double xp = x(x_index(zone)+1);
    double ym = y(y_index(zone));
    double yp = y(y_index(zone)+1);

    return std::min(xp-xm, yp-ym);
}

//---------------------------------------------------------------------------//
inline bool
Ortho_Cartesian_2D_Mesh::
pointIsInParticleZone( Vector3d_namespace::Vector3d& X, 
                                unsigned int zone ) const
{
   ASSERT( zone_is_real( zone ) );

   const double xm = x(x_index(zone));
   const double xp = x(x_index(zone)+1);
   const double ym = y(y_index(zone));
   const double yp = y(y_index(zone)+1);

   if( X.GetX() < xm || X.GetX() > xp )
   {
      return false;
   }

   if( X.GetY() < ym || X.GetY() > yp )
   {
      return false;
   }

   return true;
}
//---------------------------------------------------------------------------//

inline double
Ortho_Cartesian_2D_Mesh::
d_min_to_zone_boundary( const Vector3d_namespace::Vector3d& p,
                        const particle_zone_ID_type& zone,
                        double minPath ) const
{
    return d_min(p, zone);
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif

