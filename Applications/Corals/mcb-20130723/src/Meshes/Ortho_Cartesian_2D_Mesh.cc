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

#include <algorithm>
#include <cmath>
#include <iostream>

#include "Ortho_Cartesian_2D_Mesh.hh"
#include "ASSERT.hh"

using namespace std;
using Vector3d_namespace::Vector3d;

namespace IMC_namespace
{

Ortho_Cartesian_2D_Mesh::
Ortho_Cartesian_2D_Mesh( unsigned int N_x_zones_in, double x_min, double x_max,
                         unsigned int N_y_zones_in, double y_min, double y_max,
                         unsigned int nprocs_x_in, unsigned int nprocs_y_in, unsigned int myid_in
                       )
    : zoneIDs(0),
      N_x_zones (N_x_zones_in ),
      N_y_zones( N_y_zones_in ),
      number_of_zones (N_x_zones_in*N_y_zones_in ),
      _x( N_x_zones_in+3 ),
      _y( N_y_zones_in+3 ),
      zone_volume( N_x_zones*N_y_zones+1 ),
      N_x_faces( (N_x_zones+1)*N_y_zones ),
      N_y_faces( N_x_zones*(N_y_zones+1) ),
      number_of_faces( N_x_faces + N_y_faces ),
      number_of_boundary_faces( 2*(N_x_zones + N_y_zones) ),
      boundary_face_data( number_of_faces+1 ),
      plus_zone( number_of_faces+1 ),
      minus_zone( number_of_faces+1 ),
      boundary_face_zone_list( number_of_faces+1 ),
      face_area_list( number_of_faces+1 ),
      face_orientation_list( number_of_faces+1 ),
      nprocs_x(nprocs_x_in),
      nprocs_y(nprocs_y_in),
      nprocs(nprocs_x*nprocs_y),
      myid(myid_in),
      DD_border_faces(4)
{
    using std::cout;
    using std::endl;
    using std::exit;

//    force x_min to be greater than x_max; want to assume this
//    for consistency with signs of direction cosines
    if( x_min >= x_max )
    {
        cout << "x_min = " << x_min <<" >= x_max = " << x_max;
        cout << " - can't allow this!!" << endl;
        exit(-1);
    }

    if( y_min >= y_max )
    {
        cout << "y_min = " << y_min <<" >= y_max = " << y_max;
        cout << " - can't allow this!!" << endl;
        exit(-1);
    }

//    fill in IDs of real (= physical) zones in the mesh
    zoneIDs.reserve(number_of_zones);
    for( unsigned int zoneID = 1; zoneID <= number_of_zones; ++zoneID )
    {
        zoneIDs.push_back( zoneID );
    }

    double dx = (x_max - x_min)/N_x_zones;
    double dy = (y_max - y_min)/N_y_zones;

//    now fill in the values for the Mesh coordinates
    for( unsigned int xi = 0; xi <= N_x_zones+2; xi++ )
        _x[xi] = x_min + (xi-1)*dx;

    for( unsigned int yi = 0; yi <= N_y_zones+2; yi++ )
        _y[yi] = y_min + (yi-1)*dy;

//    Fill in the volumes.
//    For 2D, volumes are "per unit extention" in ignorable 3rd direction.
//    Since dx and dy are constant in this constructor, so is volume.

    for( unsigned int zone = 0; zone <= number_of_zones; zone++ )
        zone_volume[zone] = dx*dy;

//    set all the face info
    set_face_info();

}

//---------------------------------------------------------------------------//

Ortho_Cartesian_2D_Mesh::
Ortho_Cartesian_2D_Mesh( std::vector<double> x_in, std::vector<double> y_in, 
                         unsigned int nprocs_x_in, unsigned int nprocs_y_in, unsigned int myid_in )
    : zoneIDs(0),
      N_x_zones( x_in.size()-3 ),
      N_y_zones( y_in.size()-3 ),
      number_of_zones( (x_in.size()-3)*(y_in.size()-3) ),
      _x( x_in ),
      _y( y_in ),
      zone_volume( N_x_zones*N_y_zones + 1 ),
      N_x_faces( (N_x_zones+1)*N_y_zones ),
      N_y_faces( N_x_zones*(N_y_zones+1) ),
      number_of_faces( N_x_faces + N_y_faces ),
      number_of_boundary_faces( 2*(N_x_zones + N_y_zones) ),
      boundary_face_data( number_of_faces+1 ),
      plus_zone( number_of_faces+1 ),
      minus_zone( number_of_faces+1 ),
      boundary_face_zone_list( number_of_faces+1 ),
      face_area_list( number_of_faces+1 ),
      face_orientation_list( number_of_faces+1 ),
      nprocs_x(nprocs_x_in),
      nprocs_y(nprocs_y_in),
      nprocs(nprocs_x*nprocs_y),
      myid(myid_in),
      DD_border_faces(4)
{
    using std::cout;
    using std::endl;
    using std::exit;

//    make sure data is ascending monotonic
    for( unsigned int edge = 0; edge <= _x.size()-2; edge++ )
        if( _x[edge+1] <= _x[edge]  )
        {
            cout << "_x[" << edge << "+1] = " << _x[edge+1] << endl;
            cout << "_x[" << edge << "] = " << _x[edge] << endl;
            cout << "KL_mesh data must be ascending monotonic!!" << endl;
            exit(-1);
        }

    for( unsigned int edge = 0; edge <= _y.size()-2; edge++ )
        if( _y[edge+1] <= _y[edge]  )
        {
            cout << "_y[" << edge << "+1] = " << _y[edge+1] << endl;
            cout << "_y[" << edge << "] = " << _y[edge] << endl;
            cout << "Ortho_Cartesian_2D_Mesh data must be ascending monotonic!!" << endl;
            exit(-1);
        }

//    fill in IDs of real (= physical) zones in the mesh
    zoneIDs.reserve(number_of_zones);
    for( unsigned int zoneID = 1; zoneID <= number_of_zones; ++zoneID )
    {
        zoneIDs.push_back( zoneID );
    }

//    fill in the volumes
//    for 2D, volumes are "per unit extention" in ignorable 3rd direction

    for( unsigned int i = 1; i <= N_x_zones; i++ )
        for( unsigned int j = 1; j <= N_y_zones; j++ )
        {
            double dx = _x[i+1] - _x[i];
            double dy = _y[j+1] - _y[j];
            zone_volume[zone( i, j )] = dx*dy;
        }

//    set all the face info
    set_face_info();

}

//---------------------------------------------------------------------------//

void Ortho_Cartesian_2D_Mesh::set_face_info()
{

    setupDDParallel();

//    fill in the face information
//    first put in dummy info at std::vector[0] so indexs of real faces start at 1
    boundary_face_data[0] = false;
    plus_zone[0] = 0;
    minus_zone[0] = 0;
    boundary_face_zone_list[0] = 0;
    face_area_list[0] = 0;
    face_orientation_list[0] = minus_y;

//    First do x faces, one row at a time;
//    special loops over boundary faces will fix up data.
//    for those faces later
    for( unsigned int row = 1; row <= N_y_zones; row++ )
    {
        for( unsigned int i = 1; i <= N_x_zones+1; i++ )
        {
            unsigned int face = i + (row-1)*(N_x_zones+1);
            ASSERT( face < boundary_face_data.size() );

            boundary_face_data[face] = false;

            plus_zone[face] = face - (row-1);
            minus_zone[face] = face -1 - (row-1);

        //    i.e., it's not a boundary face
        //    boundary faces set up later in this function
            boundary_face_zone_list[face] = 0;

            face_area_list[face] = y(row+1) - y(row);
            ASSERT( face_area_list[face] > 0.0 );

            face_orientation_list[face] = plus_x;
        }
    }

//    Now do faces with normals in y direction;
//    boundary faces will get taken care of later
    for( unsigned int row = 1; row <= N_y_zones+1; row++ )
    {
        for( unsigned int i = 1; i <= N_x_zones; i++ )
        {
            unsigned int face = N_x_faces + i + (row-1)*(N_x_zones);

            ASSERT( face > N_x_faces );
            ASSERT( face < boundary_face_data.size() );

            boundary_face_data[face] = false;

            plus_zone[face] = face - N_x_faces;
            minus_zone[face] = face - N_x_faces - N_x_zones;

        //    i.e., it's not a boundary face - fixed up later
            boundary_face_zone_list[0] = 0;

            face_area_list[face] = x(i+1) - x(i);
            ASSERT( face_area_list[face] > 0.0 );

            face_orientation_list[face] = plus_y;
        }
    }

//    Now do the boundary faces with normals in +/- x.
    for( unsigned int row = 1; row <= N_y_zones; row++ )
    {
    //    2 boundary faces for each row
        unsigned int minus_b_face = 1 + (row-1)*(N_x_zones+1);
        ASSERT( minus_b_face < boundary_face_data.size() );

        boundary_face_data[minus_b_face] = true;

        DD_border_faces[left].push_back(minus_b_face);

    //    this face has no minus_zone
        minus_zone[minus_b_face] = static_cast<unsigned int>(-1);

        face_orientation_list[minus_b_face] = minus_x;

        boundary_face_zone_list[minus_b_face] = plus_zone[minus_b_face];

        unsigned int plus_b_face = row*(N_x_zones+1);
        ASSERT( plus_b_face < boundary_face_data.size() );

        boundary_face_data[plus_b_face] = true;

    //    this face has no plus_zone
        plus_zone[plus_b_face] = static_cast<unsigned int>(-1);

        boundary_face_zone_list[plus_b_face] = minus_zone[plus_b_face];

        DD_border_faces[right].push_back(plus_b_face);
    }

//    Now do the boundary faces with normals in +/- y
    for( unsigned int col = 1; col <= N_x_zones; col++ )
    {
    //    2 boundary faces for each column
        unsigned int minus_b_face = col + N_x_faces;
        ASSERT( minus_b_face < boundary_face_data.size() );

        boundary_face_data[minus_b_face] = true;

        DD_border_faces[down].push_back(minus_b_face);

    //    this face has no minus_zone
        minus_zone[minus_b_face] = static_cast<unsigned int>(-1);

        face_orientation_list[minus_b_face] = minus_y;

        boundary_face_zone_list[minus_b_face] = plus_zone[minus_b_face];

        unsigned int plus_b_face = number_of_faces- (N_x_zones-col);
        ASSERT( plus_b_face < boundary_face_data.size() );

        boundary_face_data[plus_b_face] = true;

    //    this face has no plus_zone
        plus_zone[plus_b_face] = static_cast<unsigned int>(-1);

        boundary_face_zone_list[plus_b_face] = minus_zone[plus_b_face];

        DD_border_faces[up].push_back(plus_b_face);
    }
}

//---------------------------------------------------------------------------//

//    function that returns list of the particle_zone_ID_types given a zone index
//    for this Mesh, particle_zone_ID_type = zone = unsigned int

std::vector<Ortho_Cartesian_2D_Mesh::particle_zone_ID_type>
Ortho_Cartesian_2D_Mesh::particle_zone_IDs( unsigned int zone ) const
{
    std::vector<Ortho_Cartesian_2D_Mesh::particle_zone_ID_type> pz_list(1);

    pz_list[0] = zone;

    return pz_list;
}

//---------------------------------------------------------------------------//

//    function that returns list of the particle_face_ID_types given a face index
//    for this Mesh, particle_face_ID_type = face = unsigned int

std::vector<Ortho_Cartesian_2D_Mesh::particle_face_ID_type>
Ortho_Cartesian_2D_Mesh::particle_face_IDs( unsigned int face ) const
{
    std::vector<Ortho_Cartesian_2D_Mesh::particle_face_ID_type> pf_list(1);

    pf_list[0] = face;

    return pf_list;
}

//---------------------------------------------------------------------------//

unsigned int
Ortho_Cartesian_2D_Mesh::
particleZoneOfPoint( const Vector3d& X,
                     bool& foundZone ) const
{
   foundZone = false;

   for ( unsigned int zn = 0; zn <= number_of_zones; zn++ )
   {
      if (zone_volume[zn] > 0.)
      {
         if (d_min(X, zn) >= 0.0)
         {
            foundZone = true;
            return zn;
         }
      }
   }

   return number_of_zones+1;
}

//---------------------------------------------------------------------------//

unsigned int
Ortho_Cartesian_2D_Mesh::
particleZoneOfPoint( const Vector3d& X,
                     const particle_zone_ID_type& pZone,
                     bool& foundZone ) const
{
   foundZone = false;
   unsigned int xi = x_index(pZone);
   unsigned int yi = y_index(pZone);
   unsigned int zn = zone(xi, yi);

   if (zone_is_real(zn) && zone_volume[zn] > 0.)
   {
      if (d_min(X, zn) >= 0.)
      {
         foundZone = true;
         return zn;
      }

      if (xi > 0)
      {
         unsigned int zxm = zone(xi-1, yi);

         if (zone_volume[zxm] > 0.)
         {
            if (d_min(X, zxm) >= 0.)
            {
               foundZone = true;
               return zxm;
            }
         }
      }
      if (xi < N_x_zones)
      {
         unsigned int zxp = zone(xi+1, yi);

         if (zone_volume[zxp] > 0.)
         {
            if (d_min(X, zxp) >= 0.)
            {
               foundZone = true;
               return zxp;
            }
         }
      }
      if (yi > 0)
      {
         unsigned int zym = zone(xi, yi-1);

         if (zone_volume[zym] > 0.)
         {
            if (d_min(X, zym) >= 0.)
            {
               foundZone = true;
               return zym;
            }
         }
      }
      if (yi < N_y_zones)
      {
         unsigned int zyp = zone(xi, yi+1);

         if (zone_volume[zyp] > 0.)
         {
            if (d_min(X, zyp) >= 0.)
            {
               foundZone = true;
               return zyp;
            }
         }
      }
   }

   for ( unsigned int zn = 0; zn <= number_of_zones; zn++ )
   {
      if (zone_volume[zn] > 0.)
      {
         if (d_min(X, zn) >= 0.)
         {
            foundZone = true;
            return zn;
         }
      }
   }

   return number_of_zones+1;
}

//---------------------------------------------------------------------------//

double Ortho_Cartesian_2D_Mesh::
d_min( const Vector3d& X, unsigned int current_zone ) const
{
    using std::min;

    unsigned int currentx_index = x_index( current_zone );
    unsigned int currenty_index = y_index( current_zone );

    double x_minus = _x[currentx_index];
    double x_plus = _x[currentx_index+1];

    double y_minus = _y[currenty_index];
    double y_plus = _y[currenty_index+1];

//    distances from particle to x faces
    double dx_minus, dx_plus;
    dx_minus = X.GetX() - x_minus;
    dx_plus = x_plus - X.GetX();

//    distances from particle to y faces
    double dy_minus, dy_plus;
    dy_minus = X.GetY() - y_minus;
    dy_plus = y_plus - X.GetY();

//    now find smallest distance
    return min( min(dx_minus,dx_plus), min(dy_minus,dy_plus) );
}

//---------------------------------------------------------------------------//

//    distance to boundary crossing for particle, and what zone
//    it will be in if it crosses, or if it goes out of bounds

double Ortho_Cartesian_2D_Mesh::
distanceToBoundary( const Vector3d& X,
                    const Vector3d& Omega,
                    unsigned int current_zone,
                    unsigned int& crossing_face,
                    unsigned int& next_zone,
                    bool& exits_problem,
                    bool& particle_trapped ) const
{
    const double large_distance = 1.0e100;
#ifndef NOASSERT
    const double d_b_min = -1.0e-9;    //    min d_boundary allowed
#endif

    particle_trapped = false;

    unsigned int x_crossing_face = 0, y_crossing_face = 0;

    unsigned int currentx_index = x_index( current_zone );
    unsigned int currenty_index = y_index( current_zone );

    double x_minus = _x[currentx_index];
    double x_plus = _x[currentx_index+1];

    double y_minus = _y[currenty_index];
    double y_plus = _y[currenty_index+1];

    if( Omega.GetZ() == 1.0 || (Omega.GetX() == 0.0 && Omega.GetY() == 0.0) )
    {
        exits_problem = false;  //    it's up z axis; wont cross any boundary
        next_zone = current_zone;
        crossing_face = 0;    //    doesn't cross a face
        return large_distance;
    }

    double d_boundary_x;
    unsigned int newx_index;
//    row ( in y direction, 1 ... N_y_zones ) that the zone is in
    unsigned int row = (current_zone-1)/N_x_zones + 1;
    if( Omega.GetX() > 0.0 )    //  it's headed in +x direction
    {
        d_boundary_x = (x_plus - X.GetX())/Omega.GetX();
        newx_index = currentx_index + 1;
        x_crossing_face = row + current_zone;
    }
    else if( Omega.GetX() < 0.0 )  //  it's headed in -x direction
    {
        d_boundary_x = -(X.GetX() - x_minus)/Omega.GetX();
        newx_index = currentx_index - 1;
        x_crossing_face = row + current_zone - 1;
    }
    else{    //  it's not moving in x; any large number will do
        d_boundary_x = large_distance;
        newx_index = 0;
    }

    double d_boundary_y;
    unsigned int newy_index;
    if( Omega.GetY() > 0.0 )    //  it's headed in +y direction
    {
        d_boundary_y = (y_plus - X.GetY())/Omega.GetY();
        newy_index = currenty_index + 1;
        y_crossing_face = N_x_faces + current_zone + N_x_zones;
    }
    else if( Omega.GetY() < 0.0 )  //  it's headed in -y direction
    {
        d_boundary_y = -(X.GetY() - y_minus)/Omega.GetY();
        newy_index = currenty_index - 1;
        y_crossing_face = N_x_faces + current_zone;
    }
    else{    //  it's not moving in y; any large number will do
        d_boundary_y = large_distance;
        newy_index = 0;
    }

//    now see which distance was smaller, and whether particle escaped
    if( d_boundary_x <= d_boundary_y )    //  it crosses x
    {
        if( (newx_index > N_x_zones) || (newx_index < 1) )
        {
            exits_problem = true;
            next_zone = 0;
        }
        else
        {
            exits_problem = false;
            next_zone = zone(newx_index,currenty_index);
        }

        crossing_face = x_crossing_face;
        ASSERT( d_boundary_x > d_b_min );  //  allow < 0 due to roundoff
        return d_boundary_x;
    }

//    if you're here, it crosses in y direction

    if( (newy_index > N_y_zones) || (newy_index < 1) )
    {
        exits_problem = true;
        next_zone = 0;
    }
    else
    {
        exits_problem = false;
        next_zone = zone(currentx_index,newy_index);
    }

    crossing_face = y_crossing_face;
    ASSERT( d_boundary_y > d_b_min );  //  allow < 0 due to roundoff
    return d_boundary_y;
}

//---------------------------------------------------------------------------//

//    given zone and random number generator, give a location (x,y,z) for
//    a Monte Carlo particle in the zone.

void Ortho_Cartesian_2D_Mesh::
randomZoneLocation( rng& rand,
                    mesh_zone_ID_type mesh_zone,
                    Vector3d& X,
                    particle_zone_ID_type& particle_zone) const
{
    double r1, r2, r3;
    r1 = rand.random_number();
    r2 = rand.random_number();
    r3 = rand.random_number();

//    set x, y, and z with call to overloaded randomZoneLocation
    randomZoneLocation( r1, r2, r3, mesh_zone, X);

//    for this mesh, particle_zone and mesh_zone are the same
    particle_zone = mesh_zone;
}

//---------------------------------------------------------------------------//

//    give cell and 3 random numbers in (0,1), give a location (x,y,z) for
//    a Monte Carlo particle in the zone.
//    This version, used the version above,
//    does not weight particle location with slope of T^4
//    r3 is not used in 2D

void Ortho_Cartesian_2D_Mesh::
randomZoneLocation( double r1, double r2, double r3, unsigned int zone,
                    Vector3d& X ) const
{
    unsigned int currentx_index = x_index( zone );
    unsigned int currenty_index = y_index( zone );

    double x_minus = _x[currentx_index];
    double x_plus = _x[currentx_index+1];
    double delta_x = x_plus - x_minus;

    double y_minus = _y[currenty_index];
    double y_plus = _y[currenty_index+1];
    double delta_y = y_plus - y_minus;

    X.SetX(x_minus + r1*delta_x);

    X.SetY(y_minus + r2*delta_y);

//    don't care what this is in 2D Mesh - z is ignorable
    X.SetZ( 0.0 );
}

//---------------------------------------------------------------------------//

void Ortho_Cartesian_2D_Mesh::
randomFaceLocation( rng& rand, mesh_face_ID_type mesh_face, Vector3d& particle_X,
                    particle_face_ID_type& particle_face) const
{
//    for this mesh, mesh_face_ID_type and particle_face_ID_type
//    are the same type
    particle_face = mesh_face;

    double r1 = rand.random_number();

    if( mesh_face <= N_x_faces )    //    face normal in +/- x
    {
        double y_minus = y( (mesh_face-1)/(N_x_zones+1) + 1 );
        double y_plus = y( (mesh_face-1)/(N_x_zones+1) + 2 );

        particle_X.SetY( y_minus + r1*( y_plus - y_minus ));

        particle_X.SetX( x( (mesh_face-1)%(N_x_zones+1) + 1 ));
    }
    else    //    face normal in +/- y
    {
        double x_minus = x( (mesh_face-N_x_faces-1)%N_x_zones + 1 );
        double x_plus = x( (mesh_face-N_x_faces-1)%N_x_zones + 2 );

        particle_X.SetX( x_minus + r1*( x_plus - x_minus ));

        particle_X.SetY( y( (mesh_face - N_x_faces - 1)/N_x_zones + 1 ));
    }

    particle_X.SetZ(0.0);   //    don't care what this is in 2D Mesh
}

//---------------------------------------------------------------------------//

//    return outward normal to a boundary face; used to calculate the angles of
//    particles born on boundaries
//    for this mesh, faces are planes, so x, y, and z are ignorable

Vector3d Ortho_Cartesian_2D_Mesh::
face_normal( unsigned int face, const Vector3d& X) const
{
    using std::cout;
    using std::endl;
    using std::exit;

    switch( face_orientation(face) )
    {
    case minus_y:
        return Vector3d( 0.0, -1.0, 0.0);
    case plus_x:
        return Vector3d( 1.0, 0.0, 0.0);
    case plus_y:
        return Vector3d( 0.0, 1.0, 0.0);
    case minus_x:
        return Vector3d( -1.0, 0.0, 0.0);
    default:
        cout << "bad face orientation in";
        cout << "Ortho_Cartesian_2D_Mesh::face_normal" << endl;
        cout << "face = " << face << endl;
        cout << "orientation of face = ";
        cout << static_cast<unsigned int>( face_orientation(face) ) << endl;
        break;
    }

    return Vector3d();
}

//---------------------------------------------------------------------------//

//    return constant normal to a face; used to calculate flux tallies.
Vector3d Ortho_Cartesian_2D_Mesh::
natural_face_normal( unsigned int face, const Vector3d& X) const
{
    using std::cout;
    using std::endl;
    using std::exit;

    switch( face_orientation(face) )
    {
    case plus_x:
    case minus_x:
        return Vector3d( 1.0, 0.0, 0.0);
    case plus_y:
    case minus_y:
        return Vector3d( 0.0, 1.0, 0.0);
    default:
        cout << "bad face orientation in";
        cout << "Ortho_Cartesian_2D_Mesh::boundary_face_normal" << endl;
        cout << "face = " << face << endl;
        cout << "orientation of face = ";
        cout << static_cast<unsigned int>( face_orientation(face) ) << endl;
        return Vector3d(0.0);
        break;
    }
}

//---------------------------------------------------------------------------//

//    Functions used in debugging, esp. in advance_particle, when particle takes
//    too many steps

double Ortho_Cartesian_2D_Mesh::
print_distanceToBoundary( const Vector3d& X,
                          const Vector3d& Omega,
                          unsigned int current_zone,
                          unsigned int& crossing_face,
                          unsigned int& next_zone,
                          bool& exits_problem,
                          bool& particle_trapped ) const
{
    using std::cout;
    using std::endl;

    const double large_distance = 1.0e100;
#ifndef NOASSERT
    const double d_b_min = -1.0e-9;    //    min d_boundary allowed
#endif

    particle_trapped = false;

    unsigned int x_crossing_face = 0, y_crossing_face = 0;

    unsigned int currentx_index = x_index( current_zone );
    unsigned int currenty_index = y_index( current_zone );

    double x_minus = _x[currentx_index];
    double x_plus = _x[currentx_index+1];

    double y_minus = _y[currenty_index];
    double y_plus = _y[currenty_index+1];

    if( Omega.GetZ() == 1.0 || (Omega.GetX() == 0.0 && Omega.GetY() == 0.0) )
    {
        exits_problem = false;  //    it's up z axis; wont cross any boundary
        next_zone = current_zone;
        crossing_face = 0;    //    doesn't cross a face
        cout << "going up z axis; doesn't cross a face" << endl;
        return large_distance;
    }

    double d_boundary_x;
    unsigned int newx_index;
//    row ( in y direction, 1 ... N_y_zones ) that the zone is in
    unsigned int row = (current_zone-1)/N_x_zones + 1;
    if( Omega.GetX() > 0.0 )    //  it's headed in +x direction
    {
        d_boundary_x = (x_plus - X.GetX())/Omega.GetX();
        newx_index = currentx_index + 1;
        x_crossing_face = row + current_zone;
    }
    else if( Omega.GetX() < 0.0 )  //  it's headed in -x direction
    {
        d_boundary_x = -(X.GetX() - x_minus)/Omega.GetX();
        newx_index = currentx_index - 1;
        x_crossing_face = row + current_zone - 1;
    }
    else{    //  it's not moving in x; any large number will do
        d_boundary_x = large_distance;
        newx_index = 0;
    }

    double d_boundary_y;
    unsigned int newy_index;
    if( Omega.GetY() > 0.0 )    //  it's headed in +y direction
    {
        d_boundary_y = (y_plus - X.GetY())/Omega.GetY();
        newy_index = currenty_index + 1;
        y_crossing_face = N_x_faces + current_zone + N_x_zones;
    }
    else if( Omega.GetY() < 0.0 )  //  it's headed in -y direction
    {
        d_boundary_y = -(X.GetY() - y_minus)/Omega.GetY();
        newy_index = currenty_index - 1;
        y_crossing_face = N_x_faces + current_zone;
    }
    else{    //  it's not moving in y; any large number will do
        d_boundary_y = large_distance;
        newy_index = 0;
    }

    double d_b;

//    now see which distance was smaller, and whether particle escaped
    if( d_boundary_x <= d_boundary_y )    //  it crosses x
    {
        if( (newx_index > N_x_zones) || (newx_index < 1) )
        {
            exits_problem = true;
            next_zone = 0;
        }
        else
        {
            exits_problem = false;
            next_zone = zone(newx_index,currenty_index);
        }

        crossing_face = x_crossing_face;
        ASSERT( d_boundary_x > d_b_min );  //  allow < 0 due to roundoff
        d_b = d_boundary_x;
    }

//    if you're here, it crosses in y direction

    if( (newy_index > N_y_zones) || (newy_index < 1) )
    {
        exits_problem = true;
        next_zone = 0;
    }
    else
    {
        exits_problem = false;
        next_zone = zone(currentx_index,newy_index);
    }

    crossing_face = y_crossing_face;
    ASSERT( d_boundary_y > d_b_min );  //  allow < 0 due to roundoff
    d_b = d_boundary_y;

    cout << endl;
    cout << "in print_d_boundary:" << endl;
    cout << "d_boundary_x = " << d_boundary_x << endl;
    cout << "x_crossing_face = " << x_crossing_face << endl;
    cout << "d_boundary_y = " << d_boundary_y << endl;
    cout << "y_crossing_face = " << y_crossing_face << endl;
    cout << "next_zone = " << next_zone << endl;
    cout << "crossing_face = " << crossing_face << endl;
    cout << "exits_problem = " << exits_problem << endl;

    return d_b;
}

//---------------------------------------------------------------------------//

std::ostream& operator <<( std::ostream& os, const Ortho_Cartesian_2D_Mesh& Mesh )
{
    using std::endl;

    os << "For Ortho_Cartesian_2D_Mesh:" << endl;
    os << "x_zones = " << Mesh.N_x_zones << endl;
    os << "y_zones = " << Mesh.N_y_zones << endl;
    os << "number_of_zones = " << Mesh.number_of_zones << endl;
    os << endl;

    os << "Mesh data:" << endl;
    os << "x values" << endl;
    for( unsigned int xi = 1; xi <= Mesh.N_x_zones+1; xi++ )
        os << xi << "  " << Mesh._x[xi] << endl;
    os << endl;

    os << "y values" << endl;
    for( unsigned int xi = 1; xi <= Mesh.N_y_zones+1; xi++ )
        os << xi << "  " << Mesh._y[xi] << endl;
    os << endl;

    os << "zone indicies:" << endl;
    os << "number of real zones = " << Mesh.N_zones() << endl;
    for( unsigned int ci = 1; ci <= Mesh.N_zones(); ci++ )
    {
        unsigned int xi = Mesh.x_index(ci);
        unsigned int yi = Mesh.y_index(ci);
        unsigned int test_zone = Mesh.zone(xi,yi);
        os << ci << "  " << test_zone << "  ";
        os << xi << "  " << yi << endl;
    }
    os << endl;
    os << endl;

    os << endl;

    os << "zone volumes" << endl;
    for( unsigned int zone_count = 1; zone_count <= Mesh.N_zones(); zone_count++ )
        os << zone_count << "  " << Mesh.zone_volume[zone_count] << endl;
    os << endl;
    os << endl;

    os << "number of faces = " << Mesh.number_of_faces << endl;
    os << "number of x faces = " << Mesh.N_x_faces << endl;
    os << "number of y faces = " << Mesh.N_y_faces << endl;
    os << "number of boundary faces = " << Mesh.number_of_boundary_faces << endl;
    os << endl;

    for( unsigned int face = 1; face <= Mesh.number_of_faces; face++)
    {
        if( !Mesh.boundary_face(face) )
        {
            os << "face = " << face << " abuts zones " <<
                Mesh.minus_zone[face] << " and " << Mesh.plus_zone[face] << endl;
        }
        else
        {
            os << "face = " << face << " is a boundary face adjacent to zone ";
            os << Mesh.boundary_face_zone_list[face] << endl;
        }

        if( face <= Mesh.N_x_faces )    //    face normal in +/- x
        {
            double y_minus = Mesh.y( (face-1)/(Mesh.N_x_zones+1)+1 );
            double y_plus = Mesh.y( (face-1)/(Mesh.N_x_zones+1)+2 );
            double x_face =  Mesh.x( (face-1)%(Mesh.N_x_zones+1)+1 );

            os << "  face bound by y = ( " << y_minus << " , ";
            os << y_plus << " ) and x = " << x_face << endl;
        }
        else
        {
            double x_plus = Mesh.x( (face-Mesh.N_x_faces-1)%Mesh.N_x_zones+2 );
            double x_minus = Mesh.x( (face-Mesh.N_x_faces-1)%Mesh.N_x_zones+1 );
            double y_face = Mesh.y( (face-Mesh.N_x_faces-1)/Mesh.N_x_zones+1 );

            os << "  face bound by x = ( " << x_minus << " , ";
            os << x_plus << " ) and y = " << y_face << endl;
        }

        os << "   face area = " << Mesh.face_area_list[face] << endl;
        os << "   normal points in  ";
        os << static_cast<unsigned int>( Mesh.face_orientation_list[face] );

        os << endl;
        os << endl;
    }
    os << endl;

    return os;
}

//---------------------------------------------------------------------------//

//    We need to set up some MPI communicators

void
Ortho_Cartesian_2D_Mesh::
setupMPI()
{
#ifdef USE_MPI

//    since this mesh only does DD, all procs are masters
    master_comm = MPI_COMM_WORLD;

    MPI_Group group_world;
    MPI_Group group_replicas;
    
//    only this proc is a replica because this mesh can only do domain decomposition
    vector<int> replicaIDsVec;
    replicaIDsVec.push_back( myid );

    MPI_Comm_group(MPI_COMM_WORLD, &group_world);
    MPI_Group_incl(group_world, 1, &replicaIDsVec[0], &group_replicas);
    MPI_Comm_create(MPI_COMM_WORLD, group_replicas, &replica_comm);

    MPI_Group_free( &group_world );
    MPI_Group_free( &group_replicas );

#endif
}

//---------------------------------------------------------------------------//

void Ortho_Cartesian_2D_Mesh::
setupDDParallel()
{

   setupMPI();

   using std::cout;
   using std::exit;
   using std::endl;

   if( nprocs_x <= 0 )
   {
        cout << "nprocs_x must be greater than 1.  You said nprocs_x=" << nprocs_x  << endl;
        exit(-1);
   }
   if( nprocs_y <= 0 )
   {
        cout << "nprocs_y must be greater than 1.  You said nprocs_y=" << nprocs_y  << endl;
        exit(-1);
   }

   if( myid < 0 || myid >= nprocs )
   {
        cout << "0 <= myid < nprocs_x*nprocs_y, but you said myid=" << myid << endl;
        exit(-1);
   }


   // Assumes a decomposition of the grid into nprocs_x * nprocs_y blocks.
   // For nprocs_x = 3, nprocs_y = 4, we'd have
   //
   //             top
   //     -------------------
   //     |  9  |  10 |  11 |
   //  l  ------------------- r     y
   //  e  |  6  |  7  |  8  | i     ^
   //  f  ------------------- g     |
   //  t  |  3  |  4  |  5  | h     |---> x
   //     ------------------- t
   //     |  0  |  1  |  2  |
   //     -------------------
   //           bottom 
   //

   // bottom neighbor
   if( myid >= nprocs_x )
   {
      int n = ( myid - nprocs_x );
//       std::cout << myid << ": down " << n << std::endl;
      DD_neighbors.push_back(n);
      DD_direction.push_back(down);
   }

   // top neighbor
   if( myid < nprocs - nprocs_x )
   {
      int n = ( myid + nprocs_x );
//       std::cout << myid << ": up " << n << std::endl;
      DD_neighbors.push_back(n);
      DD_direction.push_back(up);
   }

   // Left neighbor
   if( myid % nprocs_x != 0 )
   {
      int n = myid - 1 ;
//       std::cout << myid << ": left " << n << std::endl;
      DD_neighbors.push_back( n );
      DD_direction.push_back(left);
   }

   // Right neighbor
   if( (myid+1) % nprocs_x != 0 )
   {
      int n = myid + 1 ;
//       std::cout << myid << ": right " << n << std::endl;
      DD_neighbors.push_back( n );
      DD_direction.push_back(right);
   }

}

//---------------------------------------------------------------------------//

//  argument X is ignored in this mesh - it is needed for meshes where faces
//  are split, such as AMR meshes

void Ortho_Cartesian_2D_Mesh::
setNewDomainZoneInfo( const particle_zone_ID_type& current_particle_zone,
                      const particle_zone_ID_type& next_zone,
                      const particle_face_ID_type& face,
                      Vector3d_namespace::Vector3d& X, 
                      newDomainZoneIDType& newDomainZoneInfo ) const
{

   // Save X and Y coordinates of the zone that we are leaving.
   newDomainZoneInfo.x = x_zone(current_particle_zone);
   newDomainZoneInfo.y = y_zone(current_particle_zone);
   //newDomainZoneInfo.from_proc = myid;
   //newDomainZoneInfo.from_zone = current_particle_zone;

   //std::cout << myid << ": Buffering at " 
   //   << newDomainZoneInfo.x << "  " << newDomainZoneInfo.x  << std::endl;
}

//---------------------------------------------------------------------------//

//! Find the new index for the X and Y coordinates given the old zone coordinates.
//! This starts at the first interior face and ends at the last ineterior face.
//! We want to treat everything outside of the mesh as begin inside, since
//! the newDomainZoneInfo is really the old zone center position.
//! Drawing a picture will help.

void Ortho_Cartesian_2D_Mesh::
correctNewDomainZone( const newDomainZoneIDType& newDomainZoneInfo,
                      Vector3d_namespace::Vector3d& X, 
                      particle_zone_ID_type& current_particle_zone ) const
{

   typedef std::vector<double>::const_iterator it_type;

   unsigned int xd;
   unsigned int yd;
   it_type xbegin = _x.begin(), xend = _x.begin(), xi = _x.begin();
   it_type ybegin = _y.begin(), yend = _y.begin(), yi = _y.begin();

   if( N_x_zones == 1)
   {
      xd = 1;
   }
   else
   {
      xbegin = _x.begin() + 2;
      xend = _x.end() - 2;
      xi = std::lower_bound( xbegin, xend, newDomainZoneInfo.x );

      xd = std::distance( xbegin, xi )+1;
   }

   if( N_y_zones == 1)
   {
      yd = 1;
   }
   else
   {
      ybegin = _y.begin() + 2;
      yend = _y.end() - 2;
      yi = std::lower_bound( ybegin, yend, newDomainZoneInfo.y );

      yd = std::distance( ybegin, yi )+1;
   }

   current_particle_zone = zone(xd, yd);
   
   //std::cout << myid << ": Unpacking  ( " 
   //   << newDomainZoneInfo.x << " " 
   //   << newDomainZoneInfo.y << " " 
   //   << newDomainZoneInfo.from_proc << " "
   //   << newDomainZoneInfo.from_zone << " ) ( "
   //   << *xbegin<< " "
   //   << *xend<< " "
   //   << *xi<< " ) ( "
   //   << *ybegin << " "
   //   << *yend << " "
   //   << *yi << " ) ( "
   //   << x_zone(current_particle_zone) << " "
   //   << y_zone(current_particle_zone) << ") ( "
   //   << xd << " "
   //   << yd << " "
   //   << zone(xd,yd) << " ) "
   //   << std::endl;


}

//---------------------------------------------------------------------------//

//    Since this mesh can only do domain decomp, all procs are masters, so
//    all neighbors are masters

std::vector<int> 
Ortho_Cartesian_2D_Mesh::
getNeighborMasterMPITasks() const
{
    return getNeighborMPITasks();
}

//---------------------------------------------------------------------------//

//    Since this mesh can only do domain decomp, domain ID = proc ID, so
//    neighbor domains = neighbor procs

std::vector<int> 
Ortho_Cartesian_2D_Mesh::
getNeighborDomains() const
{
    return getNeighborMPITasks();

}

///---------------------------------------------------------------------------//

//    Since this mesh only does DD, domain ID = proc ID, so neighbor proc of
//    a neighboring domain is the domain ID

vector<int> 
Ortho_Cartesian_2D_Mesh::
getNeighborMPITasks( int neighborDomain ) const
{
    vector<int> neighborMPITasks;

    neighborMPITasks.push_back( neighborDomain );

   return neighborMPITasks;
}

//---------------------------------------------------------------------------//

//    Return vector of process IDs that are running replicas of the given
//    domain. Since this mesh can only do domain decomposition, only the
//    current process is running the domain.

std::vector<int> 
Ortho_Cartesian_2D_Mesh::
getProcessIDsFromDomain( int domainID ) const
{
    std::vector<int> procIDs;

    procIDs.push_back( domainID );

    return procIDs;
}

//---------------------------------------------------------------------------//

#ifdef USE_MPI

//     Since this mesh can only do domain decomp, this communicator
//     replica_comm only contains this proc

const MPI_Comm& 
Ortho_Cartesian_2D_Mesh::
getDomainGroupCommunicator() const
{
    return replica_comm;
}

//---------------------------------------------------------------------------//

//    MPI communicator that encompasses all the master procs for each domain.
//    Since this mesh can only do domain decomp, this communicator is MPI_COMM_WORLD

const MPI_Comm&
Ortho_Cartesian_2D_Mesh::
getDomainMastersCommunicator() const
{
    return master_comm;
}

//---------------------------------------------------------------------------//

//     Since this mesh can only do domain decomp, this communicator
//     replica_comm only contains this proc

const MPI_Comm &
Ortho_Cartesian_2D_Mesh::
getWorldCommunicator() const
{
    return master_comm;
}

//---------------------------------------------------------------------------//

int 
Ortho_Cartesian_2D_Mesh::
getRank() const
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    return rank;
}

#endif    //    #ifdef USE_MPI

//---------------------------------------------------------------------------//

//    Since other procID is the same as the domain ID, we can use them
//    interchangeably

vector<Ortho_Cartesian_2D_Mesh::mesh_face_ID_type>
Ortho_Cartesian_2D_Mesh::
getBorderFacesFromDomain( unsigned int other_procID ) const
{

   for(unsigned int p=0; p<DD_neighbors.size(); ++p)
   {
     if( static_cast<unsigned int>(DD_neighbors[p]) == other_procID )
      {
         ASSERT( static_cast<unsigned int>(DD_direction[p]) < DD_border_faces.size() );
         return DD_border_faces[ DD_direction[p] ];
      }
   }

   std::cout << "ERROR Ortho_Cartesian_2D_Mesh::getBorderFaces():\n";
   std::cout << "  other_procID=" << other_procID << " is not a neighbor processor\n";
   std::cout << "  neighbor processors are: ";
   for(unsigned int p=0; p<DD_neighbors.size(); ++p)
   {
      std::cout << DD_neighbors[p] << " ";
   }
   std::cout << std::endl;

   throw( "Ortho_Cartesian_2D_Mesh::getBorderFaces(): other_procID not in neighbor list." );

   // Return something bogus to make the compiler happy.
   return DD_border_faces[0];
   
}

//---------------------------------------------------------------------------//
//---------------------------------------------------------------------------//


}    //    IMC_namespace
