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

#ifndef __Domain_exchange_hh__
#define __Domain_exchange_hh__

namespace IMC_namespace
{
    template <class Mesh_type,class Particle_type>
    class Domain_exchange;
}

#include <iostream>
#include <cmath>
#include <vector>
#include <list>
#include <mpi.h>

#include "ASSERT.hh"
#include "Buffered_MPI_Send.hh"

#include "buffered_particle.hh"

//    need to template on mesh so we can have a member of class
//    Mesh_type::particle_zone

namespace IMC_namespace
{

//===========================================================================//

/*!  \brief Transfers particles across processor boundaries.

    Copies a particle to a list when it crosses a face, and augments
    it with extra information to find the zone on the new processor.

    The particles are buffered until max_buffer_size is reached, then
    the buffer is sent.  There is a standing nonblocking receive 
    request to immediately get particles from the other processor.
    This request must be cancled with finalize() at the end of each time
    step.

    Upon receiving particles, the new zone is found and the particle added to 
    the list. It does NOT change positions - that is, it assumes that the
    domains AND the faces between them are are contiguous.

    You can send to yourself.  This is useful for periodic boundaries,
    although any translations and rotions must be done before buffering
    the particle.

    This implements part of the LANL Milagro parallel algorithm.   The rest
    is implemented by Domain_photon_source and IMC.  All parts work together.
    (Although this should work fine by itself.  The Domain_photon_source
    figures out when the Domains are done exchanging particles, and IMC
    runs each particle.)
    */

//===========================================================================//
template <class Mesh_type, class Particle_type>
class Domain_exchange
{
  public: 

    typedef typename Mesh_type::mesh_face_ID_type mesh_face_ID_type;
    typedef typename Mesh_type::particle_zone_ID_type particle_zone_ID_type;
    typedef typename Mesh_type::particle_face_ID_type particle_face_ID_type;

    Domain_exchange( int this_procID, int other_procID, 
                     const Mesh_type& Mesh_in, unsigned int max_buffer_size_in );

    ~Domain_exchange();

    //! Stores particle as it crosses domain boundary
    void buffer_particle( const particle_face_ID_type& face,
                          const particle_zone_ID_type& next_zone,
                          Particle_type& crossing_particle );
                                  
    //! Initialize nonblocking receive.
    void setUp();

    //! Flush buffer and send particles to other processor.
    void send_particles();

    //! Get particles and add them to particle_list.
    void get_and_add_particles( std::list<Particle_type>& particle_list, 
                                bool allow_inconsistent_zone_and_location = false );

    //! Release MPI requests.
    void finalize();

    //! Debuging info
    void print_DE_info() const;

    //! Set the tag.
    void set_tag( int tag_in ){ tag = tag_in; }

    //! Get the tag.
   int get_tag() { return tag; }

    //! Set receive request pointer.
    void set_request( MPI_Request* req_in );
    
    unsigned long long get_N_buffered() const {return N_buffered; }
    unsigned long long get_N_received() const {return N_received; }
    unsigned long long get_N_buffers_sent() const {return N_buffers_sent; }
    unsigned long long get_N_buffers_received() const {return N_buffers_received; }

    void set_N_buffered( unsigned long long in ) {N_buffered = in; }
    void set_N_received( unsigned long long in ) {N_received = in; }
    void set_N_buffers_sent( unsigned long long in ) {N_buffers_sent = in; }
    void set_N_buffers_received( unsigned long long in ) {N_buffers_received = in; }

  private:  
    //! Local processor rank
    const int this_procID;
    
    //! Processor on the other side of the boundary
    const int other_procID;
    
    //! Mesh this object is on
    const Mesh_type& Mesh;

    //! Zone information to find zone on the other processor.
    typedef typename Mesh_type::newDomainZoneIDType newDomainZoneIDType;
    //! shorthand for type of particle in the buffer.
    typedef buffered_particle<Particle_type,newDomainZoneIDType> buffered_particle_type;
   
    //! Storage of particles.
    std::vector<buffered_particle_type> buffered_particles;

    //! Handle for nonblocking receive.
    MPI_Request* receive_request;
    
    //! Persistant buffer to receive particles.  Currently kept between timesteps
    char* receive_buffer;

    //! Number of particles to buffer before sending to other processor
    const unsigned int max_buffer_size;
    //! Bytes for particles plus one unsigned int indicating how many particles there are.
    const int mpi_buffer_size;
    //! MPI tag
    int tag;


    Buffered_MPI_Send buffer;

    // These need to be saved in the restart file.
    unsigned long long N_buffered;
    unsigned long long N_received;
    unsigned long long N_buffers_sent;
    unsigned long long N_buffers_received;

#ifdef USE_OPENMP
    std::vector<buffered_particle_type>& mSharedBufferedParticleVector();
    size_t& mSharedCombinedBufferSize();
#endif

};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template<typename Mesh_type, typename Particle_type>
Domain_exchange<Mesh_type,Particle_type>::
~Domain_exchange()
{ 
#ifdef USE_OPENMP
#pragma omp master
#endif
    {
        delete [] receive_buffer;
    }
}

//---------------------------------------------------------------------------//

//! Set receive request pointer.

template<typename Mesh_type, typename Particle_type>
void 
Domain_exchange<Mesh_type,Particle_type>::
set_request( MPI_Request* req_in )
{
    ASSERT(receive_request == 0);
#ifdef USE_OPENMP
#pragma omp master
#endif
    {
        receive_request = req_in;
    }
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif    //    __Domain_exchange_hh__

