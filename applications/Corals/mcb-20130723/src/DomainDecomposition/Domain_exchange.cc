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

#include <iostream>
#include "Domain_exchange.hh"
#include "ASSERT.hh"
#include <cstdio>

#ifdef USE_OPENMP
#include <omp.h>
#endif

using namespace std;

namespace IMC_namespace
{


//---------------------------------------------------------------------------//

/*! Set up this object.  Calculates all sorts of sizes, and allocates the
  receive buffer.  The particle buffer is also allocated.
  */

template<typename Mesh_type, typename Particle_type>
Domain_exchange<Mesh_type,Particle_type>::
Domain_exchange( int this_procID_in, int other_procID_in, 
                 const Mesh_type& Mesh_in, unsigned int max_buffer_size_in )
  : this_procID( this_procID_in ),
    other_procID( other_procID_in ),
    Mesh( Mesh_in ),
    buffered_particles( 0 ),
    receive_request(0),
    receive_buffer(0),
    max_buffer_size(max_buffer_size_in),
    mpi_buffer_size((max_buffer_size+1)*sizeof(buffered_particle_type)),
    tag( 55 ),
    N_buffered(0),
    N_received(0),
    N_buffers_sent(0),
    N_buffers_received(0)
{
    ASSERT( this_procID >= 0 );
    ASSERT( other_procID >= 0 );


#ifdef USE_OPENMP
    //
    // for the OPENMP case we will use a shared vector hold the particles we will be sending
    //
#pragma omp single
    {
        std::vector<buffered_particle_type>& theBufferedParticles = mSharedBufferedParticleVector();

        theBufferedParticles.reserve( max_buffer_size + 1);
        
    }   

#pragma omp master
    {
        //    set aside some memory for vector buffered_particles which we'll use on the
        // master thread
        buffered_particles.reserve( max_buffer_size + 1 );
    }
    
#else
    //
    // non-OPEN_MP case we use a regular vector for particle buffering
    //

    //    set aside some memory for vector buffered_particles
    buffered_particles.reserve( max_buffer_size + 1 );
#endif
    

    //
    // set up the receive buffer on the master thread which will handle all MPI communication
    //
#ifdef USE_OPENMP
#pragma omp master
#endif
    {
        receive_buffer = new char[mpi_buffer_size];
    }
    
//    for debugging
//    print_DE_info();
}

//---------------------------------------------------------------------------//

/*! Copy the particle, augument it with extra information from
  Mesh_type::newDomainZoneIDType, then store it in the buffer.

  If the buffer is full, send it to the other processor.
  */

template<typename Mesh_type, typename Particle_type>
void Domain_exchange<Mesh_type,Particle_type>::
buffer_particle( const particle_face_ID_type& face,
                 const particle_zone_ID_type& next_zone,
                 Particle_type& crossing_particle )
{    
    ASSERT( this_procID >= 0 );
    ASSERT( other_procID >= 0 );

    typename Mesh_type::newDomainZoneIDType newDomainZoneInfo;
    Mesh.setNewDomainZoneInfo( crossing_particle.zone, 
                               next_zone,
                               face,
                               crossing_particle.X,
                               newDomainZoneInfo );

    buffered_particle_type new_particle( crossing_particle,
                                         newDomainZoneInfo );

#ifdef USE_OPENMP
    std::vector<buffered_particle_type>& theBufferedParticles = mSharedBufferedParticleVector();
#pragma omp critical (AppendBufferParticle)
    {
        theBufferedParticles.push_back( new_particle );

        ++N_buffered;
    
        //
        // only the master thread is going to actually execute the send so
        // theBufferedParticle vector could get larger than max_buffer_size
        // until the master thread gets here to execute the sends
        //
        if( theBufferedParticles.size() >= max_buffer_size )
        {
            send_particles();
        }    
    }// end of critical section


#else
    //
    // non-OPENMP case
    //
    buffered_particles.push_back( new_particle );
    ++N_buffered;

    if( buffered_particles.size() == max_buffer_size )
    {
        send_particles();
    }
#endif

    

}

//---------------------------------------------------------------------------//
#ifdef USE_OPENMP
template<typename Mesh_type,typename Particle_type>
std::vector<buffered_particle<Particle_type,typename Mesh_type::newDomainZoneIDType> >&  
Domain_exchange<Mesh_type,Particle_type>::
mSharedBufferedParticleVector( )
{
    static std::map<int, std::vector<buffered_particle_type> > sSharedBufferedParticles;

    return sSharedBufferedParticles[other_procID];
            
}

//---------------------------------------------------------------------------//
template<typename Mesh_type,typename Particle_type>
size_t&  
Domain_exchange<Mesh_type,Particle_type>::
mSharedCombinedBufferSize( )
{
    static size_t sSharedCombinedBufferSize;
    return sSharedCombinedBufferSize;
}
#endif
//---------------------------------------------------------------------------//

/*!  The number of particles and the particle data are packed into one
  array for communication.  The layout is:

  [ unsigned int with particle count, particle 0, particle 1, ..., particle N-1 ]

  \todo Consider using a slightly different format of creating 
  one extra dummy particle and use its storage for the particle count.
  This would avoid the memory copy, you'd also have to make sure
  buffered_particles.size() == max_buffer_size + 1 always.
  */

template<typename Mesh_type,typename Particle_type>
void Domain_exchange<Mesh_type,Particle_type>::
send_particles( )
{
    ASSERT( this_procID >= 0 );
    ASSERT( other_procID >= 0 );

    int tid=0, numOmpThreads=1, mpi_rank=0;
#ifdef USE_MPI
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
#endif

#ifdef USE_OPENMP
    tid= omp_get_thread_num();
#endif

#ifdef USE_OPENMP
    //
    // if we're running multiple threads we have accumulated each
    // thread's list of buffered particles into the shared buffer
    // which the master thread will now send 
    //

    //
    // only the master thread does the communication
#pragma omp master
    {
        //
        // grab the shared vector 
        std::vector<buffered_particle_type>& theSharedBufferedParticles = mSharedBufferedParticleVector();
 
        unsigned int N_remaining = theSharedBufferedParticles.size();
        
        size_t theStartIndex=0;
        
        // Don't bother sending if no data to send.
        while( N_remaining > 0 )
        {
            //
            // we send messages in chunk sizes of max_buffer_size at a 
            // time.  We repeat until the shared buffer is empty.
            //

            unsigned int N_send = min( N_remaining , max_buffer_size );
            
            //
            // copy this block of the shared buffer vector into the sending vector
            //
            for(size_t i=theStartIndex;i<theStartIndex+N_send;i++)
            {
                buffered_particles.push_back( theSharedBufferedParticles[i] );
                const Particle_type &theParticle = buffered_particles[i-theStartIndex];

                
                if( !theParticle.consistent() )
                {
                    ASSERT( theParticle.consistent() );
                }
            }

            ++N_buffers_sent;

            // Copy the first photon to the end.  We're going to use the first 
            // particle's space to send the number of particles.
            buffered_particles.push_back( buffered_particles[0] );

            // Get storage for buffer
            char* data = reinterpret_cast<char*>(&(buffered_particles[0]));


            // Pack length of data onto array
            ASSERT( N_send <= max_buffer_size );
            
            *(reinterpret_cast<unsigned int*>(data)) = N_send;

            // Copy particles to this data.  This could be avoided if we added
            // an extra particle to the end of real data, then do type cast tricks
            // to put the count in the last "photon's" storage location.
            int bytes_to_send =  (N_send+1) * sizeof(buffered_particle_type);
            ASSERT( bytes_to_send <= mpi_buffer_size );


#ifdef USE_MPI
            buffer.send( data,
                         bytes_to_send,
                         MPI_CHAR,
                         other_procID,
                         tag,
                         Mesh.getWorldCommunicator() );
#endif


            //
            // is this even needed since check_and_free are called at the end 
            // of buffer.send() ?
            //
            buffer.check_and_free();
            
            // reduce N_remaining by the amount we just sent
            N_remaining -= N_send;
            
            // increment the starting index of the next portion of 
            // theSharedBufferedParticles we still need to send
            theStartIndex+=N_send;

            //
            // clear the local send vector
            //
            buffered_particles.clear();
            
        }//end of N_remaining > 0 condition        
  
        //
        // clear out the already sent shared buffered particle vector
        //
        theSharedBufferedParticles.clear();

    } //end of omp master section

#else 
    //
    // non OPENMP case (original MPI-only code)
    //
    unsigned int N_send = buffered_particles.size();

    // Don't bother sending if no data to send.
    if( N_send == 0 )
       return;

    ++N_buffers_sent;

    // Copy the first photon to the end.  We're going to use the first 
    // particle's space to send the number of particles.
    buffered_particles.push_back( buffered_particles[0] );

    // Get storage for buffer
    char* data = reinterpret_cast<char*>(&(buffered_particles[0]));

    // Pack length of data onto array
    ASSERT( N_send <= max_buffer_size );
    *(reinterpret_cast<unsigned int*>(data)) = N_send;

    // Copy particles to this data.  This could be avoided if we added
    // an extra particle to the end of real data, then do type cast tricks
    // to put the count in the last "photon's" storage location.
    int bytes_to_send =  (N_send+1) * sizeof(buffered_particle_type);
    ASSERT( bytes_to_send <= mpi_buffer_size );

    buffer.send( data,
                 bytes_to_send,
                 MPI_CHAR,
                 other_procID,
                 tag,
                 Mesh.getWorldCommunicator() );

    buffered_particles.clear();

    buffer.check_and_free();

#endif // ifdef USE_OPENMP

}

//---------------------------------------------------------------------------//

template<typename Mesh_type, typename Particle_type>
void Domain_exchange<Mesh_type,Particle_type>::
setUp( )
{
   int retval=0;
#ifdef USE_MPI

#ifdef USE_OPENMP
#pragma omp master
#endif
   {
       ASSERT( receive_request != 0 );
    
       retval = MPI_Irecv( receive_buffer,
                           mpi_buffer_size,
                           MPI_CHAR,
                           other_procID,
                           tag,
                           Mesh.getWorldCommunicator(),
                           receive_request );
   }//end of omp master section
#endif
    CONTRACT_VAR(retval);
    ASSERT( retval == MPI_SUCCESS );
}

//---------------------------------------------------------------------------//

/*! Check to make sure there are no outstanding messages, then
  release the nonblocking receive.
  */

template<class Mesh_type,class Particle_type>
void Domain_exchange<Mesh_type,Particle_type>::
finalize( )
{ 
   // Make sure there are no particles to send
   ASSERT( buffered_particles.size() == 0 );

   // Make sure there are no messages we haven't received.
#ifndef NOASSERT

#ifdef USE_OPENMP
#pragma omp master
#endif
   {
       MPI_Status stat;
       
       int done;
       
       // Have we received anything?
       int retval = MPI_Test( receive_request, &done, &stat);
       CONTRACT_VAR(retval);
       ASSERT( retval == MPI_SUCCESS );
       
       ASSERT( !done );
   }//end of omp master section
#endif
    
#ifdef USE_OPENMP
#pragma omp master
#endif
   {
       // Maybe move this up to Domain_Source?
       if( *receive_request != MPI_REQUEST_NULL)
       {
           int retval = MPI_Cancel( receive_request );
           CONTRACT_VAR(retval);
           ASSERT( retval == MPI_SUCCESS );
       }
       
       buffer.wait_until_all_done();
   }//end of omp master section

   receive_request = 0;
}

//---------------------------------------------------------------------------//

/*! Processes incoming message.  Somebody else, who owns the MPI_Request,
  must check to make sure we've received it before calling this function.
  The particle zones decoded, and then the particle is added to the particle_list
  buffer.

  Once this is complete, another nonblocking receive is initialized.

  */

template<class Mesh_type,class Particle_type>
void Domain_exchange<Mesh_type,Particle_type>::
get_and_add_particles( list<Particle_type>& particle_list, 
                       bool allow_inconsistent_zone_and_location )
{ 

    ASSERT( this_procID >= 0 );
    ASSERT( other_procID >= 0 );

    int tid=0, mpi_rank=0;
#ifdef USE_MPI
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
#endif

#ifdef USE_OPENMP
    tid= omp_get_thread_num();
#endif

#ifdef USE_OPENMP
#pragma omp master
#endif
    {
        // Free any memory from sent messages.
        buffer.check_and_free();

        // Get particle count
        unsigned int N_in = *(reinterpret_cast<unsigned int*>(receive_buffer));

        N_received += N_in;
        ++N_buffers_received;

        // Unpack particle array.
        buffered_particle_type* p_recv;
        p_recv = reinterpret_cast<buffered_particle_type*>(receive_buffer);
        // Skip the first one since we used it for storage for N_in
        p_recv++;

        for( unsigned int i = 0; i < N_in; ++i )
        {
            
            buffered_particle_type &thisPart = p_recv[i];
            
            //    If zone has been sent over only partially set, make it all
            //    correct here
            Mesh.correctNewDomainZone( p_recv[i].newDomainZoneInfo,
                                       p_recv[i].X,
                                       p_recv[i].zone );

            ASSERT( Mesh.zone_is_real(p_recv[i].zone) );

            //    check that particle is in zone we think it is
            if (!allow_inconsistent_zone_and_location)
            {
                ASSERT( Mesh.pointIsInParticleZone( p_recv[i].X, p_recv[i].zone) );
            }


            // take the buffered_particle_type vector, make a Particle_type
            // out of each one, and put it onto particle_list
            Particle_type new_particle = p_recv[i];  //  slice


            if( !new_particle.consistent() )
            {
                ASSERT( new_particle.consistent() );
            }
            
            particle_list.push_back( new_particle );
        }
    
    }//end of omp master section

    // Start a new nonblocking receive.
    setUp();

}

//---------------------------------------------------------------------------//

//! Debugging information.

template<typename Mesh_type, typename Particle_type>
void Domain_exchange<Mesh_type,Particle_type>::
print_DE_info() const
{
   cout << "  address = " << this;
   cout << "; this_procID = ";
   cout << this_procID;
   cout << "; other_procID = ";
   cout << other_procID << endl;
   cout << endl;
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace








