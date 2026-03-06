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

#ifndef RELATIVES_HH
#define RELATIVES_HH

#include <vector>
#include <cmath>
#include <algorithm>
#include <functional>
#include "ASSERT.hh"

namespace IMC_namespace{

   /*! \brief Computes parent and children nodes in a minium spanning tree.

     This code was taken from, then modified from mpich-1.2.6/src/coll/intra_fns_new.c
     in the intra_Reduce() and intra_Bcast() functions.   It uses the algorithm 
     by Rolf Rabenseifner, "Optimization of Collective Reduction Operations",
     International Conference on Computational Science, June 7-9, Krakow,
     Poland, 2004.  This algorithm is better than a straight binary tree for
     reduce operations for long messages.  Since it has a shorter tree
     depth than, it should also be good for the asynchronous communications that
     we want to use it for.  intra_Bcast() in this file also has the same
     algorithm for small messages.  It calls this a Minimum Spanning Tree.
     It's coding was a little easier to understand, so this looks a little
     bit more like it.

     The algorithm has been modified slightly so that there is no option to have
     a root other than 0.  It looks like a lot of work for something we're never
     going to use.

     The input parameters are rank and size, which should be values returned by
     MPI_Comm_rank() and MPI_Comm_size(), respectively.  Output is the
     parent node number, and a vector of children nodes.  The children node
     vector is cleared on input and sorted in reverse order on output.
     When using these values for broadcasts, the largest child should
     be sent to first.  (The largest child  has the most children of its
     own to broadcast to.)  When receiving from the children, the lowest
     numbered ones should be received from first.  The reverse sort is optimized
     for broadcasts.  If there is no parent node, it will return -1 in parent.

     What follows are the comments taken straight from mpich.  The intra_Bcast()
     function has a different set of comments that may make more sense to you.

     ----------------------------------------------------------------

     Here's the algorithm.  Relative to the root, look at the bit pattern in 
     my rank.  Starting from the right (lsb), if the bit is 1, send to 
     the node with that bit zero and exit; if the bit is 0, receive from the
     node with that bit set and combine (as long as that node is within the
     group).

     Note that by receiving with source selection, we guarantee that we get
     the same bits with the same input.  If we allowed the parent to receive 
     the children in any order, then timing differences could cause different
     results (roundoff error, over/underflows in some cases, etc).

     Because of the way these are ordered, if root is 0, then this is correct
     for both commutative and non-commutative operations.  If root is not
     0, then for non-commutative, we use a root of zero and then send
     the result to the root.  To see this, note that the ordering is
     mask = 1: (ab)(cd)(ef)(gh)            (odds send to evens)
     mask = 2: ((ab)(cd))((ef)(gh))        (3,6 send to 0,4)
     mask = 4: (((ab)(cd))((ef)(gh)))      (4 sends to 0)

     */

     inline void relatives_mst( const int rank,
                                const int size,
                                int& parent,
                                std::vector<int>& children )
     {

        ASSERT(rank >= 0);
        ASSERT(size > 0);
        ASSERT(rank < size);

        // Estimate the maximum number of children processors.
        // This is only an estimate due to roundoff...
        unsigned int max_connections = 0;
        if(size > 1){
           double mc = std::log(size - 1.0)/std::log(2.0);
           max_connections = static_cast<unsigned int>(mc) + 1;
        }

        // Clear the vector, and reserve space.
        children.clear();
        children.reserve( max_connections );

        // -----------------------------------------------
        // Compute parent first.

        // Flag if no parents, should the case for root==0 only;
        parent = -1;

        int mask = 0x1;
        while (mask < size) {
           if (mask & rank) {
              parent = ((rank & (~ mask))) % size;
              break;
           }
           mask <<= 1;
        }

        // Make sure we have a parent if we're not the root of the tree.
        ASSERT( (rank==0 && parent == -1) || (rank!=0 && parent >= 0) );

        // ---------------------------------------------
        // Calculate the children.

        mask >>= 1;
        while (mask > 0) {
           if (mask + rank < size) {
              int child = rank + mask;
              ASSERT( child >= 0 );
              ASSERT( child < size );
              children.push_back(child);
           }
           mask >>= 1;
        }

        // Reverse sort, which is optimized for broadcasts
        std::sort( children.begin(), children.end(), std::greater<int>() );

     }

     //----------------------------------------------------------------------

     /*! \brief Computes parent and children nodes in binary tree.

       A binary tree.   The root is node zero.  The parent is given
       by floor((rank-1)/2).  The children are given by 2*rank+1
       and 2*rank+1, if these ranks are less than size.

       See http://en.wikipedia.org/wiki/Binary_tree for more details.
      */

     inline void relatives_binary( const int rank,
                                   const int size,
                                   int& parent,
                                   std::vector<int>& children )
     {

        ASSERT(rank >= 0);
        ASSERT(size > 0);
        ASSERT(rank < size);

        parent = (rank-1)/2;
        int left = 2*rank + 1; 
        int right = 2*rank + 2;


        if( rank == 0 ){
           parent = -1;
        }

        children.clear();
        if(left < size)
           children.push_back(left);
        if(right < size)
           children.push_back(right);

     }

     //----------------------------------------------------------------------

     /*! \brief Computes parent and children nodes in collective communications.

       This is a really fat tree, in which all processors have a parent of zero and
       processor zero has children of all other processors.

      */

     inline void relatives_fat( const int rank,
                                const int size,
                                int& parent,
                                std::vector<int>& children )
     {

        ASSERT(rank >= 0);
        ASSERT(size > 0);
        ASSERT(rank < size);

        if( rank != 0 ){
           parent = 0;
           children.clear();
        } else {
           parent = -1;
           children.resize(size-1);
           for(int i=1; i<size; ++i)
              children[i-1]=i;
        }

     }

     //----------------------------------------------------------------------
     //! \brief The default tree.  Use this function.
     //!   Currently an alias to relatives_binary().

     inline void relatives( const int rank,
                            const int size,
                            int& parent,
                            std::vector<int>& children ){

        ASSERT(rank >= 0);
        ASSERT(size > 0);
        ASSERT(rank < size);

        relatives_binary(rank, size, parent, children);
     }

}

#endif // RELATIVES_HH

