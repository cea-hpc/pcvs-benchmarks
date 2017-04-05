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
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cmath>
#include <map>
#include <vector>

#include "relatives.hh"

using namespace std;

/*! \brief Unit test for broadcast or reduce map.

  This function tests all connections from the function
  IMC_namespace::relatives() to make sure they are all consistent.
  The only checks this does are to make sure that connection graph makes
  sense from an MPI point of view.  It passes no judgment if the 
  connection graph is efficient.  This checks

  -# Exactly one send connection from each node
  -# Each send has a matching receive
  -# Root of tree is zero
  -# Parents and children all numbered between [0,size).

  The function returns true if all tests have passed, false otherwise.
  The connection graph is output the the outstream for use with dot,
  which is part of the graphviz package from AT&T.  If the tests fail,
  the dot output has all the error messages and the background is red.
  If everything is good, there is a message to that effect, and the
  background is white.  
*/
  
bool relative_test( int size, ostream& os ){

   // Don't do anything if size is too small;
   if(size < 1)
      return true;

   // Set up dot (graphviz) header
   os << "digraph MST {\n";
   os << "rankdir = LR\n";
   os << "node [color=black,fillcolor=yellow,fontcolor=black,style=filled]\n";

   // Assume we pass until we notice otherwise.
   bool pass = true;

   // Build connection maps from the perspective of parents and children.
   // They should be equal so that we don't get any lost messages.
   multimap<int,int> parents;
   multimap<int,int> kids;

   // Make sure each processor is in the above maps exactly once (except for root).
   vector<int> count(size,0);

   // More dot header info
   for(int i=0; i< size; ++i){
      os << "node" << i << "[label = \""<<  i << "\"];\n";
   }

   // Used for informational purposes only.  Keeps track of the maximum
   // number of connection to any given node.
   unsigned int max_con = 0;

   for(int rank = 0; rank < size; ++rank){

      vector<int> children;
      int parent;

      IMC_namespace::relatives(rank, size, parent, children);

      if(children.size() > max_con)
         max_con = children.size();

      // If we have a parent, add it to the list and dot output.
      if( parent != -1){
         pair<const int, int> p(rank,parent);
         parents.insert(p);

         os << "\"node" << rank << "\" -> \"node" << parent << "\";\n";
      }

      // Make sure only rank zero is root.
      if( parent == -1 && rank != 0){
         os << "fail0[label=\"FAIL: rank " << rank <<" does not have a parent.\",shape=box,fillcolor=white];\n";
         pass = false;
      }
      if( parent != -1 && rank == 0){
         os << "fail1[label=\"FAIL: rank " << rank <<" has a parent " << parent << ".\",shape=box,fillcolor=white];\n";
         pass = false;
      }

      for(unsigned int j = 0; j<children.size(); ++j ){

         if( children[j] < 0 ){
            os << "fail2[label=\"FAIL: rank " << rank <<" has a negative child " <<  children[j] 
               << ".\",shape=box,fillcolor=white];\n";
            pass = false;
         }

         if( children[j] >= size ){
            os << "fail3[label=\"FAIL: rank " << rank <<" has a child (" << children[j] <<")  bigger than " <<  size
               << ".\",shape=box,fillcolor=white];\n";
            pass = false;
         }

         pair<const int, int> c(children[j],rank);
         kids.insert(c);
         count[children[j]]++;
      }
   }

   if( kids != parents ){
      os << "fail4[label=\"FAIL: kids != parents\",shape=box,fillcolor=white];\n";
      cerr << "FAIL: kids != parents\n";
      pass = false;
   }

   if( count[0] != 0 ){
         pass = false;
         cerr << "Node 0 in children list " << count[0] << "times\n";
         os << "fail5a[label=\"FAIL: connection count incorrect for rank 0: " << count[0] 
            << " \",shape=box,fillcolor=white];\n";
   }

   for(int i=1; i<size; ++i)
      if( count[i] != 1 ){
         pass = false;
         cerr << "Node " << i << " in children list " << count[i] << "times\n";
         os << "fail5[label=\"FAIL: connection count incorrect for rank " << i << ": " << count[i] 
            << " \",shape=box,fillcolor=white];\n";
      }

   if(pass){
      os << "good[label=\"PASS: All tests passed.\",shape=box,fillcolor=white];\n";
      // output below, don't need it here.
      // cerr << "PASS: All tests passed.\n";
   } else{
      os << "bgcolor=red;\n";
   }

   os << "conn[label=\"Maximum connections " << max_con <<"\",shape=box,fillcolor=white];\n";

   // If we didn't pass, also put on children to parent arrows.  This aids debugging,
   // but is ugly if we did pass.
   if(!pass){
      for(int rank = 0; rank < size; ++rank){

         vector<int> children;
         int parent;

         IMC_namespace::relatives(rank, size, parent, children);

         for(unsigned int j = 0; j<children.size(); ++j ){
            os << "\"node" << rank << "\" -> \"node" << children[j] << "\";\n";
         }
      }
   }

   os << "}\n";

   return pass;
}

int main ( int argc, char** argv ){

   if( argc == 2 ){
      int size = atoi(argv[1]);
      return relative_test(size, cout);
   }

   vector<int> proc_list;
   for(int i=1; i<257; ++i){
      proc_list.push_back(i);
   }
   proc_list.push_back(1023);
   proc_list.push_back(1024);
   proc_list.push_back(1025);
   proc_list.push_back(10000);

   bool all_passed = true;

   for(unsigned int s = 0; s < proc_list.size(); ++s){

      ostringstream file_name;
      file_name << "relatives_" << proc_list[s] << ".dot";

      ofstream os(file_name.str().c_str());

      cout << "Running size " << proc_list[s] << "... ";
      bool passed = relative_test(proc_list[s], os);
      if(passed)
         cout << "PASS!\n";
      else{
         all_passed = false;
         cout << "FAILED!\n";
      }

      os.close();

   }

   if(all_passed)
      cout << "All tests PASSED.\n";
   else
      cout << "At least one test FAILED.\n";

   return all_passed;

}
