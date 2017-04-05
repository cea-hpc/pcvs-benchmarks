#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"
#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdlib>
#include <sys/time.h>
#include <cassert>
#include <sched.h>
#include <stdio.h>

#define DEFAULT_NB_CELLS 1000
#define DEFAULT_NB_LOOPS 100
struct mytask {
  mytask(size_t n)
    :_n(n)
  {}
  void get() {
	for (int i=0;i<1000000;++i) {_n = 1;}  // Deliberately run slow
  }

  size_t getN() {return _n;}
  size_t _n;
};

tbb::atomic<size_t> nbCalls;
struct executor
{
  executor(std::vector<mytask>& t)
    :_tasks(t)
  {}

  void operator()(const tbb::blocked_range<size_t>& r) const {
	nbCalls++;
	size_t begin = r.begin();
	size_t end = r.end();
    for (size_t i=begin;i<end;++i){
	  _tasks[i].get();
	 }
  }

  std::vector<mytask>& _tasks;
};
 std::vector<mytask> tasks, tasks2;
int main(int argc,char** argv) {

  size_t nb_cells, nb_loops;
  struct timeval pst, pend, pres, sst, send, sres;
 
  nb_loops = DEFAULT_NB_LOOPS;
  nb_cells = (argc > 1) ? atoi(argv[1]) : DEFAULT_NB_CELLS;
  //// INIT TAB
  for (int i=0;i<nb_cells;++i)
  {
    tasks.push_back(mytask(i));
	tasks2.push_back(mytask(i));
  }
  // CREATE TBB ROOT OBJECT
  executor exec(tasks);
  
  //RUN PARALLEL FOR
  std::cerr << "Running Parallel for...";
  gettimeofday(&pst, NULL);
  tbb::parallel_for(tbb::blocked_range<size_t>(0,tasks.size()),exec);
  gettimeofday(&pend, NULL);
  timersub(&pend, &pst, &pres);
 
  return 0;
}
