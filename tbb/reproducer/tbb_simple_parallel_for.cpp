#define _GNU_SOURCE
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

#define NB_COLORS 3UL
#define DEFAULT_NB_CELLS 1000
#define DEFAULT_NB_LOOPS 1000000
#define pad(u) std::right << std::setfill('0') << std::setw(u)

const char * tab_colors[] ={"\033[0;32m", "\033[0;33m", "\033[0;31m"};

void check_affinity(void)
{
	cpu_set_t set;
	CPU_ZERO(&set);
	sched_getaffinity(0, sizeof(cpu_set_t), &set);
	for(int i=0; i < sizeof(cpu_set_t); i++)
	{
		if(CPU_ISSET(i, &set))
		{
			fprintf(stderr, "A thread is bound to core %d\n", i);
		}
	}
}
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
	int col = std::min(r.size() / 4, NB_COLORS -1 );
	//std::cerr << tab_colors[col] << "[" << r.size() << "]\033[0;0m\t";
	size_t begin = r.begin();
	size_t end = r.end();
    for (size_t i=begin;i<end;++i){
	  _tasks[i].get();
	 }
	//check_affinity();
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
  std::cerr << "\rRunning Parallel for : Done !";

  std::cerr << std::endl;

  tbb::task_scheduler_init init(4);

  // RUN SEQUENTAIL FOR
  std::cerr << "Running Sequential for...";
  gettimeofday(&sst, NULL);
  for(int i=0; i <nb_cells; ++i)
  {
     tasks2[i].get();
  }
  gettimeofday(&send, NULL);
  std::cerr << "\rRunning Sequential for : Done !";
 
  std::cerr << std::endl;

  // ASSERTIONS
  std::cerr << "Checking matching between parallel and sequential..."; 
  for(int i=0; i < nb_cells; ++i)
  {
	  assert(tasks[i].getN() == tasks2[i].getN() && tasks[i].getN() == 1);
  }
  std::cerr << "\rChecking matching between parallel and sequential : Done !"; 
  
  timersub(&pend, &pst, &pres);
  timersub(&send, &sst, &sres);
 
  int seq = (sres.tv_sec*1000000) + (sres.tv_usec);
  int par = (pres.tv_sec*1000000) + (pres.tv_usec);
  float speedup = ((float)seq/(float)par);
  ////////// SUMMARY /////////
  std::cout << std::endl
	  << " * tabs size       : " << nb_cells << " cell(s)" << std::endl
	  << " * Simulation      : " << nb_loops << " loop(s) per chunk" << std::endl
	  << " * nb TBB calls    : " << nbCalls << std::endl
	  << " * Measured time (us)  |  |  |" << std::endl
	  << "   - PARALLEL_FOR  = " << pad(9) << par << std::endl
	  << "   - SEQUENTIAL    = " << pad(9) << seq << std::endl
      << " * Approx Speedup  : x" << std::setprecision(3) << speedup << std::endl
  ;
  return 0;
}
