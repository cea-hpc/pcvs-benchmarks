/****************************************************************************/
/*                                                                          */
/*                         Copyright or (C) or Copr.                        */
/*       Commissariat a l'Energie Atomique et aux Energies Alternatives     */
/*                                                                          */
/* Version : 2.0                                                            */
/* Date    : Tue Jul 22 13:28:10 CEST 2014                                  */
/* Ref ID  : IDDN.FR.001.160040.000.S.P.2015.000.10800                      */
/* Author  : Julien Adam <julien.adam@cea.fr>                               */
/*           Marc Perache <marc.perache@cea.fr>                             */
/*                                                                          */
/* This file is part of JCHRONOSS software.                                 */
/*                                                                          */
/* This software is governed by the CeCILL-C license under French law and   */
/* abiding by the rules of distribution of free software.  You can  use,    */
/* modify and/or redistribute the software under the terms of the CeCILL-C  */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info".                                                */
/*                                                                          */
/* As a counterpart to the access to the source code and  rights to copy,   */
/* modify and redistribute granted by the license, users are provided only  */
/* with a limited warranty  and the software's author,  the holder of the   */
/* economic rights,  and the successive licensors  have only  limited       */
/* liability.                                                               */
/*                                                                          */
/* In this respect, the user's attention is drawn to the risks associated   */
/* with loading,  using,  modifying and/or developing or reproducing the    */
/* software by the user in light of its specific status of free software,   */
/* that may mean  that it is complicated to manipulate,  and  that  also    */
/* therefore means  that it is reserved for developers  and  experienced    */
/* professionals having in-depth computer knowledge. Users are therefore    */
/* encouraged to load and test the software's suitability as regards their  */
/* requirements in conditions enabling the security of their systems and/or */
/* data to be ensured and,  more generally, to use and operate it in the    */
/* same conditions as regards security.                                     */
/*                                                                          */
/* The fact that you are presently reading this means that you have had     */
/* knowledge of the CeCILL-C license and that you accept its terms.         */
/*                                                                          */
/****************************************************************************/

#ifndef SCHEDTIMEPOLICY_H
#define SCHEDTIMEPOLICY_H

#include "Policy.h"

/// Policy implementation for the "scheduling by Time" policy
/**
 * Implements specific behaviour routines
 * In this policy, jobs are grouped by the fact : "the biggest first". Thus, each
 * worker is filled with various jobs and the limit is reached when maximum
 * execution time for one worker is reached. The number of resources associated
 * to the worker depends on number of workers allowed to run at the same time.
 * \see Policy
 */
class SchedTimePolicy : public Policy
{
private:
	/************** STATICS **************/
	/**** NON-CONST ****/
	static double curJobManSurface;  ///< Current remaining working surface
	static int nbCalls;              ///< number of calls to fillingAlgo in this class

	/************* FUNCTIONS *************/
	/// recompute current working surface contained in job manger
	/**
	 * Update static variable curJobManSurface
	 * \param[in] currentJM the jobs lists
	 * \param[in] nbLists number of lists
	 */
	static void recomputeCurSurface(std::list < Job * >*currentJM, size_t nbLists);
	    
public:	
	/************* FUNCTIONS *************/
	/// create a new "scheduling by time" policy
	SchedTimePolicy(JobManager* job, Configuration* config);
	virtual void fillingAlgo(Worker * cur, size_t maxIndice,
			     size_t nbCurResources);

};

#endif // SCHEDTIMEPOLICY_H
