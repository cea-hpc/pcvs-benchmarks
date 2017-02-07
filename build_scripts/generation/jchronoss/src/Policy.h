/****************************************************************************/
/*                                                                          */
/*                         Copyright or (C) or Copr.                        */
/*       Commissariat a l'Energie Atomique et aux Energies Alternatives     */
/*                                                                          */
/* Version : 1.2                                                            */
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

#ifndef POLICY_H
#define POLICY_H
#include "JobManager.h"
#include "Worker.h"
#include "ResourcesManager.h"

/// Defines policy implementation structure for derived classes
/**
 * Policy class is an abstract class which is used as a template to add his own scheduling policies to
 * the application. Some function are required by derived class in order to be used as a policy.
 */
class Policy
{
protected:
	/************** MEMBERS **************/
	JobManager *jobMan;      ///< the referent Job manager
	Configuration *config;   ///< the global configuration
public:
	/************* FUNCTIONS *************/
	/**** ABSTRACTS ****/
	/// create a policy, setting class members
	Policy(JobManager* job, Configuration* config);
	/// defining how to fill a worker according to the policy
	/**
	 * The function have to fill the worker with jobs from job manager
	 * \warning Each derived class have to implement this function !
	 * \warning Do not try to take jobs from jobs list in a list where indice is larger than maxIndice !
	 * \param[in] cur the current worker to fill
	 * \param[in] maxIndice the max jobs list indice where jobs can be taken
	 * \param[in] nbCurResources current number of availables resources (used in some policies)
	 */
	virtual void fillingAlgo(Worker *cur, size_t maxIndice, size_t nbCurResources) = 0;
	///virtual destructor to unset a policy
	virtual ~Policy();
};

#endif // POLICY_H
