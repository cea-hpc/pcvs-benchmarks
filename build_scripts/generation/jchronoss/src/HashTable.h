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

#ifndef HASHTABLE_H
#define HASHTABLE_H
#include "utils.h"
#include "Worker.h"
/// class gathering runnning worker into an hash table with tuple (pid, worker*)
/**
 * When a worker is launched, the master needs to wait the worker ends. it stores worker information
 * in this table. When worker ends, the master looks for matching worker in the table and fill jobs with
 * results found in binary file. This class just allows to manipulate the table.
 */
class HashTable {
private:
	/************** MEMBERS **************/
	///clas hashItem, which represents an entry in table
	/**
	 * An entry in table contains the process's pid where the worker is launched and and pointer
	 * on the worker. This class is a private class member, only the HashTable class should be
	 * able to manipulate it
	 */
	class HashItem {
	public:
		int pid;       ///< the pid for the matching launched worker
		Worker* work;  ///< a pointer to the worker
		/// construct a entry
		/**
		 * \param[in] p the pid
		 * \param[in] w the worker pointer
		 */
		HashItem(int p, Worker* w) : pid(p), work(w) {};
	};
	std::list<HashItem*> table;   ///< the workers table

	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// looks for a launched worker in the list from the given pid returned by wait
	/**
	 * \param[in] pid the pid attached to the worker when it have been launched
	 * \return <b>Iterator</b> on element if found
	 * \return <b>list.end() </b> otherwise
	 */
	std::list<HashItem*>::iterator find( int pid );
	
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// add an entry into the table
	/**
	 * Given arguments, a new entry is created in the table, the worker shouldn't be NULL
	 * \param[in] pid the pid attached to the worker
	 * \param[in] nw the worker pointer
	 */
	void add( int pid, Worker* nw );
	/// remove an entry according to the pid
	/**
	 * \param[in] pid the pid attached to the worker to remove
	 * \return <b>Worker pointer</b> if found in table
	 * \return <b>NULL</b> otherwise
	 */
	Worker* remove(int pid);
	///stop all running workers when a signal is caught
	void stopAll();
	/**
	 * generate jobs list from currently run jobs
	 * \return a jobs new pointer
	 */
	std::list<Job*>* getRunningJobs() const;

	/****** CONST ******/
	///check if table is empty (used for tests)
	/**
	 * \return <b>True</b> if list is empty
	 * \return <b>False</b> otherwise
	 */
	bool isEmpty() const;
};

#endif // HASHTABLE_H
