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

#ifndef _SERVER_TYPES_H_
#define _SERVER_TYPES_H_

#include <stdlib.h>
#include <pthread.h>
#include <list>
#define HOSTNAME_SIZE (1024)
#define RANDOM_PORT_NUMBER 0
#define MAX_REQUEST_SIZE 1024

#ifdef NDEBUG
#define LWS_LOG_LEVEL LLL_ERR
#else
#define LWS_LOG_LEVEL LLL_ERR | LLL_WARN 
#endif

/**
 * A chunk of data
 */
typedef struct data_s
{
	void* data;      /**< data pointer */
	size_t data_len; /**< data length */
} data_t;

/**
 * class derived from list<data_t>, containing a lock to ensure thread-safety
 */
class safe_list : public std::list<data_t>
{
	private:
		pthread_mutex_t mlock; /**< the lock associated to the list */
	public:
		/// default constructor 
		safe_list() {pthread_mutex_init(&mlock, NULL);}
		/// take the lock for the list
		void lock() {pthread_mutex_lock(&mlock);}
		/// relase the lock over the list
		void unlock() {pthread_mutex_unlock(&mlock);}
		/// test if the lock can be taken. If so, takes it.
		bool trylock() {return pthread_mutex_trylock(&mlock) == 0;}
};

#endif
