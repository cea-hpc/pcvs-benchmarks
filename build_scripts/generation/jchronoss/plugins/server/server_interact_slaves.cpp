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
#include "defines.h"
#include "server_types.h"
#include "server_interact_slaves.h"
#include <list>
#include <map>

extern safe_list slaves_data;

void slave_read(struct ev_loop* loop, ev_io* w, int evts)
{
	ssize_t nb;
	size_t sz = 0;

	assert(loop); UNUSED(loop);
	assert(w); UNUSED(w);
	/* check possible bad events */
	if(evts != EV_READ)
	{
		fprintf(stderr, "This event should not be raised for any event but EV_READ");
		abort();
	}

	/* reading */
	nb = safe_recv(w->fd, &sz, sizeof(sz));
	if( nb > 0)
	{
		data_t temp;
		temp.data_len = sz;
		temp.data = malloc(sz);
		safe_recv(w->fd, temp.data, sz);

		slaves_data.lock();
		slaves_data.push_back(temp);
		slaves_data.unlock();
	}
	else if(nb < 0) 
	{
		perror("slave content recv");
	}
	else if(nb == 0)
	{
		ev_io_stop(loop, w);
		free(w);
	}
}

void slave_accept(struct ev_loop* loop, ev_io* w, int evts)
{
	struct sockaddr_in addr;
	int client_socket;
	socklen_t len = sizeof(struct sockaddr_in);
	ev_io* client_worker = (ev_io*)malloc(sizeof(ev_io));
	assert(client_worker);

	assert(loop); UNUSED(loop);
	assert(w); UNUSED(w);
	/* check possible bad events */
	if(evts != EV_READ)
	{
		fprintf(stderr, "This event should not be raised for any event but EV_READ");
		abort();
	}

	/* accept new connection */
	if((client_socket = accept(w->fd, (struct sockaddr*)&addr, &len)) < 0)
	{
		perror("slave accept");
		abort();
	}

	/* create a new event tracker for this new socket */
	ev_io_init(client_worker, slave_read, client_socket, EV_READ );
	ev_io_start(loop, client_worker);
}
