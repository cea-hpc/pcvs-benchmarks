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
#include "json.h"
#include "server_types.h"
#include "defines.h"
#include "server_interact_master.h"
#include "server_interact_slaves.h"
#include "server_interact_ws.h"
#include <ev.h>
#include <assert.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>
#include <netdb.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <strings.h>
#include <netinet/in.h>
#include <list>
#include <map>

char server_hostname[HOSTNAME_SIZE];
int server_port;
int frontend_port;
int server_sock;
data_t run_config;
safe_list slaves_data;

void server_trigger_signal(struct ev_loop*loop, ev_signal *w, int revents)
{
	assert(loop);
	assert(w);
	if(revents != EV_SIGNAL)
	{
		fprintf(stderr, "server_trigger_signal should not raise any event but EV_SIGNAL");
	}
	ev_break(loop, EVBREAK_ALL);
	close(server_sock);
	ws_stop();
	//fprintf(stderr, "Stopping the server (handling signal %d)\n", w->signum);
	exit(0);
}

int server_init_socket()
{
	int sock;
	struct sockaddr_in addr;
	socklen_t len = sizeof(struct sockaddr_in);
	
	/* building the socket */
	if((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) 
	{
		perror("Server socket");
		abort();
	}

	bzero(&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(RANDOM_PORT_NUMBER);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);

	/* attaching the address space to the socket */
	if(bind(sock, (struct sockaddr*)&addr, sizeof(struct sockaddr_in)) != 0)
	{
		perror("Server bind");
		abort();
	}

	/* starting to listening for connection requests */
	if(listen(sock, 128) < 0) 
	{
		perror("Server listen");
		abort();
	}

	if (getsockname(sock, (struct sockaddr *)&addr, &len) == -1)
	{
		perror("Servert getsockname");
		abort();
	}

	gethostname(server_hostname, HOSTNAME_SIZE);
	server_port = ntohs(addr.sin_port);
	
	return sock;
}

void * server_listen_frontends(void * arg)
{
	UNUSED(arg);
	//fprintf(stderr, "Server launching the libwebsockets interface\n");
	ws_start();
	return NULL;
}

void * server_listen_backends(void * arg)
{
	struct ev_loop* looper;
	UNUSED(arg);
	//fprintf(stderr, "Server launching the libev over slave connections\n");
	
	ev_io accept_watcher;
	ev_signal signaler, signaler2;

	looper = ev_default_loop(ev_recommended_backends());
	
	ev_signal_init(&signaler, server_trigger_signal, SIGTERM);
	ev_signal_init(&signaler2, server_trigger_signal, SIGUSR1);
	ev_signal_start(looper, &signaler);
	ev_signal_start(looper, &signaler2);

	/* Starting the accept() connexion request watcher */
	ev_io_init(&accept_watcher, slave_accept, server_sock, EV_READ);
	ev_io_start(looper, &accept_watcher);

	ev_run(looper,0);
	return NULL;
}

int main(int argc, char *argv[])
{	
	server_sock = server_init_socket();
	ws_init();

	/* if args, we should send back our net adress to the given socket */
	if(argc > 1)
	{
		master_cast_infos(argv[1], atoi(argv[2]));
	}
	//else
	//{
		//fprintf(stderr, "Server running at %s:%d (PID %d)\n", server_hostname, server_port , getpid());
	//}

	/** STARTING THE THREADS: One for slave connections, one for websockets */
	pthread_t t[2];
	pthread_create(t, NULL, server_listen_frontends, NULL);
	pthread_create(t+1, NULL, server_listen_backends, NULL);

	pthread_join(t[0], NULL);
	pthread_join(t[1], NULL);

	return 0;
}
