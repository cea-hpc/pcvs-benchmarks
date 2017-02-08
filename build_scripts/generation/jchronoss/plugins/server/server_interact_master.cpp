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
#include "server_types.h"
#include "server_interact_master.h"

extern char server_hostname[];
extern int server_port;
extern int frontend_port;
extern data_t run_config;

void master_cast_infos(char * name, int port){

	char * config_str = NULL;
	size_t str_len = 0;
	int sock = socket(AF_INET, SOCK_STREAM, 0);
	struct sockaddr_in address;
	struct hostent * host;

	assert(name);
	if((host = gethostbyname(name)) == NULL)
	{
		perror("Master gethostbyname");
		abort();
	}

	bzero(&address, sizeof(sockaddr_in));
	address.sin_family = AF_INET;
	address.sin_port = htons(port);
	address.sin_addr = * (struct in_addr*)host->h_addr; 

	if(connect(sock, (struct sockaddr*)&address, sizeof(struct sockaddr)) != 0)
	{
		perror("Master connect");
		abort();
	}
	str_len = strlen(server_hostname) + 1;
	/* Send the log server info: host & port */
	safe_send(sock, &str_len, sizeof(size_t));
	safe_send(sock, server_hostname, str_len);
	safe_send(sock, &server_port, sizeof(int) );
	safe_send(sock, &frontend_port, sizeof(int) );

	/* Retrieve the complete configuration */
	safe_recv(sock, &str_len, sizeof(size_t));
	config_str = (char*)malloc(sizeof(char) * (str_len));
	safe_recv(sock, config_str, str_len);
	close(sock); /* maybe not */

	run_config.data = (void*)config_str;
	run_config.data_len = str_len;
}
