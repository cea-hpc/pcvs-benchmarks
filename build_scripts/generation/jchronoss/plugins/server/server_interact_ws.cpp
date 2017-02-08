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
#include <stdio.h>
#include <iostream>
#include <map>
#include <list>
#include <assert.h>
#include <strings.h>
#include <string.h>
/* better to put it last beceause libwebsockets-generated warnings are skipped */
#include "server_interact_ws.h"
static volatile bool stop;
static volatile bool data_left;
static size_t nb_connections = 0;
static size_t nb_closed_connections = 0;
static lws_context* context;

extern char server_hostname[HOSTNAME_SIZE];
extern int frontend_port;
extern data_t run_config;
extern safe_list slaves_data;

/**
 * Callback handled by libwebsockets, called each time a new event is raised.
 *
 * @param[in] the currenly bound interface.
 * @param[in] reason the raised event
 * @param[in] user a pointer to an user-defined structure (a ws_per_sessions_data_t)
 * @param[in] in the received content from the client
 * @param[in] len size of received buffer
 * @returns always 0
 */
static int ws_callback(struct lws *wsi, enum lws_callback_reasons reason, void *user, void *in, size_t len)
{
	/* get back our own data */
	ws_per_session_data_t* pss = (ws_per_session_data_t*)user;
	//lwsl_warn("JCHRONOSS: CALLBACK_CALL\n");

	/* depending on the reason, we apply the associated response */
	switch(reason)
	{
		case LWS_CALLBACK_ESTABLISHED: /* a new client has been connected */
			pss->id = ++nb_connections;
			lwsl_warn("JCHRONOSS: %d - CONNECTION OPENING\n", pss->id);
			
			/* initialize the per session data structure : sending the configuration */
			pss->tosend = (char*)run_config.data;
			pss->len = run_config.data_len;
			pss->last = slaves_data.end();

			/* prepare the server to send a request (the config) */
			lws_callback_on_writable(wsi);
			break;

		case LWS_CALLBACK_SERVER_WRITEABLE: /* the server is ready to send contents */
			{
				lwsl_warn("JCHRONOSS: %d - DATA SENDING\n", pss->id);
				char * eos = (char*)"EOS";
				if(pss->tosend == NULL)
				{
					/* special case: send EOS token */
					if(pss->len == (size_t)-1)
					{
						pss->tosend = eos;
						pss->len = strlen(eos);
					}
					else /* request writeable() but nothing to send */
						break;
				}

				/* we have to copy the buffer in a larger memory buffer because
				 * libwebsockets ask for a memory space before the payload (header stuffs) */
				char* buffer = (char*)malloc(pss->len + LWS_SEND_BUFFER_PRE_PADDING);
				char *payload = buffer + LWS_SEND_BUFFER_PRE_PADDING;

				memcpy(payload, pss->tosend, pss->len);

				/* currently asking to lws to send the data */
				if((size_t)lws_write(wsi, (unsigned char*)payload, pss->len, LWS_WRITE_TEXT) != pss->len)
				{
					lwsl_err("JCHRONOSS: Unable to send data to Client %d\n", pss->id);
					abort();
				}
				/* reset: notify data has been sent */
				pss->tosend = NULL;
				pss->len = 0;
				free(buffer);
				break;
			}

		case LWS_CALLBACK_RECEIVE: /* a client sent a request */
			{
				lwsl_warn("JCHRONOSS: %d - DATA RECEIVING\n", pss->id);

				/* sanity check */
				if(strncmp((char*)in, "polling", len) != 0)
				{
					lwsl_err("JCHRONOSS: Does not handle request '%s'\n", (char*)in);
					abort();
				}
				
				/* if we don't have anything to sent (no collected data), stop here */
				if(slaves_data.size() == 0)
				{
					break;
				}

				/* send the next list chunk */
				safe_list::iterator it;
				if(pss->last == slaves_data.end())
				{
					it = pss->last = slaves_data.begin();
				}
				else
				{
					it = pss->last;
					it++;
				}
				
				slaves_data.lock();
				/* if there are somthing to send: */
				if(it != slaves_data.end())
				{
					pss->tosend = (char*)(*it).data;
					pss->len = (*it).data_len;
				
					pss->last = it;
				}
				else if(!data_left)
				{
					// send 'End Of Stream'
					pss->tosend = NULL;
					pss->len = (size_t)-1;
				}
				lws_callback_on_writable(wsi);
				slaves_data.unlock();
				break;
			}

		case LWS_CALLBACK_CLOSED: /* a client disconnected */
			lwsl_warn("JCHRONOSS: %d - CONNECTION CLOSING\n", pss->id);
			nb_closed_connections++;
			break;

		/* IGNORED HANDLERS */
		case LWS_CALLBACK_GET_THREAD_ID:
		case LWS_CALLBACK_PROTOCOL_INIT:
			break;
		default:
			break;
	}
	return 0;
}

/** list of protocols handled by our implentation.
 * Here, we consider our own protocol, where requests does not exceed MAX_REQUEST_SIZE bytes of data */
static struct lws_protocols protocols[] = {
	/* protocol_name , protocol_func_ptr, sizeof(custom_data), max_request_size, spcecific protocol id, not used */ 
	{"websockets", ws_callback, sizeof(ws_per_session_data_t), MAX_REQUEST_SIZE, 1, NULL},
	{NULL, 0, 0, 0, 0, NULL}
};

/**
 * initialize the lws interface.
 * This function sets:
 * - frontend_port : the listening port for the frontend
 * - context : the lws-handled structure
 */
void ws_init()
{
	struct lws_context_creation_info info;
	int port = RANDOM_PORT_NUMBER;
	const char * interface = NULL;
	const char * certpath = NULL;
	const char * keypath  = NULL;
	int opt = 0;
	stop = false;

	lws_set_log_level(LWS_LOG_LEVEL, NULL);
	bzero(&info, sizeof(info));
	info.port = port;
	info.iface = interface;
	info.protocols = protocols;
	info.extensions = NULL;
	info.ssl_cert_filepath = certpath;
	info.ssl_private_key_filepath = keypath;
	info.gid = -1;
	info.uid = -1;
	info.max_http_header_pool = 16;
	info.options = opt | LWS_SERVER_OPTION_VALIDATE_UTF8;
	info.timeout_secs = 5;
	info.ssl_cipher_list = "ECDHE-ECDSA-AES256-GCM-SHA384:"
		"ECDHE-RSA-AES256-GCM-SHA384:"
		"DHE-RSA-AES256-GCM-SHA384:"
		"ECDHE-RSA-AES256-SHA384:"
		"HIGH:!aNULL:!eNULL:!EXPORT:"
		"!DES:!MD5:!PSK:!RC4:!HMAC_SHA1:"
		"!SHA1:!DHE-RSA-AES128-GCM-SHA256:"
		"!DHE-RSA-AES128-SHA256:"
		"!AES128-GCM-SHA256:"
		"!AES128-SHA256:"
		"!DHE-RSA-AES256-SHA256:"
		"!AES256-GCM-SHA384:"
		"!AES256-SHA256";

	context = lws_create_context(&info);
	frontend_port = info.port;
	assert(context != NULL);
	
}

/** Start to listen to client requests.
 * When JCHRONOSS wants to stop, the boolean 'stop' is set to true
 */
void ws_start()
{
	data_left = true;
	while(!stop)
	{
		lws_service(context, 50);
	}
}

/**
 * Stop to listen (just before shutdown).
 * This function has to wait for any client to be disconnected before closing (to avoid lossing data).
 * However, the server does not sent 'End Of Stream' to the client to automatically close the connection
 */
void ws_stop()
{
	data_left = false;
	while(nb_closed_connections != nb_connections);
	stop = true;
	lws_context_destroy(context);
}
