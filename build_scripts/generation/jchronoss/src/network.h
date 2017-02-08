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

#ifndef __NETWORK_H_
#define __NETWORK_H_

#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <iostream>
#include <sys/types.h>
#include <sys/socket.h>
#include <assert.h>
using namespace std;

static inline int safe_send(int fd, const void* buffer, size_t sz)
{

	size_t nb = 0;
	
	while (nb < sz )
	{
		nb = send(fd, buffer, sz, 0);
		assert(nb == sz || nb == (size_t) -1);
		if(nb == (size_t)-1)
		{
			if(errno == EINTR)
			{
				continue;
			}
			else
			{
				perror("safe_send");
				nb = -1;
				break;
			}
		}
	}


	assert(nb == sz);
	return nb;
}

static inline int safe_recv(int fd, void* buffer, size_t sz)
{
	size_t cur = 0, tmp = 0;
	size_t res = sz;
	while(cur < res)
	{
		tmp = recv (fd, (char*)buffer + cur, res - cur, 0);
		if(tmp == 0)
		{
			res = cur;
			break;
		}
		if(tmp == (size_t) -1)
		{
			if(errno == EINTR)
			{
				continue;
			}
			else
			{
				perror("safe_recv");
				res = -1;
				break;
			}
		}

		cur += tmp;

	}
	assert(res <= sz);
	return res;
}

#endif
