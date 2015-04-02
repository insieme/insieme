/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include "TCPClient.h"
#include <string.h>

int TCPClient::BUFFERSIZE = 4086;

#ifdef _WIN32
int TCPClient::instanceCounter = 0;

ReaderWriter TCPClient::LOCK = ReaderWriter();

TCPClient::TCPClient(string ip, int port) : ip(ip), port(port) {
	sock = INVALID_SOCKET;

	TCPClient::LOCK.writeLock();
	TCPClient::instanceCounter++;
	TCPClient::LOCK.writeUnlock();
}

TCPClient::~TCPClient(void) {

	TCPClient::LOCK.writeLock();
	TCPClient::instanceCounter--;

	if(instanceCounter <= 0) {
		WSACleanup();
	}
	TCPClient::LOCK.writeUnlock();

}

int TCPClient::Connect() {

	if(sock == INVALID_SOCKET) {

		long rc;

		SOCKADDR_IN addr;

		// start Winsock
		rc=startWinsock();
		if(rc!=0) {
			return -1;
		}

		// Create socket 
		sock=socket(AF_INET,SOCK_STREAM,0);
		if(sock==INVALID_SOCKET) {
			return -1;
		}

		// Connect to server
		memset(&addr,0,sizeof(SOCKADDR_IN));
		addr.sin_family=AF_INET;
		addr.sin_port=htons(port);
		addr.sin_addr.s_addr=inet_addr(ip.c_str());

		rc=connect(sock, ((SOCKADDR*)&addr), sizeof(SOCKADDR));

		if(rc==SOCKET_ERROR) {
			return -1;
		}
	}

	return 0;

}

int TCPClient::Disconnect() {
	if(sock != INVALID_SOCKET) {

		if(closesocket(sock) == 0) {
			sock = INVALID_SOCKET;
			return 0;
		}

		return -1;

	} else {
		return 0;
	}
}

int TCPClient::startWinsock() {

	int ret;
	TCPClient::LOCK.readLock();

	if(TCPClient::instanceCounter == 1) {
		WSADATA wsa;
		ret = WSAStartup(MAKEWORD(2,0),&wsa);
	}
	TCPClient::LOCK.readUnlock();

	return ret;
}

int TCPClient::sndMsg(string msg) {

	return send(sock,msg.c_str(), msg.size(),0);

}

int TCPClient::rcvMsg(string & msg) {

	char buf[TCPClient::BUFFERSIZE];
	long rc=recv(sock,buf,TCPClient::BUFFERSIZE,0);

	buf[rc]='\0';

	msg = buf;

	return rc;
}

#else

TCPClient::TCPClient(string ip, int port) :
	ip(ip), port(port), sockFileDescr(-1) {
}

TCPClient::~TCPClient(void) {

}

int TCPClient::Connect() {

	if (sockFileDescr < 0) {

		sockFileDescr = socket(AF_INET, SOCK_STREAM, 0);

		struct hostent * server;
		struct sockaddr_in serv_addr;

		if (sockFileDescr < 0) {
			return -1;
		}

		server = gethostbyname(this->ip.c_str());
		if (server == NULL) {
			return -1;
		}
		bzero((char *) &serv_addr, sizeof(serv_addr));
		serv_addr.sin_family = AF_INET;
		bcopy((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr,
				server->h_length);
		serv_addr.sin_port = htons(this->port);
		if (connect(sockFileDescr, (struct sockaddr *) &serv_addr,
				sizeof(serv_addr)) < 0) {
			return -1;
		}
	}
	return 0;

}

int TCPClient::Disconnect() {
	if (sockFileDescr >= -1) {
		int ret;

		shutdown(sockFileDescr, SHUT_RDWR);

		if ((ret = close(sockFileDescr)) == 0) {

			sockFileDescr = -1;
		}

		return ret;

	}
	return -1;

}

int TCPClient::sndMsg(string msg) {

	return send(sockFileDescr, msg.c_str(), msg.size(), 0);

}

int TCPClient::rcvMsg(string & msg) {

	char buf[TCPClient::BUFFERSIZE];
	bzero(buf, TCPClient::BUFFERSIZE);

	int bytes_read = recv(sockFileDescr, buf, TCPClient::BUFFERSIZE, 0);

	if (bytes_read > 0) {
		buf[bytes_read] = '\0';

		msg = buf;
	}

	return bytes_read;
}

#endif
