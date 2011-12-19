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

#ifndef TCPCLIENT_H_
#define TCPCLIENT_H_



#include <string>


using std::string;

#ifdef _WIN32
#pragma warning( disable : 4290 )
#include <Windows.h>
#include "ReaderWriter.h"
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif

/*
 * Instances of this class can be used to connect
 * to a TCP server and send/receive messages.
 *
 */
class TCPClient{
public:
	/*
	 * @param ip - of the server, format 255.255.255.255
	 */
	TCPClient(string ip, int port);
	virtual ~TCPClient(void);

	/*
	 * Opens a new connection to the server using ip and port.
	 *
	 * returns 0 if connecting was successful or the client 
	 * was already connected, or -1 if an error occured.
	 */
	int Connect();

	/*
	 * Closes the current connection.
	 *
	 * return 0 if closing was successful, or -1 if an
	 * error occured.
	 */
	int Disconnect();
	
	/*
	 * @param msg - to store the message
	 *
	 * (Blocking / no timeout) Receives and stores a message to the given string.
	 *
	 * Max number of received chars within one message is defined by
	 * TCPClient::BUFFERSIZE
	 *
	 * returns the number of received bytes or -1 if an error occurred
	 */
	int rcvMsg(string& msg);

	/*
	 * @param msg - message to send.
	 *
	 * Sends the given message to the server
	 *
	 * returns the number of sent bytes or -1 if an error occurred
	 */
	int sndMsg(string msg);



protected:

	/*
	* Max. number of chars to receive from server 
	* within a single message.
	*/
	static int BUFFERSIZE;

#ifdef _WIN32
	/*
	 * initiates use of the Winsock DLL
	 *
	 * returns 0 on success, otherwise a different int. 
	 */
	int startWinsock(void);
#endif


	string ip;
	int port;


private:
#ifdef _WIN32
	static int instanceCounter;
	static ReaderWriter LOCK;
	SOCKET sock;
#else
	int sockFileDescr;

#endif

};

#endif
