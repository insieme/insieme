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

#include "EthernetReceiver.h"

#ifdef _WIN32

EthernetReceiver::EthernetReceiver(EthernetLogDisposer * disposer, string ip, int port, unsigned int intervalTimeMS): intervalTime(intervalTimeMS), keepRunning(false) {

	this->parser = disposer;
	this->client = new TCPClient(ip, port);

}
#else

EthernetReceiver::EthernetReceiver(EthernetLogDisposer * disposer, string ip,
		int port, unsigned int intervalTimeMS) :
	keepRunning(false) {

	this->disposer = disposer;
	this->client = new TCPClient(ip, port);

	int sec = intervalTimeMS / 1000;
	this->waitingTime.tv_sec = sec;
	this->waitingTime.tv_nsec = ((intervalTimeMS - (sec * 1000)) * 1000000);

}

#endif

EthernetReceiver::~EthernetReceiver(void) {

	delete client;
}

void EthernetReceiver::stop() {

	this->keepRunning = false;

}

void* EthernetReceiver::run() {

	try {

		if (!keepRunning) {

			this->client->Connect();

			this->keepRunning = true;

			string msg;

			msg = ":FRF?\n";
			client->sndMsg(msg);
			client->rcvMsg(msg);

			time_t t = time(NULL);
			disposer->addHeader(t, msg);

			msg = ":DSE 2\n";
			client->sndMsg(msg);
			client->rcvMsg(msg);

			while (this->keepRunning) {

				while (true) {
					msg = ":DSR?\n";
					client->sndMsg(msg);
					client->rcvMsg(msg);
					if (msg.compare("2\n") == 0) {
						break;
					}

				}

				msg = ":FRD?\n";
				client->sndMsg(msg);
				client->rcvMsg(msg);

				disposer->addValueLine(msg);

#ifdef _WIN32
				Sleep(this->intervalTime);
#else
				nanosleep(&waitingTime, &backup);
#endif

			}

			client->sndMsg(":REM:OFF\n");

			client->Disconnect();
		}

	} catch (std::exception * e) {

		std::cerr << e->what() << std::endl;
	}

	return reinterpret_cast<void*> (0);

}

EthernetLogDisposer * EthernetReceiver::getEthernetLogDisposer() {
	return this->disposer;
}
