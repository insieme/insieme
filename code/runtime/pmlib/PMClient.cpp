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

#include "PMClient.h"
#include <iostream>
#include "ListenerThread.h"

const string PMClient::UNATTACHED = "";

const string PMClient::DELIMITER = ":";
const string PMClient::START = "001" + DELIMITER;
const string PMClient::STOP = "002" + DELIMITER;
const string PMClient::SUSPEND = "003" + DELIMITER;
const string PMClient::RESUME = "004" + DELIMITER;
const string PMClient::PARAMS = "005" + DELIMITER;
const string PMClient::LISTEN = "006" + DELIMITER;

const string PMClient::RESULTS_CONTINUE = "110" + DELIMITER + "Continue";
const string PMClient::RESULTS_STOP = "111" + DELIMITER + "Stop";

const int PMClient::SERVER_OK = 1;
const int PMClient::SERVER_ERROR = 9;
const int PMClient::SERVER_HEADER = 11;
const int PMClient::SERVER_VALUE = 12;

PMClient::PMClient(string & ip, int port, string & sessionId) :
	TCPClient(ip, port), sid(sessionId), key(UNATTACHED), listener(NULL) {
}

PMClient::~PMClient() {

}

bool PMClient::receiveAndDecompose(int & code, string & msg) {

	try {

		string completeMsg;
		if (this->rcvMsg(completeMsg) == -1) {
			code = -1;
			return false;
		}

		//TODO std::cout << "Client received: " << completeMsg << std::endl;

		size_t pos = completeMsg.find(DELIMITER);

		if (pos != string::npos) {

			string prefix = completeMsg.substr(0, pos);

			code = atoi(prefix.c_str());

			msg = completeMsg.erase(0, pos + 1);

			return true;

		} else {
			return false;
		}

	} catch (std::exception & e) {
		return false;
	}

}

int PMClient::sendMsg(string& prefix, string & msg) {

	string m = prefix + msg;
	return this->sndMsg(m);
}

bool PMClient::startSession() {

	if (key.compare(UNATTACHED) != 0) {

		return false;
	} else {

		this->Connect();

		this->sendMsg((string&) START, this->sid);

		int code;
		string msg;
		bool ret = this->receiveAndDecompose(code, msg);

		if (ret) {
			this->key = msg;
		}

		this->Disconnect();

		return ret;

	}

}

MeasurementAnalyzer * PMClient::stopSession(string * dirPath)
		throw (std::invalid_argument) {

	MeasurementAnalyzer * m;

	EthernetLogDisposer * p;

	if (dirPath != NULL) {
		p = new PMFileParser(this->sid, *dirPath);
		m = new MeasurementAnalyzer((PMFileParser*) p);
	} else {
		p = new PMMemoryParser();
		m = new MeasurementAnalyzer((PMMemoryParser*) p);
	}

	bool b = true;
	if (retrieveResults((string &) STOP, this->key, dirPath, p, b)) {

		return m;
	} else {
		delete m;
		return NULL;
	}

}

bool PMClient::connectToSession(EthernetLogDisposer * p, string * dirPath) {

	if (listener == NULL) {

		listener = new ListenerThread(this, dirPath, p);
		listener->start();

		return true;
	}

	return false;

}

bool PMClient::disconnectFromSession() {

	if (listener != NULL) {

		string msg = "stop";

		((ListenerThread*) listener)->stop();

		if (this->sendMsg((string &) PMClient::STOP, msg)) {
			listener->join();
			return true;
		}

	}
	return false;

}

bool PMClient::retrieveResults(string & command, string msg, string * dirPath,
		EthernetLogDisposer * p, bool & run) {

	this->Connect();

	this->sendMsg(command, msg);

	int code = 0;

	this->receiveAndDecompose(code, msg);

	if (code == SERVER_OK) {

		msg = RESULTS_CONTINUE;
		this->sndMsg(msg);
		this->receiveAndDecompose(code, msg);

		if (code == SERVER_HEADER) {

			size_t splitPos = msg.find(";");

			time_t t = atoi(msg.substr(0, splitPos).c_str());

			splitPos++;
			string header = msg.substr(splitPos, msg.size() - splitPos);
			header.replace(0, 6, "");
			header += "\n";

			try {

				p->addHeader(t, header);

				bool first = true;

				while ((code == 12 || first) && run) {
					if (first) {
						first = false;
					} else {

						p->addValueLine(msg);
					}

					this->sndMsg(RESULTS_CONTINUE);
					this->receiveAndDecompose(code, msg);

				}
				// TODO std::cout << "Client disconnects." << std::endl;
				this->Disconnect();

				return true;

			} catch (std::invalid_argument & e) {

				this->sndMsg(RESULTS_STOP);
			}

		}
	}
	this->Disconnect();

	return false;

}

bool PMClient::sendString(string & prefix, string & str) {

	this->Connect();

	this->sendMsg(prefix, str);

	string msg;
	int code;

	this->receiveAndDecompose(code, msg);

	this->Disconnect();

	return (code == PMClient::SERVER_OK);
}

bool PMClient::suspendSession() {

	return sendString((string &) SUSPEND, this->key);

}

bool PMClient::resumeSession() {

	return sendString((string &) RESUME, this->key);

}

void PMClient::setMeasurementParameters(unsigned int numargs, MEASUREMENT_PARAMS parameterArray[]) {


	stringstream ss(stringstream::in | stringstream::out);
	// Checking all column numbers and putting them into a set
	for (unsigned int i = 0; i < numargs; i++) {

		unsigned int arg = parameterArray[i];

		ss << arg;

	}

	string msg = PARAMS + ss.str();

	this->Connect();

	this->sndMsg(msg);
	this->rcvMsg(msg);

	this->Disconnect();

}
