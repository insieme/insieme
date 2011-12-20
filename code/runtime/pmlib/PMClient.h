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

#ifndef PMCLIENT_H_
#define PMCLIENT_H_

#include "TCPClient.h"
#include <string>
#include <exception>
#include <stdexcept>
#include <sstream>
#include "MeasurementInfo.h"
#include "MeasurementAnalyzer.h"
#include "PMFileParser.h"
#include "PMMemoryParser.h"
#include "SThread.h"

using std::string;
using std::stringstream;

class PMClient: protected TCPClient {
public:
	PMClient(string & ip, int port, string & sessionId);
	virtual ~PMClient();

	void setMeasurementParameters(unsigned int numargs, MEASUREMENT_PARAMS parameterArray[]);

	bool startSession();

	MeasurementAnalyzer * stopSession(string * dirPath = NULL)
			throw (std::invalid_argument);

	bool suspendSession();

	bool resumeSession();



	bool connectToSession(EthernetLogDisposer * p, string * dirPath = NULL);

	bool disconnectFromSession();

	bool retrieveResults(string & command, string msg, string * dirPath,
			EthernetLogDisposer * m, bool & run);

	const static string UNATTACHED;
	const static string DELIMITER;
	const static string START;
	const static string STOP;
	const static string SUSPEND;
	const static string RESUME;
	const static string PARAMS;
	const static string LISTEN;

	const static string RESULTS_CONTINUE;

	const static string RESULTS_STOP;

	const static int SERVER_OK;
	const static int SERVER_ERROR;
	const static int SERVER_HEADER;
	const static int SERVER_VALUE;

protected:

	/*
	 * @param code - to store the received code
	 * @param msg - to store the message after ther code
	 *
	 * Receives a message from the server. If it is of the
	 * format "000:string" it gets split and stored into
	 * the given parameters.
	 *
	 * return true if receiving and splitting was successful,
	 * otherwise false.
	 *
	 */
	bool receiveAndDecompose(int & code, string & msg);

	/*
	 * @param prefix
	 * @param msg
	 *
	 * Concatenates the two given string and sends the
	 * the result
	 *
	 * return -1 if an error occurred, otherwise the number
	 * of sent bytes
	 */
	int sendMsg(string& prefix, string & msg);

	/*
	 * @param prefix - prefix to use for the message
	 * @param str - to use for the message
	 *
	 * Sends a message containing "prefix:string"
	 * e.g. if prefix is SUSPEND and str is the key
	 * the message would look like: "003:sid_012345"
	 *
	 * returns false if the answer of the server signalizes
	 * an error, otherwise true
	 */
	bool sendString(string & prefix, string & str);

private:
	string sid;
	string key;
	string dirPath;
	SThread * listener;
};

#endif /* PMCLIENT_H_ */
