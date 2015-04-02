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

#ifndef SESSIONLISTENER_H_
#define SESSIONLISTENER_H_

#include "PMClient.h"
#include "MeasurementAnalyzer.h"
#include <string>

using std::string;

/*
 * Instances of this class allow to join / listen to a currently running session
 * on the PowerMeasurement (PM) server.
 *
 * Usage:
 *
 * SessionListener sl("127.0.0.1", 5025)
 *
 * MeasurementAnalyzer * analyzer = sl.startListening();
 *
 * ....
 *
 * sl.stopListening();
 *
 * @see MeasurementAnalyzer * startListening(string dirPath="");
 * @see bool stopListening();
 */
class SessionListener {
public:
	SessionListener(string serverIp, int port);
	virtual ~SessionListener();

	/*
	 * @param dirPath - (optional) path to the directory where to store the log file
	 *
	 * Joins a currently running session on the server and forwards, until stopListening()
	 * gets called or the session is ended, the received measurements to the returned
	 * MeasurementAnalyzer
	 *
	 * return a MeasurementAnalyzer which receives the updates from the server.
	 */
	MeasurementAnalyzer * startListening(string dirPath="");

	/*
	 * Stops the MeasurementAnalyzer returned by startListening() from receiving updates.
	 *
	 * If startListening() hasn't been called before this function has no effect.
	 *
	 * return true if log off from the server was successful, otherwise false.
	 */
	bool stopListening();

private:
	static const string EMPTY;
	static const string PREFIX;

	bool connected;
	PMClient client;
};

#endif /* SESSIONLISTENER_H_ */
