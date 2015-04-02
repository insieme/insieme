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

#ifndef SESSIONOBJECT_H_
#define SESSIONOBJECT_H_

#include "MeasurementAnalyzer.h"
#include "PMClient.h"
#include "MeasurementInfo.h"
#include <string>
#include <stdexcept>

using std::string;

/*
 * Instances of this class allow to start, stop, suspend and resume
 * measurement sessions on the PowerMeasurement (PM) server.
 *
 * Usage:
 *
 * SessionObject so(sid, ip, port);
 *
 * .... some code ....
 *
 * so.start(); // start a new session
 *
 * ... code to measure ....
 *
 * so.suspend():
 * ... code which shouldn't be measured ....
 * so. resume();
 *
 * .... code to measure ....
 *
 * MeasurementAnalyzer * analyzer = so.stop();
 *
 * ... code to handle measurements ....
 *
 */
class SessionObject {
public:
	/*
	 * @param sessionId - name of this session
	 * @param dirPath - (optional) path to the directory where the log file containing
	 *                  the received measurements shall be stored
	 *
	 * Creates a new instance of this class using the given parameters.
	 *
	 * If dirPath has been set, on calling stop() measured results are stored locally inside
	 * that directory.
	 *
	 * @Throws invalid_argument if the given dirPath is not valid.
	 */
	SessionObject(string & sessionId, string & serverIp, int serverPort,
			string dirPath = "") throw (std::invalid_argument);

	virtual ~SessionObject();

	/*
	 * Starts a new measurement session on the server.
	 *
	 * After the call of this method, the code which should be measured must be included.
	 *
	 * To end the measurement session call stop()
	 *
	 * returns true if the server accepted the new session, otherwise false.
	 */
	bool start();

	/*
	 *
	 * Stops the current measurement session, and stores the log file to dirpath
	 * if it was defined on construction of this instance.
	 *
	 * Call this method after the code region to measure.
	 *
	 * If a valid dirpath was specified this function can take some time to complete, depending on
	 * the number of measurements to process.
	 *
	 * returns a MeasurementAnalyzer which allows to access the measurement results, or NULL
	 * if an error occurred.
	 *
	 */
	MeasurementAnalyzer * stop();

	/*
	 * Calling this function will cause the Server to pause logging until
	 * resume() is called.
	 *
	 * returns true if the server suspended the session, otherwise false
	 */
	bool suspend();

	/*
	 * Calling this function will cause the Server to resume logging.
	 *
	 * If suspend() hasn't been called before, calling this method will have no effect.
	 *
	 * returns true if the server resumed the session, otherwise false
	 */
	bool resume();


	/**
	 * @param numargs - number of elements in the given array
	 * @param arr - array of measurements parameters.
	 *
	 * Sets the given values on the Voltech PM1000+ device for
	 * the measurement session.
	 *
	 * NOTE: If Whr is included in the array, the PM1000+ will be
	 * set to integration mode.
	 */
	void setParameters(int numargs, MEASUREMENT_PARAMS arr[]);

private:
	string sessionId;
	string dirPath;
	bool started;
	bool suspended;
	PMClient client;

};

#endif /* SESSIONOBJECT_H_ */
