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

#include "PMInterface.h"

/*
 * @param sessionId - session id to use for the session handled by the SessionObject
 * @param serverIP - ip of the PowerMeasurement (PM) server
 * @param serverPort - port of the PowerMeasurement (PM) server
 * @param dirPath - (optional) path to the directory where to store the measurements
 * received from the server
 *
 * Creates and returns a new SessionObject which allows to start, stop, suspend and resume
 * a measurement session on the PowerMeasurement (PM) server
 */
SessionObject * PMInterface::createSessionObject(string & sessionId,
		string & serverIp, int serverPort, string dirPath) {

	return new SessionObject(sessionId, serverIp, serverPort, dirPath);

}

/*
 * @param serverIp - ip of the PowerMeasurement (PM) server
 * @param port - port of the PowerMeasurement (PM) server
 *
 * Creates and returns a new SessionListener which allows to listen to a currently
 * running measurement session on the PowerMeasurement (PM) server.
 */
SessionListener * PMInterface::createSessionListener(string serverIp, int port) {

	return new SessionListener(serverIp, port);

}

PMInterface::PMInterface() {

}

PMInterface::~PMInterface() {

}
