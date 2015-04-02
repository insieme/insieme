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
#include <string>
#include <csignal>

using std::string;

static SessionObject * so = NULL;
static MeasurementAnalyzer* analyzer = NULL;

extern "C" void pmCreateNewSession(char* sessionId, char * serverIp, int serverPort,
		char * dirPath) {

	string sid = string(sessionId);
	string ip = string(serverIp);
	string path;
	if (dirPath != NULL) {
		path = string(dirPath);
	} else {
		path = string("");
	}

	if (so != NULL) {
		delete so;
	}
	so = PMInterface::createSessionObject(sid, ip, serverPort, path);
	
	struct sigaction act;
	act.sa_handler = SIG_IGN;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	sigaction(SIGPIPE, &act, NULL);
}

extern "C" int pmSetSessionParameters(int numargs, MEASUREMENT_PARAMS arr[]) {

	if (so != NULL) {

		so->setParameters(numargs, arr);

		return 0;

	} else {
		return -1;
	}

}

extern "C" int pmStartSession() {

	if (so != NULL && so->start()) {
		return 0;
	} else {
		return -1;
	}
}

extern "C" int pmStopSession() {

	if (so != NULL) {

		if (analyzer != NULL) {
			delete analyzer;
		}

		analyzer = so->stop();

		if (analyzer != NULL) {
			return 0;
		}

	}

	return -1;

}

extern "C" int pmSuspendSession() {

	if (so != NULL && so->suspend()) {
		return 0;
	} else {
		return -1;
	}
}

extern "C" int pmResumeSession() {

	if (so != NULL && so->resume()) {
		return 0;
	} else {
		return -1;
	}

}

extern "C" int pmGetNumberOfMeasurements() {

	if (analyzer != NULL) {

		return analyzer->getNumberOfMeasurements();

	} else {
		return -1;
	}

}

extern "C" int pmGetMeasuredParameters(MEASUREMENT_PARAMS arr[]) {

	if (analyzer != NULL) {

		list<MeasurementInfo>* l = analyzer->getMeasurementParameterList();

		arr = (MEASUREMENT_PARAMS*) malloc(
				sizeof(MEASUREMENT_PARAMS) * l->size());

		list<MeasurementInfo>::iterator it = l->begin();

		int i = 0;
		for (; it != l->end(); it++) {
			arr[i] = it->param;
			i++;
		}

		return 0;

	} else {
		return -1;
	}

}

extern "C" double * pmGetValues(unsigned int resultFrom, unsigned int resultTo,
		MEASUREMENT_PARAMS param, int* resultSize) {

	if (analyzer != NULL) {

		list<double>* resultList = new list<double> ();

		if (analyzer->getValueList(resultFrom, resultTo, param, *resultList)
				== 0) {

			list<double>::iterator it = resultList->begin();

			*resultSize = resultList->size();

			double * arr = (double*) malloc(sizeof(double) * (*resultSize));

			int i = 0;
			for (; it != resultList->end(); it++) {
				arr[i] = (*it);
				i++;
			}

			delete resultList;
			return arr;
		}

	}

	return NULL;
}

extern "C" int pmCalculateDiff(unsigned int firstIndex, unsigned int secondIndex,
		MEASUREMENT_PARAMS param, double* result) {

	if (analyzer != NULL) {

		return analyzer->calculateDiff(param, firstIndex, secondIndex, result);

	}

	return -1;
}

