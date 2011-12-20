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

//#include "MeasurementInfo.h"

#ifndef CINTERFACE_H_
#define CINTERFACE_H_

/**
 * @param sessionId - id to assign to the session
 * @param serverIp - id of the PM server (e.g. 127.0.0.1)
 * @param serverPort - port of the PM server (standard 5025)
 * @param dirPath - (optional) path to the directory where the logfile shall be stored
 *
 * Creates a new session, must be called before the other
 *
 */
void pmCreateNewSession(char* sessionId, char * serverIp, int serverPort, char * dirPath);

/**
 * @param numargs - number of elements in the given array
 * @param arr - array of measurements parameters.
 *
 * Sets the given values on the Voltech PM1000+ device for
 * the measurement session.
 *
 * NOTE: If Whr is included in the array, the PM1000+ will be
 * set to integration mode.
 *
 * NOTE: pmCreateNewSession() has to be called before.
 */
int pmSetSessionParameters(int numargs, int arr[]);

/**
 * Starts a new session on the PM server.
 *
 * returns 0 if starting the session was successful, otherwise -1.
 *
 * NOTE: pmCreateNewSession() has to be called before.
 */
int pmStartSession();

/**
 * Stops a running session on the PM server.
 *
 * returns 0 if stopping the session was successful, otherwise -1.
 *
 * NOTE: pmCreateNewSession() and pmStartSession() have to be called before.
 */
int pmStopSession();

/**
 * Suspends a running session on the PM server.
 *
 * returns 0 if suspending the session was successful, otherwise -1.
 */
int pmSuspendSession();

/**
 * Resumes a suspended session on the PM server.
 *
 * returns 0 if resuming the session was successful, otherwise -1.
 */
int pmResumeSession();

/**
 *
 * returns the number of measurements received from the server, or -1
 * if an error occurred.
 *
 * NOTE: pmStopSession() has to be called before to receive values.
 */
int pmGetNumberOfMeasurements();

/**
 * @param arr - array to store the measured parameters in
 *
 * returns the number of measured parameters, or -1 if an error occurred,
 */
int pmGetMeasuredParameters(int arr[]);

/**
 * @param firstIndex - index of the value from which the second value shall be subtracted,
 * 						(or 0 for the last available measurement)
 * @param secondIndex - index of the value which shall be subtracted from the first value.
 * 						(or 0 for the first available measurement)
 * @param param - type of measurement for which the calculation shall be performed.
 * @param result - to store the result [ v(firstIndex)-v(secondIndex) ]
 *
 * Calculates v(firstIndex)-v(secondIndex) for the given measurement parameter and stores
 * the result in the given result variable.
 *
 * returns 0 if the calculation was successful, otherwise -1.
 *
 * NOTE: pmStopSession() has to be called before to receive values.
 */
int pmCalculateDiff(unsigned int firstIndex, unsigned int secondIndex, int param, double* result);

/*
 * @param resultFrom - number of starting row from which (incl.)
 * 					   the results shall be taken into account (index starts at 0)
 * @param resultTo -  number of ending row from which (incl.)
 * 					   the results shall be taken into account,
 * 					   (0 alternatively addresses the last result)
 *
 * @param param - type of measurement parameter for which the values shall be returned
 * @param resultSize - where the number of elements in array get stored
 *
 * Retrieves and stores the values from resultFrom to resultTo for the given
 * measurement type to resultArray.
 *
 * returns a pointer/ first array element or NULL if an error occurred.
 */
double * pmGetValues(unsigned int resultFrom, unsigned int resultTo, int param, int* resultSize);

#endif /* CINTERFACE_H_ */
