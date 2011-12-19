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

#ifndef _ETHERNETRECV_H_
#define _ETHERNETRECV_H_

#include "SThread.h"
#include "EthernetLogDisposer.h"
#include "TCPClient.h"

#ifndef _WIN32

	#include <time.h>

#endif


/*
 * EthernetReceiver is a subclass of SThread.
 *
 * Instances of this class can be used to forward the logging informations,
 * to an EthernetLogDisposer,either received directly from the Voltech PM1000+
 * Power Analyzer, or from the (Windows) logging server.
 */
class EthernetReceiver : public SThread{

public:

	EthernetLogDisposer * getEthernetLogDisposer();

	/*
	 * @param disposer - to pass the received logresults to.
	 * @param ip - of the server/Power Analyzer
	 * @param port -of the server/Power Analyzer (Power Analyzer uses 5025)
	 * @param intervalTimeMS - time interval at which the logging messages are requested
	 *
	 * Creates a new instance of this class using the given parameters.
	 */
	EthernetReceiver(EthernetLogDisposer * disposer, string ip, int port, unsigned int intervalTimeMS);
	virtual ~EthernetReceiver(void);
	void stop();

	virtual void* run();



protected:

	EthernetLogDisposer* disposer;
	TCPClient * client;

	bool isRunning();

#ifdef _WIN32

	unsigned int intervalTime;

#else
	struct timespec waitingTime;
	struct timespec backup;
#endif
	

private:
	bool keepRunning;


	
};

#endif
