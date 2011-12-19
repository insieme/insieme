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
#include "SessionObject.h"
#include "TestClient.h"
#include "MeasurementInfo.h"
#include "MeasurementAnalyzer.h"

TestClient::TestClient() {

}

TestClient::~TestClient() {

}

void TestClient::printResults(MeasurementAnalyzer *a) {
	std::cout << "*****************" << std::endl;
	std::cout << "*****************" << std::endl;
	if (a == NULL) {

		std::cout << "a is NULL" << std::endl;

	} else {

		list<MeasurementInfo> * mList = a->getMeasurementParameterList();

		std::cout << "* 1 *" << std::endl;

		list<MeasurementInfo>::const_iterator it = mList->begin();

		std::cout << "Measured parameters: ";

		for (; it != mList->end(); it++) {

			std::cout << it->title << ", ";

		}
		std::cout << std::endl;
		std::cout << "Measured results:" << a->getNumberOfMeasurements()
				<< std::endl;

		double res;
		int counter;
		a->sumOfColumn(0, 0, W, res, counter);
		std::cout << "Sum of Watt: " << res << " " << counter << std::endl;
		double min, max, avg;
		if (a->calculateMinMaxAvg(0, 0, W, min, max, avg) == 0) {
			std::cout << "Min: " << min << std::endl;
			std::cout << "Max: " << max << std::endl;
			std::cout << "Avg: " << avg << std::endl;
		} else {
			std::cout << "Parameter has not been measured: Watt" << std::endl;
		}
		if (a->calculateMinMaxAvg(0, 0, Vcf, min, max, avg) == 0) {
			std::cout << "Min: " << min << std::endl;
			std::cout << "Max: " << max << std::endl;
			std::cout << "Avg: " << avg << std::endl;
		} else {
			std::cout << "Parameter has not been measured: Vcf" << std::endl;
		}

		delete a;
	}
}

void* TestClient::run() {

	sleep(2);
	string ip = "127.0.0.1";
	string sid = "test";
	SessionObject o2 = SessionObject(sid, ip, 5025);

	SessionObject o = SessionObject(sid, ip, 5025);
	o.start();
	sleep(2);
	o.suspend();
	sleep(10);
	o.resume();
	sleep(3);
	MeasurementAnalyzer * a = o.stop();

	std::cout << "*****************" << std::endl;
	std::cout << "*****************" << std::endl;

	if (a == NULL) {

		std::cout << "a is NULL" << std::endl;

		return 0;

	}

	list<MeasurementInfo> * mList = a->getMeasurementParameterList();

	std::cout << "* 1 *" << std::endl;

	list<MeasurementInfo>::const_iterator it = mList->begin();

	std::cout << "Measured parameters: ";

	for (; it != mList->end(); it++) {

		std::cout << it->title << ", ";

	}
	std::cout << std::endl;
	std::cout << "Measured results:" << a->getNumberOfMeasurements()
			<< std::endl;

	double res;
	int counter;
	a->sumOfColumn(0, 0, W, res, counter);
	std::cout << "Sum of Watt: " << res << " " << counter << std::endl;
	double min, max, avg;
	if (a->calculateMinMaxAvg(0, 0, W, min, max, avg) == 0) {
		std::cout << "Min: " << min << std::endl;
		std::cout << "Max: " << max << std::endl;
		std::cout << "Avg: " << avg << std::endl;
	} else {
		std::cout << "Parameter has not been measured: Watt" << std::endl;
	}
	if (a->calculateMinMaxAvg(0, 0, Vcf, min, max, avg) == 0) {
		std::cout << "Min: " << min << std::endl;
		std::cout << "Max: " << max << std::endl;
		std::cout << "Avg: " << avg << std::endl;
	} else {
		std::cout << "Parameter has not been measured: Vcf" << std::endl;
	}

	delete a;

	return 0;
}
