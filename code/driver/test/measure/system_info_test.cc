/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <gtest/gtest.h>

#include "insieme/driver/measure/system_info.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace measure {

	using namespace std;

	TEST(SystemInfo, HardwareSpecs) {
		#ifndef USE_PAPI
			std::cout << "Compiled without PAPI support, not testing hardware specs\n";
			return;
		#endif
		SystemInfo sysInfo;

		ASSERT_TRUE(sysInfo.isValid());

		const unsigned numberOfCPUsTotal = sysInfo.getNumberOfCPUsTotal();
		EXPECT_GT(numberOfCPUsTotal, 0);
		EXPECT_LT(numberOfCPUsTotal, 4096);

		const unsigned numberOfCoresPerSocket = sysInfo.getNumberOfCoresPerSocket();
		EXPECT_GT(numberOfCoresPerSocket, 0);
		EXPECT_LE(numberOfCoresPerSocket, 128);

		const unsigned numberOfHWThreadsPerCore = sysInfo.getNumberOfHWThreadsPerCore();
		EXPECT_GT(numberOfHWThreadsPerCore, 0);
		EXPECT_LE(numberOfHWThreadsPerCore, 8);

		const unsigned numberOfSockets = sysInfo.getNumberOfSockets();
		EXPECT_GT(numberOfSockets, 0);
		EXPECT_LE(numberOfSockets, 8);

		// some generic queries
		const string CPUVendor = sysInfo.queryGenericStringSingle("CPU vendor:\\s*(\\w+)");
		EXPECT_FALSE(CPUVendor.empty());

		const string CPUModel = sysInfo.queryGenericStringSingle("CPU model:\\s*(\\w+)");
		EXPECT_FALSE(CPUModel.empty());

		std::cout << "target system info: "
		          << "CPU vendor: " << CPUVendor << ", CPU model: " << CPUModel << ", total logical cpus: " << numberOfCPUsTotal
		          << ", sockets: " << numberOfSockets << ", cores per socket: " << numberOfCoresPerSocket
		          << ", HW threads per core: " << numberOfHWThreadsPerCore << "\n";
	}


} // end namespace measure
} // end namespace driver
} // end namespace insieme
