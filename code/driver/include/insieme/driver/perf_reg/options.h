/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#pragma once

#include <vector>
#include <string>

#include <boost/program_options.hpp>


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;
	namespace bpo = boost::program_options;

	struct Options {

		string outputFile;
		vector<string> inputFiles;
		vector<string> keys;
		unsigned precision;
		unsigned minDataPoints;
		unsigned avgOverLastX;
		int graphLimit;
		bool idAsXAxis;
		string failFile;

		bool parseCommandLine(int argc, char **argv);

		static Options &getInstance() {
			static Options instance;
			return instance;
		}

	private:
		Options() {}
		Options(Options const&) = delete;
		Options operator=(Options const&) = delete;

		bpo::variables_map vm;

		static void check_min_dp(unsigned val);
	};

} // namespace perf_reg
} // namespace driver
} // namespace insieme
