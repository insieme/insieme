/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
		unsigned plotLastX;
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
