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

#include "insieme/utils/debug/backtrace.h"

#include <sstream>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {
namespace debug {

#if defined(INS_DEBUG) && defined(__GNUC__) && !defined(__clang__)

#include <execinfo.h>
#include <cxxabi.h>

	// http://mykospark.net/2009/09/runtime-backtrace-in-c-with-name-demangling/
	std::string demangle(const char* symbol) {
		size_t size;
		int status;
		char temp[128];
		char* demangled;
		// first, try to demangle a c++ name
		if(1 == sscanf(symbol, "%*[^(]%*[^_]%127[^)+]", temp)) {
			if(NULL != (demangled = abi::__cxa_demangle(temp, NULL, &size, &status))) {
				std::string result(demangled);
				free(demangled);
				return result;
			}
		}
		// if that didn't work, try to get a regular c symbol
		if(1 == sscanf(symbol, "%127s", temp)) { return temp; }

		// if all else fails, just return the symbol
		return symbol;
	}

	std::string getBacktraceString(int offset) {
		const static int MAX_FRAMES = 4096;
		void** buffer = new void*[MAX_FRAMES];
		int numTraced = backtrace(buffer, MAX_FRAMES);
		char** symbols = backtrace_symbols(buffer, numTraced);

		std::stringstream ss;

		// for(int i=numTraced-1; i>0; --i) {
		for(int i = offset; i < numTraced; ++i) {
			ss << format("%4d: %s\n", i - offset + 1, demangle(symbols[i]));
		}

		free(symbols); // malloc'ed by backtrace_symbols()
		delete[] buffer;

		return ss.str();
	}

	#else

	std::string getBacktraceString(int /*offset*/) {
		return "Backtrace not supported on this compiler\n";
	}

	#endif
}
}
}
