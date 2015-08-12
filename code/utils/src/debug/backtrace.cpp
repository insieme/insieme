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

#include "insieme/utils/debug/backtrace.h"

#include <sstream>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {
namespace debug {

#if INS_BACKTRACE_AVAILABLE

#include <execinfo.h>
#include <cxxabi.h>

// http://mykospark.net/2009/09/runtime-backtrace-in-c-with-name-demangling/
std::string	demangle(const char* symbol) {
	size_t size;
	int status;
	char temp[128];
	char* demangled;
	//first, try to demangle a c++ name
	if(1 == sscanf(symbol, "%*[^(]%*[^_]%127[^)+]", temp)) {
		if(NULL != (demangled = abi::__cxa_demangle(temp, NULL, &size, &status))) {
			std::string result(demangled);
			free(demangled);
			return result;
		}
	}
	//if that didn't work, try to get a regular c symbol
	if(1 == sscanf(symbol, "%127s", temp)) {
		return temp;
	}
	
	//if all else fails, just return the symbol
	return symbol;
}

std::string getBacktraceString(int offset) {
	const static int MAX_FRAMES = 4096;
	void** buffer = new void*[MAX_FRAMES];
	int numTraced = backtrace(buffer, MAX_FRAMES);
	char** symbols = backtrace_symbols(buffer, numTraced);
	
	std::stringstream ss;
	
	//for(int i=numTraced-1; i>0; --i) {
	for(int i=offset; i<numTraced;++i) {
		ss << format("%4d: %s\n", i-offset+1, demangle(symbols[i]));
	}
	
	free(symbols); // malloc'ed by backtrace_symbols()
	
	return ss.str();
}

#else

std::string getBacktraceString(int offset) {
	return "Backtrace not supported on this compiler\n";
}

#endif

}
}
}
