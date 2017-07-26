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

#include <iostream>
#include <mutex>

#include "insieme/utils/abstraction.h"

#include "insieme/common/env_vars.h"

namespace insieme {
namespace utils {
namespace log {

	/**
	 * An enumeration of the supported log levels.
	 * Each level includes all the messages of the higher levels.
	 * E.g. all errors are also printed in case the mode is set to warning.
	 */
	enum Level { DEBUG, INFO, WARNING, ERROR, FATAL };

} // end namespace log

/**
 * Another namespace to avoid the import of those symbols
 * into other namespaces.
 */
namespace logger_details {

	/**
	 * Temporary object used to wrap the log stream. This object is responsible to
	 * collect logs and flush the stream once the object is deallocated.
	 *
	 * A lock is used to maintain exclusivity of logs, in case of multi-threaded application
	 * the logger guarantees mutual exclusion between threads using the stream.
	 */
	class SynchronizedStream {

		// the underlying stream
		std::ostream& out;

		// a reference to a synchronizing mutex
		std::recursive_mutex& mutex;

		// a flag indicating that this instance is the current owner of the stream
		bool owner;

	public:

		SynchronizedStream(std::ostream& out, std::recursive_mutex& mutex) : out(out), mutex(mutex), owner(true) {
			mutex.lock(); // acquire exclusive access to the output stream
		}

		SynchronizedStream(SynchronizedStream&& other) : out(other.out), mutex(other.mutex), owner(true) {
			other.owner = false; // pass on access to output stream
		}

		~SynchronizedStream() {
			if (owner) {
				out << std::endl;
				out.flush();
				mutex.unlock();	// release access to output stream
			}
		}

		std::ostream& getStream() const {
			return out;
		}
	};

	/**
	 * Obtains a properly set up stream for the printing of a message of the given
	 * level, in the given file and line.
	 */
	SynchronizedStream getLogStreamFor(log::Level level, const char* file, int line);

	/**
	 * Determines whether the given function name is covered by the current function
	 * name filter setup.
	 */
	bool isIncludedInFilter(const char* fullFunctionName);

	/**
	 * Determines the current log level.
	 */
	log::Level getLogLevel();

	/**
	 * Determines the current verbosity level.
	 */
	unsigned short getVerbosityLevel();

	/**
	 * Causes the logging system to re-load its configuration from the environment
	 * variables.
	 */
	void reloadConfiguration();

} // end namespace logger_details
} // end namespace utils
} // end namespace insieme

/**
 * Import logging namespace into all translation units to access
 * LOG-Level enumeration without qualification.
 */
using namespace insieme::utils::log;

#define LOG(LEVEL)                                                                                                                                             \
	if(insieme::utils::logger_details::getLogLevel() > LEVEL || !insieme::utils::logger_details::isIncludedInFilter(FUNCTION_SIGNATURE)) {/* nothing */               \
	} else                                                                                                                                                     \
	insieme::utils::logger_details::getLogStreamFor(LEVEL, __FILE__, __LINE__).getStream()

#define VLOG(VerbLevel)                                                                                                                                        \
	if(VerbLevel > insieme::utils::logger_details::getVerbosityLevel()) {                                                                                      \
	} else                                                                                                                                                     \
	LOG(DEBUG)

#define VLOG_IS_ON(VerbLevel)                                                                                                                                  \
	(VerbLevel <= insieme::utils::logger_details::getVerbosityLevel() && insieme::utils::logger_details::isIncludedInFilter(FUNCTION_SIGNATURE))
