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
 *
 */
#include "insieme/utils/logging.h"

#include <stdexcept>
#include <regex>

#include <boost/algorithm/string.hpp>

#include "insieme/utils/assert.h"

namespace insieme {
namespace utils {
namespace logger_details {

	#define LOG_DEFAULT ERROR

	namespace {

		struct LoggingLevelNotDefined : public std::runtime_error {
			LoggingLevelNotDefined(const std::string& message) : std::runtime_error(message) {}
		};

		std::string loggingLevelToStr(const log::Level& level) {
			switch(level) {
			case DEBUG: return "DEBUG";
			case INFO: return "INFO ";
			case WARNING: return "WARN ";
			case ERROR: return "ERROR";
			case FATAL: return "FATAL";
			default: assert_fail(); return "UNKNOWN";
			}
		}

		log::Level loggingLevelFromStr(std::string level) {
			boost::to_upper(level);
			if(level.empty()) { return LOG_DEFAULT; }
			if(level == "DEBUG") { return DEBUG; }
			if(level == "INFO") { return INFO; }
			if(level == "WARNING") { return WARNING; }
			if(level == "ERROR") { return ERROR; }
			if(level == "FATAL") { return FATAL; }
			std::ostringstream os;
			os << "Logging level '" << level << "' not valid. Available logging levels are: 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'" << std::endl;
			throw LoggingLevelNotDefined(os.str());
		}

		log::Level getLevelFromEnv() {
			auto lvl = getenv(LOG_LEVEL_ENV);
			if(lvl != nullptr) { return loggingLevelFromStr(lvl); }
			return LOG_DEFAULT;
		}

		unsigned short getVerbosityFromEnv() {
			auto verb = getenv(LOG_VERBOSITY_ENV);
			if(verb != nullptr) { return (unsigned short)atoi(verb); }
			return 0;
		}

		std::regex getFilterFromEnv() {
			auto filter = getenv(LOG_FILTER_ENV);
			if(filter != nullptr) { return std::regex(filter); }
			return std::regex(".*");
		}


		std::string formatFileName(const std::string& filename) {
			// Cut out the entire path and prints the file name
			size_t pos = filename.find_last_of('/');
			if(pos == std::string::npos) {
				return filename;
			} else {
				return filename.substr(pos + 1);
			}
		}

		struct Setup {

			std::ostream* out;
			std::recursive_mutex lock;
			Level level;
			unsigned short verbosity;
			std::regex filter;

			Setup()
				: out(&std::cout), lock(),
				  level(getLevelFromEnv()), verbosity(getVerbosityFromEnv()), filter(getFilterFromEnv()) {}

			static Setup& get() {
				static Setup setup;
				return setup;
			}

			static void reload() {
				get().level = getLevelFromEnv();
				get().verbosity = getVerbosityFromEnv();
				get().filter = getFilterFromEnv();
			}
		};

	} // end namespace

	// -- log filter handling --

	bool isIncludedInFilter(const char* fullFunctionName) {
		return std::regex_search(fullFunctionName, Setup::get().filter);
	}

	Level getLogLevel() {
		return Setup::get().level;
	}

	unsigned short getVerbosityLevel() {
		return Setup::get().verbosity;
	}

	// -- log stream management --

	SynchronizedStream getLogStreamFor(log::Level level, const char* file, int line) {
		auto& setup = Setup::get();
		SynchronizedStream out(*setup.out, setup.lock);
		out.getStream() << loggingLevelToStr(level) << " ";
		out.getStream() << formatFileName(file) << ":" << line << " - ";
		return out;
	}

	void reloadConfiguration() {
		Setup::reload();
	}

} // end namespace logger_details
} // end namespace utils
} // end namespace insieme

