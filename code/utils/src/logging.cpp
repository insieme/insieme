#include "insieme/utils/logging.h"

#include <stdexcept>

#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>

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
			if(verb != nullptr) { return atoi(verb); }
			return 0;
		}

		boost::regex getFilterFromEnv() {
			auto filter = getenv(LOG_FILTER_ENV);
			if(filter != nullptr) { return boost::regex(filter); }
			return boost::regex(".*");
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
			std::mutex lock;
			unsigned short verbosity;
			boost::regex filter;

			Setup()
				: out(&std::cout), lock(),
				  verbosity(getVerbosityFromEnv()), filter(getFilterFromEnv()) {}

			static Setup& get() {
				static Setup setup;
				return setup;
			}

			static void reload() {
				get().verbosity = getVerbosityFromEnv();
				get().filter = getFilterFromEnv();
			}
		};

	} // end namespace


	// -- the log level instance --

	Level level = getLevelFromEnv();

	// -- log filter handling --

	bool isIncludedInFilter(const char* fullFunctionName) {
		return boost::regex_search(fullFunctionName, Setup::get().filter);
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
		level = getLevelFromEnv();
		Setup::reload();
	}

} // end namespace logger_details
} // end namespace utils
} // end namespace insieme
